# --- 0. 載入必要的套件 ---
library(ggplot2)
library(dplyr)   # 用於 bind_rows，處理 data.frame 合併
library(purrr)   
library(readr)  

# ===================================================================
#           PART 1: 核心模擬函式
# ===================================================================

#' 運行一次多階段滲碳模擬
#'
#' @param recipe_stages 一個 list，定義了每個製程階段的參數。
#' @param L 鋼材厚度 (m)。
#' @param C_o 初始碳濃度 (wt%)。
#' @param Nx 空間網格點數量。
#' @return 一個包含 'final_profile' 和 'plot_data' 的 list。


run_simulation <- function(recipe_stages, L=0.005, C_o=0.2, Nx=101) {
  # 阿瑞尼斯方程式參數
  D0 <- 2.3e-5      # m^2/s
  Qd <- 148000      # J/mol
  R_const <- 8.314  # J/(mol·K)

  # --- 1a. 物理參數 ---
  calculate_D <- function(temp_C) { D0 * exp(-Qd / (R_const * (temp_C + 273.15))) }
  
  # --- 1b. 數值計算參數 ---
  total_time_s <- sum(sapply(recipe_stages, function(s) s$duration)) * 3600
  max_temp <- max(sapply(recipe_stages, function(s) s$temp_C))
  D_max <- calculate_D(max_temp)
  x <- seq(0, L, length.out = Nx); dx <- x[2] - x[1]
  dt <- 0.9 * (dx^2) / (2 * D_max); Nt <- as.integer(total_time_s / dt)
  
  # --- 1c. 初始化 ---
  C <- rep(C_o, Nx)
  plot_df <- data.frame(
    depth_mm = x * 1000, concentration = C, time_hr = 0, stage = "Initial"
  )
  stage_end_times_s <- cumsum(sapply(recipe_stages, function(s) s$duration * 3600))
  
  # --- 1d. 時間推進主迴圈 ---
  current_stage_index <- 1
  
  for (n in 1:Nt) {
    # 核心 FDM 計算
    current_stage <- recipe_stages[[current_stage_index]]
    current_D <- calculate_D(current_stage$temp_C)
    current_Cs <- current_stage$surface_C
    C_new <- C; lam <- current_D * dt / dx^2
    C_new[2:(Nx - 1)] <- C[2:(Nx - 1)] + lam * (C[3:Nx] - 2 * C[2:(Nx - 1)] + C[1:(Nx - 2)])
    C_new[1] <- current_Cs; C_new[Nx] <- C_new[Nx - 1]
    C <- C_new
    
    # 在計算完成後，檢查是否到達階段末尾
    if (current_stage_index < length(recipe_stages)) { # 只檢查到倒數第二個階段
      if ((n * dt) >= stage_end_times_s[current_stage_index]) {
        stage_info <- recipe_stages[[current_stage_index]] #取出當前這個即將結束的階段的詳細說明名字
        temp_df <- data.frame( 
          depth_mm = x * 1000, concentration = C,
          time_hr = round(stage_end_times_s[current_stage_index] / 3600, 1),
          stage = sprintf("End of %s", stage_info$name)
        )
        plot_df <- rbind(plot_df, temp_df)
        
        current_stage_index <- current_stage_index + 1 # 切換階段
      }
    }
  }
  
  # --- 1e. 手動記錄最終階段的狀態 ---
  # 在迴圈結束後，C 向量儲存的是最終時刻的濃度分佈
  final_stage_info <- recipe_stages[[length(recipe_stages)]]
  final_temp_df <- data.frame(
    depth_mm = x * 1000, concentration = C,
    time_hr = round(total_time_s / 3600, 1),
    stage = sprintf("End of %s", final_stage_info$name)
  )
  
  plot_df <- rbind(plot_df, final_temp_df)
  
  
  # --- 1e. 準備返回值 ---
  final_profile <- data.frame(depth_mm = x * 1000, concentration_pct = C)
  
  return(list(final_profile = final_profile, plot_data = plot_df))
}


# ===================================================================
#           PART 2: 資料庫儲存函式
# ===================================================================

setup_database <- function(base_path) {
  dir.create(base_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(base_path, "profiles"), showWarnings = FALSE)
  dir.create(file.path(base_path, "plot_data"), showWarnings = FALSE)
  
  recipes_path <- file.path(base_path, "recipes.csv")
  if (!file.exists(recipes_path)) {
    # 創建一個包含所有可能欄位的空檔案頭
    empty_df <- data.frame(Run_ID=character(),
                           Profile_File_Path=character(), Plot_Data_File_Path=character())
    write.csv(empty_df, recipes_path, row.names = FALSE)
  }

  opt_log_path <- file.path(base_path, "optimization_log.csv")
  if (!file.exists(opt_log_path)) {
    write.csv(data.frame(Iteration=integer(), Timestamp=character(),
                         Acquisition_Function=character(), Max_AF_Value=double(),
                         Selected_Run_ID=character()), opt_log_path, row.names = FALSE)
  }
}

save_run_to_db <- function(base_path, run_id, recipe, simulation_results) {
  
  # 從 list 中提取數據
  final_profile <- simulation_results$final_profile
  plot_data <- simulation_results$plot_data
  
  # 1. 準備並儲存 Recipe 主表
  recipe_list <- list(Run_ID = run_id)
  for (i in 1:length(recipe)) {
    stage <- recipe[[i]]
    recipe_list[[paste0("Stage", i, "_Temp_C")]] <- stage$temp_C
    recipe_list[[paste0("Stage", i, "_Time_hr")]] <- stage$duration
    recipe_list[[paste0("Stage", i, "_Cs_pct")]] <- stage$surface_C
  }
  
  profile_filename <- paste0(run_id, "_profile.csv")
  plot_data_filename <- paste0(run_id, "_plot_data.csv")
  recipe_list$Profile_File_Path <- file.path("profiles", profile_filename)
  recipe_list$Plot_Data_File_Path <- file.path("plot_data", plot_data_filename)
  
  recipes_path <- file.path(base_path, "recipes.csv")
  # 讀取現有數據，使用 bind_rows 進行合併，可以處理欄位不匹配的情況
  existing_recipes <- read_csv(recipes_path, col_types = cols(.default = "c")) %>% mutate_all(as.character)
  new_recipe_df <- as.data.frame(lapply(recipe_list, as.character))
  updated_recipes <- bind_rows(existing_recipes, new_recipe_df)
  write_csv(updated_recipes, recipes_path, na = "")
  
  # 2. 儲存 Raw Profile
  write_csv(final_profile, file.path(base_path, "profiles", profile_filename))
  
  # 3. 儲存 Plotting Data
  if (!is.null(plot_data)) {
    write_csv(plot_data, file.path(base_path, "plot_data", plot_data_filename))
  }
  cat(sprintf("Saved all data for Run_ID: %s\n", run_id))
}

log_iteration <- function(base_path, iter, af_value, selected_run_id) {
  log_df <- data.frame(
    Iteration = iter, Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    Acquisition_Function = "EI", Max_AF_Value = af_value, Selected_Run_ID = selected_run_id
  )
  write.table(log_df, file = file.path(base_path, "optimization_log.csv"), append = TRUE,
              sep = ",", row.names = FALSE, col.names = FALSE)
}

# ===================================================================
#           PART 3: 定義參數空間與生成配方
# ===================================================================

# --- 3a. 定義製程參數的範圍 ---
param_space <- list(
  temp_C    = c(850, 900, 950),
  surface_C = c(0.4, 0.8, 1.2, 1.6),
  duration  = c(0.5, 1.0, 1.5, 2.0, 2.5)
)

# --- 3b. 生成所有可能的單段製程組合 ---
single_stage_recipes <- expand.grid(param_space) %>%
  mutate(name = paste0("T", temp_C, "_C", surface_C, "_D", duration))

cat(sprintf("Total possible single-stage recipes: %d\n", nrow(single_stage_recipes)))

# --- 3c. 生成所有可能的兩段製程組合 ---
all_two_stage_combinations <- expand.grid(
  Stage1_idx = 1:nrow(single_stage_recipes),
  Stage2_idx = 1:nrow(single_stage_recipes)
)

cat(sprintf("Total possible two-stage recipes: %d\n", nrow(all_two_stage_combinations)))

# --- 3d. 不再抽樣，直接使用所有組合 ---
recipes_to_run <- all_two_stage_combinations


# ===================================================================
#           PART 4: 執行模擬並建立資料庫
# ===================================================================

# --- 4a. 初始化資料庫 ---
database_path <- "C:/Users/USER/Desktop/PYCProfessor/CarburizationDB_Full"
setup_database(database_path)

# --- 4b. 遍歷抽樣的配方並運行模擬 ---
total_runs <- nrow(recipes_to_run) # 這裡 total_runs 會是 3600
cat(sprintf("\n--- Starting to generate all %d recipes. This will take a long time. ---\n", total_runs))

for (i in 1:total_runs) {
  
  # 創建唯一、數字化的 Run_ID
  run_id <- sprintf("%04d", i)
  
  # 獲取當前要運行的組合的索引
  comb <- recipes_to_run[i, ] 
  stage1_params <- single_stage_recipes[comb$Stage1_idx, ]
  stage2_params <- single_stage_recipes[comb$Stage2_idx, ]
  
  # 組合成 run_simulation 需要的格式，並手動賦予階段名稱
  stage1_list <- as.list(stage1_params)
  stage2_list <- as.list(stage2_params)
  
  # 為每個階段手動添加 'name'
  stage1_list$name <- "Boost"
  stage2_list$name <- "Diffuse"
  
  current_recipe <- list(stage1_list, stage2_list)
  
  # 打印進度
  cat(sprintf("\nRunning %s (%d/%d)...\n", run_id, i, total_runs))
  
  # 運行模擬 
  simulation_results <- run_simulation(recipe_stages = current_recipe)
  
  # 儲存到資料庫 (使用不含 iteration 的 save_run_to_db 版本)
  save_run_to_db(base_path = database_path,
                 run_id = run_id,
                 recipe = current_recipe,
                 simulation_results = simulation_results)
}

cat("\n--- Full 3600-run database generation complete! ---\n")