# --- 0. 載入必要的套件 ---
library(ggplot2)
library(dplyr)
library(readr)
library(purrr)

# ===================================================================
#           PART 1: 設定與數據讀取
# ===================================================================

# --- 1a. 設定資料庫路徑 ---
database_path <- "C:/Users/USER/Desktop/PYCProfessor/CarburizationDB_Final"

# --- 1b. 讀取核心數據表 ---
recipes_db_path <- file.path(database_path, "recipes.csv")
optimization_log_path <- file.path(database_path, "optimization_log.csv")

# 檢查檔案是否存在
if (!file.exists(recipes_db_path)) {
  stop(paste("Error: recipes.csv not found at:", recipes_db_path, 
             "\nPlease run the data generation script first."))
}

# 使用 readr::read_csv 讀取，並指定欄位類型以增加穩健性
# 'col_types = cols(.default = "c")' 先將所有欄位讀取為文字
# 'mutate(across(...))' 再將需要的欄位轉換為數字
recipes_df <- read_csv(recipes_db_path, col_types = cols(.default = "c")) %>%
  mutate(across(where(is.character) & !c(Run_ID, Recipe_Name, Profile_File_Path, Plot_Data_File_Path), as.numeric))

cat("Successfully loaded recipes database with", nrow(recipes_df), "runs.\n")

if (file.exists(optimization_log_path)) {
  opt_log_df <- read_csv(optimization_log_path)
  cat("Successfully loaded optimization log with", nrow(opt_log_df), "iterations.\n")
} else {
  opt_log_df <- NULL
  cat("Warning: optimization_log.csv not found.\n")
}

# ===================================================================
#           PART 2: 可視化函式 
# ===================================================================

#' 繪製單次運行的多階段圖 (精確複製範例風格)
#' @param run_id 要繪製的 Run ID。
#' @param db_path 資料庫的根路徑。
#' @param all_recipes 包含所有配方資訊的 data.frame。
plot_run_with_stages <- function(run_id, db_path, all_recipes) {
  
  run_info <- all_recipes %>% filter(Run_ID == run_id)
  if (nrow(run_info) == 0) { stop(paste("Run_ID", run_id, "not found.")) }
  
  # 構建 plot_data 檔案的路徑
  plot_data_filepath <- file.path(db_path, run_info$Plot_Data_File_Path[1])
  if (!file.exists(plot_data_filepath)) { stop(paste("Plotting data file not found:", plot_data_filepath)) }
  
  plot_data <- read_csv(plot_data_filepath)
  
  # --- 核心程式碼---
  p <- ggplot(plot_data, aes(x = depth_mm, y = concentration, color = stage, group = stage)) +
    
    # 同時繪製線和點
    geom_line(linewidth = 1.1) + 
    geom_point(size = 2.0, alpha = 0.9) +
    
    # 設定標籤
    labs(
      title = "Simulation of a 2-Stage Steel Carburization Process",
      x = "Depth into Steel (mm)",
      y = "Carbon Concentration (wt%)",
      color = "Process Stage" # 圖例標題
    ) +
    
    # 使用 theme_bw() 得到帶有網格線的白色背景
    theme_bw(base_size = 8) + # 增大基礎字體大小，讓標籤更清晰
    
    # 微調主題細節
    theme(
      # 標題設定
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10)),
      # 軸標題設定
      axis.title.x = element_text(size = 18, margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, margin = margin(r = 10)),
      # 軸刻度文字設定
      axis.text = element_text(size = 14),
      
      # 圖例設定
      legend.position = "bottom",              
      legend.title = element_text(face = "bold", size = 16), 
      legend.text = element_text(size = 12),      
      legend.key = element_rect(fill = "white", colour = "white"), # 圖例背景
      legend.background = element_rect(fill = "white", linetype = "solid", colour = "white")
    ) +
    
    # 手動設定顏色
    scale_color_manual(values = c(
      "Initial" = "cornflowerblue",
      "End of Boost" = "salmon",
      "End of Diffuse" = "forestgreen"
      # 如果有更多 stage，可以在這裡繼續添加
    )) +
    
    # 設定座標軸範圍和刻度
    scale_x_continuous(limits = c(0, 5.1), breaks = seq(0, 5, by = 1)) +
    scale_y_continuous(limits = c(NA, 1.25), breaks = seq(0, 1.25, by = 0.25))
  
  return(p)
}

# ===================================================================
#           PART 3: 執行可視化
# ===================================================================

# --- 範例 1: 畫出 'init_01' 的完整階段圖 ---
try({
  p1 <- plot_run_with_stages("init_01", database_path, recipes_df)
  print(p1)
}, silent = TRUE)


# --- 範例 2: 畫出 'iter_02' 的完整階段圖 ---
try({
  p2 <- plot_run_with_stages("iter_02", database_path, recipes_df)
  print(p2)
}, silent = TRUE)

