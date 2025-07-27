library(ggplot2)
library(dplyr)

base_path <- "C:/Users/USER/Desktop/PYCProfessor/CarburizationDB_Full"

recipes_file_path <- file.path(base_path, "recipes.csv")
recipes_df <- read.csv(recipes_file_path)

head(recipes_df)

# --- 1. 實作圖一：隨機抽樣觀察 ---
set.seed(123) 

sampled_rows <- recipes_df %>% sample_n(30)

list_of_sampled_profiles <- list()

# 迴圈讀取這 30 個 profile 檔案
for (i in 1:nrow(sampled_rows)) {
  current_row <- sampled_rows[i, ]
  
  # 取得檔案路徑和 Run_ID
  profile_path <- as.character(current_row$Profile_File_Path)
  run_id <- current_row$Run_ID
  
  full_profile_path <- file.path(base_path, profile_path)
  
  tryCatch({
    temp_df <- read.csv(full_profile_path)
    
    # 加上一個 ID 欄位，用來在 ggplot 中分組
    temp_df$ID <- run_id 
    
    # 將讀取到的 data.frame 存入 list 中
    list_of_sampled_profiles[[i]] <- temp_df
  }, error = function(e) {
    message(paste("Warning: Unable to read archive", full_profile_path))
  })
}

# 將 list 中的所有 data.frame 合併成一個
sampled_profiles_df <- bind_rows(list_of_sampled_profiles)

ggplot(sampled_profiles_df, aes(x = depth_mm, y = concentration_pct, group = ID)) +
  geom_line(alpha = 0.5, color = "steelblue") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Randomly sample 30 carburizing curves",
    subtitle = "Observe the overall distribution and trend of the data",
    x = "Depth (mm)",
    y = "Carbon concentration (%)"
  )

# --- 2-1. 實作圖二：控制變因分析 (以第一段時間為例) ---

comparison_rows <- recipes_df %>%
  filter(
    Stage1_Temp_C == 900,
    Stage1_Cs_pct == 1.2,
    Stage2_Temp_C == 900,
    Stage2_Time_hr == 1.5,
    Stage2_Cs_pct == 0.8
  )

print(comparison_rows)

list_of_comparison_profiles <- list()

# 迴圈讀取篩選出來的這幾筆資料
for (i in 1:nrow(comparison_rows)) {
  current_row <- comparison_rows[i, ]
  profile_path <- as.character(current_row$Profile_File_Path)
  
  # 用第一段的時間當作圖例的標籤
  label <- paste(current_row$Stage1_Time_hr, "hr")
  
  full_profile_path <- file.path(base_path, profile_path)
  
  tryCatch({
    temp_df <- read.csv(full_profile_path)
    
    # 加上一個 Label 欄位，用來在 ggplot 中設定顏色和圖例
    temp_df$Label <- label
    
    list_of_comparison_profiles[[i]] <- temp_df
  }, error = function(e) {
    message(paste("Warning: Unable to read archive", full_profile_path))
  })
}

# 合併資料
comparison_df <- bind_rows(list_of_comparison_profiles)

ggplot(comparison_df, aes(x = depth_mm, y = concentration_pct, color = Label)) +
  geom_line(linewidth = 1.2) + 
  theme_minimal(base_size = 14) +
  labs(
    title = "Control Variance Analysis: Effect of Stage 1 (Stage1_Time_hr)",
    subtitle = "Fixed parameters: T1=900, C1=1.2; T2=900, t2=1.5, C2=0.8",
    x = "Depth (mm)",
    y = "Carbon concentration (%)",
    color = "First period" 
  )

# --- 2-2. 控制變因分析 (以第一段溫度為例) ---
comparison_rows_temp <- recipes_df %>%
  filter(
    Stage1_Time_hr == 1.5,   # 固定第一段時間
    Stage1_Cs_pct == 1.2,
    Stage2_Temp_C == 900,
    Stage2_Time_hr == 1.5,
    Stage2_Cs_pct == 0.8
  )

print(comparison_rows_temp)

list_of_comparison_profiles_temp <- list()

for (i in 1:nrow(comparison_rows_temp)) {
  current_row <- comparison_rows_temp[i, ]
  profile_path <- as.character(current_row$Profile_File_Path)
  
  # 用第一段的溫度當作圖例的標籤
  label <- paste(current_row$Stage1_Temp_C, "°C")
  
  full_profile_path <- file.path(base_path, profile_path)
  
  tryCatch({
    temp_df <- read.csv(full_profile_path)
    
    temp_df$Label <- label
    
    list_of_comparison_profiles_temp[[i]] <- temp_df
  }, error = function(e) {
    message(paste("Warning: Unable to read archive", full_profile_path))
  })
}

# 合併資料
comparison_df_temp <- bind_rows(list_of_comparison_profiles_temp)

comparison_df_temp$Label <- factor(comparison_df_temp$Label, levels = c("850 °C", "900 °C", "950 °C"))

ggplot(comparison_df_temp, aes(x = depth_mm, y = concentration_pct, color = Label)) +
  geom_line(linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Control Variable Analysis: Effect of Stage 1 Temperature (Stage1_Temp_C)",
    subtitle = "Fixed parameters: t1=1.5, C1=1.2; T2=900, t2=1.5, C2=0.8",
    x = "Depth (mm)",
    y = "Carbon concentration (%)",
    color = "First section temperature" 
  )

# --- 2-3. 控制變因分析 (以第二段碳勢為例) ---
comparison_rows_c2 <- recipes_df %>%
  filter(
    Stage1_Temp_C == 900,
    Stage1_Time_hr == 1.5,
    Stage1_Cs_pct == 1.2,
    Stage2_Temp_C == 900,
    Stage2_Time_hr == 1.5
  )

print(comparison_rows_c2)

list_of_comparison_profiles_c2 <- list()

for (i in 1:nrow(comparison_rows_c2)) {
  current_row <- comparison_rows_c2[i, ]
  profile_path <- as.character(current_row$Profile_File_Path)
  
  # 這次用第二段的碳勢當作圖例的標籤
  label <- paste(current_row$Stage2_Cs_pct, "%")
  
  full_profile_path <- file.path(base_path, profile_path)
  
  tryCatch({
    temp_df <- read.csv(full_profile_path)
    
    temp_df$Label <- label
    
    list_of_comparison_profiles_c2[[i]] <- temp_df
  }, error = function(e) {
    message(paste("Warning: Unable to read archive", full_profile_path))
  })
}


comparison_df_c2 <- bind_rows(list_of_comparison_profiles_c2)

comparison_df_c2$Label <- factor(comparison_df_c2$Label, levels = c("0.4 %", "0.8 %", "1.2 %", "1.6 %"))

ggplot(comparison_df_c2, aes(x = depth_mm, y = concentration_pct, color = Label)) +
  geom_line(linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Controlling Variable Analysis: Effect of Stage 2 Carbon Potential (Stage2_Cs_pct)",
    subtitle = "Fixed parameters: T1=900, t1=1.5, C1=1.2; T2=900, t2=1.5",
    x = "Depth (mm)",
    y = "Carbon concentration (%)",
    color = "The second carbon potential" 
  )