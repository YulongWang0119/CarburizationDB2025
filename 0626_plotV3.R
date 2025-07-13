# --- 0. 載入必要的套件 ---
library(ggplot2)
library(dplyr)
library(readr)
library(purrr)
library(patchwork)

# ===================================================================
#           PART 1: 設定與數據讀取
# ===================================================================

# --- 1a. 設定資料庫路徑 ---
plot_data_folder_path <- "C:/Users/USER/Desktop/PYCProfessor/CarburizationDB_Full/plot_data"
cat("PART 1: Setup complete. Plot data folder path is set to:\n", plot_data_folder_path, "\n")

# ===================================================================
#           PART 2: 可視化函式 
# ===================================================================

#' 繪製單次運行的多階段圖 (精確複製範例風格)
#' @param run_id 要繪製的 Run ID。
#' @param db_path 資料庫的根路徑。
#' @param all_recipes 包含所有配方資訊的 data.frame。
#    - 輸入更改成已經讀好的 data frame ( plot_data)
#    - 加了一個 plot_title 參數，讓標題可以自訂
generate_concentration_plot <- function(plot_data, plot_title = "Steel Carburization Process") {
  p <- ggplot(plot_data, aes(x = depth_mm, y = concentration, color = stage, group = stage)) +
    geom_line(linewidth = 1.1) + 
    geom_point(size = 2.0, alpha = 0.9) +
    
    labs(
      title = plot_title, # <-- 使用傳入的參數作為標題
      x = "Depth into Steel (mm)",
      y = "Carbon Concentration (wt%)",
      color = "Process Stage" 
    ) +
    
    theme_bw(base_size = 14) + # 我建議把 base_size 調大一點，圖更清晰
    
    theme(
      plot.title = element_text(hjust = 0.5, size = rel(0.95), face = "bold", margin = margin(b = 10)),
      axis.title.x = element_text(size = rel(0.9), margin = margin(t = 10)),
      axis.title.y = element_text(size = rel(0.7), margin = margin(r = 10)),
      axis.text = element_text(size = rel(1.0)),
      legend.position = "bottom",              
      legend.title = element_text(face = "bold", size = rel(0.9)), 
      legend.text = element_text(size = rel(0.9)),      
      legend.key = element_rect(fill = "white", colour = "white"),
      legend.background = element_rect(fill = "white", linetype = "solid", colour = "white")
    ) +
    
    scale_color_manual(values = c(
      "Initial" = "cornflowerblue",
      "End of Boost" = "salmon",
      "End of Diffuse" = "forestgreen"
    )) +
    
    scale_x_continuous(limits = c(0, 5.1), breaks = seq(0, 5, by = 1)) +
    scale_y_continuous(limits = c(NA, 1.25), breaks = seq(0, 1.25, by = 0.25))
  
  return(p)
}
# ===================================================================
#           PART 3: 執行可視化 (簡單直觀版)
# ===================================================================

# --- 圖 1: 處理 "0001_plot_data.csv" ---

# 步驟 a: 構建第一個檔案的完整路徑
file1_path <- file.path(plot_data_folder_path, "0001_plot_data.csv")

# 步驟 b: 讀取第一個檔案的數據
# 把讀取到的數據存到一個叫 data1 的 data.frame 裡
data1 <- read_csv(file1_path)

# 步驟 c: 呼叫我們的繪圖函數來畫圖
# - plot_data 參數用剛讀好的 data1
plot1 <- generate_concentration_plot(
  plot_data = data1, 
  plot_title = "Profile for 0001_plot_data.csv"
)

# 步驟 d: 顯示第一張圖
print(plot1)


# --- 圖 2: 處理 "0002_plot_data.csv" ---

# 重複上面的步驟，只是檔名和變數名稱換一下
file2_path <- file.path(plot_data_folder_path, "0002_plot_data.csv")
data2 <- read_csv(file2_path)
plot2 <- generate_concentration_plot(
  plot_data = data2, 
  plot_title = "Profile for 0002_plot_data.csv"
)
print(plot2)


# 用 + 號就可以把兩張 ggplot 圖拼起來
combined_plot <- plot1 + plot2
print(combined_plot)
