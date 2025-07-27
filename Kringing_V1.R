# --- 任務二：手寫 Kriging 建立骨架 目前嘗試:準備 1D 訓練資料集 ---

# 選擇「第一段時間」的數據集 (comparison_df) 作為 1D 案例
# 來自 EDA_V1  R 腳本 2-1. 實作圖二：控制變因分析 (以第一段時間為例)

# 1. 準備輸入 X_train (第一段的時間)
# 從 comparison_rows 中提取時間，確保順序正確
train_data_info <- comparison_rows %>% 
  arrange(Stage1_Time_hr) 

X_train <- as.matrix(train_data_info$Stage1_Time_hr)

# 2. 準備輸出 Y_train (有效滲碳深度)
# 定義有效滲碳深度的標準：碳濃度降至 0.4% 的深度(假設)
TARGET_CONCENTRATION <- 0.4

# 使用 dplyr 來計算每條曲線的有效深度
effective_depths <- comparison_df %>%
  group_by(Label) %>%
  summarize(
    effective_depth = {
      # 找到最接近目標濃度的點的索引
      idx <- which.min(abs(concentration_pct - TARGET_CONCENTRATION))
      # 回傳該點的深度
      depth_mm[idx]
    }
  ) %>%
  # 從 Label 中提取數值並排序，確保與 X_train 對應
  mutate(Time1_num = as.numeric(gsub(" hr", "", Label))) %>%
  arrange(Time1_num)

Y_train <- as.matrix(effective_depths$effective_depth)

# 3. 檢查並合併成一個 data.frame
train_set_1D <- data.frame(
  Time1 = X_train,
  EffectiveDepth = Y_train
)

print("--- 1D Kriging training data set ---")
print(train_set_1D)

ggplot(train_set_1D, aes(x = Time1, y = EffectiveDepth)) +
  geom_point(color = "red", size = 4) +
  geom_line(linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(
    title = "1D Training material: First period vs. effective carburizing depth (C=0.4%)",
    x = "First period (hours)",
    y = "Effective carburizing depth (mm)"
  )

# 上一步準備好的訓練資料
# X_train 是 5x1 矩陣 (時間)
# Y_train 是 5x1 矩陣 (有效深度)
print(train_set_1D)
X_train <- as.matrix(train_set_1D$Time1)
Y_train <- as.matrix(train_set_1D$EffectiveDepth)

# --- 步驟 A: 實作高斯核函數 ---

# 這個函數會計算兩點 x1 和 x2 之間的關聯性
# theta 是一個超參數，控制關聯性隨距離下降的速度
# theta 越大，關聯性下降得越快

kernel_gaussian <- function(x1, x2, theta) {
  return(exp(-theta * (x1 - x2)^2))
}

# 測試
# 觀察時間點 0.5 和 1.0 之間的關聯性是多少
# 先手動假設 theta = 1.0
test_correlation <- kernel_gaussian(x1 = 0.5, x2 = 1.0, theta = 1.0)
print(paste("Test for correlation:", test_correlation))
#在此測試高斯核函數，以驗證其計算結果是否符合預期，並建立對關聯性衰減的直觀理解。

# --- 步驟 B: 建立共變異數矩陣 ---
# 取得訓練資料的數量
n <- nrow(X_train)

# 先手動設定超參數 theta
theta <- 1.0 

# 建立一個 n x n 的空矩陣
Psi <- matrix(0, nrow = n, ncol = n)

# 使用雙迴圈來填充矩陣
for (i in 1:n) {
  for (j in 1:n) {
    Psi[i, j] <- kernel_gaussian(X_train[i], X_train[j], theta)
  }
}

# 檢查
# 印出這個矩陣。你應該會看到：
# 1. 對角線全部是 1 (因為一個點跟自己的關聯性是 100%)
# 2. 是一個對稱矩陣 (Psi[i,j] == Psi[j,i])
print("--- Covariance matrix Ψ ---")
print(Psi)

# --- 步驟 C: 實作 Kriging 預測函數 ---

predict_kriging <- function(x_new, X_train, Y_train, theta) {
  
  n <- nrow(X_train)
  one_vector <- matrix(1, nrow = n, ncol = 1)
  
  # 1. 根據 theta 和 X_train 建立共變異數矩陣 Psi
  Psi <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      Psi[i, j] <- kernel_gaussian(X_train[i], X_train[j], theta)
    }
  }
  
  # 為了數值穩定性，加上一個微小的 "nugget" 到對角線
  # 可以防止矩陣過於接近奇異 (singular) 而無法求逆
  Psi <- Psi + diag(n) * 1e-6
  
  # 2. 計算 Psi 的逆矩陣
  Psi_inv <- solve(Psi)
  
  # 3. 計算 μ_hat (對應文獻公式 2.30)
  #    μ = (1' * Ψ^-1 * y) / (1' * Ψ^-1 * 1)
  mu_hat <- (t(one_vector) %*% Psi_inv %*% Y_train) / (t(one_vector) %*% Psi_inv %*% one_vector)
  mu_hat <- as.numeric(mu_hat) # 轉為純量
  
  # 4. 計算 psi_vector (新點 x_new 與所有訓練點的關聯性向量)
  psi_vector <- matrix(0, nrow = n, ncol = 1)
  for (i in 1:n) {
    psi_vector[i] <- kernel_gaussian(X_train[i], x_new, theta)
  }
  
  # 5. 計算最終預測值 y_hat (對應文獻公式 2.40)
  #    ŷ = μ + ψ' * Ψ^-1 * (y - 1*μ)
  y_hat <- mu_hat + t(psi_vector) %*% Psi_inv %*% (Y_train - one_vector * mu_hat)
  
  return(as.numeric(y_hat))
}

# 測試
# 預測一個不在訓練集裡的新點，例如時間 t=1.25 小時
# 繼續使用手動設定的 theta = 1.0
predicted_depth <- predict_kriging(x_new = 1.25, X_train, Y_train, theta = 1.0)
print(paste("At the predicted depth of t=1.25:", predicted_depth)) # 應該會得到約 0.375

# --- 步驟 D: 視覺化 ---
# 1. 產生一系列密集的測試點，用來畫平滑曲線
x_test <- as.matrix(seq(0.5, 2.5, length.out = 101))

# 2. 對每一個測試點進行預測
y_pred <- sapply(x_test, FUN = predict_kriging, 
                 X_train = X_train, 
                 Y_train = Y_train, 
                 theta = 1.0)

# 3. 將預測結果整理成 data.frame
prediction_df <- data.frame(Time1 = x_test, PredictedDepth = y_pred)

# 4. 畫圖
ggplot() +
  # 畫出原始的訓練資料點 (紅色大點)
  geom_point(data = train_set_1D, aes(x = Time1, y = EffectiveDepth), color = "red", size = 4) +
  # 畫出模型的預測曲線 (藍色線)
  geom_line(data = prediction_df, aes(x = Time1, y = PredictedDepth), color = "blue", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Handwritten Kriging Model Skeleton (1D Proof of Concept)",
    subtitle = paste("Manually set theta =", theta),
    x = "First period (hours)",
    y = "Effective carburizing depth (mm)"
  )