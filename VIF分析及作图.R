# 加载必要的包
# install.packages("car")
# install.packages("readr")
# install.packages("dplyr")

library(car)
library(readr)
library(dplyr)

# 读取数据
file_path <- "C:/Users/ahlil/Desktop/vif_data.csv"
data <- read_csv(file_path)

# 准备数据，移除目标变量
predictors <- select(data, -EventStatus)

# 检查并移除缺失值
predictors <- na.omit(predictors)

# 计算方差膨胀因子
vif_results <- vif(lm(EventStatus ~ ., data = data))

# 显示VIF结果
print(vif_results)


# 
# file_path <- "C:/Users/ahlil/Desktop/cor_data.csv"
# cor_data <- read_csv(file_path)
# # cor_data$sbp_mean<- as.numeric(cor_data$sbp_mean)
# # cor_data$dbp_mean<- as.numeric(cor_data$dbp_mean)
# # cor_data$heart_rate_mean<- as.numeric(cor_data$heart_rate_mean)
# # str(cor_data)
# # 按列转换并保持数据框结构
# cor_data_num <- as.data.frame(lapply(cor_data, as.numeric))
# 
# ############### 计算变量间相关系数##########
# cor_matrix <- cor(cor_data_num, use = "complete.obs")
# 
# 
# ##################  筛选相关系数高的变量
# # 1. 移除对角线（设置为 NA）
# diag(cor_matrix) <- NA
# 
# # 2. 设定阈值（例如 r > 0.9 或 r < -0.9）
# threshold <- 0.5
# high_cor_pairs <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)
# 
# # 3. 提取变量名称及相关系数值
# high_cor_df <- data.frame(
#   var1 = rownames(cor_matrix)[high_cor_pairs[, 1]],
#   var2 = colnames(cor_matrix)[high_cor_pairs[, 2]],
#   corr = cor_matrix[high_cor_pairs]
# )
# 
# # 按相关性强弱排序
# high_cor_df <- high_cor_df[order(abs(high_cor_df$corr), decreasing = TRUE), ]
# 
# head(high_cor_df)  # 查看前几对强相关性变量



################ 作图

library(ggplot2)

# 如果 vif_results 是向量或列表，转换为数据框
vif_data <- data.frame(
  Variable = names(vif_results),
  VIF = as.numeric(vif_results)
)

# 按VIF值排序
vif_data <- vif_data[order(vif_data$VIF, decreasing = TRUE), ]
vif_data$Variable <- factor(vif_data$Variable, levels = vif_data$Variable)  # 固定柱状图顺序


ggplot(vif_data, aes(x = Variable, y = VIF)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +  # 标注VIF=10的阈值
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictor Variables",
    y = "VIF Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # 变量名倾斜45度
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # 调整y轴范围


