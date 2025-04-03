# 加载包
library(randomForest)
library(shapr)
# library(DALEX)
library(ggplot2)
library(readr)
library(fastshap)

# 加载数据
file_path <- "C:/Users/ahlil/Desktop/random_forest.csv"
data <- read_csv(file_path)
# vif_data <- read_csv("C:/Users/ahlil/Desktop/vif_data.csv")
# 将 EventStatus 转换为因子类型
data$EventStatus <- as.factor(data$EventStatus)

# 设置随机种子
set.seed(123)

# 划分数据集
sample_index <- sample(1:nrow(data), size = 0.7 * nrow(data))
training_data <- data[sample_index, ]
testing_data <- data[-sample_index, ]


# 训练随机森林模型
rf_model <- randomForest(EventStatus ~ ., data = training_data)

# 步骤2：定义自定义预测函数（关键！针对分类模型）
# 注意：shapr要求输出为数值向量（概率或回归值）
predict_model <- function(model=rf_model, newdata=testing_data) {
  # 如果是分类模型，输出概率矩阵的第二列（通常为类别"1"的概率）
  # 如果是回归模型，直接使用 predict(model, newdata)
  pred_prob <- predict(model, newdata, type = "prob")[, 2]
  return(as.numeric(pred_prob))
}


# 准备数据
x_train <- training_data[, -c(1)] # 排除目标变量
x_test <- testing_data[, -c(1)]

# training_data$EventStatus <- as.numeric(training_data$EventStatus)
# y_var <- training_data$EventStatus

p <- mean(data$EventStatus == 1)

# 使用 shapr 计算 SHAP 值
# 计算SHAP值
shap_values <- shapr::explain(model = rf_model, x_explain = x_test, x_train = x_train,
                       approach = "empirical",
                       predict_model = predict_model,  # 传入自定义预测函数
                       phi0 = p,
                       iterative = TRUE,   # 启用迭代近似（必须！否则无法计算高维特征）
                       n_batches = 10,     # 并行批次（加速计算）
                       n_combinations = 50  # 限制组合数（平衡精度与速度）
                       )
 

# 可视化 SHAP 值
# 使用 shapr 包中的 plot.shapr 方法直接可视化

library(shapr)
library(ggplot2)    # 在绘图时需要
library(ggbeeswarm) # 在绘制蜜蜂图时需要
# 使用自定义颜色绘制蜜蜂图
# plot(shap_values, plot_type = "beeswarm", col = c("red", "blue"), corral = "wrap")


# 绘制 Shapley 值的条形图（默认）
plot(shap_values, index_x_explain = 1:4)
# 绘制瀑布图
plot(shap_values, plot_type = "waterfall", index_x_explain = 1:4)
# 绘制散点图，展示特征值与 Shapley 值的关系
plot(shap_values, plot_type = "scatter")


# 使用自定义颜色绘制蜜蜂图
 
plot(
  shap_values, 
  plot_type = "beeswarm",
  # 不再直接指定 cex，改用 size 控制点大小
  size = 0.6,                  # 使用 size 参数代替 cex（默认值通常为 1）
  alpha = 0.35,               
  corral = "wrap",            
  corral.width = 0.8          
) +
  theme_minimal()
