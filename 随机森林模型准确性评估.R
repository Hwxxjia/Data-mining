# 假设 rf_model 是你的随机森林模型

# 1. 查看 OOB 错误率
oob_error_rate <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
cat("OOB Error Rate:", oob_error_rate, "\n")

# 2. 混淆矩阵
confusion_matrix <- rf_model$confusion
cat("Confusion Matrix:\n")
print(confusion_matrix)

# 计算准确率
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# 计算其他指标，如灵敏度、特异性
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

# 3. 变量重要性
importance_values <- rf_model$importance
cat("Variable Importance:\n")
print(importance_values)

# 如果你想可视化变量重要性
library(ggplot2)
importance_df <- data.frame(
  Variable = rownames(importance_values),
  Importance = importance_values[, 1]
)

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variable Importance", x = "Variables", y = "Mean Decrease in Gini") +
  theme_minimal()
