#####为了分析 NLR_change 与 28 天死亡率 (EventStatus) 之间的关系，控制基线 NLR_Baseline 的影响，
####并利用限制性三次样条 (RCS) 进行分析

# 加载必要的包
library(dplyr)         # 数据处理
library(survival)      # 生存分析
library(rms)           # 限制性三次样条 (RCS)
library(ggplot2)       # 可视化


# 读取数据
data <- read.csv("C:/Users/ahlil/Desktop/NLR_change_Baseline_quantiles.csv")

# 根据 subject_id 去重，确保每个病例只有一行数据
data_unique <- data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup()


# 使用 rms 包中的函数
dd <- datadist(data_unique)  # 设置数据分布
options(datadist = "dd")

# 构建 Cox 比例风险模型，包含 NLR_change 的 RCS 项
fit <- cph(Surv(EventTime, EventStatus) ~ rcs(NLR_change, 3) + NLR_Baseline,
           data = data_unique, x = TRUE, y = TRUE)

# 查看模型摘要
summary(fit)



# 提取 RCS 的预测值
pred <- Predict(fit, NLR_change, fun = exp)  # 计算风险比 (HR)

# 绘制 RCS 图
# ggplot(pred, aes(x = NLR_change, y = yhat)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#   labs(
#     title = "RCS of NLR_change in NLR_Baseline group low",
#     x = "NLR_change",
#     y = "Hazard Ratio (HR)"
#   ) +
#   theme_minimal()


 
# 使用 ggplot2 绘制 RCS 图
ggplot(pred, aes(x = NLR_change, y = yhat)) +
  geom_line(color = "blue", size = 1) +  # 画 HR 曲线
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +  # 置信区间
  labs(
    title = "RCS of NLR_change",
    x = "NLR_change",
    y = "Hazard Ratio (HR)"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # 添加基线 HR=1
  # scale_x_continuous(limits = c(-10, 30)) +  # 设置 X 轴范围
  # scale_y_continuous(limits = c(0, 5)) +  # 设置 Y 轴范围
  theme_minimal()

# write.csv(pred,file = "C:/Users/ahlil/Desktop/overall_pred.csv",row.names = FALSE)
