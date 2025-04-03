#####为了分析 NLR_change 与 28 天死亡率 (EventStatus) 之间的关系，控制基线 NLR_Baseline 的影响，
####并利用限制性三次样条 (RCS) 进行分析

# 加载必要的包
library(dplyr)         # 数据处理
library(survival)      # 生存分析
library(rms)           # 限制性三次样条 (RCS)
library(ggplot2)       # 可视化


# 读取数据
data <- read.csv("C:/Users/ahlil/Desktop/NLR_change_Baseline_group_high.csv")

# 根据 subject_id 去重，确保每个病例只有一行数据
data_unique <- data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup()


# 使用 rms 包中的函数
dd <- datadist(data_unique)  # 设置数据分布
options(datadist = "dd")

# 构建 Cox 比例风险模型，包含 NLR_change 的 RCS 项
fit <- cph(Surv(EventTime, EventStatus) ~ rcs(NLR_change, 3)+ NLR_Baseline ,
           data = data_unique, x = TRUE, y = TRUE)

# 查看模型摘要 
summary(fit)



# 提取 RCS 的预测值
 # pred <- Predict(fit, NLR_change, fun = exp)  # 计算风险比 (HR)
# pred <- Predict(fit, NLR_change = seq(min(data_unique$NLR_change), max(data_unique$NLR_change), length = 100), fun = exp)
# 
# baseline_HR <- pred$yhat[pred$NLR_change == 0]  # 获取 NLR_change = 0 时的 HR
# pred$yhat <- pred$yhat / baseline_HR  # 调整 HR 使 NLR_change = 0 时 HR = 1
# # 找到 NLR_change 最接近 0 的索引
# baseline_index <- which.min(abs(pred$NLR_change))
# baseline_HR <- pred$yhat[baseline_index]  # 获取对应的 HR
# pred$yhat <- pred$yhat / baseline_HR  # 归一化，使 HR(0) = 1
 
 pred <- Predict(fit, NLR_change = seq(min(data_unique$NLR_change), max(data_unique$NLR_change), length = 100), fun = identity)
 
 # 计算 `NLR_change = 0` 的 log(HR)
 pred0 <- Predict(fit, NLR_change = 0, fun = identity)
 baseline_x <- pred0$yhat  # 取出 log(HR)
 
 # 归一化
 pred$yhat <- exp(pred$yhat - baseline_x)
 

# 绘制 RCS 图
 # 计算置信区间的 HR（对 lower 和 upper 进行 exp 变换）
 pred$lower <- exp(pred$lower - baseline_x)
 pred$upper <- exp(pred$upper - baseline_x)
 
 
 # pred$upper <- pmin(pred$upper, 4)  # 限制上界最大为 5
 
 # 使用 ggplot2 绘制 RCS 图
 ggplot(pred, aes(x = NLR_change, y = yhat)) +
   geom_line(color = "blue", linewidth = 1) +  # 画 HR 曲线
   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +  # 置信区间
   labs(
     title = "RCS of NLR_change in NLR_Baseline group high",
     x = "NLR_change",
     y = "Hazard Ratio (HR)"
   ) +
   geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # 添加基线 HR=1
   scale_x_continuous(limits = c(-40, 40)) +  # 设置 X 轴范围
   # scale_y_continuous(limits = c(0, 4)) +  # 设置 Y 轴范围
   coord_cartesian(ylim = c(0, 8)) +  # 使用 coord_cartesian 严格限制 Y 轴范围
   theme_minimal()
 
 
 
 #######  提取特定NLR_change值时对应的HR  ###########################

 

 
 
 # 使用 Predict 函数计算特定值下的预测结果
 pred_specific <- Predict(fit, NLR_change =c(-20, -15, -10,-8, -5,	0,	5,	8,	10,	15,	20, 30), fun = identity)
 
 # pred$yhat <- pred$yhat / baseline_HR  # 归一化，使 HR(0) = 1
 # 计算 `NLR_change = 0` 的 log(HR)
 pred_specific0 <- Predict(fit, NLR_change = 0, fun = identity)
 baseline_x <- pred_specific0$yhat  # 取出 log(HR)
 # 归一化
 pred_specific$yhat <- exp(pred_specific$yhat - baseline_x)
 
 # 提取该特定值的 log(HR)
 hr_specific <- pred_specific$yhat
 # 输出结果
 cat("HR =", hr_specific, "\n")
 
 
