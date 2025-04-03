library(survival)
library(survminer)
file_path <- "C:\\Users\\ahlil\\Desktop\\NLR_change_Baseline_quantiles.csv"
data <- read.csv(file_path)
# 确保 NLR_Baseline_group 是因子类型
data$NLR_Baseline_group <- as.factor(data$NLR_Baseline_group)

# 构建 Surv 对象
surv_obj <- Surv(time = data$EventTime, event = data$EventStatus)

# 构建 Cox 比例风险模型
cox_model <- coxph(surv_obj ~ NLR_Baseline_group, data = data)

# 查看模型结果
summary(cox_model)


# 生成分层生存曲线
fit <- survfit(Surv(EventTime, EventStatus) ~ NLR_Baseline_group, data = data)
# 绘制时指定颜色或线条区分组别
ggsurvplot(fit, 
           data = data, 
           risk.table = FALSE, 
           pval = TRUE,          # 显示组间差异的 p 值
           conf.int = TRUE,
           xlab = "Time",
           ylab = "Survival Probability",
           legend.title = "NLR Baseline Group",
           palette = "hue")      # 自动分配颜色区分组别


# 绘制生存曲线
surv_plot <- ggsurvplot(
  fit, 
  data = data,
  conf.int = TRUE,            
  pval = "P<0.0001",           
  risk.table = FALSE,           
  # risk.table.height = 0.3,     
  xlim = c(0, 30),             
  ylim = c(0, 1),             
  break.time.by = 7,           
  xlab = "Time",
  ylab = "Survival Probability",
  legend.title = "",
  # palette = "hue",
  # ggtheme = theme_bw() + 
  #   theme(
  #     panel.grid.major = element_blank(),  
  #     panel.grid.minor = element_blank(),
  #     panel.border = element_blank(),  # 移除上边和右边边框
  #     axis.line = element_line(color = "black"), # 仅保留 x/y 轴
  #     legend.position = "topright"
  #   )
)

# 修改 x 轴标题
surv_plot$plot <- surv_plot$plot + xlab("Time (Day)")

surv_plot$plot <- surv_plot$plot +
  scale_x_continuous(
    limits = c(0, 30),
    breaks = seq(0, 28, by = 7),  # 明确设置刻度
    expand = c(0, 0.05)  # 右边留出一点点空隙，防止28被裁剪
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  )

print(surv_plot)