library(lcmm)
library(survival)
data <- read.csv("C:/Users/ahlil/Desktop/nlr_cci_apsiii_clean_1.csv", header = TRUE, stringsAsFactors = FALSE)
data$NLR <-as.numeric(data$NLR)
data_clean <- data[!is.na(data$NLR), ]
###########################################################################################################
################gridsearch进行hlme分析
initial_hlme <- hlme(fixed = NLR ~ time,  
               random = ~ time,  
               subject = "subject_id",  
               ng = 1,
               data = data_clean,
               verbode = TRUE)

rep <- 25       # 从50个随机初始值出发
maxiter <- 100  # 每次优化的最大迭代次数

# 进行网格搜索
final_hlme_2 <- gridsearch(
  m = hlme(fixed = NLR ~ time,  mixture=~time,
           random = ~ time,  
           subject = "subject_id",  
           ng = 2,
           data = data_clean,
           verbose = TRUE),
           rep = rep,
           maxiter = maxiter,
           minit = initial_hlme
           )

final_hlme_3 <- gridsearch(
  m = hlme(fixed = NLR ~ time,  mixture=~time,
           random = ~ time,  
           subject = "subject_id",  
           ng = 3,
           data = data_clean,
           verbose = TRUE),
  rep = rep,
  maxiter = maxiter,
  minit = initial_hlme
)


final_hlme_4 <- gridsearch(
  m = hlme(fixed = NLR ~ time,  mixture=~time,
           random = ~ time,  
           subject = "subject_id",  
           ng = 4,
           data = data_clean,
           verbose = TRUE),
  rep = rep,
  maxiter = maxiter,
  minit = initial_hlme
)



final_hlme_5 <- gridsearch(
  m = hlme(fixed = NLR ~ time,  mixture=~time,
           random = ~ time,  
           subject = "subject_id",  
           ng = 5,
           data = data_clean,
           verbose = TRUE),
  rep = rep,
  maxiter = maxiter,
  minit = initial_hlme
)

####################################################################################################
############# 作图
# 检查异常值
boxplot(data_clean$NLR, main = "Boxplot of NLR", ylab = "NLR")

# 进一步分析：绘制生存曲线
### 使用 predictClass 提取类别归属概率
class_probs <- predictClass(final_hlme_3, newdata = data_clean)

# 将类别信息合并到 data_clean
data_clean <- merge(data_clean, class_probs[, c("subject_id", "class")], by = "subject_id")

# 将类别转换为因子
data_clean$class <- factor(data_clean$class, levels = unique(data_clean$class), labels = paste0("Class ", unique(data_clean$class)))

# 计算生存函数
survfit_object <- survfit(Surv(EventTime, EventStatus) ~ class, data = data_clean)
# 绘制生存曲线
plot(survfit_object, main = "Survival Curves by Class", xlab = "Time", ylab = "Survival Probability", col =c("forestgreen","goldenrod","firebrick") , lwd = 2)
# 添加图例
legend("bottomleft", legend = levels(factor(data_clean$class)), col =c("forestgreen","goldenrod","firebrick"), lwd = 2)

# 进一步分析：绘制纵向轨迹
library(ggplot2)

ggplot(data_clean, aes(x = time, y = NLR, group = subject_id, color = class)) +
  geom_line(alpha = 0.5) +
  labs(title = "Longitudinal Trajectories by Class", x = "Time", y = "NLR") +
  theme_minimal()

# 进一步分析：绘制类别归属概率图
ggplot(class_probs, aes(x = subject_id, y = prob1)) +
  geom_point(alpha = 0.5) +
  labs(title = "Class Membership Probabilities", x = "Subject ID", y = "Probability of Class 1") +
  theme_minimal()

########绘制各个类别（ng=3）的拟合轨迹
# 确保 model3$pprob 中有 subject_id 列（该列名称应与你的数据匹配）
final_hlme_3$pprob$subject_id <- final_hlme_3$pprob[, "subject_id"]

# 按 subject_id 将类别合并到原始数据中
library(dplyr)
data_clean <- data_clean %>%
  left_join(
    final_hlme_4$pprob %>% 
      select(subject_id, class) %>% 
      mutate(Assigned_Class = factor(class)), 
    by = "subject_id"
  )

# 绘制轨迹图（示例代码）：
library(ggplot2)

# 生成预测值
time_seq <- seq(min(data_clean$time), max(data_clean$time), length.out = 100)

fixed_effects <- data.frame(
  class = 1:3,  # 保持为数值型
  intercept = c( 62.05, 12.00, 23.34),  # 来自模型输出的固定效应截距
  slope = c(-8.79, -0.55, 7.38)       # 来自模型输出的固定效应斜率
)



traj_data <- expand.grid(
  time = time_seq,
  Assigned_Class = 1:3  # 保持数值型以匹配fixed_effecs
) 

traj_data <- traj_data %>% 
  left_join(fixed_effects, by = c("Assigned_Class" = "class")) %>% 
  mutate(pred = intercept + slope * time)

# 绘制原始数据和拟合轨迹
ggplot(traj_data, aes(x = time, y = pred)) +
  geom_line(aes(color = factor(Assigned_Class)), linewidth = 1.5) +  # 转换因子用于分组
  labs(title = "Fitted Trajectories by Class",
       x = "Time",
       y = "Predicted NLR",
       color = "Class") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +  # 添加第 4 种颜色
  theme_minimal(base_size = 14)

##########################################################################################################
###########  SSBIC计算
# 提取对数似然值
log_likelihood <- final_hlme_4$loglik

# 提取参数数量
num_params <- length(coef(final_hlme_4))

# 提取观测数据数量
# 使用 ns 直接获取总的观测数据数量
num_obs <- final_hlme_4$ns

# 检查 num_obs 是否为数值
if (!is.numeric(num_obs) || is.na(num_obs) || num_obs <= 0) {
  stop("无法提取有效的观测数据数量")
}

# 计算 SSBIC
ssbic_value <- -2 * log_likelihood + num_params * log(num_obs)

# 打印提取的值和计算得到的 SSBIC
cat("Log-likelihood:", log_likelihood, "\n")
cat("Number of parameters:", num_params, "\n")
cat("Number of observations:", num_obs, "\n")
cat("SSBIC:", ssbic_value, "\n")

########### 计算Entropy

# 提取每个主体的概率分布
pprob_df_2 <- final_hlme_2$pprob

# 查看前几行 pprob_df_2
head(pprob_df_2)

# 定义计算熵的函数
calculate_entropy <- function(probabilities) {
  # 避免 log(0) 错误，添加一个小的正数
  probabilities <- probabilities + 1e-10
  -sum(probabilities * log(probabilities))
}

# 初始化熵列表
entropies_2 <- numeric(nrow(pprob_df_2))

# 计算每个主体的熵
for (i in 1:nrow(pprob_df_2)) {
  probabilities <- pprob_df_2[i, c("prob1", "prob2")]
  entropies_2[i] <- calculate_entropy(probabilities)
}

# 计算平均熵
mean_entropy_2 <- mean(entropies_2)

# 打印平均熵
cat("平均熵 (ng=2):", mean_entropy_2, "\n")

#########################################################################################3
################## 平均后验概率  文献里把Entropy 和平均后验概率混淆了

# 提取 ng=3 的后验概率数据框
pprob_df_2 <- final_hlme_2$pprob

mean_probs_2 <- pprob_df_2 %>%
  rowwise() %>%  # 逐行操作
  mutate(assigned_prob = get(paste0("prob", class))) %>%  # 动态选择对应 prob 列
  group_by(class) %>%
  summarise(
    mean_assigned_prob = mean(assigned_prob),
    n = n(),
    .groups = "drop"
  )

# 输出结果
mean_probs_2

#######################################################################################################33
############################################################################################################
########################################################################################################
##########        绘制轨迹图：
# 1.横坐标为time，纵坐标为NLR；
# 2.每个subject_id的实际轨迹 用浅灰色描绘；
# 3.在实际轨迹的基础上拟合出模型预测的轨迹，要求形成近似曲线，不是一条直线，颜色为class1=黄，class2=绿，class3=红；
# 4.图片的题目为类似于“Class1：N=100（5%）”样式，颜色与拟合曲线颜色一致，位于图片正上方。
# 加载必要的包
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

# 数据预处理 ----------------------------------------------------------------
# 合并分组概率和原始数据
data_merged <- data_clean %>%
  left_join(final_hlme_3$pprob %>% 
              select(subject_id, class) %>% 
              mutate(Assigned_Class = factor(class)),
            by = "subject_id") %>%
  distinct(subject_id, time, .keep_all = TRUE)

# 计算各类统计信息 ----------------------------------------------------------
class_summary <- data_merged %>%
  group_by(Assigned_Class) %>%
  summarise(
    n = n_distinct(subject_id),
    total = n_distinct(data_clean$subject_id)
  ) %>%
  mutate(percent = n / total * 100)

# 固定效应参数（示例数值，需替换为实际模型输出） ------------------------------
fixed_params <- tibble(
  Assigned_Class = factor(1:3),
  intercept = c(62.05, 12.00, 23.34),
  slope = c(-8.79, -0.55, 7.38)
)

# 生成预测轨迹数据 ----------------------------------------------------------
time_seq <- seq(min(data_clean$time), max(data_clean$time), length.out = 100)

traj_data <- expand_grid(
  time = time_seq,
  Assigned_Class =factor(1:3)
) %>%
  left_join(fixed_params, by = "Assigned_Class") %>%
  mutate(pred_NLR = intercept + slope * time)

# 分类型绘图函数 ------------------------------------------------------------
plot_class <- function(class_num, color) {
  class_label <- paste0("Class", class_num)
  
  ggplot() +
    # 绘制所有实际轨迹背景
    geom_line(data = data_merged,
              aes(x = time, y = NLR, group = subject_id),
              color = "gray100", alpha = 0.1, linewidth = 0.1) +
    # 绘制当前类别实际轨迹
    geom_line(data = filter(data_merged, Assigned_Class == class_num),
              aes(x = time, y = NLR, group = subject_id),
              color = "gray70", alpha = 0.3, linewidth = 0.1) +
    # 绘制预测轨迹
    geom_line(data = filter(traj_data, Assigned_Class == class_num),
              aes(x = time, y = pred_NLR),
              color = color, linewidth = 0.8) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    labs(
      title = sprintf("%s: N=%d (%.1f%%)", 
                      class_label,
                      class_summary$n[class_num],
                      class_summary$percent[class_num]),
      x = "Time (Day)",
      y = "NLR"
    ) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +  # 新增：确保x轴从0开始（需确保time数据本身非负）
    theme_bw(base_size = 10) +
    
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(color = color, hjust = 0.5, face = "plain"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),                     # 移除整个边框
      axis.line = element_line(color = "black", size = 0.3), # 添加基础轴线
      axis.line.x.top = element_blank(),                  # 移除顶部线
      axis.line.y.right = element_blank()                 # 移除右侧线
          )
}

# 生成三个分类图 ------------------------------------------------------------
class_colors <- c(Class1 = "goldenrod", 
                  Class2 = "forestgreen", 
                  Class3 = "firebrick")

# 显示 Class1 轨迹图
plot_class(1, "goldenrod")

# 显示 Class2 轨迹图
plot_class(2, "forestgreen")

# 显示 Class3 轨迹图
plot_class(3, "firebrick")

##保存三个分类图
walk(1:3, ~{
  p <- plot_class(.x, class_colors[.x])
  ggsave(sprintf("Class%s_Trajectory.png", .x), p, 
         width = 6, height = 4, dpi = 300)
})

# 生成总图 -----------------------------------------------------------------
# 1. 定义正确命名的颜色向量
class_colors <- c("1" = "goldenrod", "2" = "forestgreen", "3" = "firebrick")
# 2. 强制因子化并检查数据范围
traj_data <- traj_data %>%
  mutate(Assigned_Class = factor(Assigned_Class, levels = c("1", "2", "3"))) %>%
  filter(pred_NLR >= 0, pred_NLR <= 100)
# 3. 绘图代码
overall_plot <- ggplot() +
  geom_line(
    data = data_merged,
    aes(x = time, y = NLR, group = subject_id),
    color = "gray90", alpha = 0.2, linewidth = 0.2
  ) +
  geom_line(
    data = traj_data,
    aes(x = time, y = pred_NLR, color = Assigned_Class),
    linewidth = 0.8
  ) +
  scale_color_manual(
    name = "Classes",
    values = class_colors,
    labels = sprintf("Class%d (%.1f%%)", 1:3, class_summary$percent)
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +  # 新增：确保x轴从0开始
  theme_bw(base_size = 10) +
  labs(title = "Overall Trajectories", x = "Time (Day)", y = "NLR") +
  theme_bw(base_size = 10) +
  theme(
    legend.position.inside = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.9)),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(face = "bold"),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.3),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank()
  )



ggsave("Overall_Trajectories.png", overall_plot, 
       width = 8, height = 6, dpi = 600)


##################################################################################################
####################################################################################################
################################### K-M统计分析及作图 #######################################
#######################################################################################################
########   绘制生存曲线
# 安装并加载必要的包

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

### 使用 predictClass 提取类别归属概率
class_probs <- predictClass(final_hlme_3, newdata = data_clean)

# 将类别信息合并到 data_clean
data_clean <- merge(data_clean, class_probs[, c("subject_id", "class")], by = "subject_id")
# 将class列转换为因子格式
data_clean$class <- as.factor(data_clean$class)

# 仅保留每个 subject_id 的第一条数据
data_surve <- data_clean %>% distinct(subject_id, .keep_all = TRUE)

# 构建生存对象
surv_obj <- Surv(time = data_surve$EventTime, event = data_surve$EventStatus)

# 使用survfit函数拟合生存曲线
fit <- survfit(surv_obj ~ class, data = data_surve)

#使用 survdiff() 进行 Log-rank 检验 来计算 P 值，以比较不同 class 组的生存曲线是否有显著差异
survdiff(Surv(EventTime, EventStatus) ~ class, data = data_clean)

####比较两两之间的生存率差异
pairwise_survdiff(Surv(EventTime, EventStatus) ~ class, data = data_clean,p.adjust.method = "BH")



# 绘制生存曲线
surv_plot <- ggsurvplot(
  fit, 
  data = data_surve,
  conf.int = FALSE,            
  pval = "P<0.0001",           
  risk.table = TRUE,           
  risk.table.height = 0.3,     
  xlim = c(0, 28),             
  ylim = c(0, 1),             
  break.time.by = 7,           
  legend.title = "Class",      
  legend.labs = c("Class 1", "Class 2", "Class 3"), 
  palette = c("goldenrod", "forestgreen", "firebrick"), 
  ggtheme = theme_bw() + 
    theme(
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),  # 移除上边和右边边框
      axis.line = element_line(color = "black"), # 仅保留 x/y 轴
      legend.position = "topright"
    )
)

# 修改 x 轴标题
surv_plot$plot <- surv_plot$plot + xlab("Time (Day)")

surv_plot$plot <- surv_plot$plot + 
  scale_x_continuous(
    limits = c(0, 28), 
    breaks = seq(0, 28, by = 7),  # 明确设置刻度
    expand = c(0, 0.05)  # 右边留出一点点空隙，防止28被裁剪
  ) +
  scale_y_continuous(
    limits = c(0, 1), 
    expand = c(0, 0)
  )

# 显示优化后的生存曲线
print(surv_plot)


#######################################################################################################
######################################################################################################
######################样条函数拟合更灵活的曲线
library(splines)
initial_splines_hlme<-hlme(fixed = NLR ~ ns(time, df=4), 
     random = ~ time, subject = "subject_id", ng = 1, data = data_clean,verbose=TRUE)


final_splines_hlme_2 <- gridsearch(
  m = hlme(fixed = NLR ~ ns(time, df=4), mixture = ~ ns(time, df=4), 
           random = ~ time, subject = "subject_id", ng = 2, data = data_clean,
           verbose = TRUE),
  rep = 5,
  maxiter = maxiter,
  minit = initial_splines_hlme
)

final_splines_hlme_3_rep25 <- gridsearch(
  m = hlme(fixed = NLR ~ ns(time, df=4), mixture = ~ ns(time, df=4), 
                random = ~ time, subject = "subject_id", ng = 3, data = data_clean,
           verbose = TRUE),
  rep = 25,
  maxiter = maxiter,
  minit = initial_splines_hlme
)



final_splines_hlme_4_rep25 <- gridsearch(
  m = hlme(fixed = NLR ~ ns(time, df=4), mixture = ~ ns(time, df=4), 
           random = ~ time, subject = "subject_id", ng = 4, data = data_clean,
           verbose = TRUE),
  rep = 25,
  maxiter = maxiter,
  minit = initial_splines_hlme
)

 ###### 总结：splines拟合效果不如线性拟合！！！！！
###########################################################################################################
############################################################################################################
#######绘制splines 轨迹
library(ggplot2)
library(dplyr)
library(splines)


# 预测时间点
new_time <- seq(min(data_clean$time), max(data_clean$time), length.out = 100)

# 计算样条基函数
spline_basis <- ns(new_time, df = 4)  # 生成四个自由度的样条基函数矩阵

# 将样条基函数转换为数据框，并查看列名
spline_basis_df <- as.data.frame(spline_basis)

# 计算每个类别的预测值
predictions <- data.frame(
  time = new_time,
  class1 = 11.88546 + spline_basis_df[[1]] * (-0.71700) + spline_basis_df[[2]] * (-4.08091) + 
    spline_basis_df[[3]] * (0.01099) + spline_basis_df[[4]] * (-4.93868),
  class2 = 26.54476 + spline_basis_df[[1]] * (19.04754) + spline_basis_df[[2]] * (44.48309) + 
    spline_basis_df[[3]] * (43.17337) + spline_basis_df[[4]] * (-18.28362),
  class3 = 93.99560 + spline_basis_df[[1]] * (-58.23740) + spline_basis_df[[2]] * (-62.31752) + 
    spline_basis_df[[3]] * (-138.86804) + spline_basis_df[[4]] * (-35.03044)
)


# 转换为长格式
predictions_long <- predictions %>%
  tidyr::pivot_longer(cols = -time, names_to = "class", values_to = "NLR")

# 绘图
ggplot(predictions_long, aes(x = time, y = NLR, color = class, group = class)) +
  geom_line(size = 1.2) +
  labs(title = "NLR 轨迹图",
       x = "时间",
       y = "NLR",
       color = "类别") +
  theme_minimal()
   



