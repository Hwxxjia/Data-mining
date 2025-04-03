#############################################################################################3
#################  比较不同水平NLR_Baseline 的死亡风险(中位数9.590，分为low和high组)

# 加载必要的包
library(dplyr)         # 数据处理
library(survival)      # 生存分析
library(rms)           # 限制性三次样条 (RCS)

# 读取数据
data <- read.csv("C:/Users/ahlil/Desktop/data_clean_NLR_change_denull_detime.csv")



# 根据 subject_id 去重，确保每个病例只有一行数据
data_unique <- data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup()



# 方法 1：三分位法划分 NLR_Baseline
quantiles <- quantile(data_unique$NLR_Baseline, probs = c(0, 1/2, 1))

data_unique <- data_unique %>%
  mutate(NLR_Baseline_group = case_when(
    NLR_Baseline <= quantiles[2] ~ "Low",
    NLR_Baseline > quantiles[2] ~ "High"
  ))

write.csv(data_unique, 
          file = "C:/Users/ahlil/Desktop/NLR_change_Baseline_quantiles.csv",
          row.names = FALSE)
# 生成数据分布对象
dd <- datadist(data_unique)
options(datadist = "dd")  # 设置全局选项

# 检查分组结果
group_counts <- data_unique %>%
  group_by(NLR_Baseline_group) %>%
  summarise(n = n())

print(group_counts)

# 构建 Cox 比例风险模型
fit_group <- cph(Surv(EventTime, EventStatus) ~ NLR_Baseline_group + NLR_change, 
                 data = data_unique, x = TRUE, y = TRUE)

# 查看模型摘要
summary(fit_group)


