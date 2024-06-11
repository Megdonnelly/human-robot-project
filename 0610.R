getwd()
setwd("D:/programming/R/ergonomic")
library(readxl)
library(ggplot2)
library(dplyr)
data <- read_excel("C:/Users/redch/Desktop/test.xlsx")
str(data)
data <- data %>% select(-学号)
colSums(is.na(data)) # 检查缺失值
numeric_columns <- c("水迷宫测试1", "水迷宫测试2", "Corsi记忆广度量表", "MRT心理旋转能力", "任务完成时间", "机器人碰撞次数")
data[numeric_columns] <- lapply(data[numeric_columns], as.numeric)
summary(data)
cor_matrix <- cor(data %>% select_if(is.numeric))
print(cor_matrix)

# 可视化相关性矩阵
cor_data <- as.data.frame(as.table(cor_matrix))
ggplot(cor_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "#87CEFA", high = "#FFB6C1", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

# 回归分析: 任务完成时间与各指标的关系
model_time_interaction <- lm(任务完成时间 ~ 水迷宫测试1 * 水迷宫测试2 + 水迷宫测试1 * MRT心理旋转能力 + 水迷宫测试1 * Corsi记忆广度量表 +
                   水迷宫测试2 * Corsi记忆广度量表 +
                   水迷宫测试2 * MRT心理旋转能力 + MRT心理旋转能力 * Corsi记忆广度量表, data = data)
summary(model_time_interaction)
library(broom) # 用于将模型输出转化为数据框
model_time_interaction <- tidy(model_time_interaction)
colnames(model_time_interaction)[colnames(model_time_interaction) == "statistic"] <- "t.value"


# 回归分析: 碰撞次数与各指标的关系
# model_collisions <- lm(机器人碰撞次数 ~ 水迷宫测试1 + 水迷宫测试2 + MRT心理旋转能力 + Corsi记忆广度量表, data = data)
# summary(model_collisions)
model_collisions_interaction <- lm(机器人碰撞次数 ~ 水迷宫测试1 * 水迷宫测试2 + 水迷宫测试1 * MRT心理旋转能力 + 水迷宫测试1 * Corsi记忆广度量表 +
                                     水迷宫测试2 * Corsi记忆广度量表 +
                                     水迷宫测试2 * MRT心理旋转能力 + MRT心理旋转能力 * Corsi记忆广度量表, data = data)
summary(model_collisions_interaction)
library(broom) # 用于将模型输出转化为数据框
model_collisions_interaction <- tidy(model_collisions_interaction)
colnames(model_collisions_interaction)[colnames(model_collisions_interaction) == "statistic"] <- "t.value"




# 任务完成时间的回归结果可视化
ggplot(data, aes(x = MRT心理旋转能力, y = 任务完成时间)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "任务完成时间与MRT心理旋转能力的关系", x = "MRT心理旋转能力", y = "任务完成时间")

# 机器人碰撞次数的回归结果可视化
ggplot(data, aes(x = MRT心理旋转能力, y = 机器人碰撞次数)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "机器人碰撞次数与MRT心理旋转能力的关系", x = "MRT心理旋转能力", y = "机器人碰撞次数")
