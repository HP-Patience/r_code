# 设置工作目录（可选）
setwd("E:/实验设计/R")

# 安装必要包（只需安装一次）
install.packages("reshape2")
install.packages("DescTools")

# 加载包
library(reshape2)
library(DescTools)
#例1
#设有3个葡萄品种，随机抽样，每个品种各测定5株的单株果重(单位：kg)，如下表，问不同品种的单株果重有无显著差异？
#      表1 葡萄不同品种单株果重
#     品种        单株果重
#      甲    11  6   12  8   6
#      乙    12  16  21  13  16
#      丙    13  12  8   3   6
# ============================
# 例1：不同葡萄品种单株果重分析
# ============================
#例1 #设有3个葡萄品种，随机抽样，每个品种各测定5株的单株果重(单位：kg)，如下表，问不同品种的单株果重有无显著差异？ # 表1 葡萄不同品种单株果重 # 品种 单株果重 # 甲 11 6 12 8 6 # 乙 12 16 21 13 16 # 丙 13 12 8 3 6
# 构建数据：3个品种，每个品种5株
grape <- data.frame(
  品种 = factor(rep(c("甲", "乙", "丙"), each = 5)),
  单株果重 = c(
    11, 6, 12, 8, 6,    # 甲
    12, 16, 21, 13, 16, # 乙
    13, 12, 8, 3, 6     # 丙
  )
)

# 查看数据
grape

# 探索性分析
mean(grape$单株果重)
tapply(grape$单株果重, grape$品种, mean)  # 按品种计算均值

# ============================
# 方差分析
# ============================
aov.grape <- aov(单株果重 ~ 品种, data = grape)
summary(aov.grape)

# 编制完整方差分析表函数
anova.tab <- function(fm) {
  tab <- summary(fm)[[1]]
  k <- nrow(tab)
  temp <- c(sum(tab[, "Df"]), sum(tab[, "Sum Sq"]), rep(NA, ncol(tab)-2))
  tab["Total",] <- temp
  tab
}

anova.tab(aov.grape)





#例2.1.1 绿茶的叶酸含量
#       表2.1.2 绿茶的叶酸含量
#  因子A的水平        数据（毫克）
#      A1        7.9   6.2   6.6   8.6   8.9   10.1   9.6
#      A2        5.7   7.5   9.8   6.1   8.4    
#      A3        6.4   7.1   7.9   4.5   5.0   4.0
#      A4        6.8   7.5   5.0   5.3   6.1   7.4

# 构建数据框
tea <- data.frame(
  因子A = factor(rep(c("A1", "A2", "A3", "A4"), times = c(7, 5, 6, 6))),
  叶酸含量 = c(
    7.9, 6.2, 6.6, 8.6, 8.9, 10.1, 9.6,   # A1
    5.7, 7.5, 9.8, 6.1, 8.4,              # A2
    6.4, 7.1, 7.9, 4.5, 5.0, 4.0,         # A3
    6.8, 7.5, 5.0, 5.3, 6.1, 7.4          # A4
  )
)

# 查看数据
tea

# 探索性分析
mean(tea$叶酸含量)
tapply(tea$叶酸含量, tea$因子A, mean)  # 按因子水平计算均值

# ============================
# 方差分析
# ============================
aov.tea <- aov(叶酸含量 ~ 因子A, data = tea)
summary(aov.tea)

# 使用之前定义的完整方差分析表函数
anova.tab <- function(fm) {
  tab <- summary(fm)[[1]]
  k <- nrow(tab)
  temp <- c(sum(tab[, "Df"]), sum(tab[, "Sum Sq"]), rep(NA, ncol(tab)-2))
  tab["Total",] <- temp
  tab
}

anova.tab(aov.tea)



#例2.2.5  比较四种不同牌号的防锈剂防锈能力
#       表2.2.4 防锈能力数据及有关计算
#     因子A        数据 
#      A1        43.9   39.0   46.7   43.8   44.2   47.7   43.6  38.9  43.6  40.0
#      A2        89.8   87.1   92.7   90.6   87.7   92.4   86.1  88.1  90.8  89.1    
#      A3        68.4   69.3   68.5   66.4   70.0   68.1   70.6  65.2  63.8  69.2
#      A4        36.2   45.2   40.7   40.5   39.3   40.3   43.2  38.7  40.9  39.7

# ============================
# 例2.2.5 不同牌号防锈剂防锈能力
# ============================

library(reshape2)
library(DescTools)

# 构建宽格式数据
antirust <- data.frame(
  A1 = c(43.9, 39.0, 46.7, 43.8, 44.2, 47.7, 43.6, 38.9, 43.6, 40.0),
  A2 = c(89.8, 87.1, 92.7, 90.6, 87.7, 92.4, 86.1, 88.1, 90.8, 89.1),
  A3 = c(68.4, 69.3, 68.5, 66.4, 70.0, 68.1, 70.6, 65.2, 63.8, 69.2),
  A4 = c(36.2, 45.2, 40.7, 40.5, 39.3, 40.3, 43.2, 38.7, 40.9, 39.7)
)

# 转换为长数据
data.antirust <- melt(antirust,
                      variable.name = "因子A",
                      value.name = "防锈能力")

# 单因素方差分析
aov.antirust <- aov(防锈能力 ~ 因子A, data = data.antirust)
summary(aov.antirust)

# 完整方差分析表
anova.tab <- function(fm) {
  tab <- summary(fm)[[1]]
  temp <- c(sum(tab[, "Df"]), sum(tab[, "Sum Sq"]), rep(NA, ncol(tab)-2))
  tab["Total",] <- temp
  tab
}

anova.tab(aov.antirust)











###多重比较
#install.packages("DescTools")##确保自己的R程序是最新的版本R4.2.1，才可以安装DescTools程序包.
library(DescTools)
# PostHocTest(x, which = NULL,
# method = c("hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"),
# conf.level = 0.95, ordered = FALSE, ...)
# #参数	意义
#参数 x：	为方差分析的模型，即model
#参数method：	选择何种检验方法
#参数 conf.level：	给定置信区间对应的置信水平

#例2.2.5  比较四种不同牌号的防锈剂防锈能力
#Tukey法（重复数相等）
PostHocTest(aov.antirust,method = "hsd")

#例2.1.1 绿茶的叶酸含量
#scheffe法（重复数不等）
PostHocTest(aov.tea,method = "scheffe")


###正态性检验
#正态Q-Q图（样本量≥8）
qqnorm(data.antirust$防锈能力[data.antirust$因子A == "A1"])
qqline(data.antirust$防锈能力[data.antirust$因子A == "A1"])


#残差Q-Q图（样本量过少时采用）
res <- residuals(aov.tea)
qqnorm(res)
qqline(res)


#W检验
shapiro.test(data.antirust$防锈能力[data.antirust$因子A == "A1"])
shapiro.test(res)

###方差齐性检验（样本量≥5）
bartlett.test(叶酸含量 ~ 因子A, data = tea)






















###课后习题
# 请用R语言实现习题2.2-14、2.3-3的方差分析、多重比较、正态性检验、方差齐性检验，并汇总为实验报告
# （word、pdf格式均可，文件命名为学号+名字+作业1），并于10.9前上传至ftp://10.3.19.43上对应的文件夹内，
# 即实验设计//23统计//作业提交//作业1。
# （ftp://10.3.19.43/%CA%B5%D1%E9%C9%E8%BC%C6/23%CD%B3%BC%C6/%D7%F7%D2%B5%CC%E1%BD%BB/%D7%F7%D2%B51/）
