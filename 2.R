anova.tab=function(fm){
  tab=summary(fm)[[1]]
  k=length(tab)-2
  temp=c(sum(tab[,1]),sum(tab[,2]),rep(NA,k))
  tab["Total",]=temp
  tab
}


###随机化完全区组设计（RCBD）



#例3.1.1
#方法一
# 处理因子（5个处理 × 4个区组）
treat <- gl(5, 4, labels = c("A1", "A2", "A3", "A4", "A5"))

# 区组因子
block <- factor(rep(c("B1", "B2", "B3", "B4"), 5))

# 实验结果（按行展开）
result <- c(
  17, 13, 8, 2,
  35, 32, 28, 11,
  25, 15, 5, 4,
  17, 19, 8, 10,
  33, 25, 10, 19
)

# 合并为数据框
data.rice <- data.frame(treat, block, result)
data.rice

#方法二
library(reshape2)

# 构建“宽数据”
rice <- data.frame(
  treat = c("A1", "A2", "A3", "A4", "A5"),
  B1 = c(17, 35, 25, 17, 33),
  B2 = c(13, 32, 15, 19, 25),
  B3 = c(8, 28, 5, 8, 10),
  B4 = c(2, 11, 4, 10, 19)
)

# 转换为长数据
data.rice <- melt(
  rice,
  id = "treat",
  variable.name = "block",
  value.name = "result"
)

data.rice

#方差分析
aov.rice <- aov(result ~ treat + block, data = data.rice)
summary(aov.rice)
anova.tab(aov.rice)

#多重比较
library(DescTools)

PostHocTest(aov.rice, method = "hsd")


#残差Q-Q图
res <- residuals(aov.rice)
qqnorm(res)
qqline(res)











#例3.1.3
#方法一
# 处理因子（4 个处理 × 5 个区组）
treat <- gl(4, 5, labels = c("A1", "A2", "A3", "A4"))

# 区组因子
block <- factor(rep(c("B1", "B2", "B3", "B4", "B5"), 4))

# 实验结果（按行展开）
result <- c(
  3, -1, 3, 1, -3,
  3, -2, 4, 2, -1,
  5, 2, 4, 3, -2,
  5, 2, 7, 5, 2
)

# 方差分析
aov.chemic <- aov(result ~ treat + block)
summary(aov.chemic)
anova.tab(aov.chemic)

#方法二
library(reshape2)

# 构建“宽格式”数据
chemic <- data.frame(
  treat = c("A1", "A2", "A3", "A4"),
  B1 = c(3, 3, 5, 5),
  B2 = c(-1, -2, 2, 2),
  B3 = c(3, 4, 4, 7),
  B4 = c(1, 2, 3, 5),
  B5 = c(-3, -1, -2, 2)
)

# 转成长数据
data.chemic <- melt(
  chemic,
  id = "treat",
  variable.name = "block",
  value.name = "result"
)

# RCBD 方差分析
aov.chemic <- aov(result ~ treat + block, data = data.chemic)
anova.tab(aov.chemic)






#多重比较
library(DescTools)
PostHocTest(aov.chemic, method = "hsd")


#残差Q-Q图
res <- residuals(aov.chemic)
qqnorm(res)
qqline(res)






###拉丁方设计（LSD）
library(reshape2)
library(tidyr)

# 构建宽数据（单元格为 Latin=结果）
rocket <- data.frame(
  row = c("R1", "R2", "R3", "R4"),
  C1 = c("A=18", "B=20", "C=16", "D=14"),
  C2 = c("B=22", "C=19", "D=15", "A=17"),
  C3 = c("C=21", "D=18", "A=20", "B=16"),
  C4 = c("D=19", "A=21", "B=17", "C=18")
)

# melt
pre.rocket <- melt(
  rocket,
  id = "row",
  variable.name = "col",
  value.name = "data"
)

# 分离 Latin 和结果
data.rocket <- separate(
  pre.rocket,
  data,
  into = c("Latin", "result"),
  sep = "="
)

data.rocket$result <- as.numeric(data.rocket$result)
# LSD 方差分析
aov.rocket <- aov(result ~ Latin + row + col, data = data.rocket)
summary(aov.rocket)
anova.tab(aov.rocket)
# 多重比较
library(DescTools)
PostHocTest(aov.rocket, method = "hsd")





#希腊拉丁方设计（GLSD）
y ~ Latin + Greek + row + col
# 构建宽数据
rocket1 <- data.frame(
  row = c("R1", "R2", "R3", "R4"),
  C1 = c("A=α=18", "B=β=20", "C=γ=16", "D=δ=14"),
  C2 = c("B=γ=22", "C=δ=19", "D=α=15", "A=β=17"),
  C3 = c("C=δ=21", "D=α=18", "A=β=20", "B=γ=16"),
  C4 = c("D=β=19", "A=γ=21", "B=δ=17", "C=α=18")
)

# melt
pre.rocket1 <- melt(
  rocket1,
  id = "row",
  variable.name = "col",
  value.name = "data"
)

# 分离 Latin、Greek、结果
data.rocket1 <- separate(
  pre.rocket1,
  data,
  into = c("Latin", "Greek", "result"),
  sep = "="
)

data.rocket1$result <- as.numeric(data.rocket1$result)
# GLSD 方差分析
aov.rocket1 <- aov(result ~ Latin + Greek + row + col, data = data.rocket1)
summary(aov.rocket1)
anova.tab(aov.rocket1)
# 多重比较
PostHocTest(aov.rocket1, method = "hsd")






###课后习题
#习题3.1-5、6，其中正态性用残差Q-Q图检验，因第6题区组效应为随机效应，故将6（3）改为各处理效应的估计以及区组效应的方差分量的估计。
#（word、pdf格式均可，文件命名为学号+名字+作业2），并于10.28前上传至ftp://10.3.19.43上对应的文件夹内，
# 即实验设计//23统计//作业提交//作业2。
#（ftp://10.3.19.43/%CA%B5%D1%E9%C9%E8%BC%C6/23%CD%B3%BC%C6/%D7%F7%D2%B5%CC%E1%BD%BB/%D7%F7%D2%B52/）

































