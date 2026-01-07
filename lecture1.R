
#编写R程序或载入路径时尽量避免出现中文，否则会出现乱码。若出现乱码，可通过如下方法进行解决：
#依次点击File-Reopen with encoding-Choose encoding-'utf-8'确认
#点击Tools-global options-code-saving调整text encoding为‘utf-8'，关闭窗口重新再次导入
#更改路径
#ctrl+shift+H更改路径或利用setwd()
setwd("E:/实验设计/R")
#导入数据
grape=read.csv("grape.csv")
#安装包
install.packages("reshape2")
library(reshape2)
install.packages("DescTools")##确保自己的R程序是最新的版本R4.2.1，才可以安装DescTools程序包.
library(DescTools)







#例1
#设有3个葡萄品种，随机抽样，每个品种各测定5株的单株果重(单位：kg)，如下表，问不同品种的单株果重有无显著差异？
#      表1 葡萄不同品种单株果重
#     品种        单株果重
#      甲    11  6   12  8   6
#      乙    12  16  21  13  16
#      丙    13  12  8   3   6


#直接输入法
#构建数据列
y=c(11,6,12,8,6,12,16,21,13,16,13,12,8,3,6)
#构建因子列
A=gl(3,5)#重复次数相等
A
A=factor(rep(1:3,c(5,5,5)))
A=factor(rep(1:3,c(5,3,5)))#重复次数不等

#探索性分析
mean(y)
c(mean(y[A==1]),mean(y[A==2]),mean(y[A==3]))


#.csv数据导入方法
install.packages("reshape2")
library(reshape2)
grape=read.csv("grape.csv")#如遇乱码可加encoding="utf-8_sig"或"utf-8-BOM"
#确保文件在工作目录里，可用ctrl+shift+H快捷键更改到文件所在目录。
data.grape=melt(grape)
#melt函数把宽数据改为一对一的长数据，会提示No id variables; using all as measure variables，是因为没有
#设置固定不变的数据，我们这里不需要固定数据，所以不用管，具体可以看如下例子。
a=data.frame(id = c("1", "1", "2", "2"),
             time = c("1", "2", "1", "2"),
             x1 = c("5", "3", "6", "2"),
             x2 = c("6", "5", "1", "4"))
m=melt(a, id = c("id", "time"))
#未赋值variable.name，value.name则默认为variable,value.



#方差分析

aov.grape=aov(value~variable,data.grape)#直接输入法可用aov(y~A)
summary(aov.grape)
#编制函数构建完整方差分析表
anova.tab=function(fm){
  tab=summary(fm)[[1]]
  k=length(tab)-2
  temp=c(sum(tab[,1]),sum(tab[,2]),rep(NA,k))
  tab["Total",]=temp
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

tea=read.csv("tea.csv")
data.tea=melt(tea)
aov.tea=aov(value~variable,data.tea)
summary(aov.tea)
anova.tab(aov.tea)

#例2.2.5  比较四种不同牌号的防锈剂防锈能力
#       表2.2.4 防锈能力数据及有关计算
#     因子A        数据 
#      A1        43.9   39.0   46.7   43.8   44.2   47.7   43.6  38.9  43.6  40.0
#      A2        89.8   87.1   92.7   90.6   87.7   92.4   86.1  88.1  90.8  89.1    
#      A3        68.4   69.3   68.5   66.4   70.0   68.1   70.6  65.2  63.8  69.2
#      A4        36.2   45.2   40.7   40.5   39.3   40.3   43.2  38.7  40.9  39.7

antirust=read.csv("antirust.csv")
data.antirust=melt(antirust)
aov.antirust=aov(value~variable,data.antirust)
summary(aov.antirust)
anova.tab(aov.antirust)


###多重比较
install.packages("DescTools")##确保自己的R程序是最新的版本R4.2.1，才可以安装DescTools程序包.
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
qqnorm(data.antirust$value[data.antirust$variable=="A1"])
qqline(data.antirust$value[data.antirust$variable=="A1"])

#残差Q-Q图（样本量过少时采用）
res=residuals(aov.tea)
qqnorm(res)
qqline(res)

#W检验
shapiro.test(data.antirust$value[data.antirust$variable=="A1"])
shapiro.test(res)

###方差齐性检验（样本量≥5）
bartlett.test(value~variable, data = data.tea)



















###课后习题
# 请用R语言实现习题2.2-14、2.3-3的方差分析、多重比较、正态性检验、方差齐性检验，并汇总为实验报告
# （word、pdf格式均可，文件命名为学号+名字+作业1），并于10.9前上传至ftp://10.3.19.43上对应的文件夹内，
# 即实验设计//23统计//作业提交//作业1。
# （ftp://10.3.19.43/%CA%B5%D1%E9%C9%E8%BC%C6/23%CD%B3%BC%C6/%D7%F7%D2%B5%CC%E1%BD%BB/%D7%F7%D2%B51/）
