#更改路径
#ctrl+shift+H更改路径或利用setwd()
#导入数据
#调用所需包

#2.3-3
library(reshape2)
worker=read.csv("worker.csv")
data.worker=melt(worker)
#方差分析
aov.worker=aov(value~variable,data.worker)
summary(aov.worker)
#多重比较
library(DescTools)
PostHocTest(aov.worker,method = "scheffe")
#正态性检验
#1.残差Q-Q图
res=residuals(aov.worker)
qqnorm(res)
qqline(res)
#2.W检验
shapiro.test(res)
#方差齐性检验
bartlett.test(value~variable, data = data.worker)





#3.1-5
account=read.csv("account.csv")
data.account=melt(account, id = c("treat"),variable.name = "block",value.name = "result")
aov.account=aov(result~treat+block,data=data.account)
data.account=melt(account)#省略写法
aov.account=aov(value~treat+variable,data=data.account)
summary(aov.account)
#残差Q-Q图
res=residuals(aov.account)
qqnorm(res)
qqline(res)
#多重比较
PostHocTest(aov.account,method = "hsd")



#3.3-3
elec=read.csv("elec.csv")
data.elec=melt(elec, id = c("treat"),variable.name = "block",value.name = "result")
data.elec=na.omit(data.elec)
aov.elec=aov(result~block+treat,data=data.elec)#处理放后面
summary(aov.elec)
#多重比较
library(agricolae)
bib.elec=BIB.test(block=data.elec$block, trt=data.elec$treat, y=data.elec$result, test = "tukey", alpha = 0.05, group = T,console=T)










#4.3-1
cotton=read.csv("cotton.csv",fileEncoding = "UTF-8-BOM")
X=cotton[,c(2:3,5:6)]
y=cotton[,9]
result.cotton=oa.range(X,y)
result.cotton
oa.rplot(result.cotton$Kmean,colnames(X))
#方差分析
data.cotton=data.frame(apply(X,2,factor),y)
aov.cotton=aov(y~A+B+C+A:C,data=data.cotton)
summary(aov.cotton)
adj.cotton=anova.adj(aov.cotton)
oa.com(X,y,"AC",k=0)
comb=c("A2C1","C1","B2")
oa.estimate(comb,X,y,adj.cotton,0.1)
#参数估计
0.11875+qt(0.95,4)*sqrt(0.00375/4)/sqrt(1.6)
0.11875-qt(0.95,4)*sqrt(0.00375/4)/sqrt(1.6)







