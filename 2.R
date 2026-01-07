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
treat=gl(5,4,labels=c("A1", "A2", "A3", "A4", "A5"))##生成处理列，gl(k,n),k是处理数，n是区组数
#treat=gl(5,4)
block=factor(c(rep(c("B1", "B2", "B3", "B4"), 5))) ###生成区组
#block=factor(rep(1:4,5))
result= c(17,13,8,2,35,32,28,11,25,15,5,4,17,19,8,10,33,25,10,19) #按行展开：y11,y12,y13,...y21,y22,y23,...,yv1,yv2,yv3,...

#方法二
library(reshape2)
rice=read.csv("rice.csv")
data.rice=melt(rice) #注意列名
data.rice=melt(rice, id = c("treat"),variable.name = "block",value.name = "result")
#melt(短数据名称，id="不做变换的列名"，variable.name="自定义",value.name="自定义"),其中variable和value的值可自定义，默认值为variable和value。
#id:你不想改变的数据列，未指定的剩余变量归为measure变量，会被melt，也就是变成长数据。id有时会默认，但是正规起见我们还是提前定义。


#方差分析
aov.rice=aov(result~treat+block,data=data.rice)#若利用直接输入法，可直接写aov(result~treat+block)
summary(aov.rice)
anova.tab(aov.rice)
aov.rice2=aov(result~treat,data=data.rice)#不设立区组，会引出错误结论。
summary(aov.rice2)
anova.tab(aov.rice2)

#多重比较
library(DescTools)
PostHocTest(aov.rice,method = "hsd")#T法

#残差Q-Q图
res=residuals(aov.rice)
qqnorm(res)
qqline(res)




#例3.1.3

treat=gl(4,5,labels=c("A1", "A2", "A3", "A4"))##生成处理列，gl(k,n),k是处理数，n是区组数
block=factor(c(rep(c("B1", "B2", "B3", "B4", "B5"), 4))) ###生成区组
result= c(3,-1,3,1,-3,3,-2,4,2,-1,5,2,4,3,-2,5,2,7,5,2) 
aov.chemic =aov(result ~ treat+block) ##随机区组方差分析
summary(aov.chemic)             ##查看模型
anova.tab(aov.chemic)

chemic=read.csv("chemic.csv")
data.chemic=melt(chemic, id = c("treat"),variable.name = "block",value.name = "result")
aov.chemic=aov(result~treat+ block,data=data.chemic)
anova.tab(aov.chemic)
aov.chemic2=aov(result~treat,data=data.chemic)#不设立区组，会引出错误结论。
anova.tab(aov.chemic2)

#多重比较
library(DescTools)
PostHocTest(aov.chemic,method = "hsd")

#残差Q-Q图
res=residuals(aov.chemic)
qqnorm(res)
qqline(res)





###拉丁方设计（LSD）
install.packages("tidyr")



rocket=read.csv("rocket.csv")
library(reshape2)
pre.rocket=melt(rocket, id = c("row"),variable.name = "col",value.name = "data")#id必须要设，命名可默认
library(tidyr)
data.rocket=separate(pre.rocket,c("data"),into=c("Latin","result"),sep="=")
aov.rocket=aov(result~Latin+row+col,data.rocket)
summary(aov.rocket)
anova.tab(aov.rocket)

library(DescTools)
PostHocTest(aov.rocket,method = "hsd")

#希腊拉丁方设计（GLSD）

rocket1=read.csv("rocket1.csv")
library(reshape2)
pre.rocket1=melt(rocket1, id = c("row"),variable.name = "col",value.name = "data")
data.rocket1=separate(pre.rocket1,c("data"),into=c("Latin","Greek","result"),sep="=")
aov.rocket1=aov(result~Latin+Greek+row+col,data.rocket1)
summary(aov.rocket1)
anova.tab(aov.rocket1)

library(DescTools)
PostHocTest(aov.rocket1,method = "hsd")







###课后习题
#习题3.1-5、6，其中正态性用残差Q-Q图检验，因第6题区组效应为随机效应，故将6（3）改为各处理效应的估计以及区组效应的方差分量的估计。
#（word、pdf格式均可，文件命名为学号+名字+作业2），并于10.28前上传至ftp://10.3.19.43上对应的文件夹内，
# 即实验设计//23统计//作业提交//作业2。
#（ftp://10.3.19.43/%CA%B5%D1%E9%C9%E8%BC%C6/23%CD%B3%BC%C6/%D7%F7%D2%B5%CC%E1%BD%BB/%D7%F7%D2%B52/）

































