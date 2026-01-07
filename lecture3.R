anova.tab=function(fm){
  tab=summary(fm)[[1]]
  k=length(tab)-2
  temp=c(sum(tab[,1]),sum(tab[,2]),rep(NA,k))
  tab["Total",]=temp
  tab
}





#BIBD
method=read.csv("method.csv")#区组列名和处理行名加入字符A,B,如果只是输入数值1,2,..的话后面方差分析时需要进行因子化。
#处理效应、区组效应估计
estimate=function(data){
  #data=method#仅作展示用，写函数时无需加入
  M=as.matrix(data[,-1])#去掉第一列后转换为矩阵
  N=M
  M[is.na(M)]=0#把所有空值变为0
  Ti=apply(M,1,sum)#算行和Ti
  Bj=apply(M,2,sum)#算列和Bj
  T=sum(Ti)#算总和T #T=sum(Bj)
  N[!is.na(N)]=1#把非NA数据转为1
  N[is.na(N)]=0#把NA数据转为0，从而生成BIBD关联矩阵
  v=nrow(N)#由行数推出处理数v
  b=ncol(N)#由列数推出区组数b
  r=sum(N[1,])#由关联矩阵行和得处理重复数r
  k=sum(N[,1])#由关联矩阵列和得区组大小k
  lambda=sum(N[1,]*N[2,])#由关联矩阵两行内积得相遇数lambda
  n=v*r#总试验次数
  mu=T/n#总均值mu的估计值
  ai=k/(lambda*v)*(Ti-N%*%Bj/k)#处理效应ai的估计值
  bj=Bj/k-mu-t(N)%*%ai/k#区组效应bj的估计值
  cat("处理效应为：",ai,"区组效应为：",bj)#输出数据
}
estimate(method)


library(reshape2)
pre.method=melt(method, id = c("treat"),variable.name = "block",value.name = "result")
data.method=na.omit(pre.method)#na.omit函数可以直接删除NA值所在的行
aov.method=aov(result~block+treat,data=data.method)####记住区组序列和处理序列顺序，调整项放后面。调整处理平方和——教材
anova.tab(aov.method)
aov.method1=aov(result~treat+block,data=data.method)#调整区组平方和，不采用！
anova.tab(aov.method1)

install.packages("agricolae")
library(agricolae)
#agricolae包中的函数BIB.test(block, trt, y, test = c("lsd","tukey","duncan","waller","snk"), 
#alpha = 0.05, group = TRUE,console=FALSE)，包含方差分析（调整处理平方和）+多重比较结果
#其中：block：区组序列, trt：处理序列, y：数据结果，test：多重比较方法，alpha：显著性水平，group：分组结果，console：为TRUE时展示方差分析结果，默认FALSE则不展示。
bib.method=BIB.test(block=data.method$block, trt=data.method$treat, y=data.method$result, test = "tukey", alpha = 0.05, group = TRUE,console=TRUE)
#切记：不能用DescTools里的PostHocTest函数，因这是不完全设计，处理重复数不再为区组数。





#习题3.3-5

steel=read.csv("steel.csv")
estimate(steel)
pre.steel=melt(steel, id = c("treat"),variable.name = "block",value.name = "result")
data.steel=na.omit(pre.steel)
aov.steel=aov(result~block+treat,data=data.steel)#记住区组序列和处理序列顺序，调整项放后面。调整处理平方和——教材
anova.tab(aov.steel)
bib.steel=BIB.test(block=data.steel$block, trt=data.steel$treat, y=data.steel$result, test = "tukey", alpha = 0.05, group = TRUE,console=TRUE)



















###课后习题
#习题3.3(旧版3.2)-2、3、4，（word、pdf格式均可，文件命名为学号+名字+作业3），并于11.11前上传至ftp://10.3.19.43上对应的文件夹内，
# 即实验设计//23统计//作业提交//作业3。
#（ftp://10.3.19.43/%CA%B5%D1%E9%C9%E8%BC%C6/23%CD%B3%BC%C6/%D7%F7%D2%B5%CC%E1%BD%BB/%D7%F7%D2%B53/）

