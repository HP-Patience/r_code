#例4.2.1
# 正交试验数据（L9, 3因素3水平）
process <- data.frame(
  run = 1:9,
  A = c(1,1,1,2,2,2,3,3,3),
  B = c(1,2,3,1,2,3,1,2,3),
  C = c(1,2,3,2,3,1,3,1,2),
  y = c(45, 48, 50, 46, 52, 49, 51, 47, 53)
)

# 因子列
X <- process[, 2:4]

# 结果列
y <- process$y


#直观分析
##########计算各水平观测值总和、均值
oa.range=function(X,y){
  X=as.matrix(X)#转换为矩阵
  result=list()#建立结果列表
  n1=ncol(X)#n1:X的列数
  x=unique(as.vector(X))#数据转为向量后剔除重复元素，列出互不相同的元素，即水平1,2,3,...,q
  a=length(x)#算出不同元素的个数，也就是水平数q
  A1=matrix(0,a,n1);A2=A1;A3=A1#创建元素全为0，行数为a，列数为n1的矩阵A1,A2以及A3
  for(i in x){
    G=1*(X==i)#通过相乘把逻辑值转换为数值，或用apply(X==i,2,as.numeric)转换为对应数值矩阵。G矩阵中对应i水平所在的位置赋值1，其余位置赋为0
    A1[i,]=apply(G*y,2,sum)#G的每一列与数据结果列y对应元素相乘，代表把值为1的位置赋值为所在行的数据结果。算出每列的求和，即为该列i水平的和Ti
    A2[i,]=apply(G,2,sum)#算出每列和，即为出现i水平的个数
    A3[i,]=A1[i,]/A2[i,]#Ti/水平重复数=\bar{Ti}
  }
  R=matrix(0,2,n1)#创建元素为0，行数为2，列数为n1的矩阵R
  #for(j in 1:n1){R[,j]=range(A3[,j][!is.na(A3[,j])])}
  for(j in 1:n1){R[,j]=range(A3[,j], na.rm = TRUE)}#去除空值后，给出每列的最大最小值
  result$Ksum=A1#result的Ksum列表包括所有列的Ti
  result$Kmean=A3#result的Kmean列表包括所有列的\bar{Ti}
  result$Max.min=R#result的Max.min列表包括所有列的最大最小值
  result$Range=R[2,]-R[1,]#result的Max.min列表包括所有列的极差
  result$Korder=colnames(X)[order(result$Range,decreasing = T)]####################利用列名来排主次顺序
  result#输出结果
}

##########水平均值图
oa.rplot=function(Kmean,Kset){#####################利用列名来列出对应的均值图
  nk1=nrow(Kmean);nk2=ncol(Kmean)#nk1:均值矩阵的行数，nk2：均值矩阵的列数
  K=as.vector(rbind(Kmean,NA))#与NA向量行合并后转换为向量，即按列展开Kmean，并在每两列间加NA隔开
  n3=length(K)#n3：向量K的长度
  plot(K,type="l",lwd=2,xaxt="n")  #type="l" 显示线条，lwd：线条宽度为2,xaxt = "n" 隐藏横坐标轴
  points(K,col=2,pch=16)#pch:点的形状,col:点的颜色
  Cset=rep(c(1:nk1,NA),nk2)#横坐标轴向量
  axis(1,at=seq(1,n3,1),label=Cset)#1表示在图形的下方绘制坐标轴，seq(from,to,by)函数是产生等距间隔数列的函数
  axis(3,at=seq(2,n3-1,length.out =length(Kset)),tick=F,label=Kset)#3表示在图形的上方绘制坐标轴，写上对应的因子名
}



result.process=oa.range(X,y)#代入正交表因子列以及数据结果列
result.process
oa.rplot(result.process$Kmean,colnames(X))#画出水平均值图





#方差分析
data.process=data.frame(apply(X,2,factor),y)#建立数据框
aov.process=aov(y~A+B+C,data=data.process)#方差分析
summary(aov.process)






##########先把小于误差均方和的因子/交互作用均方和对应的平方和归入到误差平方和中，得到调整的方差分析表1；
##########再在方差分析表1中把不显著的因子/交互作用平方和（p值大于给定的显著性水平alpha，默认为0.1）归入误差平方和中，得到最终的方差分析表。
anova.adj=function(fm,alpha=0.1){
  tab=summary(fm)[[1]]
  a0=which(tab[1:nrow(tab)-1,3]<tab[nrow(tab),3])#找出小于误差均方和的因子均方和所在位置
  if(length(a0)>=1) {
    for(i in 1:length(a0)){
      tab[nrow(tab),1]=tab[nrow(tab),1]+tab[a0[i],1]#把对应因子自由度并入误差自由度
      tab[nrow(tab),2]=tab[nrow(tab),2]+tab[a0[i],2]}#把对应因子平方和并入误差平方和
    tab[nrow(tab),3]=tab[nrow(tab),2]/tab[nrow(tab),1]#重新计算误差均方和
    tab=tab[-a0,]#去除不显著因子所在行
    tab[1:nrow(tab)-1,4]=tab[1:nrow(tab)-1,3]/tab[nrow(tab),3]#重新计算各因子F比
    for(i in 1:nrow(tab)-1){
      tab[i,5]=pf(tab[i,4],tab[i,1],tab[nrow(tab),1],lower.tail = FALSE)}#计算对应p值
  }
  print("调整方差分析表1(确定显著因子/交互作用)")
  print(tab)
  a=which(tab[1:nrow(tab)-1,5]>alpha)#找出不显著的因子平方和所在位置
  if(length(a)>=1) {
    for(i in 1:length(a)){
      tab[nrow(tab),1]=tab[nrow(tab),1]+tab[a[i],1]#把不显著因子自由度并入误差自由度
      tab[nrow(tab),2]=tab[nrow(tab),2]+tab[a[i],2]}#把不显著因子平方和并入误差平方和
    tab[nrow(tab),3]=tab[nrow(tab),2]/tab[nrow(tab),1]#重新计算误差均方和
    tab=tab[-a,]#去除不显著因子所在行
    tab[1:nrow(tab)-1,4]=tab[1:nrow(tab)-1,3]/tab[nrow(tab),3]#重新计算各因子F比
    for(i in 1:nrow(tab)-1){
      tab[i,5]=pf(tab[i,4],tab[i,1],tab[nrow(tab),1],lower.tail = FALSE)}#计算对应p值
  }
  print("调整方差分析表2(确定新误差均方和与自由度)")
  print(tab[nrow(tab),1:3])
  tab
}

#例4.2.1
#调整方差分析表
adj.process=anova.adj(aov.process)









#例4.3.1
# 例4.3.1 正交试验（含交互作用）
medicine <- data.frame(
  run = 1:9,
  A = c(1,1,1,2,2,2,3,3,3),
  B = c(1,2,3,1,2,3,1,2,3),
  C = c(1,2,3,2,3,1,3,1,2),
  D = c(1,2,3,3,1,2,2,3,1),
  AB = c(1,2,3,2,3,1,3,1,2),  # 交互作用列
  y = c(82, 85, 88, 84, 90, 87, 89, 86, 91)
)

# 因子与交互作用列
X <- medicine[, c("A", "B", "C", "D", "AB")]

# 结果列
y <- medicine$y
#直观分析
result.medicine=oa.range(X,y)#代入正交表因子列以及数据结果列
result.medicine
oa.rplot(result.medicine$Kmean,colnames(X))#画出水平均值图
#方差分析
data.medicine=data.frame(apply(X,2,factor),y)#建立数据框
aov.medicine=aov(y~A+B+A:B+C+D,data=data.medicine)#方差分析
summary(aov.medicine)
adj.medicine=anova.adj(aov.medicine)









##########最优搭配
oa.com=function(X,y,z,k=1){#k=1（默认）代表指标是望大特性，选择最大值；否则指标是望小特性（例如可取k=0），选择最小值;z取交互作用字符，如“AB"
  X=as.matrix(X)#转换为矩阵
  com=list()#建立结果列表
  z=c(substr(z,1,1),substr(z,2,2))#分出交互作用中的两个因子名,substr(x, start, stop)
  A=X[,colnames(X)==z[1]]#找到对应因子所在列
  B=X[,colnames(X)==z[2]]
  x1=unique(A)#数据转为向量后剔除重复元素，列出互不相同的元素，即水平1,2,3,...,q
  a1=length(x1)#算出不同元素的个数，也就是水平数q
  x2=unique(B)#数据转为向量后剔除重复元素，列出互不相同的元素，即水平1,2,3,...,q
  a2=length(x2)#算出不同元素的个数，也就是水平数q
  G=matrix(0,a1,a2);col=rep(NA,a2);row=rep(NA,a1);
  for(i in x1){
    for(j in x2){
      G[i,j]=sum((A==i)*(B==j)*y)/sum((A==i)*(B==j))#找出AiBj搭配下的数据，求取均值
      col[j]=paste(z[2], j, sep = "")#列名序列，如B与j的字符合并为Bj
    }
    row[i]=paste(z[1], i, sep = "")#行名序列
  }
  colnames(G)=col;rownames(G)=row#添加二元搭配表的行名、列名
  if(k==1){
    optimal=t(rbind(z,which(G==max(G),arr.ind = TRUE)))}#指标为望大特性，选取最大值
  else{optimal=t(rbind(z,which(G==min(G),arr.ind = TRUE)))}#否则选取最小值
  colnames(optimal)=NULL;rownames(optimal)=NULL#去除最优搭配中的行名、列名
  com$G=G#二元搭配表
  com$optimal=optimal#最优搭配
  com#输出结果
}

##########估计
oa.estimate=function(comb,X,y,adj.fm,alpha){#comb: 最佳水平组合，如c("A2B1","C2","A2"...),，若单独因子A也显著，则需要再写上“A2”(与交互作用中的水平要对应);alpha：置信水平中的参数
  estimate=list()#建立结果列表
  lb=length(comb)#显著因子/交互作用总个数
  X=as.matrix(X)#转换为矩阵
  n=nrow(X)#n:X的行数
  G=matrix(0,n,lb+1)#创建元素全为0，行数为n，列数为lb+1的矩阵G
  G[,ncol(G)]=matrix(1,n,1)/n#让矩阵G最后一列为\bar{y}中的所有分量
  for(i in 1:lb){
    if(nchar(comb[i])>2){#字符串长度大于2，则存在交互作用，例如A:B
      JH=strsplit(comb[i],"")[[1]]#分割字符串，得到A,i,B,j的序列
      Tj=rep(0,n);Th=rep(0,n)
      bb=comb[-i]#去掉第i个字符串去寻找剩余字符串中是否有显著的单独因子A,B
      j=grep(JH[1],bb)#找出其他字符中带交互作用A:B中的因子A
      h=grep(JH[3],bb)#找出其他字符中带交互作用A:B中的因子B
      if(length(j)>0&&length(which(nchar(bb[j])==2))>0){#若其他字符中有单独的因子A，则因子A显著，ai不等于0，算出对应的估计
        Tj=1*(X[,colnames(X)==JH[1]]==JH[2])
        Tj=Tj/sum(Tj)-G[,ncol(G)]
      }
      if(length(h)>0&&length(which(nchar(bb[h])==2))>0){#若其他字符中有单独的因子B，则因子B显著，bj不等于0，算出对应的估计
        Th=1*(X[,colnames(X)==JH[3]]==JH[4])
        Th=Th/sum(Th)-G[,ncol(G)]
      }
      Tjh=(X[,colnames(X)==JH[1]]==JH[2])*(X[,colnames(X)==JH[3]]==JH[4])#找出最优搭配分量
      G[,i]=Tjh/sum(Tjh)-Tj-Th-G[,ncol(G)]#计算交互作用效应分量
    }else{
      J=strsplit(comb[i],"")[[1]]
      G[,i]=1*(X[,colnames(X)==J[1]]==J[2])#找出显著因子水平\bar{T_{jk}}的分量
      G[,i]=G[,i]/sum(G[,i])-G[,ncol(G)]#计算单独因子效应分量
    }
  }
  k=apply(G,1,sum)#最优水平组合下指标均值的估计分量ki
  ne=1/sum(k^2)#计算ne=1/sum(ki^2)
  mu=sum(k*y)#显著水平组合下指标均值的估计sum(ki*yi)
  sigma=sqrt(adj.fm[nrow(adj.fm),3])#提取调整后的误差均方和，开根号得sigma估计
  lower.inter=mu-qt(1-alpha/2,adj.fm[nrow(adj.fm),1])*sigma/sqrt(ne)#计算置信区间下限
  upper.inter=mu+qt(1-alpha/2,adj.fm[nrow(adj.fm),1])*sigma/sqrt(ne)#计算置信区间上限
  estimate$mu=mu
  estimate$lower.inter=lower.inter
  estimate$upper.inter=upper.inter
  estimate$ne=ne
  estimate
}

#例4.3.1
#最优搭配
oa.com(X,y,"AB")
#估计
comb.medicine=c("A2B1","C2")#显著因子及其对应最优水平
oa.estimate(comb.medicine,X,y,adj.medicine,0.05)





#习题
# antibiotics 正交试验数据（不读 csv）
antibiotics <- data.frame(
  run = 1:12,
  A = c(1,1,1,2,2,2,3,3,3,1,2,3),
  B = c(1,2,3,1,2,3,1,2,3,3,1,2),
  C = c(1,2,3,2,3,1,3,1,2,2,3,1),
  D = c(1,2,3,3,1,2,2,3,1,1,2,3),
  AB = c(1,2,3,2,3,1,3,1,2,3,2,1),
  BC = c(1,2,3,3,1,2,2,3,1,2,3,1),
  y = c(78, 82, 85, 80, 88, 84, 90, 86, 89, 83, 87, 91)
)
X <- antibiotics[, c(2:5, 7)]  # A B C D BC
y <- antibiotics[, 8]          # y

result.antibiotics=oa.range(X,y)#代入正交表因子列以及数据结果列
result.antibiotics
oa.rplot(result.antibiotics$Kmean,colnames(X))#画出水平均值图
#方差分析
data.antibiotics=data.frame(apply(X,2,factor),y)#建立数据框
aov.antibiotics=aov(y~A+B+C+A:B+B:C,data=data.antibiotics)
summary(aov.antibiotics)
adj.antibiotics=anova.adj(aov.antibiotics)




#最优搭配
oa.com(X,y,"AB")#望大特性，默认k=1
#估计
comb.antibiotics=c("A2B1","C1","A2")#显著因子及其对应最优水平，若单独因子A也显著，则需要再写上“A2”(与交互作用中的水平要对应)
oa.estimate(comb.antibiotics,X,y,adj.antibiotics,0.05)






















































###课后习题
#习题: 4.3-1,2,3
#（word、pdf格式均可，文件命名为学号+名字+作业4），并于12.9前上传至ftp://10.3.19.43上对应的文件夹内，
# 即实验设计//23统计//作业提交//作业4。
#（ftp://10.3.19.43/%CA%B5%D1%E9%C9%E8%BC%C6/23%CD%B3%BC%C6/%D7%F7%D2%B5%CC%E1%BD%BB/%D7%F7%D2%B54/）


