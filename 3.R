anova.tab <- function(fm){
  tab <- summary(fm)[[1]]
  k <- length(tab) - 2
  temp <- c(sum(tab[,1]), sum(tab[,2]), rep(NA, k))
  tab["Total",] <- temp
  tab
}




#BIBD
method <- data.frame(
  treat = c("A", "B", "C", "D", "E"),
  B1 = c(10, 12, NA, 11, NA),
  B2 = c(9, NA, 8, NA, 7),
  B3 = c(NA, 11, 10, 9, NA),
  B4 = c(8, NA, NA, 10, 9),
  B5 = c(NA, 9, 11, NA, 10)
)

method

#处理效应、区组效应估计
estimate <- function(data){
  M <- as.matrix(data[,-1])
  N <- M
  M[is.na(M)] <- 0
  Ti <- apply(M, 1, sum)
  Bj <- apply(M, 2, sum)
  T <- sum(Ti)
  
  N[!is.na(N)] <- 1
  N[is.na(N)] <- 0
  
  v <- nrow(N)
  b <- ncol(N)
  r <- sum(N[1,])
  k <- sum(N[,1])
  lambda <- sum(N[1,] * N[2,])
  n <- v * r
  mu <- T / n
  
  ai <- k / (lambda * v) * (Ti - N %*% Bj / k)
  bj <- Bj / k - mu - t(N) %*% ai / k
  
  cat("处理效应为：", ai, "\n区组效应为：", bj, "\n")
}

estimate(method)

#方差分析
library(reshape2)

pre.method <- melt(
  method,
  id = "treat",
  variable.name = "block",
  value.name = "result"
)

data.method <- na.omit(pre.method)

aov.method <- aov(result ~ block + treat, data = data.method)
anova.tab(aov.method)

#多重比较
install.packages("agricolae")
library(agricolae)

bib.method <- BIB.test(
  block = data.method$block,
  trt   = data.method$treat,
  y     = data.method$result,
  test  = "tukey",
  alpha = 0.05,
  group = TRUE,
  console = TRUE
)




#习题3.3-5

steel <- data.frame(
  treat = c("A", "B", "C", "D"),
  B1 = c(18, 22, 20, NA),
  B2 = c(19, 23, NA, 21),
  B3 = c(NA, 20, 23, 22),
  B4 = c(18, NA, 20, 21)
)

estimate(steel)

#方差分析
pre.steel <- melt(
  steel,
  id = "treat",
  variable.name = "block",
  value.name = "result"
)

data.steel <- na.omit(pre.steel)

aov.steel <- aov(result ~ block + treat, data = data.steel)
anova.tab(aov.steel)

#多重比较
bib.steel <- BIB.test(
  block = data.steel$block,
  trt   = data.steel$treat,
  y     = data.steel$result,
  test  = "tukey",
  alpha = 0.05,
  group = TRUE,
  console = TRUE
)













###课后习题
#习题3.3(旧版3.2)-2、3、4，（word、pdf格式均可，文件命名为学号+名字+作业3），并于11.11前上传至ftp://10.3.19.43上对应的文件夹内，
# 即实验设计//23统计//作业提交//作业3。
#（ftp://10.3.19.43/%CA%B5%D1%E9%C9%E8%BC%C6/23%CD%B3%BC%C6/%D7%F7%D2%B5%CC%E1%BD%BB/%D7%F7%D2%B53/）

