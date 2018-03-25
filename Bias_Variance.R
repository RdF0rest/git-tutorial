#function
  #N:测试集数量；K：循环次数；n：训练集数量
N <- 2000; K <- 2000; n <- 500 

main <- function(p, sigma){
  X <- rt(N, 6)
  Y <- 3*log(X^2)-X+rnorm(N, 0, sigma)
  S <- Taylor(matrix(X, N, p), p) #调用Taylor函数生成p阶泰勒展开
  
  data_test <- data.frame(y = Y, s = S) #生成测试集
  py <- matrix(0,N,K)
  
  for(i in 1:p)
    assign(paste0('py',i), py)    #批量定义py
    
  #对训练集进行训练
  for(k in 1:K){
    x <- rt(n, 6)
    s <- Taylor(matrix(x, n, p), p)
    y <- 3*log(x^2)-x+rnorm(n, 0, sigma)
    
    data_train <- data.frame(y = y, s = s)
    for(i in 1:p){ #批量回归并储存数据
      assign(paste0('lm',i), lm(as.formula(paste0('y~',paste0('s.',i,collapse = '+'))), data_train))
      nr <- get(paste0('py',i))
      nr[,k] <- as.numeric(predict(get(paste0('lm',i)), data_test))
      assign(paste0('py',i), nr)
    }
  }
  B <- c(); V <- c()
  
  #计算Bias和Variance
  for(i in 1:p){
    assign(paste0("bpy",i), mean((apply(get(paste0('py',i)), 1, mean) - 3*log(X^2)+X)^2))
    assign(paste0('vpy',i), mean(apply(get(paste0('py',i)), 1, var)))
    B <- append(B, get(paste0('bpy',i)))
    V <- append(V, get(paste0('vpy',i)))
  }
  date()
  para <- data.frame('B' = B, 'V' = V) #输出data.frame
  return(para)
}

Taylor <- function(S, p){		#生成Taylor展开的X矩阵
  for (i in 2:p)
    S[,i] <- 10*S[,i]*S[,i-1]
  return (S)
}