clr1 <- c(rgb(1,0,0,1), rgb(0,0,1,1))
clr2 <- c(rgb(1,0,0,.2), rgb(0,0,1,.2))
x <- c(.4,.55,.65,.9,.1,.35,.5,.15,.2,.85)
y <- c(.85,.95,.8,.87,.5,.55,.5,.2,.1,.3)
z <- c(1,1,1,1,1,0,0,1,0,0)

df <- data.frame(x,y,z)

plot(x, y, pch=19,cex = 2, col = clr1[z+1])

reg <- glm(z~x+y, data = df, family = binomial)
summary(reg)

pred_1 <- function(x,y){
   predict(reg,newdata=data.frame(x=x,
    y=y),type="response")>.5
}

x_grid <- seq(0, 1, length = 101)
y_grid <- seq(0, 1, length = 101)
z_grid <- outer(x_grid, y_grid, pred_1)
image(x_grid,y_grid,z_grid,col=clr2)
points(x,y,pch=19,cex=2,col=clr1[z+1])


clr1=c(rgb(1,0,0,1),rgb(1,1,0,1),rgb(0,0,1,1))
clr2=c(rgb(1,0,0,.2),rgb(1,1,0,.2),rgb(0,0,1,.2))
x=c(.4,.55,.65,.9,.1,.35,.5,.15,.2,.85)
y=c(.85,.95,.8,.87,.5,.55,.5,.2,.1,.3)
z=c(1,2,2,2,1,0,0,1,0,0)
df=data.frame(x,y,z)
plot(x,y,pch=19,cex=2,col=clr1[z+1])

library(nnet)

reg=multinom(z~x+y,data=df)
summary(reg)

pred_2 <- function(x,y){
  which.max(predict(reg,
    newdata=data.frame(x=x,y=y),type="probs"))
}

x_grid<-seq(0,1,length=101)
y_grid<-seq(0,1,length=101)
Outer <- function(x,y,fun) {
   mat <- matrix(NA, length(x), length(y))
   for (i in seq_along(x)) {
    for (j in seq_along(y)) mat[i,j]=fun(x[i],y[j])
   }
   return(mat)
}
z_grid <- Outer(x_grid,y_grid,pred_2)

image(x_grid,y_grid,z_grid,col=clr2)
points(x,y,pch=19,cex=2,col=clr1[z+1])