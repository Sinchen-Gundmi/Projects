
install.packages("glmnet")
require(glmnet)
install.packages("biglasso")
require(biglasso)

home <- read.csv("C:/R file/train.csv") 
attach(home)

home=na.omit(home)
with(home, sum(is.na(home$medv)))

x=model.matrix(medv~.-1,data= home)
y=medv
str(home)

head(x)
summary(x)
par(mfrow=c(3,5))
for(i in 1:14){
  plot(x[,i], y, xlab= paste("feature",i,sep=" "))
  abline(lm(y~x[,i]))
}


ols_model = lm(y~x)
summary(ols_model)

?glmnet
lasso_model <- glmnet(x,y,alpha = 1)
print(lasso_model)
par(mfrow=c(1,3))
plot.glmnet(lasso_model,xvar = "norm", label = TRUE)
plot.glmnet(lasso_model,xvar = "lambda", label = TRUE)
plot.glmnet(lasso_model,xvar = "dev", label = TRUE)


cv.lasso <- cv.glmnet(x,y,alpha=1,nlambda=100)
par(mfrow=c(1,1))
plot.cv.glmnet(cv.lasso)
lambda_min = cv.lasso$lambda.min
lambda_min
lambda_1se = cv.lasso$lambda.1se
lambda_1se


lasso_model1 = glmnet(x,y,alpha =1, lambda = lambda_min)
lasso_model1$beta


lasso_model2 = glmnet(x,y,alpha =1, lambda = lambda_1se)
lasso_model2$beta

red_model=lm(y~ chas + rm + dis+ ptratio + black + lstat ,data=home)
summary(red_model)

