trees
summary(trees)
hist(trees$Girth)
hist(trees$Height)
hist(trees$Volume)

boxplot(trees)
plot(trees$Girth, trees$Height)
x=(lm(Girth~Height,data=trees))
summary(x)
        
abline(x)
abline(trees$Girth, trees$Height)
plot(trees$Girth, trees$Volume)

plot(trees$Volume, trees$Height)

plot(trees$Girth~trees$Height, xlab = "Height", ylab = "% Body fat")
panel.smooth(trees$Girth,trees$Height)
qqnorm(trees$Volume, xlab="Volume",)
qqnorm(trees$Height)
rubber.summary

d <- density(trees$Girth)
plot(d)
d <- density(trees$Hieght)
plot(d)

library(DAAG)
library(latticeExtra)
library(MASS)
library(ggplot2)
library(ggcorrplot)
Rubber

a<-1
a
corr=cor(Rubber)
ggcorrplot(corr)
oddbooks

m2=lm(weight~log(thick)+log(height)+log(breadth), data=oddbooks)
summary(m2)
corr=cor(oddbooks)
corr
ggcorrplot(corr)


