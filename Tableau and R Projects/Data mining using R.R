data("bodyfat",package = "TH.data")
dim(bodyfat)
attributes(bodyfat)
data("bodyfat",package = "TH.data")
dim(bodyfat)
attributes(bodyfat)
bodyfat.test <- bodyfat[ind==2]
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat.ctree <- ctree (myFormula,data= bodyfat.train)
plot(bodyfat.ctree)
pred <-predict (bodyfat. ctree, newdata = bodyfat.test)
print(pred)
table (pred, bodyfat.test$DEXfat)
xlim <- range(bodyfat$DEXfat)
plot (pred ~ DEXfat, data=bodyfat.test, xlab="Observed", ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)
a <- NbClust(data = bodyfat[,-2], diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
method = "kmeans", index = "all", alphaBeale = 0.1)
fviz_nbclust(bodyfat[,-2], kmeans, method = "silhouette")



