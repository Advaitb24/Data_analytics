setwd("D:\\Projects\\greatlakes\\FRA")
getwd()

library(xlsx)
rawdata=read.xlsx(file.choose(),1)
View(rawdata)
head(rawdata)
str(rawdata)
summary(rawdata)
#validdata=read.xlsx(file.choose(),1)
colnames(rawdata)
rawdata=rawdata[,c(-1,-22)]
rawdata[,1]
class(rawdata[,1])
temp=as.data.frame(rawdata[,1])



#add default column to dataset
for(i in 1:nrow(temp)) {
  if(temp[i,]>0){
    rawdata$default[i] <- 0
  }
  else
  {
    rawdata$default[i] <- 1
  }
  # do stuff with row
}

#rawdata$default <- apply(rawdata, 1, function(x) {ifelse(rawdata[,1] > 0, 0, 1)})
rawdata$default=as.factor(rawdata$default)
library(gmodels)
CrossTable(rawdata$default)


write.xlsx(rawdata,"raw_data_final.xlsx")
?write.xlsx
#####################################################

summary(rawdata)

rawdatafinal=read.xlsx(file.choose(),1)

#install.packages("gmodels")
library(gmodels)

CrossTable(rawdatafinal$Default)

str(rawdatafinal)
summary(rawdatafinal)
colnames(rawdatafinal)
rawdatafianl=rawdatafinal[,c(-1,-22,-25,-32,-52)]

histogram <- hist(rawdatafinal$Networth.Next.Year, breaks = 50, xlab = "Net Worth Next Year", 
                  main = "Histogram",col = "red")

###################################


#logistic Model
model = glm(rawdatafianl$Default ~ ., data = rawdatafianl, family="binomial")

summary(model)

res=predict(model, type = "response")
head(res)

confusion=table(res>0.5,rawdatafianl$Default)
confusion


Accuracy=sum(diag(confusion)/sum(confusion))
Accuracy

################################################

rawdatavalidation$Creditors.turnover=as.numeric(rawdatavalidation$Creditors.turnover)
rawdatavalidation$Debtors.turnover=as.numeric(rawdatavalidation$Debtors.turnover)
rawdatavalidation$Finished.goods.turnover=as.numeric(rawdatavalidation$Finished.goods.turnover)
rawdatavalidation$WIP.turnover=as.numeric(rawdatavalidation$WIP.turnover)
rawdatavalidation$Raw.material.turnover=as.numeric(rawdatavalidation$Raw.material.turnover)
rawdatavalidation$Shares.outstanding=as.numeric(rawdatavalidation$Shares.outstanding)
rawdatavalidation$Equity.face.value=as.numeric(rawdatavalidation$Equity.face.value)

str(rawdatavalidation)

rawdatavalidation=rawdatavalidation[,c(-1,-22,-25,-32,-52)]
#5.2 Impute missing values using missforest package which runs a randomForest on each variable using the observed part and predicts the na values.
#Set (parallelize="variables") to compute several forests on multiple variables at the same time 
?missForest

registerDoParallel(cores = 3)
set.seed(999)

rawdatavalidaion.mis <- missForest(xmis = rawdatavalidation, maxiter = 10, ntree = 30, variablewise = FALSE,
                           decreasing = FALSE, verbose = TRUE, mtry = floor(sqrt(ncol(rawdatavalidation))), replace = TRUE,
                           classwt = NULL, cutoff = NULL, strata = NULL, sampsize = NULL, nodesize = NULL, 
                           maxnodes = NULL, xtrue = NA, parallelize = "variables")

rawdatavalidaion.mis
rawdatavalidaionna <- rawdatavalidaion.mis$ximp
write.xlsx(rawdatavalidaionna,"rawdatavalidaionna.xlsx")


################################################
rawdatavalidationfinal=read.xlsx(file.choose(),1)
str(rawdatavalidationfinal)
summary(rawdatavalidationfinal)
rawdatavalidationfinal$Default=as.factor(rawdatavalidationfinal$Default...1)
CrossTable(rawdatavalidationfinal$Default)

model_valid = glm(rawdatavalidationfinal$Default ~ ., data = rawdatavalidationfinal, family="binomial",maxit = 100)

summary(model_valid)

res=predict(model_valid, type = "response")
head(res)

confusion=table(res>0.5,rawdatavalidationfinal$Default)
confusion


Accuracy=sum(diag(confusion)/sum(confusion))
Accuracy
