library(caret)
library(glmnet)
DataSetByAge450K<-readRDS("DataSetByAge450K.rds")
IID_SampleID_Age<-readRDS("IID_SampleID_Age.rds")
y<-IID_SampleID_Age$AgeUsed
DataSetByAge450K<-t(DataSetByAge450K)
DataSetByAge450K<-DataSetByAge450K[,1:5000]
set.seed(1)
train.rows<-sample(1:nrow(DataSetByAge450K),0.9*nrow(DataSetByAge450K))
x.train<-DataSetByAge450K[train.rows,]
x.test<-DataSetByAge450K[-train.rows,]
y.train<-y[train.rows]
y.test<-y[-train.rows]

#elastic net
enet_custom<-list(type="Regression",
                  library="glmnet",
                  loop=NULL)

#must use ... so that it can pass through the train function
enetfunc<-function(x,y,...){
  glmnet::glmnet(
    x=x.train,
    y=y.train,
    standardize=FALSE,
    family="gaussian",
    ...
  )
}
enet_custom$fit<-enetfunc

enet_custom$parameters<-data.frame(parameter=c("alpha","lambda"),
                                   class=rep("numeric",2),
                                   label=c("Alpha", "Lambda"))

elastic_tunegrid<-expand.grid(alpha=(1:10)*0.1,lambda=seq(0.0001,100,length=200))
enet_custom$grid<-function(x, y, len = NULL, search = "grid"){
  elastic_tunegrid
}

enet_predict<-function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit,newdata)
enet_custom$predict<-enet_predict

enet_prob<-function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata,type="probabilities")
enet_custom$prob<-enet_prob

trainCV<-trainControl(method="cv",number=10) #10-fold cv
set.seed(5)
elasticnet_fit<-train(x=x.train,
                      y=y.train,
                      method=enet_custom,
                      trControl=trainCV)
saveRDS(elasticnet_fit,file="elasticnet_fit.rds")
