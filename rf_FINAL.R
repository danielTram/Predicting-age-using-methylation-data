library(caret)
library(randomForest)
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

#random forest
random_forest_custom<-list(type="Regression",
                           library="randomForest",
                           loop=NULL)

num_predictors<-ncol(x.train)
rforest_tunegrid<-expand.grid(mrtry=c(num_predictors/3,sqrt(num_predictors)), ntree=c(500,1000,3000,5000))
random_forest_custom$parameters<-data.frame(parameter=c("mrtry","ntree"),
                                            class=rep("numeric",2),
                                            label=c("Predictor Space","number of trees"))
random_forest_custom$grid<-function(x,y,len=NULL,search="grid"){
  rforest_tunegrid
}

random_forest_custom$fit<-function(x,y,...){
  randomForest::randomForest(x=x.train,
                             y=y.train,
                             ...)
}

rf_predict<-function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit,newdata)
random_forest_custom$predict<-rf_predict

rf_prob<-function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata,type="probabilities")
random_forest_custom$prob<-rf_prob


set.seed(5)
rforest_fit<-train(x=x.train,y=y.train,
                   method=random_forest_custom
)
saveRDS(rforest_fit,file="rforest_fit.rds")
