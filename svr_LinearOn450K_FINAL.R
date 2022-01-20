library(caret)
library(e1071)
DataSetByAge450K<-readRDS("meth450k.filter.rds")
IID_SampleID_Age<-readRDS("IID_SampleID_Age.rds")
y<-IID_SampleID_Age$AgeUsed
set.seed(1)
train.rows<-sample(1:nrow(DataSetByAge450K),0.9*nrow(DataSetByAge450K))
x.train<-DataSetByAge450K[train.rows,]
x.test<-DataSetByAge450K[-train.rows,]
y.train<-y[train.rows]
y.test<-y[-train.rows]

svr_linear_param<-data.frame(parameter=c("cost","epsilon"),
                             class=rep("numeric",2),
                             label=c("Cost","Epsilon"))
svr_linear_tunegrid<-expand.grid(cost=seq(from=1,to=10,by=2),epsilon=seq(from=0.1,to=1,by=0.2))
#linear kernel
svr_custom<-list(type="Regression",
                 library="e1071",
                 loop=NULL)
#parameter

svr_custom$parameters<-svr_linear_param

svr_grid<-function(x, y, len=NULL, search="grid"){
  svr_linear_tunegrid
}

svr_custom$grid<-svr_grid

#using linear kernel, has no parameters to tune
svr_linear_func<-function(x,y,...){
  svm(
    x=x.train,
    y=y.train,
    scale=FALSE,
    type="eps-regression",
    kernel="linear",
    ...
  )
}
svr_custom$fit<-svr_linear_func

svr_predict<-function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit,newdata)
svr_custom$predict<-svr_predict

svr_prob<-function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata,type="probabilities")
svr_custom$prob<-svr_prob
set.seed(5)
trainCV<-trainControl(method="cv",number=10)
svr_linear_fit<-train(x=x.train,
                      y=y.train,
                      method=svr_custom,
                      trControl=trainCV)

saveRDS(svr_linear_fit,file="svr_linear_fit_450K.rds")