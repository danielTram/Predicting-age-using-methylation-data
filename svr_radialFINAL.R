library(caret)
library(e1071)
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

svr_radial_param<-data.frame(parameter=c("gamma","cost","epsilon"),
                             class=rep("numeric",3),
                             label=c("Gamma","Cost","Epsilon"))
svr_radial_tunegrid<-expand.grid(gamma=c(1,5,10,20,30,40),cost=c(1,5,10,15,25,40),epsilon=c(1,5,10,15))

#svr radial
svr_custom<-list(type="Regression",
                 library="e1071",
                 loop=NULL)

svr_predict<-function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit,newdata)
svr_custom$predict<-svr_predict

svr_prob<-function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata,type="probabilities")
svr_custom$prob<-svr_prob

svr_radial_func<-function(x,y,...){
  e1071::svm(x=x.train,
             y=y.train,
             scale=FALSE,
             type="eps-regression",
             kernel="radial",
             ...
  )
}
svr_custom$fit<-svr_radial_func
svr_custom$parameters<-svr_radial_param

svr_grid<-function(x,y,len=NULL,search="grid"){
  svr_radial_tunegrid
}

svr_custom$grid<-svr_grid
set.seed(5)
trainCV<-trainControl(method="cv",number=10)
svr_radial_fit<-train(x=x.train,
                      y=y.train,
                      method=svr_custom,
                      trControl=trainCV)

saveRDS(svr_radial_fit,file="svr_radial_fit.rds")