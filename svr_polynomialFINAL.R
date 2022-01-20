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

svr_polynomial_param<-data.frame(parameter=c("degree","gamma","coef0","cost","epsilon"),
                                 class=rep("numeric",5),
                                 label=c("Degree","Gamma","Coef0","Cost","Epsilon"))
svr_polynomial_tunegrid<-expand.grid(degree=c(1,5,10,15,30,50),gamma=c(1,5,20,30,40),coef0=c(1,5,10,15,30),cost=c(1,5,10,15,25,40),epsilon=c(1,5,10,15))
#svr polynomial 
svr_custom<-list(type="Regression",
                 library="e1071",
                 loop=NULL)

svr_predict<-function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit,newdata)
svr_custom$predict<-svr_predict

svr_prob<-function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata,type="probabilities")
svr_custom$prob<-svr_prob

svr_polynomial_func<-function(x,y,...){
  e1071::svm(x=x.train,
             y=y.train,
             scale=FALSE,
             type="eps-regression",
             kernel="polynomial",
             ...
  )
}
svr_custom$fit<-svr_polynomial_func
svr_custom$parameters<-svr_polynomial_param

svr_grid<-function(x,y,len=NULL,search="grid"){
  svr_polynomial_tunegrid
}

svr_custom$grid<-svr_grid
set.seed(5)
trainCV<-trainControl(method="cv",number=10)
svr_polynomial_fit<-train(x=x.train,
                          y=y.train,
                          method=svr_custom,
                          trControl=trainCV)

saveRDS(svr_polynomial_fit,file="svr_polynomial_fit.rds")