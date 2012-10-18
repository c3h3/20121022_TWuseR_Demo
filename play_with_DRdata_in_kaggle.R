
rm(list=ls(all=TRUE))
library(biOps)

train <- read.csv("~/ShareDIR/Kaggle/kaggle_DigitRecognizer/train.csv")

plot_one_row_data = function(one_row_data){
  one_data = as.matrix(one_row_data)
  dim(one_data) = c(28,28)
  one_data_image = imagedata(one_data)
  plot(one_data_image)
}

par(mfrow=c(2,5))

for (i in 1:10){
  plot_one_row_data(train[i,-1])
}


plot_one_row_data_t = function(one_row_data){
  one_data = as.matrix(one_row_data)
  dim(one_data) = c(28,28)
  one_data_image = imagedata(t(one_data))
  plot(one_data_image)
}


par(mfrow=c(2,5))

for (i in 1:10){
  plot_one_row_data_t(train[i,-1])
}


PCAs = lapply(as.matrix(0:9),function(x){return(prcomp(train[train$label==x,-1]))})

# par(mfrow=c(2,5))
# PCAs_mean = lapply(PCAs,function(PCA_Obj){
#   PCA_Mean = as.matrix(as.integer(PCA_Obj$center))
#   dim(PCA_Mean) = c(28,28)
#   plot(imagedata(PCA_Mean))
# })

par(mfrow=c(2,5))
PCAs_mean = lapply(PCAs,function(PCA_Obj){
  PCA_Mean = as.matrix(as.integer(PCA_Obj$center))
  dim(PCA_Mean) = c(28,28)
  plot(imagedata(t(PCA_Mean)))
})

