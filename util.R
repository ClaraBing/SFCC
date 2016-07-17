pkg <- function(){
  require('lubridate')
  require('caret')
  require('rpart')
  require('randomForest')
  require('party') # which requires strucchange
  require('e1071') # for naiveBayes
  require('glmnet') # for glmnet
  require('nnet')
}

preCat <- function(input, isTrain){
  # time
  # date time
  dt <- strptime(input$Dates, format="%Y-%m-%d %H:%M:%S")
  # year
  input["Year"] <- as.factor(year(dt))
  # month
  input["Month"] <- as.factor(month(dt))
  # day
  input["Day"] <- as.factor(day(dt))
  # hours
  h <- hour(dt)
  # group minutes into half-hours
  f <- unlist(lapply(minute(dt), function(x){ifelse(x<30, 0, 0.5)}))
  hf <- f + h
  input["HF"] <- as.factor(hf)
  
  input
  # proCoor(input, isTrain)
}

# preprocessing
preprocess <- function(input){
  copy <- input
  # category
  # if ("Category" %in% names(input)){
  #copy["Category"] <- match(as.character(input[,2]), levels(input[,2]))
  # }
  
  # time
  # date time
  dt <- strptime(input$Dates, format="%Y-%m-%d %H:%M:%S")
  # year
  copy["Year"] <- year(dt)
  # month
  copy["Month"] <- month(dt)
  # day
  copy["Day"] <- day(dt)
  # hours
  h <- hour(dt)
  # group minutes into half-hours
  f <- unlist(lapply(minute(dt), function(x){ifelse(x<30, 0, 0.5)}))
  hf <- f + h
  copy["HF"] <- hf
  # group minutes into quarter-hours
  q <- unlist(lapply(minute(dt), function(x){floor(x/15)/4}))
  hq <- q + h
  
  # day of week
  copy["DayOfWeek"] <- match(as.character(input$DayOfWeek), levels(input$DayOfWeek))
  
  # district
  copy["PdDistrict"] <- match(as.character(input$PdDistrict), levels(input$PdDistrict))
  
  # delete irrelevant features
  if ("Descript" %in% names(input)){
    copy["Descript"] <- NULL
  }
  if ("Resolution" %in% names(input)){
    copy["Resolution"] <- NULL
  }
  copy["Dates"] <- NULL
  copy["Address"] <- NULL
  
  # copy <- proCoor(copy)
  proCoor(copy)
  # scale(copy)
}

# random sample 200000 data (reasonable size for training/testing)
# the first half of the return value is for training and the second half for testing
sampleData <- function(rawData){
  rawData[sample(1:nrow(rawData), 200000),]
}

# add dummy variables to the sampled (training) data
addDummy <- function(sampledTrain){
  numRow <- nrow(sampledTrain)
  dummy <- data.frame(matrix(1, nrow=numRow, ncol=39))
  for (i in 1:30){
    dummy[i] <- sampledTrain$Category == rep(i, numRow)
  }
  cbind(sampledTrain, dummy)
  # dummy variables are in column 10-48
  # column 1-9 are: Category / DayOfWeek / PdDistrict / X / Y / Year / Month / Day / HF
}

normalize <- function(x){
  if(is.null(dim(x))){
    dim(x) <- c(length(x), 1)
  }
  x <- sweep(x, 2, apply(x, 2, min)) 
  sweep(x, 2, apply(x, 2, max), "/") 
}

proCoor <- function(x, isTrain){
  x <- sanitize(x, isTrain)
  print(nrow(x))
  # x$X <- round(normalize(x$X), digits=2)
  # x$Y <- round(normalize(x$Y), digits=2)
  x$X <- as.factor(round(normalize(x$X), digits = 2))
  x$Y <- as.factor(round(normalize(x$Y), digits = 2))
  x
}

# replace the geo outliers with the average coordinates
sanitize <- function(x, isTrain){
  coord <- which(x$X == -120.5)
  if(length(coord)!=0){
    if (isTrain){ # train: discard outliers
      x[-coord,]
    }
    else{ # test: make outliers the average
      meanX <- mean(x[-coord,]$X)
      meanY <- mean(x[-coord,]$Y)
      x[coord,]$X <- meanX
      x[coord,]$Y <- meanY
      x
    }
  }
}

MMLL <- function(act, pred, eps=1e-15){
  pred[pred < eps] <- eps
  pred[pred > 1 - eps] <- 1 - eps -1/nrow(act)*(sum(act*log(pred)))
}

#categoryMatrix <- data.frame(with(train,model.matrix(~Category+0)))
#names(categoryMatrix)<-sort(unique(train$Category))
#train<-cbind(categoryMatrix,train

dum <- function(x){
  categoryMatrix <- data.frame(with(x,model.matrix(~Category+0)))
  names(categoryMatrix)<-sort(unique(x$Category))
  x<-cbind(categoryMatrix,x)
  x
}

scale <- function(x){
  # ~PdDistrict+X+Y+DayOfWeek+Year+Month+Day+HF
  cols <- c("X","Y","PdDistrict", "DayOfWeek", "Year","Month","Day","HF")
  myWeights <- c(3,3,1,2,1,1,1,3)
  for (i in 1:8){
    x[cols[i]] <- myWeights[i] * normalize(x[cols[i]])
  }
  x
}

getMinIdx <- function(dtrain){
  # 9 minority classes
  mins <- c('TREA','PORNOGRAPHY/OBSCENE MAT','GAMBLING','SEX OFFENSES NON FORCIBLE','EXTORTION','BRIBERY','BAD CHECKS','FAMILY OFFENSES','SUICIDE', 'EMBEZZLEMENT')
  len <- 10
  treaIdx <- which(dtrain$Category == 'TREA')
  matIdx <- which(dtrain$Category == 'PORNOGRAPHY/OBSCENE MAT')
  gbIdx <- which(dtrain$Category == 'GAMBLING')
  sfIdx <- which(dtrain$Category == 'SEX OFFENSES NON FORCIBLE')
  exIdx <- which(dtrain$Category == 'EXTORTION')
  brIdx <- which(dtrain$Category == 'BRIBERY')
  bcIdx <- which(dtrain$Category == 'BAD CHECKS')
  foIdx <- which(dtrain$Category == 'FAMILY OFFENSES')
  scIdx <- which(dtrain$Category == 'SUICIDE')
  emIdx <- which(dtrain$Category == 'EMBEZZLEMENT')
  print(length(c(treaIdx,matIdx,gbIdx,sfIdx,exIdx,brIdx,bcIdx,foIdx,scIdx,emIdx)))
}


