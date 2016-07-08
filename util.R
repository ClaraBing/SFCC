pkg <- function(){
  require('lubridate')
  require('caret')
  require('rpart')
  require('randomForest')
  require('party') # which requires strucchange
}


# preprocessing
preprocess <- function(input){
  copy <- input
  # category
  if ("Category" %in% names(input)){
    #copy["Category"] <- match(as.character(input[,2]), levels(input[,2]))
  }
  
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
  
  copy
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

normCoor <- function(x){
  x <- sanitize(x)
  print(nrow(x))
  x$X <- normalize(x$X)
  x$Y <- normalize(x$Y)
  x
}

# replace the geo outliers with the average coordinates
sanitize <- function(x){
  coord <- which(x$X == -120.5)
  if(length(coord)!=0){
    meanX <- mean(x[-coord,]$X)
    meanY <- mean(x[-coord,]$Y)
    x[coord,]$X <- meanX
    x[coord,]$Y <- meanY
  }
  x
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
