# train a list of binart classification models, one for each category
# input data with dummy variables for categories added
multiTrain <- function(dummied){
  colNames <- unlist(lapply(1:39, function(n){paste('X', as.character(n), sep='')}))
  models <- list(0)
  for(i in 1:39){
    models[[i]] <- glm(unlist(dummied[colNames[i]]) ~ DayOfWeek+PdDistrict+X+Y+Year+Month+Day+HF, data=dummied, family=binomial(logit))
  }
  models
}

# apply the set of models to the testing data
# return a matrix, each row is a list of category probabilities corresponding to a sample
multiPred <- function(dtest, models){
  apply(dtest, MARGIN=1, function(sample){multiEach(sample, models)})
}

# return a list of probabilities, one for each category
multiEach <- function(sample, models){
  unlist(lapply(models, function(model)(predict(model, newdata=sample, type='response'))))
}

regre <- function(dtrain, dtest){
  # sparse matrix
  mtrain <- sparse.model.matrix(~PdDistrict+X+Y+DayOfWeek+Year+Month+Day+HF, data=dtrain)
  mtest <- sparse.model.matrix(~PdDistrict+X+Y+DayOfWeek+Year+Month+Day+HF, data=dtest)
  # train & predict
  m <- glmnet(mtrain, dtrain[,1], family='binomial')
  pred <- as.data.frame(predict(m,mtest,s=1e-15,type="response"))
  numCat <- length(dtrain$Category)
  prog <- txtProgressBar(min = 1, max = numCat, style = 3) # show progress bar
  # calculate the probability for each category
  for (i in 2:numCat){
    m <- glmnet(mtrain, dtrain[,i], family='binomial')
    pred <- cbind(pred, predict(m,mtest,s=1e-15,type="response"))
    setTxtProgressBar(prog, i) # set progress bar
  }
  pred
}

# result: train size = 100000: 2.70627 -> 1575/2335
glmReg <- function(dtrain, dtest){
  cats <- levels(dtrain$Category)
  # train & predict
  var <- "~PdDistrict+X+Y+DayOfWeek+Year+Month+Day+HF"
  m <- glm(as.formula(paste0("`", cats[1], "`", var)), data=dtrain, family = 'binomial')
  pred <- as.data.frame(predict(m,newdata=dtest[1:10000,],s=1e-15,type="response"))
  for(j in 1:88){
    pred <- cbind(pred, predict(m,newdata=dtest[(j*10000+1):(j*10000),],s=1e-15,type="response"))
    write(pred[c(T,F,F)], file=paste0('glm-2-cat1-', as.character(j), '.csv'))
  }
  pred <- cbind(pred, predict(m,newdata=dtest[880001:884262,],s=1e-15,type="response"))
  write(pred[c(T,F,F)], file=paste0('glm-2-cat', as.character(i), '.csv'))
  
  numCat <- length(cats)
  prog <- txtProgressBar(min = 1, max = numCat, style = 3) # show progress bar
  
  # calculate the probability for each category
  for (i in 2:numCat){
    m <- glm(as.formula(paste0("`", cats[i], "`", var)), data=dtrain, family = 'binomial')
    # 10000 test samples in each iteration
    for(j in 1:88){
      pred <- cbind(pred, predict(m,newdata=dtest[(j*10000+1):(j*10000),],s=1e-15,type="response"))
      write(pred[c(T,F,F)], file=paste0('glm-2-cat', as.character(i), '-', as.character(j), '.csv'))
    }
    pred <- cbind(pred, predict(m,newdata=dtest[880001:884262,],s=1e-15,type="response"))
    write(pred[c(T,F,F)], file=paste0('glm-2-cat', as.character(i), '-', as.character(j), '.csv'))
    setTxtProgressBar(prog, i) # set progress bar
  }
  pred
}

colglmReg <- function(dtrain, dtest){
  cats <- levels(dtrain$Category)
  var <- "~PdDistrict+X+Y+DayOfWeek+Year+Month+Day+HF"
  numCat <- length(cats)
  prog <- txtProgressBar(min = 1, max = numCat, style = 3) # show progress bar
  
  for(i in 1:39){
    m <- glm(as.formula(paste0("`", cats[i], "`", var)), data=dtrain, family = 'binomial')
    for(j in 0:87){
      pred <- as.data.frame(predict(m,newdata=dtest[(j*10000+1):((j+1)*10000),],s=1e-15,type="response"))
      write.csv(pred[c(T,F,F)], file=paste0('glm-trial2/cat', as.character(i), '-', as.character(j), '.csv'))
    }
    pred <- as.data.frame(predict(m,newdata=dtest[880001:884262,],s=1e-15,type="response"))
    write.csv(pred[c(T,F,F)], file=paste0('glm-trial2/cat', as.character(i), '-89.csv'))
    setTxtProgressBar(prog, i)
  }
}
