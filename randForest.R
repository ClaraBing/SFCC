randFor <- function(dtrain, dtest){
  cats <- levels(dtrain$Category)
  # train & predict
  var <- "~PdDistrict+X+Y+DayOfWeek+Year+Month+Day+HF"
  m <- randomForest(as.formula(paste0("`", cats[1], "`", var)), data=dtrain[sample(5000),], ntree=1000,importance = T)
  pred <- as.data.frame(predict(m,newdata=dtest,s=1e-15,type="response"))
  numCat <- length(cats)
  prog <- txtProgressBar(min = 1, max = numCat, style = 3) # show progress bar
  gc()
  # calculate the probability for each category
  for (i in 2:numCat){
    #    m <- glm(unlist(dtrain[cats[i]])~unlist(dtrain["PdDistrict"])+unlist(dtrain["X"])+unlist(dtrain["Y"])+unlist(dtrain["DayOfWeek"])+unlist(dtrain["Year"])+unlist(dtrain["Month"])+unlist(dtrain["Day"])+unlist(dtrain["HF"]), family='binomial')
    m <- randomForest(as.formula(paste0("`", cats[i], "`", var)), data=dtrain[sample(5000),], ntree=1000,importance = T)
    pred <- cbind(pred, predict(m, newdata=dtest,s=1e-15,type="response"))
    setTxtProgressBar(prog, i) # set progress bar
    gc()
  }
  pred
}
