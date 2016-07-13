rawTrain <- read.csv('train.csv')
source('util.R')
pkg()
dtrain <- preprocess(rawTrain)
dtrain <- dum(dtrain)

isTheft <- dtrain$`LARCENY/THEFT`
isOther <- dtrain$`OTHER OFFENSES`
nonCrime <- dtrain$`NON-CRIMINAL`
top3 <- isTheft + isOther + nonCrime
notTop3 <- mapply(function(x){1-x}, top3)
dtrain <- data.frame(top=factor(top3), nonTop=factor(notTop3), dtrain[40:48])

# binary classifier for top3 / not top 3
# biTop <- glm(top~X+Y+HF+DayOfWeek, data=dtrain[sample(500000, replace=T),], family=binomial(link='logit'))
biTop <- multinom(top~., data=dtrain[-2])
pred <- predict(biTop, dtest, type='probs')
biPred <- mapply(function(x){if(x>0.5){1}else{0}}, pred) # if the class is one of the top 3 classes

# binary classifier for top 1 / 2and3
top3Train <- dtrain[which(dtrain$top == 1),-c(1,2)]
theftIn3 <- mapply(function(x){if(x=='LARCENY/THEFT'){1}else{0}}, top3Train$Category)
top23 <- mapply(function(x){1-x}, theftIn3)
top3Train <- data.frame(most=factor(theftIn3), to3=factor(top23), top3Train)
biMost <- glm(most~X+Y+HF+DayOfWeek, data=top3Train[sample(250000, replace=T),], family=binomial(link='logit'))

multinom(most~., top3Train[-2])

length(which(pred==dtrain[rand,'top']))
length(which(pred==1)) + length(which(pred==0))
