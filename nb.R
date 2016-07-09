# write.table(AA[1:3,], file="PerRange.csv", append=T, row.names=F, col.names=F,  sep=",")

# Naive Bayes
nb <- function(dtrain, dtest){
	cats <- levels(dtrain$Category)
	tmp <- dtrain[sample(600000),]
	dfx <- data.frame(DayOfWeek=factor(tmp$DayOfWeek),X=factor(tmp$X),Y=factor(tmp$Y), Year=factor(tmp$Year),Month=factor(tmp$Month),Day=factor(tmp$Day),HF=factor(tmp$HF))
	prog <- txtProgressBar(min = 1, max = 39, style = 3)
	# 39 binary classifiers
	for (i in 1:39){
		dfy <- data.frame(C=factor(unlist(tmp[i])))
		df <- cbind(dfy, dfx)
		m <- naiveBayes(C~X+Y+DayOfWeek+Year+Month+Day+HF, data=df)
		for (j in 0:87){
			test <- dtest[(j*10000+1):((j+1)*10000),]
			dftest <- data.frame(DayOfWeek=factor(test$DayOfWeek),X=factor(test$X),Y=factor(test$Y), Year=factor(test$Year),Month=factor(test$Month),Day=factor(test$Day),HF=factor(test$HF))
			pred <- predict(m,newdata=dftest,type="raw")[,2]
			write.csv(pred,file=paste0('nb-cat', i, '-', j, '.csv'))
		}
		test <- dtest[880001:884262,]
		dftest <- data.frame(DayOfWeek=factor(test$DayOfWeek),X=factor(test$X),Y=factor(test$Y), Year=factor(test$Year),Month=factor(test$Month),Day=factor(test$Day),HF=factor(test$HF))
		pred <- predict(m,newdata=dftest,type="raw")[,2]
		write.csv(pred,file=paste0('nb-cat', i, '-', 88, '.csv'))
		setTxtProgressBar(prog, i)
	}
}
