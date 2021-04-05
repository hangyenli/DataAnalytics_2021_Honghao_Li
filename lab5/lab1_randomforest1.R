require(randomForest)
library(rpart)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF) 	# view results
importance(fitKF) # importance of each predictor

fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data = swiss)
print(fitSwiss) # view results
importance(fitSwiss) # importance of each predictor
varImpPlot(fitSwiss)

plot(fitSwiss)

getTree(fitSwiss,1, labelVar=TRUE)

#help(randomForest) # look at all the package contents and the randomForest method options

# look at rfcv - random forest cross-validation - 
#help(rfcv)

# other data....
#data(imports85)

# perform randomForest and other tree methods.....

data("Titanic")
fitT <- randomForest(Survived ~ Class + Sex + Age, data=Titanic)
print(fitT) 	# view results
importance(fitT) # importance of each predictor
varImpPlot(fitT)



require(party)
rpartT <- rpart(Survived ~ Class + Sex + Age, data=Titanic)
summary (rpartT)

ctreeT <- ctree(Survived ~ Class + Sex + Age, data=Titanic)
summary(ctreeT)

