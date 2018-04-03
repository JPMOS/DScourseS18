library(mlr)
library(glmnet)
library(MASS)
library(ggvis)

data(Boston)
dim(Boston)

Boston$lmedv <- log(Boston$medv)
Boston$medv <- NULL # drop median value

glimpse(Boston)


formula <- as.formula(lmedv ~ .^3 +
                          poly(crim , 6) +
                          poly(zn , 6) +
                          poly(indus , 6) +
                          poly(nox , 6) +
                          poly(rm , 6) +
                          poly(age , 6) +
                          poly(dis , 6) +
                          poly(rad , 6) +
                          poly(tax , 6) +
                          poly(ptratio , 6) +
                          poly(black, 6) +
                          poly(lstat , 6))

mod_matrix <- data.frame( model.matrix(formula , Boston ))
#now replace the intercept column by the response since MLR will do
#"y ~ ." and get the intercept by default
mod_matrix[, 1] = housing$lmedv
colnames(mod_matrix )[1] = "lmedv" #make sure to rename it otherwise MLR won ’t find it
head(mod_matrix ) #just make sure everything is hunky -dory

# Break up the data:
n <- nrow(mod_matrix )
train <- sample(n, size = .8*n)
test <- setdiff(1:n, train)
Boston.train <- mod_matrix[train ,]
Boston.test <- mod_matrix[test ,]


#####################
#    S E T   U P
#####################

# Define the task:
theTask <- makeRegrTask(id = "taskname", data = Boston.train, target = "lmedv")
print(theTask)

# tell mlr what prediction algorithm we'll be using (OLS)
predAlg <- makeLearner("regr.lm")

# Set resampling strategy (here let's do 6-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

# Mean RMSE across the 6 folds
# How do we have RMSE if we didn't specify our response variable... oh, the model matrix. 
# I am assuming this is the RMSE from plain OLS which requires no specification of parameters. 

print(sampleResults$aggr)

# Tell it a new prediction algorithm
predAlg <- makeLearner("regr.glmnet")

# Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),
                            makeNumericParam("alpha",lower=1,upper=1))

modelParams2 <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),
                            makeNumericParam("alpha",lower=0,upper=0)) # RIDGE

modelParams3 <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),
                            makeNumericParam("alpha",lower=0,upper=1)) # ENET

# Take 50 random guesses at lambda within the interval I specified above.

tuneMethod <- makeTuneControlRandom(maxit = 50L)


#############################
#  D O     T H E      TUNING 
#############################

# LASSO
tunedModel1 <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)
# RIDGE
tunedModel2 <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams2,
                         control = tuneMethod,
                         show.info = TRUE)
# ENET
tunedModel3 <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams3,
                         control = tuneMethod,
                         show.info = TRUE)



# Search over penalty parameter lambda and force elastic net parameter to be 0 (ridge)
# Why do we need to do this again? 

modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))
modelParams2 <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))
modelParams3 <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))


# Do the tuning again ... why? 

# LASSO
tunedModel1 <- tuneParams(learner = predAlg,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = rmse,       # RMSE performance measure, this can be changed to one or many
                          par.set = modelParams,
                          control = tuneMethod,
                          show.info = TRUE)
# RIDGE
tunedModel2 <- tuneParams(learner = predAlg,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = rmse,       # RMSE performance measure, this can be changed to one or many
                          par.set = modelParams2,
                          control = tuneMethod,
                          show.info = TRUE)
# ENET
tunedModel3 <- tuneParams(learner = predAlg,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = rmse,       # RMSE performance measure, this can be changed to one or many
                          par.set = modelParams3,
                          control = tuneMethod,
                          show.info = TRUE)



#########################
#   TRAIN & PREDICT 
#########################


# Apply the optimal algorithm parameters to the model
pred.LASSO <- setHyperPars(learner=predAlg, par.vals = tunedModel1$x)
pred.RIDGE <- setHyperPars(learner=predAlg, par.vals = tunedModel2$x)
pred.ENET <- setHyperPars(learner=predAlg, par.vals = tunedModel3$x)

# Verify performance on cross validated sample sets
resample(pred.LASSO, theTask, resampleStrat, measures=list(rmse))
resample(pred.RIDGE, theTask, resampleStrat, measures=list(rmse))
resample(pred.ENET, theTask, resampleStrat, measures=list(rmse))

# Train the final model
finalModel1 <- train(learner = pred.LASSO, task = theTask)
finalModel2 <- train(learner = pred.RIDGE, task = theTask)
finalModel3 <- train(learner = pred.ENET, task = theTask)

finalModel$learner.model

# Predict in test set!
prediction1 <- predict(finalModel1, newdata = Boston.test)
prediction2 <- predict(finalModel2, newdata = Boston.test)
prediction3 <- predict(finalModel3, newdata = Boston.test)



# Out of sample RMSE
performance(prediction1, measures = list(rmse))
performance(prediction2, measures = list(rmse))
performance(prediction3, measures = list(rmse))



