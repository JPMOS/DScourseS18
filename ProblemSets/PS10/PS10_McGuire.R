library(mlr)
library(rpart)
library(e1071)
library(kknn)
library(nnet)

library(knitr) # for kable tables

set.seed(100)

income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 
#                 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, 
#                 Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), 
#                   India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, 
#                   Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, 
#                   Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago,
#                   Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################

# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL

# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)

# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]


#####################
#    S E T   U P
#####################

# Define the task:
Classifier_Task <- makeClassifTask(id = "taskname", data = income.train, target = "high.earner")
print(Classifier_Task)

# algorithms 
alg.tree <- makeLearner("classif.rpart", predict.type = "response")
alg.logit <- makeLearner("classif.glmnet", predict.type = "response")
alg.nnet <- makeLearner("classif.nnet", predict.type = "response")
alg.nbayes <- makeLearner("classif.naiveBayes", predict.type = "response")
alg.knn <- makeLearner("classif.kknn", predict.type = "response")
alg.svm <- makeLearner("classif.svm", predict.type = "response")

# CV with three folds. 
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

# Resample ...I am pretty sure this is unecessary.  
#sample.tree <- resample(learner = alg.tree, task = Classifier_Task, resampling = resampleStrat, measures=list(f1, gmean))
#sample.logit <- resample(learner = alg.logit, task = Classifier_Task, resampling = resampleStrat, measures=list(f1, gmean))
#sample.nnet <- resample(learner = alg.nnet, task = Classifier_Task, resampling = resampleStrat, measures=list(f1, gmean))
#sample.nbayes <- resample(learner = alg.nbayes, task = Classifier_Task, resampling = resampleStrat, measures=list(f1, gmean))
#sample.knn <- resample(learner = alg.knn, task = Classifier_Task, resampling = resampleStrat, measures=list(f1, gmean))
#sample.svm <- resample(learner = alg.svm, task = Classifier_Task, resampling = resampleStrat, measures=list(f1, gmean))

# Search over penalty parameter 

params.tree <- makeParamSet(makeIntegerParam("minsplit",lower=10,upper=50), # min sample for split
                            makeIntegerParam("minbucket",lower=5,upper=50), # min sample of leaf
                            makeNumericParam("cp",lower= 0.001 ,upper= 0.2)) # governs complexity

params.logit <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3), 
                             makeNumericParam("alpha",lower=0,upper=1))

params.nnet <- makeParamSet(makeIntegerParam("size",lower=1,upper=10), # number of units in hidden layer
                            makeNumericParam("decay",lower=.1,upper=0.5), # acts like lambda
                            makeIntegerParam("maxit", lower=1000, upper=1000)) # governs iterations, fixed.

# params.nbayes needs no tuning

params.knn <- makeParamSet(makeIntegerParam("k",lower=1,upper=30))

getParamSet("classif.svm")
params.svm <- makeParamSet( 
                           makeDiscreteParam("kernel", values = c(2^-2,2^-1,2^0,2^1,2^2,2^10)),
                           makeDiscreteParam("cost",   values = c(2^-2,2^-1,2^0,2^1,2^2,2^10)) , # ????????
                           makeDiscreteParam("gamma",  values = c(2^-2,2^-1,2^0,2^1,2^2,2^10)) ) # ??????????

# Take 10 random guesses.

tuneMethod <- makeTuneControlRandom(maxit = 10L)


#############################
#  D O     T H E      TUNING 
#############################

tuning_criteria <- list(f1, gmean)

# TREE
tuned.tree <- tuneParams(learner = alg.tree,
                          task = Classifier_Task,
                          resampling = resampleStrat,
                          measures = tuning_criteria,       
                          par.set = params.tree,
                          control = tuneMethod,
                          show.info = TRUE)
# LOGIT
tuned.logit <- tuneParams(learner = alg.logit,
                          task = Classifier_Task,
                          resampling = resampleStrat,
                          measures = tuning_criteria,       
                          par.set = params.logit,
                          control = tuneMethod,
                          show.info = TRUE)
# NNET
tuned.nnet <- tuneParams(learner = alg.nnet,
                          task = Classifier_Task,
                          resampling = resampleStrat,
                          measures = tuning_criteria,       
                          par.set = params.nnet,
                          control = tuneMethod,
                          show.info = TRUE)

# KNN
tuned.knn <- tuneParams(learner = alg.knn,
                         task = Classifier_Task,
                         resampling = resampleStrat,
                         measures = tuning_criteria,      
                         par.set = params.knn,
                         control = tuneMethod,
                         show.info = TRUE)

# SVM
tuned.svm <- tuneParams(learner = alg.svm,
                         task = Classifier_Task,
                         resampling = resampleStrat,
                         measures = tuning_criteria,      
                         par.set = params.svm,
                         control = tuneMethod,
                         show.info = TRUE)


#########################
#   TRAIN & PREDICT 
#########################


# Apply the optimal algorithm parameters to the model
pred.tree <- setHyperPars(learner=alg.tree, par.vals = tuned.tree$x)
pred.logit<- setHyperPars(learner=alg.logit,par.vals = tuned.logit$x)
pred.nnet <- setHyperPars(learner=alg.nnet, par.vals = tuned.nnet$x)
pred.knn <-  setHyperPars(learner=alg.knn,  par.vals = tuned.knn$x)
pred.svm <-  setHyperPars(learner=alg.svm,  par.vals = tuned.svm$x)

pred.nbayes <- setHyperPars(learner=alg.nbayes)

# Verify performance on cross validated sample sets
resample(pred.tree, Classifier_Task, resampleStrat, measures= list(f1, gmean))
resample(pred.logit, Classifier_Task, resampleStrat, measures= list(f1, gmean))
resample(pred.nnet, Classifier_Task, resampleStrat, measures=list(f1, gmean))
resample(pred.knn, Classifier_Task, resampleStrat, measures=list(f1, gmean))
resample(pred.svm, Classifier_Task, resampleStrat, measures= list(f1, gmean))

# Train the final model
final.tree <- train(learner = pred.tree, task = Classifier_Task)
final.logit <- train(learner = pred.logit, task = Classifier_Task)
final.nnet <- train(learner = pred.nnet, task = Classifier_Task)
final.knn <- train(learner = pred.knn, task = Classifier_Task)
final.svm <- train(learner = pred.svm , task = Classifier_Task)

final.nbayes <- train(learner = pred.nbayes, task = Classifier_Task)


# Predict in test set!
test.tree <- predict(final.tree, newdata = income.test)
test.logit <- predict(final.logit, newdata = income.test)
test.nnet <- predict(final.nnet, newdata = income.test)
test.knn <- predict(final.knn, newdata = income.test)
test.svm <- predict(final.svm , newdata = income.test)
test.nbayes <- predict(final.nbayes, newdata = income.test)

  
# Out of sample RMSE
perf.tree  <- performance(test.tree, measures = list(f1, gmean))
perf.logit <- performance(test.logit, measures = list(f1, gmean))
perf.nnet  <- performance(test.nnet, measures = list(f1, gmean))
perf.knn   <- performance(test.knn, measures = list(f1, gmean))
perf.svm   <- performance(test.svm, measures = list(f1, gmean))
perf.nbayes<- performance(test.nbayes, measures = list(f1, gmean))

###########################
# Output Summary in Latex
##########################

### PREPARE SUMMARY DATAFRAMES ###

# Performance
perf <- rbind(perf.tree, perf.logit, perf.nnet, perf.knn, perf.svm, perf.nbayes)
names <- c("tree", "logit", "nnet", "knn", "svm", "nbayes")
row.names(perf) <- names


# 'optimal' parameters, given our brisk cv. 
df_measure <- melt(list(tuned.tree$x, tuned.knn$x, tuned.logit$x, tuned.svm$x, tuned.nnet$x))
df_measure <- mutate(df_measure, L1 = as.factor(L1))
levels(df_measure$L1)
levels(df_measure$L1)[levels(df_measure$L1)=="1"] <- "tree"
levels(df_measure$L1)[levels(df_measure$L1)=="2"] <- "knn"
levels(df_measure$L1)[levels(df_measure$L1)=="3"] <- "logit"
levels(df_measure$L1)[levels(df_measure$L1)=="4"] <- "svm"
levels(df_measure$L1)[levels(df_measure$L1)=="5"] <- "nnet"

### CREATE LATEX SCRIPT ###

kable(perf, format = "latex", booktabs = T) %>%
    kable_styling(latex_options = "striped")
kable(dt, format = "latex", booktabs = T) %>%
  kable_styling(latex_options = "striped")

knitr::kable(df_measure, format = "latex")
