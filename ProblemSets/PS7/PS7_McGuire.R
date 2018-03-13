library(stargazer)
library(mice)
library(MixedDataImpute)

library(RCurl)
library(foreign)

library(dplyr)

 url <- "https://raw.githubusercontent.com/tyleransom/DScourseS18/master/ModelingOptimization/wages.csv"
 
wages <- getURL(url)                
wages.data <- read.csv(textConnection(wages))

head(wages.data)
nrow(wages.data)

amount_missing <- countNA(wages.data$hgc) + countNA(wages.data$tenure)
omitted <- filter(wages.data, !is.na(hgc)) %>% filter(!is.na(tenure))
aggr(wages.data)

nrow(omitted) + amount_missing == nrow(wages.data)

stargazer(omitted)

countNA(omitted$logwage) / length(omitted$logwage) # It's about 25% missing. 

# What type if missing is it? 
# I think it's MAR, which means it's missingness is related to other variables present in data.
# Let's investigate....

wages.data %>% group_by(hgc) %>% summarise(perc_na = sum(is.na(logwage)) / n())
# Missingness appears to increase as years of education go up. 

#######################################################################################
#          IMPUTATIONS !!
######################################################################################

# FIT OMIT
fit_omit <- lm(logwage ~ hgc + college + tenure + tenure ** 2 + age + married, data = na.omit(omitted))

# MEAN IMPUTED
mean_imputed <- omitted
w <- is.na(mean_imputed$logwage)
mean_imputed$logwage[w] <- mean(na.omit(omitted$logwage))
fit_meanImp <- lm(logwage ~ hgc + college + tenure + tenure ** 2 + age + married, data = mean_imputed)
summary(fit_meanImp)

# PREDICTED IMPUTED LM
predicted_impute <- omitted
is_missing <- is.na(predicted_impute$logwage)
predicted_impute$logwage[is_missing] <- predict(fit_omit, newdata = predicted_impute)
fit_predicted <- lm(logwage ~ hgc + college + tenure + tenure ** 2 + age + married, data = predicted_impute)
summary(fit_predicted)

# STAR GAZE
stargazer(fit_omit, fit_meanImp, fit_predicted, omit.stat=c("f"))

# MICE 
mice_obj <- mice(omitted)
fit_mice <- with(mice_obj ,lm(logwage ~ hgc + college + tenure + tenure ** 2 + age + married, data = predicted_impute))
pooled <- pool(fit_mice)
pooled


# Prepare data for Mixed Data Impute
glimpse(omitted)
wage_factors <- dplyr::select(omitted, hgc, college, married) %>%  mutate_if(is.integer, as.factor) 
wage_continu <- dplyr::select(omitted, logwage, tenure, age) %>% mutate_if(is.integer, as.numeric) %>%
                            mutate_if(is.double, as.numeric)


# MIXED DATE IMPUTE
mxd_obj <- hcmm_impute(X = wage_factors, Y = wage_continu, kz = 15, ky = 60, kx = 90,
                       num.burnin = 1000, num.impute = 5 , num.skip = 100, thin.trace = 10)
fit_mxd <- with(mxd_obj ,lm(logwage ~ hgc + college + tenure + tenure ** 2 + age + married, data = predicted_impute))
pooled2 <- pool(as.mira(fit_mxd))

form = logwage ~ hgc + college + tenure + tenure ** 2 + age + married
fits = lapply(mxd_obj$imputations, function(dat) lm(form, data=dat))
pooled_ests = pool(as.mira(fits))
summary(pooled_ests)
