library(glmnet)
library(dummies)

# read dataset
dataset = read.csv("R_data/all_predictors.csv")
dataset$src_population <- dataset$src_population/1000000
dataset$src_gdp <- dataset$src_gdp/10000000000
str(dataset)

# generate dummy variables for 2 factor features: source_country and destination_country
source_dummy <- dummy(dataset$source_country, sep = ":")
destination_dummy <- dummy(dataset$destination_country, sep = ":")
dataset_dummy <- cbind(dataset, source_dummy, destination_dummy)
str(dataset_dummy)

# create design matrix X and response y
X <- model.matrix( ~ ., dataset_dummy[,c(-2,-3,-5)]) #28 variables
y <- dataset$dst_students_count

#fit a lasso regression 
lasso <- glmnet(X, y, family = 'poisson', nlambda = 50, alpha = 1)
lasso
coef(lasso, s=80) #try a lambda
coef(lasso, s=lasso$lambda[2]) #try a lambda

# use cross-validation to choose a lambda value in lasso
lasso_cv <- cv.glmnet(X, y, family = 'poisson', nlambda = 50, alpha = 1)
plot(lasso_cv)
lasso_cv$lambda.min #the optimal lambda value choosed by CV
coef(lasso_cv, s=lasso_cv$lambda.min) #value of lambda that gives minimum cvm.
coef(lasso_cv, s=lasso_cv$lambda.1se) #largest value of lambda such that error is within 1 standard error of the minimum.
coef(lasso_cv, s=134)

