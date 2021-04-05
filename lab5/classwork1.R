# ISLR: Introduction to Statistical Learning with R (textbook that we use in this class)
# Validation set example with Auto dataset.
library(ISLR)
library(MASS)
library(boot)
set.seed(1)

# read the documentation for sample() function
help("sample")
train = sample(392,196)
# We use the subset option in the lm() function to fit a linear regression using,
# only the observations corresponding to the training set.
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
# Now we use predict() function to estimate the response for all 392 observations,
# and we use the mean() function to calculate the MSE of the 196 observations in the 
# validation set. Note that the -train selects only the observations that are not in,
# the training set.
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# Therefore the estimated test MSE for the linear regression fit is 26.14

# ISLR: Introduction to Statistical Learning with R (textbook that we use in this class)
# Validation set example with Auto dataset.
library(ISLR)
library(MASS)
library(boot)
set.seed(1)

# read the documentation for sample() function
help("sample")
train = sample(392,196)
# We use the subset option in the lm() function to fit a linear regression using,
# only the observations corresponding to the training set.
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
# Now we use predict() function to estimate the response for all 392 observations,
# and we use the mean() function to calculate the MSE of the 196 observations in the 
# validation set. Note that the -train selects only the observations that are not in,
# the training set.
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# Therefore the estimated test MSE for the linear regression fit is 26.14

