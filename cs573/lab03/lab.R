library('MASS')
library('ISLR')
library('car')

#
# Single Linear Regression
#

# View our Boston data
summary(Boston)
names(Boston)

# Create a linear model for our data
fit <- lm(medv ~ lstat, data = Boston)

# Inspect the fit
summary(fit)
names(fit)

# Extract model data
coef(fit)
confint(fit)

# Predict for a value
predict(fit,
        data.frame(lstat=(c(5, 10, 15))),
        interval="confidence")

# Basic plot & linear fit
plot(Boston$lstat, Boston$medv)
abline(fit)

# Plot Residuals
plot(predict(fit), residuals(fit))

# Show largest leverage statistic
which.max(hatvalues(fit))

#
# Multiple Linear Regression
#

# Using two
fit.multi <- lm(medv ~ lstat + age, data = Boston)
summary(fit.multi)

# Using all
fit.all <- lm(medv ~ ., data = Boston)
summary(fit.all)

# Show variance inflation factors
vif(fit.all)

# Linear model without a predictor
fit.some <- lm(medv ~ . - age, data = Boston)
summary(fit.some)
