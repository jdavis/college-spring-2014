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

#
# Non-Linear Transformations of the Predictors
#

fit.lstat2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(fit.lstat2)

# Compare fits
fit.lstat <- lm(medv ~ lstat, data = Boston)
anova(fit.lstat, fit.lstat2)

par(mfrow = c(2, 4))
plot(fit.lstat)
plot(fit.lstat2)

# Polynomial fits
fit.lstat5 <- lm(medv ~ poly(lstat, 5), data = Boston)
summary(fit.lstat5)

# Log fits
fit.log = lm(medv ~ log(rm), data = Boston)
summary(fit.log)

#
# Qualitative Predictors
#

# View Carseats data
summary(Carseats)
names(Carseats)
contrasts(Carseats$ShelveLoc)

