# I'm not quite sure what this is for...
set.seed(1)

# Create uniform distribution for first input
x1 <- runif(100)

# Normal distribution for second input
x2 <- 0.5 * x1 + rnorm(100) / 10

Y <- 2 + (2 * x1) + (.3 * x2) + rnorm(100)
