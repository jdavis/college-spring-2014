## Code for Markov Chains
##
## Note: * is multiplication, but %*% is matrix multiplication.
## To see this run the following
A <- matrix(c(1, 2, 3, 4), 2, 2)
A
B <- diag(2)  # identity matrix
B
# this does not perform matrix multiplication:
A * B
# while this does perform matrix multiplication:
A %*% B

## Problem 1.

# Let state 1=black dog and 2=brown dog
# p11 = .6, p12 = .4
# p21 = .2, p22 = .8
val <- c( .6, .2, .4, .8)
P <- matrix(val, nrow=2, ncol=2)

K <- c(4, 20, 100)   # K=generation

pr <- c(0, 1)    # starting state is brown, or probability 1.0 for state 2.

for (k in 1:max(K)){
    pr <- pr %*% P

    if (k %in% K){
        print(sprintf('The state probabilities (1=black, 2=brown) after %i generations are:', k))
        print(pr)
    }
}

## Problem 4.
# Let state 1=no students, state 2=one student, state 3=two students
#
# You provide:
# 1. the probabilities for 'val'
#    Enter value by column: c(p11, p21, p31, p12, ..., p33)
# 2. the starting state probabilities for 'pr'
#    Enter values: c( P(state 1), P(state 2), P(state 3) )

val <- c( )   # 1. enter transition probabilities

P <- matrix(val, nrow=3, ncol=3)

K <- c(3, 12, 24)   # K=generation

pr <- c( )    # 2. enter starting state probabilities

for (k in 1:max(K)){
    pr <- pr %*% P

    if (k %in% K){
        print(sprintf('The state probabilities (1=no students, 2=one student, 3=two students) after %i generations are:', k))
        print(pr)
    }
}

