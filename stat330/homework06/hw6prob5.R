# This code will simulate the path each rabbit takes, according to these probabilities:
#    (to use this code, you must change the probabilities below)
#     If field, P(home)=p34, P(park)=p32
#     If park, P(field)=p23, P(road)=p21
#     If road, P(park)=p12, P(death)=p10
#
# Denote these stages with 4=home, 3=field, 2=park, 1=road, 0=death
#
# List the probabilities here:
p34 <- .4
p32 <- 1 - p34
p23 <- .8
p21 <- 1 - p23
p12 <- .5
p10 <- 1 - p12

# Each row will track the movement of a single rabbit.
#
# For example, if a row reads "2, 1, 2, 3, 2, 3, 4", then the simulated rabbit
#   went first to the road, back to the park, then to the field, returned to park,
#   went back to field, and then found its home.
#
# All rows will end in a 4 (finding home) or 0 (hit by car in road).
#
# Matrix "tracks" will store all these movements, 1 row per simulated rabbit.
# Initially this matrix has only -9 (denoting missing values).
#
# There will be N=10000 rabbits simulated in this code.

# Initialize the random number generator:
set.seed(2014)

N <- 10000
P <- 100
tracks <- matrix(data=-9, nrow=N, ncol=P)   # creates a matrix with -9 everywhere

# The first column denotes that all rabbits are starting in the park.
tracks[,1] <- 2

for (k in 1:(P-1)){
    # First decide where the park rabbits go next
    in.park <- (tracks[,k] == 2)
    if (sum(in.park)){
        tracks[in.park,k+1] <- sample(c(1,3), sum(in.park), replace=TRUE, prob=c(p21, p23))
    }

    # Then decide where the field rabbits go next
    in.field <- (tracks[,k] == 3)
    if (sum(in.field)){
        tracks[in.field,k+1] <- sample(c(2,4), sum(in.field), replace=TRUE, prob=c(p32, p34))
    }

    # Finally decide the fate of the road rabbits
    in.road <- (tracks[,k] == 1)
    if (sum(in.road)){
        tracks[in.road,k+1] <- sample(c(0,2), sum(in.road), replace=TRUE, prob=c(p10, p12))
    }

    # If there were no rabbits in the park, field, or road, then all rabbits have
    #  found a final place, so exit this for loop
    if (length(c(in.park, in.field, in.road)) == 0){
        break
    }
}

{
print(sprintf('Results:  %i rabbits found home,  %i rabbits died.  (Longest path was of length %i)',
  H <- sum(colSums(tracks == 4)), sum(colSums(tracks == 0)), P - sum(colSums(tracks == -9) == N)))
print(sprintf('Proportion of rabbits finding home:  %0.4f', H/N))
}

