#
# ComS 573 Homework 1
#

# Download data if it doesn't exist
if (!file.exists('./College.csv')) {
    download.file('http://www-bcf.usc.edu/~gareth/ISL/College.csv', destfile = './College.csv')
}

#
# Part A:
#

# Read in Data
college = read.csv('./College.csv', header = TRUE)

#
# Part B:
#

# View/edit the data
# fix(college)

# View the data
# View(college)

# Remove first column according to page 55
college <- college[,-1]

#
# Part C:
#

#
# Part I:
# Show a summary

summary(college)

#
# Part II:
#

pairs(college[,1:10])

#
# Part III:
#

plot(college$Private, college$Outstate,
     main='Outstate Colleges vs Private Colleges',
     xlab='Private College',
     ylab='Number of Outstate Colleges')
#
# Part IV:
# Divide Elite colleges
#
Elite <- rep('No', nrow(college))
Elite[college$Top10perc > 50] <- 'Yes'
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)

# Show number of elite vs non-elite colleges
summary(college)

plot(college$Elite, college$Outstate,
     main='Outstate Colleges vs Elite Colleges',
     xlab='Elite College',
     ylab='Number of Outstate Colleges')