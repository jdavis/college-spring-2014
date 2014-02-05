#
# ComS 573 Homework 1
#

# Download data if it doesn't exist
if (!file.exists('./College.csv')) {
    download.file('http://www-bcf.usc.edu/~gareth/ISL/College.csv', destfile = './College.csv')
}

#
# Part A:
# Read in the data from the CSV file.
#

# Read in Data
college = read.csv('./College.csv', header = TRUE)

#
# Part B:
# Look up the data using various functions.
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
# Show a summary of the college data.
#

summary(college)

#
# Part II:
# Show a scatterplot of the first 10 columns of college.
#

pairs(college[,1:10])

#
# Part III:
# Produce side-by-side boxplots of Outstate vs Private.
#

plot(college$Private, college$Outstate,
     main = 'Out of State Tuition vs Private Colleges',
     xlab = 'Private College',
     ylab = 'Out of State Tuition')
#
# Part IV:
# Create a new qualitative variable for Elite colleges. Show various statistics
# for the Elite colleges.
#

Elite <- rep('No', nrow(college))
Elite[college$Top10perc > 50] <- 'Yes'
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)

# Show number of elite vs non-elite colleges
summary(college)

# Show boxplot for Outstate vs Elite
plot(college$Elite, college$Outstate,
     main = 'Out of State Tuition vs Elite Colleges',
     xlab = 'Elite College',
     ylab = 'Out of State Tuition')

#
# Part V:
# Show some histograms with differing numbers of bins for a few quantitative
# variables.
#

par(mfrow=c(2,2))
hist(college$Apps, 20,
     main = 'Histogram of Number of College Applications Recieved',
     xlab = 'Number of Applications Received')
hist(college$Accept, 10,
     main = 'Histogram of Number of Applicants Accepted',
     xlab = 'Number of Applicants Accepted')
hist(college$S.F.Ratio, 10,
     main = 'Histogram of Student to Faculty Ratio',
     xlab = 'Student to Faculty Ratio')
hist(college$PhD, 10,
     main = 'Histogram of Percent of Faculty with a PhD',
     xlab = 'Percent of Faculty with a PhD')

#
# Part VI:
# Continue exploring the data.
#

# TODO