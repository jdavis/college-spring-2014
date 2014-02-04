#
# Read in Data
#
college = read.csv('./College.csv', header = TRUE)

#
# View the data
#

# View/edit the data
# fix(college)

# View the data
# View(college)

# Remove first column according to page 55
college <- college[,-1]

#
# Part I:
# Show a summary
summary(college)

#
# Part II:
# Basic scatterplot
pairs(college[,1:10])

#
# Part III:
# TODO: plot() Outstate vs Private
# plot()

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

# TODO: plot() Outstate vs Elite
# plot()
