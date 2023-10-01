#####################
# load libraries
# set wd
# clear global .envir
#####################
getwd()

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#First, it is important to perform descriptive analysis to visualise the data before 
# manipulating it
View(y)
head(y)
str(y)

mean(y) # The mean of the IQ score is the sum of all scores divided by the number of students
var(y) # The variance informs about the degree of dispersion of the data points 
# around the mean.
sd(y) # The standard deviation measures how much the IQ scores deviate from the mean
sd(y)/sqrt(length(y)) # The standard error tells us how likely is one sample to change
# from one random sample to the other, it is the measure of uncertainty
length(y) # Number of observations
sqrt(length(y)) # square root
sqrt(y)
summary(y)
hist(y)
?hist
# I will add a title and some color details to the histogram and to the plot
hist_y <- hist(y, 
                  main="Histogram of student's IQ", 
                  xlab="Student's IQ", 
                  ylab="Number of observations", 
                  col="darkgreen",        
                  border="black")
# The histogram seems to point to a normal distribution of the collected data

plot(y, 
     main="Plot of Student's IQ",   
     xlab="Observations",                    
     ylab="IQ",                    
     col="darkblue",                             
     pch=16)             
# This plot of the Student's IQ alerts to the existence of outliers, namely, the IQ scores
# of 126 and 69 are at the extreme ends of the data. These values might be considered 
# outliers, they may represent an error.

# 1. Confidence intervals are used to quantify the uncertainty of estimating parameters
# in the population based on samples of data.
# To calculate it here I will need the Sample Mean, the critical value and the standard 
# error (SE)

?qt
qt(0.010, df=length(y)-1) # critical value for first 10%
qt(0.90, df=length(y)-1) # last 10%
qt(0.010, df=length(y)-1, lower.tail=FALSE) # last 10%

t_score <- qt(0.95, df=length(y)-1)
lower_90_t <- mean(y)-(t_score)*(sd(y)/sqrt(length(y)))
upper_90_t <- mean(y)+(t_score)*(sd(y)/sqrt(length(y)))

# These bounds contain the lower and upper limits of the 90% confidence interval 
# for the average IQ in my sample.

# Print my results
lower_90_t
upper_90_t

# The 90% confidence interval for the population mean IQ score is (93.95993, 102.9201).
# Therefore, I can conclude with 90% confidence, that the true population mean IQ score
# is likely to fall within the range of 93.95993 to 102.9201.  

# Update Histogram 
hist(y, main="", xlab="IQ Scores", ylab="Observations")
abline(v=mean(y),col="purple")
abline(v=lower_90_t,col="purple",lty="dashed")
abline(v=upper_90_t,col="purple",lty="dashed")
title(main="Student's IQ distribution with 90% confidence interval")

#####################
# 2. To answer question 2, first I will formulate the hypotheses.
# Null Hypothesis (H0): The average student IQ in the school is lesser than or equal to
# the national average IQ score (μ = 100).
# Alternative Hypothesis (Ha): The average student IQ in the school is higher than the 
# national average IQ score (μ > 100).

?t.test
t_stat <- (mean(y)-100)/(sd(y)/sqrt(length(y)))
# The t-statistic measures how the standard errors between the sample mean and the 
# hypothesized population mean (100) under the assumption that the null hypothesis is 
# true. This negative result indicates that the sample mean is lower than the
# hypothesized population mean.

#Next, I calculate the p-value
P_value <- pt(abs(t_stat), df = length(y)-1, lower.tail = FALSE)

# Since p-value (0.7215) > alpha (0.05), I do not possess sufficient evidence to
# reject the null hypothesis. 

t.test(y, mu = 100, alternative = 'less') # another way of obtaining the result


####################
# Problem 2
#####################

rm(list=ls())

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

View(expenditure)
str(expenditure)
head(expenditure)
summary(expenditure)

##################### Part 1

# Create a scatter plot matrix
pairs(expenditure[, c("Y", "X1", "X2", "X3")])

# Calculate correlations
correlations <- cor(expenditure[, c("Y", "X1", "X2", "X3")])

# Create a plot of the correlations
png(file = "C:/Users/Sofia Jesus/Desktop/PhD Political Science/Learning/Quantitative Methods I/StatsI_Fall2023/problemSets/PS01/correlations.png", width = 800, height = 600)
heatmap(correlations, annot = TRUE)  # Creating a heatmap of my correlations

# By analysing the scatter plots side by side with the correlation values, it is possible
# to conclude that correlations between Y, X1, X2, and X3 are all positive and weak to moderate.
# The strongest correlation is a moderate positive correlation between X1 and X3, followed
# by the positive moderate correlation between Y and X1. Following those, there are 
# other two moderate positive correlations between Y vs. X2 and Y vs. X3.
# The remaining correlations are all weak or random, being situated below 0.40.
# Some plots have many outliers, such as Y vs. X3, which can be distorting the correlation.

##################### Part 2


# I am creating a scatter plot to visualise the relationship between per capita spending
# on housing assistance (Y) and regions (Region). Each point on the plot represents a 
# data point in the 'expenditure' dataset.

ggplot(data = expenditure, aes(y = Y, x = as.factor(Region))) +
  geom_point() +
  ggtitle("Per Capita Spending on Housing Assistance Grouped by Region") +
  xlab("Region")+
  ylab("Expenditure")+

# As the scatter plot does not help visualise which region has the highest per capita 
# expenditure on housing assistance I am also creating a boxplot.


boxplot(Y ~ Region, data = expenditure, main = "Spending on Housing Assistance Grouped by Region", ylab = "Spending")
means <- tapply(expenditure$Y, expenditure$Region, mean)
points(means, pch = 20) # add means as circles to each boxplot


# Even though the boxplot provides a visual aid to answering the question, I will
# compare the mean of the 4 regions. For this, I will create objects containing the 
# part of the dataset from each region

# I am creating a list to store mean expenditures for each region
region_means <- list()

# I am using this to calculate the mean expenditure for each region
for (region_id in unique(expenditure$Region)) {
  region_data <- expenditure[expenditure$Region == region_id, ]
  mean_expenditure <- mean(region_data$Y, na.rm = TRUE)
  region_means[[as.character(region_id)]] <- mean_expenditure
}

# I am now printing the mean expenditures for each region
region_means

# From this we get the Mean Y for each region :
#Region 1 79.44444
#Region 2 83.91667
#Region 3 69.1875
#Region 4 88.30769
# Thus it is possible to conclude that Region 4 (West) has the highest per capita spending 
# on social housing. With Region 2 (North Central) coming in second place with Region 1 (North East) 
# in third and Region 3 (South) having the lowest per capita spending on social housing.


##################### Part 3

library(ggplot2)
# Creating a plot of Expenditure per personal income in state:
ggplot(data = expenditure, aes(y = Y, x = X1)) +
  geom_point() +
  ggtitle("Relationship between Expenditure on Social Housing and Income") +
  xlab("per capita personal income in state")+
  ylab("Expenditure")


# Creating a ggplot of the three variables combined:
ggplot(data = expenditure, aes(x = X1, y = Y, color = as.factor(Region), shape = as.factor(Region))) +
  geom_point(size = 3) +
  ggtitle("Expenditure vs. Income by Region") +
  xlab("Per Capita Personal Income in State") +
  ylab("Expenditure") +
  
# Customizing color and shape:
  scale_color_manual(values = c("1" = "orange", "2" = "blue", "3" = "darkgreen", "4" = "purple")) +
  scale_shape_manual(values = c("1" = 8, "2" = 17, "3" = 18, "4" = 3))


# Description of graph: There seems to be a moderate positive correlation between the per
# capita income in the state and the per capita spending on housing assistance. It is 
# clear from the graph that Region 4 is the highest spending region in housing assistance
# within its range of income. Adversely, it is also possible to note that region 1 
# generally spends less on housing assistance than otherregions with similar income. 
# In this graph it becomes clear that a possible explanation for region 3 poor expenditure
# on housing assistance can be a low income per capita.