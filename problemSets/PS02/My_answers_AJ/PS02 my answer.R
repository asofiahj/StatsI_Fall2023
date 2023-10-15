
################### PS02 #################
## Question 1 ##
# a) #

library(tidyverse)  

# Inserting a table with the data 
data_matrix <- matrix(c(14, 6, 7, 7, 7, 1), 
                      nrow = 2, 
                      byrow = TRUE, 
                      dimnames = list(c("Upper_class", "Lower_class"), 
                                      c("Not Stopped", "Bribe requested", "Stopped/given warning")))


# Print the data
data_matrix


#            Not Stopped   Bribe requested     Stopped/given warning    Total
#Upper_class     14              6                     7                  27  
#Lower_class     7               7                     1                  15
#_____________________________________________________________________________
#Total           21             13                     8                  42

#fobserved=fo=observed frequency = the raw count (NOT THE %)
#fexpected=fe=what we would expect for independent samples
#  (Row total/Grand total) * Column total
# fe:

fe1=(27/42)*21
fe2=(27/42)*13
fe3=(27/42)*8
fe4=(15/42)*21
fe5=(15/42)*13
fe6=(15/42)*8

#            Not Stopped   Bribe requested     Stopped/given warning    Total
#Upper_class     13.5            8.357143            5.142857              27  
#Lower_class     7.5             4.642857            2.857143              15
#_____________________________________________________________________________
#Total           21             13                     8                  42


chi_squared= (14-13.5)^2/13.5 + (6-8.357143)^2/8.357143+(7-5.142857)^2/5.142857+
  +(7-7.5)^2/7.5+(7-4.642857)^2/4.642857+(1-2.857143)^2/2.857143

chi_squared=3.791169

# Confirming the chi-squared result with R function
chi_square_result <- chisq.test(data_matrix)

# Printing the chi-squared result
chi_square_result

chi_square_statistic <- 3.7912

## Question 1 ##
# b) #

# significance level (alpha)
alpha <- 0.1

# Calculate the p-value from the chi-squared test statistic
p_value <- 1 - pchisq(chi_square_statistic, df = 2, lower.tail=FALSE)

# P-value: 0.1502282 is greater than alpha = 0.1 
# Fail to reject the null hypothesis. 

## Question 1 ##
# c) #

#Standardized Residual (z) for cell (i, j) = (Observed Frequency (O)
# - Expected Frequency (E)) / √E
res1 <- (14 - 13.5) / sqrt(13.5 * (1 - 27/42) * (1 - 21/42))
res2 <- (6-8.357143)/ sqrt(8.357143 * (1-27/42)*(1-13/42))
res3 <- (7-5.142857)/ sqrt(5.142857 * (1-27/42)*(1-8/42))
res4 <- (7-7.5)/ sqrt(7.5 * (1-15/42)*(1-21/42))
res5 <- (7-4.642857)/ sqrt(4.642857 * (1-15/42)*(1-13/42))
res6 <- (1-2.857143)/ sqrt(2.857143 * (1-15/42)*(1-8/42))

cat("Standardized Residual 1:", res1, "\n")
cat("Standardized Residual 2:", res2, "\n")
cat("Standardized Residual 3:", res3, "\n")
cat("Standardized Residual 4:", res4, "\n")
cat("Standardized Residual 5:", res5, "\n")
cat("Standardized Residual 6:", res6, "\n")

#Standardized Residual 1: 0.3220306 
#Standardized Residual 2: -1.641957 
#Standardized Residual 3: 1.523026 
#Standardized Residual 4: -0.3220306 
#Standardized Residual 5: 1.641957 
#Standardized Residual 6: -1.523026

# Inserting a table with the data 
matrix_stdres <- matrix(c(0.3220306, -1.641957, 1.523026, -0.3220306, 1.641957, -1.523026), 
                      nrow = 2, 
                      byrow = TRUE, 
                      dimnames = list(c("Upper_class", "Lower_class"), 
                                      c("Not Stopped", "Bribe requested", "Stopped/given warning")))
matrix_stdres


library(ggplot2)


residuals_data <- data.frame(
  Residual = c(0.3220306, -1.641957, 1.523026, -0.3220306, 1.641957, -1.523026)
)


# Scatterplot of residuals with regression line 
  ggplot(residuals_data, aes(x = seq_along(Residual), y = Residual)) +
    geom_point() +  
    geom_smooth(method = "lm", se = FALSE, color = "black") +  
    geom_segment(aes(xend = seq_along(Residual), yend = mean(Residual)), linetype = "solid") +  
    labs(x = "Data Point", y = "Standardized Residual") +  
    ylim(min(residuals_data$Residual) - 2, max(residuals_data$Residual) + 2) +  
    theme_minimal() +  
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1)) 
  
  # standardized residuals
  std_residuals <- c(0.3220306, -1.641957, 1.523026, -0.3220306, 1.641957, -1.523026)
  
  # density plot of the standardized residuals
  plot(
    density(std_residuals),
    main = "Density of Standardized Residuals",
    ylab = "Density",
    xlab = "Standardized Residual",
    cex.axis = 0.8,
    cex.lab = 1,
    cex.main = 1,
    lwd = 3
  )
  abline(v = 0, col = "red")
    
###_________________________________________________________________________
# Question 2
###  a )
#To assess the effect of the reservation policy on the number of new or repaired
#drinking water facilities in the villages, I formulated the null and 
#alternative hypotheses as follows:
# H0: The reservation policy of a GP for women leaders has no effect on the number of new or 
#repaired drinking water facilities in the villages.
  
# H1: The reservation policy of a GP for women leaders has an effect on the number of new or 
#repaired drinking water facilities in the villages. 
 
### b )

url <- "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
data <- read.csv(url)
head(data)

library(ggplot2)

# Calculating the proportion of female politicians in reserved GP seats
proportion_reserved <- mean(data$female[data$reserved == 1])

# Calculating the proportion of female politicians in unreserved GP seats
proportion_unreserved <- mean(data$female[data$reserved == 0])

# Print 
cat("Proportion of female politicians in reserved GP seats: ", proportion_reserved, "\n")
cat("Proportion of female politicians in unreserved GP seats: ", proportion_unreserved, "\n")

#Proportion of female politicians in reserved GP seats:  1 
#Proportion of female politicians in unreserved GP seats:  0.07476636

# Creating a scatterplot to visualise the data

ggplot(data, aes(x = factor(reserved), y = water)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "Reservation policy", y = "Drinking-water facilities") + 
  theme_minimal() 

# Running a bivariate regression model to assess the relationship between the 
# two variables: "water" (the number of new or repaired drinking water facilities) 
# and "reservation" (a binary variable indicating whether the GP was reserved for female
# leaders or not).
model <- lm(water ~ reserved, data = data)

# regression results
summary(model)


# In summary, this regression analysis indicates that the gender of the council 
# head (female or male) is statistically significant in explaining the number of
#new or repaired drinking water facilities. When the council head is female, 
# there is on average an estimated increase of 7.864 facilities compared to when
# the council head is male, although the effect size is relatively small.
#The relationship is significant but explains only a small portion of the 
# variance in the outcome variable.

#Residual Standard Error:
#The residual standard error is 33.51, which indicates the typical error in 
#predicting the number of drinking water facilities based on the model. Smaller
#values would indicate a better fit.

# The coefficient estimate for "reserved" is 9.252.
# The "reserved" coefficient (9.252) represents the estimated change in the 
# output variable "water" (the number of new or repaired drinking water facilities)
# when a GP is reserved for women leaders compared to when it is not reserved.
# In this context, it means that reserving the GP is associated with an estimated
# increase of on average 9.252 in the number of new or repaired drinking water 
# facilities compared to when GP is not reserved for women.
# The coefficient is positive, which suggests that the reservation policy is 
# associated with more drinking water facilities. This coefficient is 
# statistically significant, as indicated by the asterisk (*), with a p-value 
# of 0.0197. This means that the relationship between reservation policies and the
# number of drinking water facilities is unlikely to be due to chance.





