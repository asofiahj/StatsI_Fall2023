#####################
# load libraries
# set wd
# clear global .envir
#####################

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

lapply(c("stargazer","vioplot","arm","broom","ggplot2","fastDummies"),  pkgTest)

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd("C:/Users/Sofia Jesus/Desktop/PhD Political Science/Learning/Quantitative Methods I/StatsI_Fall2023")
getwd()

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

##########Question 1##############
#1.
#Outcome variable: incumbent's vote share
#Predictor: campaign spending
model_base <- lm(voteshare~difflog, data=inc.sub)
summary(model_base)


#2.
plot(inc.sub$difflog, inc.sub$voteshare, main="Relationship Between Campaign Spending and Vote Share", 
     xlab="Campaign Spending", ylab="Vote Share")
abline(lm(voteshare ~ difflog, data=inc.sub), col="blue")

#3.
residuals <- resid(model_base)
head(residuals)

#4.
intercept <- coef(model_base)[1]  
slope <- coef(model_base)[2]      

prediction_equation_base <- paste("voteshare = ", round(intercept, 4), " + ", round(slope, 4), " * difflog")
cat("Prediction Equation:\n", prediction_equation_base, "\n")

########## Question 2 ##########

#1. outcome variable: presvote and the explanatory variable: difflog
model_pres<-lm(presvote~difflog, data=inc.sub)
summary(model_pres)

#2. 
plot(inc.sub$difflog, inc.sub$presvote, main="Relationship Between Campaign Spending and Presidential Vote Share", 
     xlab="Difference in Campaign Spending", ylab="Presidential Vote Share")
abline(lm(presvote ~ difflog, data=inc.sub), col="green")

#3.
residuals_pres <- resid(model_pres)
head(residuals_pres)

#4.
intercept_pres <- coef(model_pres)[1]  
slope_pres <- coef(model_pres)[2]      
prediction_equation_pres <- paste("presvote = ", round(intercept_pres, 4), " + ", round(slope_pres, 4), " * difflog")
cat("Prediction Equation:\n", prediction_equation_pres, "\n")


######### Question 3 ###########

# outcome variable: voteshare; explanatory variable: presvote.
#1. 
model_inc<-lm(voteshare~presvote, data=inc.sub)
summary(model_inc)

#2.
plot(inc.sub$presvote, inc.sub$voteshare, main="Relationship Between Presidential Vote Share and incumbent's electoral success", 
     xlab="Presidential vote share", ylab="Incumbent's success")
abline(lm(voteshare ~ presvote, data=inc.sub), col="Purple")

#3. 
intercept_inc <- coef(model_inc)[1]
slope_inc <- coef(model_inc)[2]
prediction_equation_inc <- paste("voteshare = ", intercept_inc, " + ", slope_inc, " * presvote")
cat("Prediction Equation:\n", prediction_equation_inc, "\n")



######### Question 4 ###########
#Outcome variable:residuals from Question 1. Explanatory variable: residuals from Question 2.
#1.
model_residuals<-lm(residuals~residuals_pres, data=inc.sub)
summary(model_residuals)

#2.
plot(residuals_pres, residuals, main="Relationship between vote share and presidential vote share residuals",
    xlab="Presidential vote share Residuals", ylab="Vote Share Residuals",
    col = "darkgreen")
abline(lm(residuals ~ residuals_pres), col="black")

#3.
intercept_res <- coef(model_residuals)[1]
slope_res <- coef(model_residuals)[2]
intercept_formatted <- formatC(intercept_res, format = "e", digits = 10)
slope_formatted <- formatC(slope_res, format = "f", digits = 10)
prediction_equation_res <- paste("residuals = ", intercept_formatted, " + ", slope_formatted, " * residuals_pres")
cat("Prediction Equation:\n", prediction_equation_res, "\n")


####### Question 5 ########
# Outcome variable: vote share. Explanatory variables: difflog and presvote.

#1. 
model_total<-lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model_total)

#2.
coefficients <- coef(model_total)
prediction_equation_total <- paste("voteshare = ", round(coefficients[1], 4), 
                             " + ", round(coefficients[2], 4), " * difflog",
                             " + ", round(coefficients[3], 4), " * presvote")
cat("Prediction Equation:\n", prediction_equation_total, "\n")


