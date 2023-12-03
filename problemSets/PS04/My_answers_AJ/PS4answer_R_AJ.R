
### Problem set 4 ###

detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("stargazer","arm","emmeans","ggplot2"),  pkgTest)

install.packages("car")
library(car)
data(Prestige)
help(Prestige)

head(Prestige)
str(Prestige) # variable type is already a Factor with 3 levels. 
levels(Prestige$type)

####### 1).########
####### a) 

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
head(Prestige)

####### b)
# Run a linear model with prestige as an outcome and income, professional, and the
# interaction of the two as predictors (Note: this is a continuous x dummy interaction.)

model1 <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(model1)

####### c) Write the prediction equation based on the result.

# prestige=21.1423+0.0032×income+37.7813×professional−0.0023×(income×professional)


############# g)

emm <- emmeans(model1, ~ professional | income, at = list(income = 6000))
plot(emm, comparisons = TRUE)
