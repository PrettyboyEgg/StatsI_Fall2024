rm(list=ls())

setwd("C:\\Users\\Owen Eglinton\\Documents\\GitHub\\StatsI_Fall2024\\problemSets\\PS04\\myanswers")

#####################
# Question 1
#####################

install.packages("car")
library(carData)
data(Prestige)
help(Prestige)

# Part a

for (i in 1:nrow(Prestige)) {
  if (is.na(Prestige$type[i]) | Prestige$type[i] != "prof") {
    Prestige$professional[i] <- 0
  } else {
    Prestige$professional[i] <- 1
  }
}

# Part b

prestige_lm <- lm(prestige ~ income*professional, Prestige)
summary(prestige_lm)

# Part f

prestige_model <- function(income, professional) {
  c1 <- prestige_lm$coefficients[1]
  c2 <- prestige_lm$coefficients[2]
  c3 <- prestige_lm$coefficients[3]
  c4 <- prestige_lm$coefficients[4]
  c1 + income*c2 + professional*c3 +income*professional*c4
}
prestige_model(1000, 1) - prestige_model(0, 1)

# Part g

prestige_model(6000, 1) - prestige_model(6000, 0)

#####################
# Problem 2
#####################

# Part a

t_val_1 <- 0.042/0.016
2*pt(t_val_1, 30-1-1, lower.tail = FALSE)

# Part b

t_val_2 <- 0.042/0.013
2*pt(t_val_2, 76-1-1, lower.tail = FALSE)