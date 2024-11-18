rm(list=ls())

setwd("C:\\Users\\Owen Eglinton\\Documents\\GitHub\\StatsI_Fall2024\\problemSets\\PS02\\template\\myanswers")

#####################
# Problem 1
#####################
        
# Part 1

data_1 <- matrix(c(14, 7, 6, 7, 7, 1), nrow=2)
total <- sum(data_1)

exp <- matrix(c(1:6), nrow=2)

for (i in 1:2) {
  for (j in 1:3) {
    exp[i, j] <- sum(data_1[i,])*sum(data_1[,j])/total
  }
}

diff <- data_1 - exp
chi2 <- sum(diff^2/exp)

# Part 2

df_1 <- (nrow(data_1)-1)*(ncol(data_1)-1)

pchisq(chi2, df_1, lower.tail = FALSE)

# Part 3

std_res <- matrix(c(1:6), nrow=2)

for (i in 1:2) {
  for (j in 1:3) {
    std_res[i, j] <- diff[i, j]/sqrt(exp[i, j])
  }
}

#####################
# Problem 2
#####################

data_2 <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=TRUE)

# Part 2

summary(lm(water~reserved, data = data_2))