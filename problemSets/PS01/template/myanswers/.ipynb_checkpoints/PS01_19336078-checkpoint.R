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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Part 1

n <- length(y)
z <- qnorm(0.95, 0, 1)
ci_u <- mean(y) + z*sd(y)/sqrt(n)
ci_l <- mean(y) - z*sd(y)/sqrt(n)
paste("The 90% CI for average student IQ is ", ci_l, " - ", ci_u)

# Part 2

test <- (mean(y) - 100)/sd(y)
pval <- pnorm(test, 0, 1, lower.tail=FALSE)
if (pval < 0.05) {
    "H0 Rejected - School mean >100"
} else {
    "Failure to reject H0 - School mean <= 100"
}

#####################
# Problem 2
#####################

expend <- read.table("C://Users/Owen Eglinton/Documents/GitHub/StatsI_Fall2024/datasets/expenditure.txt", header=T)

# Part 1

labels_exp = c("State", "Housing exp p.c.", "Income p.c.", "# Financially Insecure per 100,000", "# Urban Residents per 1000", "Region")

plot_expend <- function(x, y) {
    plot(expend[[y]],
        expend[[x]],
        xlab = labels_exp[[y]], 
        ylab = labels_exp[[x]],
    )
}

for (i in 2:4) {
    for (j in (i+1):5) {
        plot_expend(i, j)
        print(paste("The correlation between ",
                    colnames(expend)[[i]],
                    " and ",
                    colnames(expend)[[j]],
                    " is ",
                    cor(expend[[i]], expend[[j]]),
                    sep = ""
            )
        )
    }
}

# Part 2

region_labels = c("Northeast", "North Central", "South", "West")
expend$Region <- factor(expend$Region, labels = region_labels)

plot_expend(2, 6)

region_factor <- function(x) {
    as.numeric(expend$Region) == x
}

region_mean <- function(x) {
    sum(expend$Y*(region_factor(x)))/sum(region_factor(x))
}

mean_high <- region_mean(1)
highest <- region_labels[[1]]
for (i in 2:length(levels(expend$Region))) {
    if (mean_high < region_mean(i)) {
        mean_high <- region_mean(i)
        highest <- region_labels[[i]]
    }
}

paste("The region with the highest expenditure per capita is the ",
      highest,
      ", with an average per capita of $",
      round(mean_high, 2),
      sep = ""
)

# Part 3

plot_expend(2, 3)

"This graph shows a positive correlation, with higher incomes associated with higher exps on average, though the spread of the data seem heteroskedastic, with the spread of exp also increasing in income"

png(file = "scatterplot.png")

plot(expend[[3]],
     expend[[2]],
     col = expend[[6]],
     pch = as.numeric(expend$Region),
     xlab = labels_exp[[3]], 
     ylab = labels_exp[[2]], 
)

legend(x = "topleft",
    legend=region_labels,
    col=c(1:4),
    pch=c(1:4)
)

dev.off()
