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

setwd("C:\\Users\\Owen Eglinton\\Documents\\GitHub\\StatsI_Fall2024\\problemSets\\PS01\\template\\myanswers")

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Part 1

confidence <- 0.9
n <- length(y)
t <- qt((confidence + 1)/2, df = n-1)
ci_u <- round(mean(y) + t * sd(y)/sqrt(n), 2)
ci_l <- round(mean(y) - t * sd(y)/sqrt(n), 2)
paste("The 90% CI for average student IQ is", ci_l, "-", ci_u)

# Part 2

t.test(y, mu = 100, alternative = "greater")

#####################
# Problem 2
#####################

expend <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=TRUE)

# Part 1

labels_exp = c("State", "Housing Expenditure p.c.", "Income p.c.", "# Financially Insecure per 100,000", "# Urban Residents per 1000", "Region")

plot_expend <- function(x, y) {
    plot(expend[[y]],
        expend[[x]],
        xlab = labels_exp[[y]], 
        ylab = labels_exp[[x]],
    )
}

for (i in 2:4) {
    for (j in (i+1):5) {
        png(file = paste(colnames(expend)[[i]], "_",
                         colnames(expend)[[j]], ".png",
                         sep = ""))
        plot_expend(i, j)
        dev.off()
        print(paste("The correlation between",
                    colnames(expend)[[i]],
                    "and",
                    colnames(expend)[[j]],
                    "is",
                    round(cor(expend[[i]], expend[[j]]), 2))
        )
    }
}

# Part 2

region_labels = c("Northeast", "North Central", "South", "West")
expend$Region <- factor(expend$Region, labels = region_labels)

png(file = "Y_Region.png")
plot_expend(2, 6)
dev.off()

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

paste("The region with the highest per capita expenditure is the ",
      highest,
      ", with an average of $",
      round(mean_high, 2),
      sep = ""
)

# Part 3

png(file = "Y_X1_Region.png")

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
