### This is Frank Proulx's solution to ARE213 PS1a, problem 1
## Data is in the file "ps1.dta"

library(foreign) #this is to read in Stata data
library(Hmisc)
library(psych)
library(stargazer)
data <- read.dta("ps1.dta")

print(nrow(data))

## Problem 1a: Fix missing values
## The following are the error codes for each of the 15 variables that need fixing:
# cardiac: 9
# lung: 9
# diabetes: 9
# herpes: 9
# chyper: 9
# phyper: 9
# pre4000: 9
# preterm: 9
# tobacco: 9
# cigar: 99
# cigar6: 6
# alcohol: 9
# drink: 99
# drink5: 5
# wgain: 99

data <- subset (data, (cardiac != 9) & (lung != 9) & (diabetes !=9) & (herpes !=9) & (chyper !=9) & (phyper !=9) & (pre4000 !=9) & (preterm !=9) & (tobacco !=9) & (cigar !=99) & (cigar6 !=6) & (alcohol !=9) & (drink !=99) & (drink5 !=5) & (wgain !=99))

print(nrow(data)) #number of records remaining after cleaning

summarytable<-describe(data, skew=FALSE, ranges=FALSE)
print(summarytable)

stargazer(data, title = "Summary of Cleaned Dataset", out="summarytable.tex")

write.csv(data, file = "ps1dataclean.csv")

#'omaps' and 'fmaps' are the APGAR scores
#'dbrwt' is the birth weight in grams
# 'tobacco' is smoker status (1=yes, 2=no)

smokers <- subset(data, tobacco==1)
nonsmokers <- subset(data, tobacco==2)

smokerstats <- c(mean(smokers$omaps), mean(smokers$fmaps), mean(smokers$dbrwt))
nonsmokerstats <- c(mean(nonsmokers$omaps), mean(nonsmokers$fmaps), mean(nonsmokers$dbrwt))
meandif <- nonsmokerstats - smokerstats

smoketable <- matrix(c(smokerstats, nonsmokerstats, meandif), ncol=3, byrow=FALSE)
colnames(smoketable) <- c("Mean Value (Infants with Smoker Mothers)", "Mean Value (Infants with Non-Smoker Mothers)", "Mean Difference between control and treatment")
rownames(smoketable) <- c("one minute APGAR score", "five minute APGAR score", "birthweight")
smoketable <- as.table(smoketable)

stargazer(smoketable, title = "Mean values of health figures in Infants with Smoker and Non-Smoker Mothers")



print(t.test(data$omaps~data$tobacco))
print(t.test(data$fmaps~data$tobacco))
print(t.test(data$dbrwt~data$tobacco))
