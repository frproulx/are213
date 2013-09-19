### This is Frank Proulx's solution to ARE213 PS1a, problem 1
## Data is in the file "ps1.dta"

## PACKAGES -------------

library(foreign) #this is to read in Stata data
library(Hmisc)
library(psych)
library(stargazer)

library(ggplot2) # for neato plotting tools
library(plyr) # for nice data tools

## DATA -------------

ps1.data <- read.dta(file="ps1.dta")
#changed name of object from "data" to avoid ambiguity issues since "data" is often embedded in functions as a general object

print(nrow(ps1.data))


## Problem 1a: Fix missing values --------
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

# Identify which records have full data, then add a column to indicate full records or not.

full.record.flag <- which(ps1.data$cardiac != 9 & ps1.data$lung != 9 & ps1.data$diabetes !=9 & ps1.data$herpes != 9 & ps1.data$chyper != 9 & ps1.data$phyper != 9 & ps1.data$pre4000 !=9 & ps1.data$preterm != 9 & ps1.data$tobacco != 9 & ps1.data$cigar != 99 & ps1.data$cigar6 !=6 & ps1.data$alcohol != 9 & ps1.data$drink != 99 & ps1.data$drink5 !=5 & ps1.data$wgain !=99)

ps1.data$full.record <- FALSE # initialize column as F

ps1.data$full.record[full.record.flag] <- TRUE #reassign level to T for full records

# replace error rows in cigar with NA so they don't interfere with other calcs.
error.cigar <- which(ps1.data$cigar == 99)
ps1.data$cigar[error.cigar] <- NA

# compare records on things that matter for this analysis...apgar, smoking, etc.
ps1.compare.records <- ddply(ps1.data, .(full.record), summarize,
                             mean.omaps = mean(omaps),
                             sd.omaps = sd(omaps), 
                             mean.fmaps = mean(fmaps),
                             sd.fmaps = sd(fmaps),
                             mean.cigar = mean(cigar, na.rm = TRUE), 
                             sd.cigar = sd(cigar, na.rm = TRUE)
                             )

# There appears to be a variation in the mean cigarette use between groups, but with large standard deviation.

stargazer(ps1.compare.records, summary=FALSE)

# Plot to explore if missing value people smoke more cigarettes
pdf(file="img/cigar-by-record-type.pdf", width = 7, height = 6)
ggplot(ps1.data, aes(cigar)) + geom_density() + facet_grid(full.record~.) + xlab("Number of daily cigarettes") + ylab("Density of responses") + ggtitle("TRUE = Full Records Available, FALSE = Missing Records")
dev.off()

# Linear model to see if you can predict whether the data have a full record based on cigar, omaps, fmaps
unclean.cig <- lm(full.record ~ cigar, ps1.data)
unclean.cig.om <- lm(full.record ~ omaps + cigar, ps1.data)
unclean.cig.om.fm <- lm(full.record ~ omaps + fmaps + cigar, ps1.data)

# The models seem to indicate you can predict whether there is a full record based on apgar and cigarette use....unfortunate.  
stargazer(unclean.cig.om.fm)

ps1.data.clean <- subset (ps1.data, full.record == TRUE)
ps1.data.missingvalues <- subset(ps1.data, full.record == FALSE)


print(nrow(ps1.data.clean)) #number of records remaining after cleaning

# Summary table of clean data, write a new csv

summarytable<-print(describe(ps1.data.clean, skew=FALSE, ranges=FALSE))

latex(summarytable)

write.csv(ps1.data.clean, file = "ps1dataclean.csv")


#'omaps' and 'fmaps' are the APGAR scores
#'dbrwt' is the birth weight in grams
# 'tobacco' is smoker status (1=yes, 2=no)

smokers <- subset(ps1.data, tobacco==1)
nonsmokers <- subset(ps1.data, tobacco==2)

smokerstats <- c(mean(smokers$omaps), mean(smokers$fmaps), mean(smokers$dbrwt))
nonsmokerstats <- c(mean(nonsmokers$omaps), mean(nonsmokers$fmaps), mean(nonsmokers$dbrwt))
meandif <- nonsmokerstats - smokerstats

smoketable <- matrix(c(smokerstats, nonsmokerstats, meandif), ncol=3, byrow=FALSE)
colnames(smoketable) <- c("Mean Value (Infants with Smoker Mothers)", "Mean Value (Infants with Non-Smoker Mothers)", "Mean Difference between control and treatment")
rownames(smoketable) <- c("one minute APGAR score", "five munute APGAR score", "birthweight")
smoketable <- as.table(smoketable)


stargazer(smoketable, title = "Mean values of health figures in Infants with Smoker and Non-Smoker Mothers")



print(t.test(ps1.data$omaps~ps1.data$tobacco))
print(t.test(ps1.data$fmaps~ps1.data$tobacco))
print(t.test(ps1.data$dbrwt~ps1.data$tobacco))