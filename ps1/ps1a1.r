### This is Peter Alstone & Frank Proulx's solution to ARE213 PS1a
## Data is in the file "ps1.dta"


## working directories --------

# Peter
# setwd("~/Google Drive/ERG/Classes/ARE213/are213/ps1")

# Frank
setwd("~/media/frank/Data1/documents/School/Berkeley/Fall13/ARE213/are213/ps1")

## PACKAGES -------------

library(foreign) #this is to read in Stata data
library(Hmisc)
library(psych)
library(stargazer)
library(ggplot2) # for neato plotting tools
library(plyr) # for nice data tools like ddply
library(car) # "companion for applied regression" - recode fxn, etc.
library(gmodels) #for Crosstabs

# custom functions
source("../util/are213-func.R")
source("../util/watercolor.R") # for watercolor plots


## DATA -------------

ps1.data <- read.dta(file="ps1.dta")
#changed name of object from "data" to avoid ambiguity issues since "data" is often embedded in functions as a general object

print(nrow(ps1.data))



## Problem 1a: Fix missing values --------
## The following are the error codes for each of the 15 variables that need fixing:
# For cardiac - alcohol: "8" means missing record
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

# Identify which records have full data, then add a column to indicate full records or not
full.record.flag <- which(ps1.data$cardiac != 9 &
                            ps1.data$cardiac != 8 &
                            ps1.data$lung != 9 &
                            ps1.data$lung != 8 &
                            ps1.data$diabetes !=9 &
                            ps1.data$diabetes !=8 &
                            ps1.data$herpes != 9 &
                            ps1.data$herpes != 8 &
                            ps1.data$chyper != 9 &
                            ps1.data$chyper != 8 &
                            ps1.data$phyper != 9 &
                            ps1.data$phyper != 8 &
                            ps1.data$pre4000 !=9 &
                            ps1.data$pre4000 !=8 &
                            ps1.data$preterm != 9 &
                            ps1.data$preterm != 8 &
                            ps1.data$tobacco != 9 &
                            ps1.data$cigar != 99 &
                            ps1.data$cigar6 !=6 &
                            ps1.data$alcohol != 9 &
                            ps1.data$drink != 99 &
                            ps1.data$drink5 !=5 &
                            ps1.data$wgain !=99
                          )

# Column with flags for full records
ps1.data$full.record <- FALSE # initialize column as F
ps1.data$full.record[full.record.flag] <- TRUE #reassign level to T for full records


# Problem 1b: Describe dropped levels --------

# replace error rows in cigar with NA so they don't interfere with other calcs on influence of dropped values.
error.cigar <- which(ps1.data$cigar == 99)
ps1.data$cigar[error.cigar] <- NA

# compare records on things that (might) matter for this analysis...apgar, smoking, etc.
ps1.compare.records <- ddply(ps1.data, .(full.record), summarize,
                             mean.omaps = mean(omaps),
                             sd.omaps = sd(omaps), 
                             mean.fmaps = mean(fmaps),
                             sd.fmaps = sd(fmaps),
                             mean.cigar = mean(cigar, na.rm = TRUE), 
                             sd.cigar = sd(cigar, na.rm = TRUE)
                             )

#--> RESULT: There appears to be a variation in the mean cigarette use between groups, but with large standard deviation.

# Print result table for comparison
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

# Problem 1c: Summary table of clean data, write a new csv-------
var.labels <- attr(ps1.data, "var.labels")
var.labels[length(var.labels)+1] <- "full record present"

ps1.names <- data.frame("labels" = as.data.frame(var.labels))
colnames(ps1.names)[1] <- "labels" 

summarytable<-print(describe(ps1.data.clean, skew=FALSE, ranges=FALSE))

latex(title="variable", file="clean-summary.tex" , cbind(var.labels,summarytable), caption="Summary of clean Data", vbar=TRUE, size="footnotesize")

# stargazer(ps1.data.clean) # Doesn't work as well as the Hmisc version for this long table.

write.csv(ps1.data.clean, file = "ps1dataclean.csv")


#Problem 2a Simple difference in APGAR and birth weight -------

#'omaps' and 'fmaps' are the APGAR scores
#'dbrwt' is the birth weight in grams
# 'tobacco' is smoker status (1=yes, 2=no)

#change tobacco to factor and label values
ps1.data.clean$tobacco <- as.factor(ps1.data.clean$tobacco)
ps1.data.clean$tobacco <- revalue(ps1.data.clean$tobacco, c( "1" = "smoker", "2" = "nonsmoker" ))

smoke.impact <- ddply(ps1.data.clean, .(tobacco), summarize, 
                    mean.omaps = mean(omaps),
                    mean.fmaps = mean(fmaps),
                    mean.dbrwt = mean(dbrwt)
                    )

# conversion to character class for tobacco
smoke.impact$tobacco <- as.character(smoke.impact$tobacco)
# Add difference row
smoke.impact <- rbind(smoke.impact, c("difference", apply(smoke.impact[,2:4], 2, diff)))

stargazer(smoke.impact, summary=FALSE, digits = 2)

# # alt version 2a-------
# 
# smokers <- subset(ps1.data.clean, tobacco==1)
# nonsmokers <- subset(ps1.data.clean, tobacco==2)
# 
# smokerstats <- c(mean(smokers$omaps), mean(smokers$fmaps), mean(smokers$dbrwt))
# nonsmokerstats <- c(mean(nonsmokers$omaps), mean(nonsmokers$fmaps), mean(nonsmokers$dbrwt))
# meandif <- nonsmokerstats - smokerstats
# 
# smoketable <- matrix(c(smokerstats, nonsmokerstats, meandif), ncol=3, byrow=FALSE)
# colnames(smoketable) <- c("Mean Value (Infants with Smoker Mothers)", "Mean Value (Infants with Non-Smoker Mothers)", "Mean Difference between control and treatment")
# rownames(smoketable) <- c("one minute APGAR score", "five munute APGAR score", "birthweight")
# smoketable <- as.data.frame(smoketable)
# 
# 
# stargazer(smoketable, title = "Mean values of health figures in Infants with Smoker and Non-Smoker Mothers", type="latex")

# Problem 2b -------

#recode variables

ps1.data.clean$mrace3 <- as.factor(ps1.data.clean$mrace3)
ps1.data.clean$mrace3 <- revalue(ps1.data.clean$mrace3, c( "1" = "White", "2" = "Other", "3" = "Black" ))

ps1.data.clean$csex <- as.factor(ps1.data.clean$csex)
ps1.data.clean$csex <- revalue(ps1.data.clean$csex, c( "1" = "Male", "2" = "Female"))

ps1.data.clean$dplural <- as.factor(ps1.data.clean$dplural)
ps1.data.clean$dplural <- revalue(ps1.data.clean$dplural, c("1" = "Singleton", "2" = "Twin", "3"= "Triplet", "4" = "Quadruplet", "5" = "Quintuplet+"))

ps1.data.clean$alcohol <- as.factor(ps1.data.clean$alcohol)
ps1.data.clean$alcohol <- revalue(ps1.data.clean$alcohol, c("1" = "Drinker", "2" = "Nondrinker", "9" = "Unk."))

ps1.data.clean$dmar <- as.factor(ps1.data.clean$dmar)
ps1.data.clean$dmar <- revalue(ps1.data.clean$dmar, c("1" = "Married", "2" = "Unmarried"))

# T-tests for relationships.

print( t.test( omaps ~ tobacco, data = ps1.data.clean))
print( t.test( fmaps ~ tobacco, data = ps1.data.clean))
print( t.test( dbrwt ~ tobacco, data = ps1.data.clean))

#visual representation of relationship:

ps1.data.clean$all <- "all records" # flag for facets with all the same.

# birth weight - age
pdf(file="img/bw-age.pdf", width=5, height=7)
bw.age <- ggplot(ps1.data.clean, aes(dmage, dbrwt))
bw.age <- bw.age + 
  stat_smooth() +
  theme_bw() + 
  xlab("Maternal Age (years)") + 
  ylab("Birth Weight (grams)") + 
  facet_grid(all~.)

split.age <- ggplot(ps1.data.clean, aes(x=dmage))
split.age <- split.age +
  geom_density() + 
  theme_bw() + 
  xlab("Maternal Age (years)") +
  ylab("Density in Sub-sample") +
  facet_grid(tobacco~.)

arrange_ggplot2(bw.age, split.age, ncol=1)
dev.off()
  

# birth weight - marriage
pdf(file="img/bw-mar.pdf", width=5, height=7)

bw.mar <- ggplot(ps1.data.clean, aes(factor(dmar),dbrwt))
bw.mar <- bw.mar +
  geom_boxplot() +
  theme_bw() +
  xlab("Marital Status") +
  ylab("Birth Weight (grams)") +
  facet_grid(all~.)

split.mar <- ggplot(ps1.data.clean, aes(x=dmage))
split.mar <- split.age +
  geom_density() + 
  theme_bw() + 
  xlab("Maternal Age (years)") +
  ylab("Density in Sub-sample") +
  facet_grid(dmar~.)

arrange_ggplot2(bw.mar, split.mar, ncol=1)

dev.off()


# birth weight - maternal weight gain
pdf(file="img/bw-gain.pdf", width=5, height=7)
bw.gain <- ggplot(ps1.data.clean, aes(wgain, dbrwt))
bw.gain <- bw.gain + 
  stat_smooth() +
  theme_bw() + 
  xlab("Maternal Weight Gain (lbs)") + 
  ylab("Birth Weight (grams)") + 
  facet_grid(all~.)

split.gain <- ggplot(ps1.data.clean, aes(x=wgain))
split.gain <- split.gain +
  geom_density() + 
  theme_bw() + 
  xlab("Maternal Weight Gain (lbs)") +
  ylab("Density in Sub-sample") +
  facet_grid(tobacco~.)

arrange_ggplot2(bw.gain, split.gain, ncol=1)
dev.off()

## CrossTabs
ps1.xtab.data <- ps1.data.clean
# Add labels from ps1.data
for(i in 1:length(names(ps1.data))){
  label(ps1.xtab.data[[i]]) <- attr(ps1.data, "var.labels")[i]
}

# not-that-useful function for generating a list of crosstabs...
xtab.create <- function(data, const.col, cross.col, counts = FALSE){
# returns a data frame with percentiles of each cross column holding const column consant
# 
# data = a data frame
# const.col = column to be held constant (usually treatment)
# cross col = columns to vary (a concatenated list of character strings or indicies

# ERRORS
# TODO: Error handling
  # race of mother
xtab.out <- list()

for(factor in cross.col){
  xtab.factor <- CrossTable(data[[factor]],data[[const.col]])
  store.ver <- reshape(data=as.data.frame(xtab.factor$prop.row),idvar="x",direction="wide",timevar="y")
  if(counts){
    
    
  }
  xtab.out[[factor]] <- store.ver
}
return(xtab.out)
}



# # Improved labels for table (optional)
# label(ps1.data.clean$dmage) <- "Maternal Age (yr)"
# label(ps1.data.clean$tobacco) <- "Tobacco Use Status"
# label(ps1.data.clean$mrace3) <- "Maternal Race"
# label(ps1.data.clean$csex) <- "Infant Sex"
# label(ps1.data.clean$dplural) <- "Infant Plurality"
# label(ps1.data.clean$clingest) <- "Gestational Age (weeks)"
# label(ps1.data.clean$alcohol) <- "Alcohol Use Status"
# label(ps1.data.clean$phyper) <- "Preg. Hypertension"

# Grouped crosstabs using Hmisc

latex(summary( tobacco  ~ 
                 mrace3 + 
                 csex + 
                 dmar +
                 dplural + 
                 alcohol + 
                 phyper + 
                 chyper +
                 cardiac +
                 diabetes +
                 pre4000 +
                 dmeduc +
                 monpre +
                 dmage +
                 clingest + 
                 wgain, 
               data=ps1.xtab.data,  
               method="reverse", 
               overall=TRUE, long=TRUE
               ),
      title = "crosstab-tobacco",
      label = "tab:xtabTobacco",
      caption = "Contingency table for a range of factors by tobacco use status",
      exclude1=F
      )

# Problem 2c --------
# This one is all in latex doc.  Just describing things.

# Problem 2d ---------

sm.age <- lm(dbrwt ~ dmage, ps1.data.clean)

sm.age.mar <- lm(dbrwt ~ dmage + dmar, ps1.data.clean)

sm.a.m.w <- lm(dbrwt ~ dmage + dmar + wgain, ps1.data.clean)

sm.a.m.w.t <- lm(dbrwt ~ dmage + dmar + wgain + tobacco, ps1.data.clean)

sm.a.m.t <- lm(dbrwt ~ dmage + dmar + tobacco, ps1.data.clean)

sm.a.t <- lm(dbrwt ~ dmage + tobacco, ps1.data.clean)

sm.axm.t <- lm(dbrwt ~ dmage * dmar + tobacco, ps1.data.clean) #not used - look for cross of age:mar

# table without tobacco
stargazer(sm.age, sm.age.mar, sm.a.m.w, 
          type="latex",
          covariate.labels = c("Maternal Age", "Marital Status (unmarried)", "Weight Gain"),
          align = TRUE,
          style="qje", 
          single.row = FALSE,
          font.size="footnotesize",
          dep.var.labels = "Birth Weight",
          out = "combinedReg-noTob.tex"
          )

# table with tobacco
stargazer(sm.a.m.w.t, sm.a.m.t, sm.a.t, 
          type="latex",
          covariate.labels = c("Maternal Age", "Marital Status (unmarried)", "Weight Gain", "Tobacco (nonsmoker)"),
          align = TRUE,
          style="qje", 
          single.row = FALSE,
          font.size="footnotesize",
          dep.var.labels = "Birth Weight",
          out = "combinedReg-withTob.tex"
)


