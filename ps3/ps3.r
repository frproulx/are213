library(foreign) #this is to read in Stata data
library(Hmisc)
library(psych)
library(stargazer)
library(ggplot2) # for neato plotting tools
library(plyr) # for nice data tools like ddply
library(car) # "companion for applied regression" - recode fxn, etc.
library(gmodels) #for Crosstabs
library(reshape)
library(rdd) # for regression discontinuity designs

source("../util/are213-func.R")

allsites.data <- read.dta('allsites.dta')
allcovariates.data <- read.dta('allcovariates.dta')
sitecovariates.data <- read.dta('sitecovariates.dta')
twomiledata <- read.dta('2miledata.dta')

allsites.key <- data.frame(c(names(allsites.data), attr(allsites.data, "var.labels")))

## Problem 1

## Part A

regress1a.1 <- lm(data = allsites.data, lnmdvalhs0 ~ npl2000 + lnmeanhs8)
regress1a.2 <- lm(data = allsites.data, lnmdvalhs0 ~
                  npl2000 +
                  lnmeanhs8 +
                  firestoveheat80 +
                  nofullkitchen80 +
                  zerofullbath80 +
                  ## bedrms0_80occ +
                  bedrms1_80occ +
                  bedrms2_80occ +
                  bedrms3_80occ +
                  bedrms4_80occ +
                  bedrms5_80occ +
                  blt0_1yrs80occ +
                  blt2_5yrs80occ +
                  blt6_10yrs80occ +
                  blt10_20yrs80occ +
                  blt20_30yrs80occ +
                  blt30_40yrs80occ +
                  ## blt40_yrs80occ +
                  ## detach80occ +
                  ## attach80occ +
                  ## mobile80occ +
                  occupied80)

regress1a.3 <- lm(data = allsites.data, lnmdvalhs0 ~
                  npl2000 +
                  lnmeanhs8 +
                  firestoveheat80 +
                  nofullkitchen80 +
                  zerofullbath80 +
                  ## bedrms0_80occ +
                  bedrms1_80occ +
                  bedrms2_80occ +
                  bedrms3_80occ +
                  bedrms4_80occ +
                  bedrms5_80occ +
                  blt0_1yrs80occ +
                  blt2_5yrs80occ +
                  blt6_10yrs80occ +
                  blt10_20yrs80occ +
                  blt20_30yrs80occ +
                  blt30_40yrs80occ +
                  ## blt40_yrs80occ +
                  ## detach80occ +
                  ## attach80occ +
                  ## mobile80occ +
                  occupied80 +
                  pop_den8 +
                  shrblk8 +
                  shrhsp8 +
                  child8 +
                  old8 +
                  shrfor8 +
                  ffh8 +
                  smhse8 +
                  hsdrop8 +
                  no_hs_diploma8 +
                  ba_or_better8 +
                  unemprt8 +
                  povrat8 +
                  welfare8 +
                  avhhin8)

regress1a.3.robust <- robust(regress1a.3)
                  
                                 
## Problem 2
## Part A

                                        # I'm not positive about what to use as the dependent variable here..
rdhrs <- RDestimate(mdvalhs0 ~ hrs_82, data = twomiledata, cutpoint = 28.5)

pdf(file = './img/ddplot.pdf', width = 5, height = 5)                         
plot(rdhrs)
abline(v = 28.5)
dev.off()
