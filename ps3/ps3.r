library(foreign) #this is to read in Stata data
library(Hmisc)
library(psych)
library(stargazer)
library(ggplot2) # for neato plotting tools
library(plyr) # for nice data tools like ddply
library(car) # "companion for applied regression" - recode fxn, etc.
library(gmodels) #for Crosstabs
library(reshape)

source("../util/are213-func.R")

allsites.data <- read.dta('allsites.dta')
allcovariates.data <- read.dta('allcovariates.dta')
sitecovariates.data <- read.dta('sitecovariates.dta')

## Problem 1

## Part A

regress1a.1 <- lm(data = allsites.data, lnmdvalhs0 ~ npl2000)
