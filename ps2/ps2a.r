## Frank's wd
setwd("/media/frank/Data/documents/school/berkeley/fall13/are213/are213/ps2")

library(foreign) #this is to read in Stata data
library(Hmisc)
library(psych)
library(stargazer)
library(ggplot2) # for neato plotting tools
library(plyr) # for nice data tools like ddply
library(car) # "companion for applied regression" - recode fxn, etc.
library(gmodels) #for Crosstabs

source("../util/are213-func.R")
source("../util/watercolor.R") # for watercolor plots

ps2a.data <- read.dta('traffic_safety2.dta')
ps2a.data$logfatalpc <- with(ps2a.data, log(fatalities/population))
ps2a.data$sqnetyears <- with(ps2a.data, (year-1981)^2)
ps2a.datakey <- data.frame(var.name=names(ps2a.data), var.labels = attr(ps2a.data, "var.labels"))

## Problem 3
## Part A (pooled OLS, quadratic time trend, and all possible covariates)
pooled.OLS <- lm(logfatalpc ~ primary, data = ps2a.data)

pooled.quadtime <- lm(logfatalpc ~ primary + sqnetyears, data = ps2a.data)

pooled.full <- lm(logfatalpc ~ primary + secondary + beer + totalvmt + precip + snow32 + rural_speed + urban_speed, data = ps2a.data)


## Part B
pooled.OLS.robust <- robust(pooled.OLS)
pooled.quadtime.robust <- robust(pooled.quadtime)
pooled.full.robust <- robust(pooled.full)


