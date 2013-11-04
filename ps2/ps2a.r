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
library(plm) # for panel data

source("../util/are213-func.R")
source("../util/watercolor.R") # for watercolor plots

ps2a.data <- read.dta('traffic_safety2.dta')
ps2a.datakey <- data.frame(var.name=names(ps2a.data), var.labels = attr(ps2a.data, "var.labels"))
ps2a.data$logfatalpc <- with(ps2a.data, log(fatalities/population))
ps2a.data$sqnetyears <- with(ps2a.data, (year-1981)^2)

#Puts data into pdata.frame for use with the plm package.
ps2a.pdata <- pdata.frame(ps2a.data, index = c("state", "year")) 
                          
## Problem 3

## Part A (pooled OLS, quadratic time trend, and all possible covariates)
pooled.OLS <- plm(logfatalpc ~ primary, data = ps2a.pdata, model = "pooling")

pooled.quadtime <- plm(logfatalpc ~ primary + sqnetyears, data = ps2a.pdata, model = "pooling")

pooled.full <- plm(logfatalpc ~ primary + secondary + college + beer + totalvmt + precip + snow32 + rural_speed + urban_speed, data = ps2a.pdata, model = "pooling")

stargazer(pooled.OLS, pooled.quadtime, pooled.full, title = "Pooled Models of Fatalities Per Capita", out = 'p3a.tex', font.size = "footnotesize", column.labels = c("bivariate", "quadratic time", "covariates"))

## Part B
pooled.OLS.robust <- robust(pooled.OLS)
pooled.quadtime.robust <- robust(pooled.quadtime)
pooled.full.robust <- robust(pooled.full)

## still needs cluster standard error

## Part C: compute between estimator w/ and w/o covariates
between.nocov <- plm(logfatalpc ~ primary, data = ps2a.pdata, model = "between")
between.cov <- plm(logfatalpc ~ primary + secondary + college + unemploy + beer + totalvmt + precip + snow32 + rural_speed + urban_speed, data = ps2a.pdata, model = "between")


## Part D
RE.nocov <- plm(logfatalpc ~ primary, data = ps2a.pdata, model = "random") #Assumes effects are uncorrelated
RE.cov <- plm(logfatalpc ~ primary + secondary + college + unemploy + beer + totalvmt + precip + snow32 + rural_speed + urban_speed, data = ps2a.pdata, model = "random")

## Part E compute cluster standard errors for RE model



