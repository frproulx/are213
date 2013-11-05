## Frank's wd
setwd("/media/frank/Data/documents/school/berkeley/fall13/are213/are213/ps2")
## Peter's wd
# setwd("~/Google Drive/ERG/Classes/ARE213/are213/ps2")

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
ps2a.data$sqyears <- with(ps2a.data, year^2)

#Puts data into pdata.frame for use with the plm package.
ps2a.pdata <- pdata.frame(ps2a.data, index = c("state", "year")) 
                          
## Problem 3

## Part A (pooled OLS, quadratic time trend, and all possible covariates)
pooled.OLS <- plm(logfatalpc ~ primary, data = ps2a.pdata, model = "pooling")

pooled.quadtime <- plm(logfatalpc ~ primary + year + sqyears, data = ps2a.pdata, model = "pooling")

pooled.full <- plm(logfatalpc ~ primary + year + sqyears + secondary + college + beer + totalvmt + precip + snow32 + rural_speed + urban_speed, data = ps2a.pdata, model = "pooling")

stargazer(pooled.OLS, pooled.quadtime, pooled.full, title = "Pooled Models of Fatalities Per Capita", out = 'p3a.tex', font.size = "footnotesize", column.labels = c("bivariate", "quadratic time", "covariates"))

## Part B
pooled.OLS.robust <- robust(pooled.OLS)
pooled.quadtime.robust <- robust(pooled.quadtime)
pooled.full.robust <- robust(pooled.full)

# In theory you can get robust SE from adding "robust=TRUE" to a summary call of a plm object.  
summary(pooled.OLS, robust=TRUE)

## still needs cluster standard error

# See http://people.su.se/~ma/clustering.pdf -- functions in util pulled from there...but doesn't work with plm package :(
# This blog has an implementation that claims to work but I hesitate to just implement their coded:
# http://diffuseprior.wordpress.com/2012/06/15/standard-robust-and-clustered-standard-errors-computed-in-r/

# # Doesn't work with plm...
# clx(pooled.OLStest, ps2a.data, ps2a.data$state)



## Part C: compute between estimator w/ and w/o covariates
between.nocov <- plm(logfatalpc ~ primary, data = ps2a.pdata, model = "between")
between.cov <- plm(logfatalpc ~ primary + secondary + college + totalvmt + snow32 + rural_speed, data = ps2a.pdata, model = "between")

stargazer(between.nocov, between.cov, title = "Between models of effects of primary seatbelt use laws", out = "p3c.tex", font.size = "footnotesize")

## Part D
RE.nocov <- plm(logfatalpc ~ primary, data = ps2a.pdata, model = "random") #Assumes effects are uncorrelated
RE.cov <- plm(logfatalpc ~ primary + secondary + college + unemploy + beer + totalvmt + precip + snow32 + rural_speed + urban_speed, data = ps2a.pdata, model = "random")

stargazer(RE.nocov, RE.cov, title = "Random Effects Models", out = "p3d.tex", font.size = "footnotesize")

## Part E compute cluster standard errors for RE

## Part F compute FE estimator
fixed.primary <- plm(logfatalpc ~ primary + sqnetyears, data = ps2a.pdata, model = "within")

## Part G
fixed.cov <- plm(logfatalpc ~ primary + sqnetyears + secondary + college + beer + totalvmt + precip + snow32 + rural_speed + urban_speed, data = ps2a.pdata, model = "within")

stargazer(fixed.primary, fixed.cov, title = "Fixed Effects Models", out = "p3fg.tex", font.size = "footnotesize")
