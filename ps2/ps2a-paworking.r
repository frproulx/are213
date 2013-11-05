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
require(lmtest)
# require(sandwich)

source("../util/are213-func.R")
#source("../util/watercolor.R") # for watercolor plots

ps2a.data <- read.dta('traffic_safety2.dta')
ps2a.datakey <- data.frame(var.name=names(ps2a.data), var.labels = attr(ps2a.data, "var.labels"))
ps2a.data$logfatalpc <- with(ps2a.data, log(fatalities/population))
ps2a.data$sqyears <- with(ps2a.data, year^2)

# Graphical exploration of data

ggplot(ps2a.data, aes(year, fatalities/population)) + geom_point(aes(color=primary)) + facet_grid(.~primary) + stat_smooth()

pdf("plot3a.pdf", width=5, height=4)
ggplot(ps2a.data, aes(year, log(fatalities/population))) + geom_point(aes(color=primary)) + facet_grid(.~primary) + stat_smooth()
dev.off()

#Puts data into pdata.frame for use with the plm package.
ps2a.pdata <- pdata.frame(ps2a.data, index = c("state", "year")) 
                          
## Problem 3

# Issues with non-conforming matrix seem to trace to bad factor class for year....fixed here.  
ps2a.pdata$year <- as.numeric(ps2a.pdata$year)

## Part A (pooled OLS, quadratic time trend, and all possible covariates)
pooled.OLS <- plm(logfatalpc ~ primary, data = ps2a.pdata, model = "pooling")

pooled.quadtime <- plm(logfatalpc ~ primary + year + sqyears, data = ps2a.pdata, model = "pooling")

pooled.full <- plm(logfatalpc ~ primary + year + sqyears + secondary + college + beer + totalvmt + precip + snow32 + rural_speed + urban_speed, data = ps2a.pdata, model = "pooling")

stargazer(pooled.OLS, pooled.quadtime, pooled.full,
          title = "Pooled Models of Fatalities Per Capita", 
          style="qje",
          out = 'p3a.tex', 
          font.size = "footnotesize", 
          column.labels = c("bivariate", "quadratic time", "covariates"),
          label="tab:3a")

#typical
coeftest(pooled.full)
#robust
coeftest(pooled.full, vcov = vcovHC)
#clustered
coeftest(pooled.full, vcov = vcovHC(pooled.full, type="HC1", cluster = "group"))

## Part B
pooled.OLS.robust <- robust(pooled.OLS)
pooled.quadtime.robust <- robust(pooled.quadtime)
pooled.full.robust <- robust(pooled.full)

# In theory you can get robust SE from adding "robust=TRUE" to a summary call of a plm object.  
summary(pooled.OLS, robust=TRUE)

## still needs cluster standard error

# See http://people.su.se/~ma/clustering.pdf -- functions in util pulled from there...but doesn't work with plm package :(
# Clustered SE code not included in basic R pacakge so need to hard-code it :)  Based on:
# http://diffuseprior.wordpress.com/2012/06/15/standard-robust-and-clustered-standard-errors-computed-in-r/

# This function only works in the bivariate case...
ols.hetero(logfatalpc ~ primary, data = ps2a.data, robust=TRUE, cluster="state")



# ### REBOOT:  but this section doesn't work ------
# 
# # centering function
# gcenter <- function(df1,group) {
#   variables <- paste(
#   rep("c", ncol(df1)), colnames(df1), sep=".")
#   copydf <- df1
#   for (i in 1:ncol(df1)) {
#   copydf[,i] <- df1[,i] - ave(df1[,i], group,FUN=mean)}
#   colnames(copydf) <- variables
#   return(cbind(df1,copydf))}
# 
# 
# # cast df with centering
# pd.state <- gcenter(ps2a.data, ps2a.data$state)
# 
# # Part A
# require(lmtest)
# require(sandwich)
# 
# pool.ols <- lm(logfatalpc ~ primary, data = ps2a.data)
# pool.quadtime <- lm(logfatalpc ~ primary + year + sqyears, data = ps2a.data)
# pool.full <- lm(logfatalpc ~ primary + year + sqyears + secondary + college + beer + totalvmt + precip + snow32 + rural_speed + urban_speed, data = ps2a.data)
# 
# #NON-robust estimates:
# se.ols <- coeftest(pool.ols
# se.quadtime <- coeftest(pool.quadtime)
# se.full <- coeftest(pool.full)
# 
# stargazer(se.ols, se.quadtime, se.full,
#          title = "Typical Pooled Models of Fatalities Per Capita", 
#          style="qje",
#          out = 'p3a1.tex', 
#          font.size = "footnotesize", 
#          column.labels = c("bivariate", "quadratic time", "covariates"),
#          label="tab:3a")                  
#                    
# #robust ests that should match STATA:
# robust.ols <- coeftest(pool.ols, vcov = vcovHC(pool.ols, type="HC1"))
# robust.quadtime <- coeftest(pool.quadtime, vcov = vcovHC(pool.quadtime,type = "HC1"))
# robust.full <- coeftest(pool.full, vcov = vcovHC(pool.full, type = "HC1"))
# 
# stargazer(robust.ols, robust.quadtime, robust.full,
#          title = "ROBUST Pooled Models of Fatalities Per Capita", 
#          style="qje",
#          out = 'p3a2.tex', 
#          font.size = "footnotesize", 
#          column.labels = c("bivariate", "quadratic time", "covariates"),
#          label="tab:3a")
                   

                   

                   
## Part C: compute between estimator w/ and w/o covariates-----
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
