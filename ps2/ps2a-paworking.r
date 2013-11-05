## Frank's wd
setwd("/media/frank/Data/documents/school/berkeley/fall13/are213/are213/ps2")
## Peter's wd
# setwd("~/Google Drive/ERG/Classes/ARE213/are213/ps2")

# TODO: Hand-code an OLS clustered estimator for Prob 3b.  One option for this is to reference and adopt the code at http://diffuseprior.wordpress.com/2012/06/15/standard-robust-and-clustered-standard-errors-computed-in-r

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
pdf(file="allstates.pdf", width = 7, height = 7.5)
ggplot(ps2a.data, aes(year, fatalities/population)) + 
  geom_point(aes(color=primary)) + 
  scale_x_continuous(breaks = c(1985, 1995)) +
  scale_y_continuous(breaks = c(0.2, 0.4)) +
  facet_wrap("state") + 
  ylab("log(fatalities) per capita") +
  ggtitle("State-level time series in log of fatalities per capita") +
  stat_smooth()
dev.off()

#plot 3a
makeplot3a <- FALSE
if(makeplot3a){
pdf("plot3a.pdf", width=5, height=4)
ggplot(ps2a.data, aes(year, log(fatalities/population))) + geom_point(aes(color=primary)) + facet_grid(.~primary) + stat_smooth()
dev.off()
}


#Puts data into pdata.frame for use with the plm package.
ps2a.pdata <- pdata.frame(ps2a.data, index = c("state", "year")) 
                          
## Problem 3

# Issues with non-conforming matrix seem to trace to bad factor class for year....fixed here.  
ps2a.pdata$year <- as.numeric(ps2a.pdata$year)

# ADDDING square year term HERE WORKS FOR WHATEVER REASON
ps2a.pdata$sqr.year <- with(ps2a.pdata, year^2)

## Part A (pooled OLS, quadratic time trend, and all possible covariates) ------
pooled.OLS <- plm(logfatalpc ~ primary, data = ps2a.pdata, model = "pooling")

pooled.quadtime <- plm(logfatalpc ~ primary + year + sqr.year, data = ps2a.pdata, model = "pooling")

pooled.full <- plm(logfatalpc ~ primary + year + sqr.year + secondary + college + beer + totalvmt + precip + snow32 + rural_speed + urban_speed, data = ps2a.pdata, model = "pooling")

stargazer(pooled.OLS, pooled.quadtime, pooled.full,
          title = "Pooled Models of Fatalities Per Capita", 
          style="qje",
          out = 'p3a.tex', 
          font.size = "footnotesize", 
          column.labels = c("bivariate", "quadratic time", "covariates"),
          label="tab:3a")

## Part B (Standard Error Errors) --------

#typical
a.typ.full <- coeftest(pooled.full)
#robust
a.robust.full <- coeftest(pooled.full, vcov = vcovHC)
#clustered
a.clust.full <- coeftest(pooled.full, vcov = vcovHC(pooled.full, type="HC1", cluster = "group"))

stargazer(a.typ.full, a.robust.full, a.clust.full, 
          title = "Comparison of Standard Error HC Methods for Full Pooled Model",
          style = "qje",
          out = 'p3b1.tex',
          font.size = "footnotesize", 
          column.labels = c("Conventional", "HC1 Robust", "HC1 Robust + Cluster"),
          label="tab:3b1"
          )

# # OLS CASE doesn't work for some reason. ------
# #typical
# a.typ.OLS <- coeftest(pooled.OLS)
# #robust
# a.robust.OLS <- coeftest(pooled.OLS, vcov = vcovHC)
# #clustered
# a.clust.OLS <- coeftest(pooled.OLS, vcov = vcovHC(pooled.OLS, type="HC1", cluster = "group"))
# 
# stargazer(a.typ.OLS, a.robust.OLS, a.clust.OLS, 
#           title = "Comparison of Standard Error HC Methods for Bivariate Pooled OLS",
#           style = "qje",
#           out = 'p3b1b.tex',
#           font.size = "footnotesize", 
#           column.labels = c("Assume Homo", "HC1 Robust", "HC1 Robust + Cluster"),
#           label="tab:3b1b"
# )
# OLD CODE FROM Part B ------

# pooled.OLS.robust <- robust(pooled.OLS)
# pooled.quadtime.robust <- robust(pooled.quadtime)
# pooled.full.robust <- robust(pooled.full)

# # In theory you can get robust SE from adding "robust=TRUE" to a summary call of a plm object.  
# summary(pooled.OLS, robust=TRUE)

## still needs cluster standard error

# See http://people.su.se/~ma/clustering.pdf -- functions in util pulled from there...but doesn't work with plm package :(
# Clustered SE code not included in basic R pacakge so need to hard-code it :)  Based on:
# http://diffuseprior.wordpress.com/2012/06/15/standard-robust-and-clustered-standard-errors-computed-in-r/
# 
# # This function only works in the bivariate case...
# ols.hetero(logfatalpc ~ primary, data = ps2a.data, robust=TRUE, cluster="state")
# 
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

e.typ.recov <- coeftest(RE.cov)
e.clust.recov <- coeftest(RE.cov, vcov = vcovHC(RE.cov, type = "HC1", cluster = "group"))

stargazer(e.typ.recov, e.clust.recov, 
          title = "Comparison of Standard Error HC Methods for RE model with covariates",
          style = "qje",
          out = 'p3e.tex',
          font.size = "footnotesize", 
          column.labels = c("Conventional SE", "HC1 Robust + Cluster"),
          label="tab:p3e"
)



## Part F compute FE estimator
fixed.primary <- plm(logfatalpc ~ primary + year + sqr.year, data = ps2a.pdata, model = "within")

f.typ.fe <- coeftest(fixed.primary)
f.clust.fe <- coeftest(fixed.primary, vcov = vcovHC(fixed.primary, type='HC1', cluster = "group"))

stargazer(f.typ.fe, f.clust.fe, 
          title = "Comparison of Standard Error HC Methods for basic FE model",
          style = "qje",
          out = 'p3f.tex',
          font.size = "footnotesize", 
          column.labels = c("Conventional SE", "HC1 Robust + Cluster"),
          label="tab:p3f"
)


## Part G
fixed.cov <- plm(logfatalpc ~ primary + year + sqr.year + secondary + college + beer + totalvmt + precip + snow32 + rural_speed + urban_speed, data = ps2a.pdata, model = "within")

stargazer(fixed.primary, fixed.cov, 
          coeftest(fixed.cov, vcov = vcovHC(fixed.cov, type="HC1", cluster = "group")),
          title = "Fixed Effects Models", 
          out = "p3g.tex", font.size = "footnotesize",
          label = "tab:p3g",
          column.labels = c("Simple FE", "FE with Cov.", "Robust/Clustered SE")
          )
