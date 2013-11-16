## Frank's wd
setwd("/media/frank/Data/documents/school/berkeley/fall13/are213/are213/ps2")
## Peter's wd
setwd("~/Google Drive/ERG/Classes/ARE213/are213/ps2")


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


## Robust by hand
                                        #This calculates the Huber-White Robust standard errors -- code based on http://thetarzan.wordpress.com/2011/05/28/heteroskedasticity-robust-and-clustered-standard-errors-in-r/
s <- summary(pooled.full)
X <- model.matrix(pooled.full)
u2 <- residuals(pooled.full)^2
XDX <- 0

for(i in 1:nrow(X)) {
    XDX <- XDX +u2[i]*X[i,]%*%t(X[i,])
}

                                        # inverse(X'X)
XX1 <- solve(t(X)%*%X)

                                        #Compute variance/covariance matrix
varcovar <- XX1 %*% XDX %*% XX1

                                        # Degrees of freedom adjustment
dfc <- sqrt(nrow(X))/sqrt(nrow(X)-ncol(X))

stdh <- dfc*sqrt(diag(varcovar))

t <- pooled.full$coefficients/stdh
p <- 2*pnorm(-abs(t))
results.robust <- cbind(pooled.full$coefficients, stdh, t, p)
dimnames(results.robust) <- dimnames(s$coefficients)
results.robust

## cluster by hand -- using many of the same variables as defined in the robust section (above), with some modifications:
cluster <- "state"
clus <- cbind(X, "state"=ps2a.data[,cluster], "resid" = resid(pooled.full))
                                        #number of clusters
m <- dim(table(clus[,cluster]))
k <- dim(X)[2]

uj <- matrix(NA, nrow=m, ncol = k)
gs <- names(table(ps2a.data[,cluster]))
for (i in 1:m){
    uj[i,] <- t(matrix(clus[clus[,cluster]==gs[i], 'resid'])) %*% clus[clus[,cluster]==gs[i], 1:k]
}


                                        #Compute variance/covariance matrix
varcovar <- XX1 %*% crossprod(uj) %*% XX1

                                        # Degrees of freedom adjustment
dfc <- sqrt((m/(m-1)) * (nrow(X)-1)/(nrow(X)-ncol(X)))

stdh <- dfc*sqrt(diag(varcovar))

t <- pooled.full$coefficients/stdh
p <- 2*pnorm(-abs(t))
results.cluster <- cbind(pooled.full$coefficients, stdh, t, p)
dimnames(results.cluster) <- dimnames(s$coefficients)
results.cluster

hand.comparison <- cbind(rownames(results.cluster), a.typ.full[,2], results.robust[,2], results.cluster[,2])
colnames(hand.comparison) <- c("Estimand","Conventional", "Robust", "Clustered")
stargazer(data.frame(hand.comparison),
          summary = FALSE,
          title = "Comparison of Standard Error HC Methods for Full Pooled Model, as calculated by hand",
          style = "qje",
          out = 'p3b2.tex',
          font.size = "footnotesize", 
          column.labels = c("Conventional", "HC1 Robust", "HC1 Robust + Cluster"),
          label="tab:3b2",
          digits=4
          )
 

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
