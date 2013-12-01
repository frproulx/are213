

## Frank's wd
## setwd("/media/frank/Data/documents/school/berkeley/fall13/are213/are213/ps3")
## Peter's wd
# setwd("~/Google Drive/ERG/Classes/ARE213/are213/ps3")

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
library(AER) # includes ivreg function, etc.  Maybe worth using...

source("../util/are213-func.R")

all.sites <- read.dta('allsites.dta')
all.cov <- read.dta('allcovariates.dta')
site.cov <- read.dta('sitecovariates.dta')
two.mile <- read.dta('2miledata.dta')

datakey <- function(stata.data.frame){
  out <- data.frame(names = names(stata.data.frame), 
                    labels = attr(stata.data.frame, "var.labels"))
  return(out)
}

all.sites.key <- datakey(all.sites)
all.cov.key <- datakey(all.cov)
site.cov.key <- datakey(site.cov)
two.mile.key <- datakey(two.mile)


## Problem 1

## Part 1a -------

reg1a.1 <- lm(data = all.sites, lnmdvalhs0 ~ npl2000 + lnmeanhs8)

# robust standard errors test
reg1a.1.rse <- coeftest(reg1a.1, vcov = vcovHC(reg1a.1, type="HC1"))

reg1a.2 <- lm(data = all.sites, lnmdvalhs0 ~
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

#robust SE
reg1a.2.rse <- coeftest(reg1a.2, vcov = vcovHC(reg1a.2, type = "HC1"))

reg1a.3 <- lm(data = all.sites, lnmdvalhs0 ~
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

reg1a.3.rse <- coeftest(reg1a.3, vcov = vcovHC(reg1a.3, type = "HC1"))

reg1a.4 <- lm(data = all.sites, lnmdvalhs0 ~
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
                avhhin8 +
                as.factor(statefips))

reg1a.4.rse <- coeftest(reg1a.4, vcov = vcovHC(reg1a.4, type = "HC1"))

# list of state names for fixed effects model (to omit in output)
state.list.omit <- vector("list", 50)
for(i in 1:50){state.list.omit[i] <- paste0("as.factor(statefips)",i)}

stargazer(reg1a.1.rse, reg1a.2.rse, reg1a.3.rse, reg1a.4.rse,
          title = "Linear models for effect of NPL(2000) on housing value (with many additional state fixed effects omitted)",
          column.labels = c("simple model", "+housing char.", "+demographics", "+state fixed effects"),
          out = "tab1a.tex",
          type = "latex", 
          style = "qje",
          no.space = TRUE, 
          font.size = "scriptsize", 
          single.row = TRUE,
          omit = 45:100) #omit all past first 45 vars.
                  

## Part 1b -------

# comparison of all.cov by nbr_dummy
all.cov.cov <- paste("npl1990+", tail(names(all.cov),-2)) #list of all covariates to use
formula1 <- as.formula(paste("npl2000 ~", paste(all.cov.cov, collapse="+")))

latex(summary( formula1,
               data=all.cov,  
               method="reverse", 
               overall=TRUE, 
               long=TRUE,
               test = TRUE
),
      title = "tab1b-1",
      label = "tab:1b",
      digits = 2,
      round = 2,
      size = "scriptsize",
      caption = "Contingency table for a range of factors by npl2000 status",
      exclude1=F
)

# over / under 28.5

site.cov$over.limit <- site.cov$hrs_82 > 28.5

site.cov.cov <- head(names(site.cov),-1) #list of all covariates to use
formula2 <- as.formula(paste("over.limit ~", paste(site.cov.cov, collapse="+")))

latex(summary( formula2,
               data=site.cov,  
               method="reverse", 
               overall=TRUE, 
               long=TRUE,
               test = TRUE
),
      title = "tab1b-2",
      label = "tab:1b-2",
      digits = 2,
      round = 2,
      size = "scriptsize",
      caption = "Contingency table by HRS test status (over/under 28.5)",
      exclude1=F
)

# narrow in on tighter window.

near.limit <- which(site.cov$hrs_82 > 16.5 & site.cov$hrs_82 < 40.5)

latex(summary( formula2,
               data=site.cov[near.limit,],  
               method="reverse", 
               overall=TRUE, 
               long=TRUE,
               test = TRUE
),
      title = "tab1b-3",
      label = "tab:1b-3",
      digits = 2,
      round = 2,
      size = "scriptsize",
      caption = "Contingency table by HRS test status (JUST over/under 28.5)",
      exclude1=F
)


## Problem 2
## Part 2a --------
# I'm not positive about what to use as the dependent variable here..
# I think this is getting ahead of the problem...need to establish if RD makes sense in prob 2.
# rdhrs <- RDestimate(mdvalhs0 ~ hrs_82, data = two.mile, cutpoint = 28.5)
# 
# pdf(file = './img/ddplot.pdf', width = 5, height = 5)                         
# plot(rdhrs)
# abline(v = 28.5)
# dev.off()

## Part 2b -------
lim.under <- which(two.mile$hrs_82 < 28.5)
lim.over <- which(two.mile$hrs_82 >= 28.5)
gg.2b <- ggplot(two.mile, aes(hrs_82))


# todo: finish histogram with smooth lines on either side of 28.5
gg.2b + geom_histogram(binwidth=5) +
  geom_vline(aes(xintercept=28.5)) 