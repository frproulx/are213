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
## Part 2b -------
## Frank: I believe that what he is going for here is the DCdensity function (i.e. the McCrary specification test, plotted out).
## I've implmented this below.

## lim.under <- which(two.mile$hrs_82 < 28.5)
## lim.over <- which(two.mile$hrs_82 >= 28.5)
## gg.2b <- ggplot(two.mile, aes(hrs_82))
## # todo: finish histogram with smooth lines on either side of 28.5
## gg.2b <- gg.2b + geom_histogram(binwidth=5) +
##   geom_vline(aes(xintercept=28.5), color = 'red')

pdf(file = './img/ddplot.pdf', width = 5, height = 5)
DCdensity(two.mile$hrs_82, cutpoint = 28.5)
abline(v = 28.5, col = "red")
title(main = 'Density distribution of HRS', xlab = 'HRS in 1982', ylab = 'Density estimate')
dev.off()

## Problem 3
## Part 3a ------
#instrument is whether there is a site scoring above 28.5 on the 1982 HRS
# We want to regress on npl2000
two.mile.cov <- head(names(two.mile), -14)
#Decided to axe the FIPS variable- any ideas how to include this and not break anything?
# Did you try fixed effects for the states?

two.mile$diff.cutoff <- two.mile$hrs_82 - 28.5

# I took out npl1990 as a predictor for 2000...it basically washes out all the variation that hrs shoudl explain.

# I think this is the correct form for the formula (see RD class notes page 15 for fuzzy RD)
formula3.stage1 <- as.formula(paste("npl2000 ~ I(hrs_82 >= 28.5) + diff.cutoff + diff.cutoff:I(hrs_82 >= 28.5) +", paste(two.mile.cov, collapse = "+"), "- blt40_yrs80occ_nbr"))

formula3.stageRF <- as.formula(paste("lnmdvalhs0_nbr ~ I(hrs_82 >= 28.5) + diff.cutoff + diff.cutoff:I(hrs_82 >= 28.5) +", paste(two.mile.cov, collapse = "+"), "- blt40_yrs80occ_nbr"))

stage1.1 <- lm(formula3.stage1,
               data = two.mile)

stage1.2 <- lm(formula3.stage1,
               data = two.mile,
               subset = (two.mile$hrs_82 > 16.5 &
                         two.mile$hrs_82 < 40.5)
               )

stage2.1 <- lm(formula3.stageRF, 
               data = two.mile)

stage2.2 <- lm(formula3.stageRF,
               data = two.mile,
               subset = (two.mile$hrs_82 > 16.5 &
                           two.mile$hrs_82 < 40.5)
)

stargazer(stage1.2, stage2.2, 
          title = "2SLS RDD of HRS@28.5 threshold vs. 2000 Housing value (constrained 16.5 $<$ HRS $<$ 40.5",
          column.labels = c("First Stage", "Reduced Form"),
          out = "tab3a.tex",
          type = "latex", 
          style = "qje",
          no.space = TRUE, 
          font.size = "scriptsize", 
          single.row = TRUE,
          omit = 45:100) #omit all past first 45 vars.



# Function to make "local average" plots.
localAverageRD <- function(data, x.var, y.var, x.cutoff, binwidth){
  out <- data.frame(x = data[[x.var]])
  xmax <- max(data[[x.var]])
  xmin <- min(data[[x.var]])
  min.space <- x.cutoff - xmin
  max.space <- xmax - x.cutoff
  min.bins <- floor(min.space / binwidth)+1
  max.bins <- floor(max.space / binwidth)+1
  bin.breaks <- seq((x.cutoff-min.bins*binwidth),(x.cutoff+max.bins*binwidth), binwidth)
  bin.labels <- data.frame(begin = bin.breaks[1:length(bin.breaks)-1], end = bin.breaks[2:length(bin.breaks)])
  bin.labels$label <- seq(1, (min.bins + max.bins), 1)
  bin.labels$middle <- (bin.labels$begin + bin.labels$end) / 2
  for(i in 1:length(out$x)){
    binrow <- which(bin.labels$begin <= out$x[i] & bin.labels$end >= out$x[i])
    out$bin[i] <- bin.labels$middle[binrow]
  }
  out$y <- data[[y.var]]
  out.condensed <- ddply(out, .(bin), summarize, 
                         mean.y = mean(y))
  out.condensed$above.cutoff <- I(out.condensed$bin > x.cutoff)
  
  return(out.condensed)
}

pdf("fig-locavg-2000.pdf", width = 6, height = 5)
binwidth <- 2
test.locAvg <- localAverageRD(two.mile, "hrs_82", "lnmdvalhs0_nbr", 28.5, binwidth)
ggplot(test.locAvg, aes(bin, mean.y))+
  geom_point() +
  geom_vline(aes(xintercept = 28.5)) +
  stat_smooth(data = subset(test.locAvg, bin >= 16.5 & bin <= 40.5), method = "lm", aes(factor = as.factor(above.cutoff))) +
  theme_bw() +
  xlab("Middle of bin for HRS Score") + 
  ylab("Mean of the natural log median housing value (2000)") +
  ggtitle(paste("RD reduced form plot with bin width =",binwidth, "\n and linear fits on either side of the break point"))
dev.off()





## Part 3b ------
pdf("fig-3b.pdf", width = 6, height = 4)
HRSNPL.plot <- ggplot(data = two.mile, aes(x = hrs_82, y = npl2000))
HRSNPL.plot <- HRSNPL.plot +
    geom_jitter(aes(color=npl2000)) +
  geom_vline(aes(xintercept=28.5))
HRSNPL.plot
dev.off()

## Part 3c ------- Placebo test
pdf("fig-3c.pdf", width = 6, height = 4)
HRS80val.plot <- ggplot(data = two.mile, aes(x = hrs_82, y = lnmeanhs8_nbr))
HRS80val.plot <- HRS80val.plot +
    geom_point() +
  theme_bw()+
  stat_smooth(method = "lm", data = subset(two.mile, hrs_82 < 28.5 & hrs_82 > 16.5)) +
  stat_smooth(method = "lm", data = subset(two.mile, hrs_82 >= 28.5 & hrs_82 < 40.5), color = "red") +
    labs(title = "1980 housing value versus 1982 HRS score", x = "1982 HRS score", y = "ln 1980 Mean Housing Price")
HRS80val.plot
dev.off()


## Problem 4 ---

pdf("fig-4.pdf", width = 6, height = 4)
HRS0val.plot <- ggplot(data = two.mile, aes(x = hrs_82, y = lnmdvalhs0_nbr))
HRS0val.plot <- HRS0val.plot +
  geom_point() +
  theme_bw() +
  stat_smooth(method = "lm", data = subset(two.mile, hrs_82 < 28.5 & hrs_82 > 16.5)) +
  stat_smooth(method = "lm", data = subset(two.mile, hrs_82 >= 28.5 & hrs_82 < 40.5), color = "red") +
  labs(title = "2000 housing value versus 1982 HRS score", x = "1982 HRS score", y = "ln 2000 median housing value")
HRS0val.plot
dev.off()

# Attempt with canned function -------
k <- 1
i <- 20
formulaCAN.stageRF <- as.formula(paste("lnmdvalhs0_nbr ~ ", "hrs_82+ npl2000 | ", paste(two.mile.cov[k:i], collapse = "+")))
rd.subset <- two.mile$hrs_82 > 16.5 & two.mile$hrs_82 < 40.5
# doesn't seem to work with full set of covariates breaks after...why?
my.rdd <- RDestimate(formulaCAN.stageRF, data = two.mile, subset = rd.subset, cutpoint = 28.5, se.type = "HC1")
plot(my.rdd)