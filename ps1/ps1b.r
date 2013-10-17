# PROBLEM SET 1B
# ARE 213 Fall 2013
## TO DO:
# Figure out ATT in 2c
# Get a legend on the kerndensity plots to make the "beautiful and publication ready" DONE
# Figure the kernel regression by hand problem
# problem 5
# writing up a number of problems (I will get more done on this in the morning - need to go take a midterm).

# Frank's Directory
#setwd("/media/frank/Data/documents/school/berkeley/fall13/are213/are213/ps1")

# Peter's Directory
#setwd("~/Google Drive/ERG/Classes/ARE213/are213/ps1")


# Packages --------
library(foreign) #this is to read in Stata data
library(Hmisc)
library(psych)
library(stargazer)
library(ggplot2) # for neato plotting tools
library(plyr) # for nice data tools like ddply
library(epicalc) # For likelihood ratio test
library(car) # "companion for applied regression" - recode fxn, etc.
library(gmodels) #for Crosstabs
library(splines) # for series regression
library(np) #nonparametric regression
library(rms) #regression modeling tools
library(effects)
library(reshape)

# Homebrewed functions
source("../util/are213-func.R")
source("../util/watercolor.R") # for watercolor plots

# Data -------
ps1.data <- read.dta(file="ps1.dta")

var.labels <- attr(ps1.data, "var.labels")

# Data Cleaning Step
full.record.flag <- which(ps1.data$cardiac != 9 &
                            ps1.data$cardiac != 8 &
                            ps1.data$lung != 9 &
                            ps1.data$lung != 8 &
                            ps1.data$diabetes !=9 &
                            ps1.data$diabetes !=8 &
                            ps1.data$herpes != 9 &
                            ps1.data$herpes != 8 &
                            ps1.data$chyper != 9 &
                            ps1.data$chyper != 8 &
                            ps1.data$phyper != 9 &
                            ps1.data$phyper != 8 &
                            ps1.data$pre4000 !=9 &
                            ps1.data$pre4000 !=8 &
                            ps1.data$preterm != 9 &
                            ps1.data$preterm != 8 &
                            ps1.data$tobacco != 9 &
                            ps1.data$cigar != 99 &
                            ps1.data$cigar6 !=6 &
                            ps1.data$alcohol != 9 &
                            ps1.data$drink != 99 &
                            ps1.data$drink5 !=5 &
                            ps1.data$wgain !=99
)

ps1.data$full.record <- FALSE # initialize column as F
ps1.data$full.record[full.record.flag] <- TRUE #reassign level to T for full records

ps1.data.clean <- subset (ps1.data, full.record == TRUE)
ps1.data.missingvalues <- subset(ps1.data, full.record == FALSE)

# Problem 1a : Describes PS1a results. -------
# Problem 1b --------------
# This is using a series estimator. I think smooth.spline() is the right function to use, but let me know if you think we should be doing kernel regression instead. I'm also not sure how to go about adding interaction terms.  I think a kernel regression is more appropriate here...mostly because I don't know the spline function and there seems to be a good package ("np") for running kernel regression.  

# SPLINE FIT FOR # CIGS

# sm.flex <- with(ps1.data.clean, smooth.spline(cigar, y=dbrwt, nknots=10, spar = 0.7, tol = 0.0001)) # Fits a smooth line to the data
# sm.flex.df <- data.frame(sm.flex$x, sm.flex$y) #converts the fitted values into a data frame for ggplot
# 
# splineplot <- ggplot(sm.flex.df, aes(x = sm.flex.x, y=sm.flex.y))
# splineplot <- splineplot +
#   geom_point(data=ps1.data.clean, aes(x = cigar, y = dbrwt), pch = 1) +
#   geom_line(color='red') +
#   labs(x = 'Cigarettes smoked per day by mother', y= 'Birthweight')
# splineplot
# 
# ggsave(filename = 'img/splineplot.pdf')

# Using Series estimator with splines on maternal age.

ps1.data.clean$tobacco <- as.factor(ps1.data.clean$tobacco)
ps1.data.clean$dmar <- as.factor(ps1.data.clean$dmar)

wsp.ps1a <- lm(dbrwt ~ tobacco + dmage + dmar, data=ps1.data.clean)
wsp <- lm(dbrwt ~ tobacco + ns(dmage, df=3) + dmar, data=ps1.data.clean)
wsp.int <- lm(dbrwt ~ tobacco * ns(dmage, df=3) * dmar, data=ps1.data.clean)

stargazer(wsp.ps1a, wsp, wsp.int, out = 'splineresults.tex', style="qje", label = 'tab:splineresults', no.space = TRUE, dep.var.labels = "Birth Weight")



# This is the ATE with a splines regression on Age
summary(effect("tobacco", wsp))

# This is the ATE with a complex, interacting splines regression on AGe
summary(effect("tobacco", wsp.int))

# Problem 2a --------
ps1.data.clean$tobacco.rescale <- with(ps1.data.clean, recode(tobacco, "2='0'", as.numeric.result=TRUE)) #rescales the tobacco use variable to be 0/1, where 0=no and 1 = yes
ps1.data.clean$dmar.rescale <- with(ps1.data.clean, recode(dmar, "2='0'"))

smoke.propensity.all <- glm(tobacco.rescale ~ as.factor(mrace3) + dmeduc + dmar.rescale + dfage + dfeduc + as.factor(orfath) + dplural + csex + dmage, data=ps1.data.clean, family = binomial()) ## Did I miss any predetermined covariates here? No.

smoke.propensity.reduced <- glm(tobacco.rescale ~ as.factor(mrace3) + dmeduc + dmar.rescale + dfage + dfeduc + as.factor(orfath), data=ps1.data.clean, family = binomial())


stargazer(smoke.propensity.all, smoke.propensity.reduced,
           type = "latex",
           covariate.labels = c("Mother's Race not White or Black", "Mother's Years of Education", "Marital status", "Father's age", "Father's Years of Education", "Father Mexican", "Father Puerto Rican", "Father Cuban", "Father Central or South American", "Father Race Other or Unknown Hispanic", "Plurality of Infant", "Sex of Infant", "Mother's age"),
          style ="qje",
          align = TRUE,
          label = "tab:propensities",
          title = "Logistic function coefficients for propensity score models",
          dep.var.labels = "Mother Tobacco-Use Status",
          no.space = TRUE,
          out = "propensityscores.tex"
           )

ps1.data.clean$propensityfull <- predict(smoke.propensity.all, type = "response")
ps1.data.clean$propensityreduced <- predict(smoke.propensity.reduced, type = "response")

detach("package:rms")
sink(file = "lrtest.tex", append = FALSE)
lrtest(smoke.propensity.all, smoke.propensity.reduced) #Test whether the two scores are statistically different
sink()

require(rms)

#Problem 2b - Estimating a regression model using propensity scores --------

sm.propensityregression <- lm(dbrwt ~ propensityreduced * tobacco.rescale, ps1.data.clean)

#calculation of average treatment effect:
coefficients(sm.propensityregression)[2] + coefficients(sm.propensityregression)[4]*mean(ps1.data.clean$propensityreduced)

tobacco.effects <- (effect("tobacco.rescale", sm.propensityregression))

stargazer(sm.propensityregression,
          type = "latex",
          covariate.labels = c("Delta1", "Beta", "Delta2", "Constant"),
          style ="qje",
          align = TRUE,
          font.size="footnotesize",
          label = "tab:propensitymodel",
          title = "Model of effects of tobacco use on birthweight using propensity score as a control",
          dep.var.labels = "Mother Tobacco-Use Status",
          out = "propensityscoremodel.tex"
)

#Problem 2c - Using reweighting with propensity scores --------

ps1.data.clean$tobacco.rescale.n <- as.numeric(levels(ps1.data.clean$tobacco.rescale))[ps1.data.clean$tobacco.rescale]

ps1.data.clean$weightingterm1 <- with(ps1.data.clean, ((tobacco.rescale.n*dbrwt)/propensityreduced))
ps1.data.clean$weightingterm2 <- with(ps1.data.clean, (((1-tobacco.rescale.n)*dbrwt)/(1-propensityreduced)))

weightingestimator <- with(ps1.data.clean, sum((weightingterm1/sum(tobacco.rescale.n/propensityreduced))-(weightingterm2/sum((1-tobacco.rescale.n)/(1-propensityreduced))))) #This should be the average treatment effect

ATTweight <- with(ps1.data.clean, sum(((propensityreduced*weightingterm1)/sum(propensityreduced))-(((1-propensityreduced)*weightingterm2)/sum(1-propensityreduced))))

# Problem 2d - Kernel Density Estimator
tot.propensity.nosm <- with(subset(ps1.data.clean, tobacco.rescale == 0), sum(propensityreduced))
tot.propensity.sm <- with(subset(ps1.data.clean, tobacco.rescale == 1), sum(propensityreduced))

kd.plot.fn <- function(h, plot.w = 5, plot.h = 3){
kerndensity.nosm <- with(subset(ps1.data.clean, tobacco.rescale == 0), 
                         density(dbrwt, #if nobody smoked
                                 kernel = "epanechnikov",
                                 bw = h,
                                 weights = propensityreduced/tot.propensity.nosm))
kerndensity.nosm.df <- data.frame(birth.weight = kerndensity.nosm$x, non.smoke = kerndensity.nosm$y)

kerndensity.sm <- with(subset(ps1.data.clean, tobacco.rescale == 1), 
                       density(dbrwt, #if everybody smoked
                               kernel = "epanechnikov",
                               bw = h,
                              weights = propensityreduced/tot.propensity.sm))
kerndensity.sm.df <- data.frame(birth.weight = kerndensity.sm$x, smoke = kerndensity.sm$y)

kdens <- join(kerndensity.sm.df, kerndensity.nosm.df, by="birth.weight", type = "full")

kdens.m <- melt.data.frame(kdens, id.vars="birth.weight", measure.vars = c("smoke", "non.smoke"), na.rm = TRUE)

kd.plot <- ggplot(kdens.m, aes(birth.weight, value, factor(variable)))
kd.plot <- kd.plot +
  geom_line(aes(color=variable)) + 
  labs(title = paste("Density of birthweights estimated using \n propensity score-weighted kernel regression \n Bandwidth=", as.factor(h)), x = "Birthweight (grams)", y = "Density") +
  guides(color = guide_legend(title = "Treatment")) +
  theme_bw() 


kd.plot

ggsave(width = plot.w, height = plot.h, file = paste0('img/kerndensity', h,'.pdf'), plot = kd.plot)

}




##Problem 2d - calculating kernel value by 'hand' at dbrwt = 3000 --------
##I can't figure out what to do here. Most of this is probably wrong but maybe something is right. Want to take a whack?


h <- 30

# added identity function to kernel (cuts off outside bw)
kernel.epa <- function(u){
  if(abs(u)<1){return(0.75*(1-u*u))
  }else{return(0)}
}

propensity3000.sm <- with(ps1.data.clean, mean(propensityreduced[which(dbrwt == 3000 & tobacco.rescale == 1)]))
propensity3000.nosm <- with(ps1.data.clean, mean(propensityreduced[which(dbrwt == 3000 & tobacco.rescale == 0)]))
##for(i in 1:nrow(subset(ps1.data.clean, tobacco.rescale == 1))){
##with(subset(ps1.data.clean, tobacco.rescale == 1),
##     kern3000.sm.num <- kern3000.sm.num +
##      kernel.epa(((propensity3000.sm-propensityreduced[i])/h)*dbrwt))
## with(subset(ps1.data.clean, tobacco.rescale == 1),
##      kern3000.sm.den <- kern3000.sm.den +
##      kernel.epa((propensity3000.sm-propensityreduced[i])/h))
## }
## kern3000.sm <- kern3000.sm.num / kern3000.sm.den


## kernel3000.sm <- with(subset(ps1.data.clean,tobacco.rescale == 1), data.frame(window = (3000 - dbrwt/h)))
## kernel3000.sm$numerator <- with(subset(ps1.data.clean, tobacco.rescale == 1), kernel.epa(((3000/propensity3000.sm) - (dbrwt/propensityreduced))/h))
## kernel3000.sm$denominator <- with(subset(ps1.data.clean, tobacco.rescale == 1), kernel.epa(((3000/propensity3000.sm) - (dbrwt/propensityreduced))/h))

## with(kernel3000.sm[window < 1 & window > -1], sum(numerator))/(nrow(kernel3000.sm[abs(window < 1)])*h)

#Problem 2e --------

for(bw in seq(10,100,10)){kd.plot.fn(bw)}

kd.plot.fn(40,7,4)



### Problem 3
## Using blocking estimator
# Divide smokers into ~100 equally spaced blocks
prop.max <- with(ps1.data.clean, max(propensityreduced))
prop.min <- with(ps1.data.clean, min(propensityreduced))
prop.binsize <- (prop.max - prop.min)/99

ps1.data.clean$blocknumber <- with(ps1.data.clean,
                                   round(propensityreduced/prop.binsize, digits = 0) + 1)

blocktreatmenteffects <- ddply(ps1.data.clean, .(blocknumber), summarize, smokers = sum(tobacco.rescale == 1), nonsmokers = sum(tobacco.rescale == 0), smokerdbrwt = mean(dbrwt[tobacco.rescale == 1]), nonsmokerdbrwt = mean(dbrwt[tobacco.rescale == 0]))

blocktreatmenteffects$badbin <- with(blocktreatmenteffects, as.numeric(smokers == 0 | nonsmokers == 0))

cleaned.blocks <- subset(blocktreatmenteffects, badbin == 0)
cleaned.blocks$avgtreatmenteffect <- with(cleaned.blocks, smokerdbrwt - nonsmokerdbrwt)
cleaned.blocks$weight <- with(cleaned.blocks, (smokers + nonsmokers)/sum(smokers + nonsmokers))
cleaned.blocks$weightedTE <- with(cleaned.blocks, weight * avgtreatmenteffect)

blocks.plot <- ggplot(data=ps1.data.clean, aes(x=blocknumber, fill = tobacco.rescale))
blocks.plot <- blocks.plot +
    geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
    labs(title = 'Size of propensity score blocks', x = 'Ordered block number', y = 'Frequency') +
    scale_fill_discrete(name = "Mother's \n Smoking Status",
                        breaks = c("0", "1"),
                        labels = c("Non-Smoker", "Smoker"))
ggsave(filename = 'img/blockplot.pdf', plot=blocks.plot)

blocksATE <- sum(cleaned.blocks$weightedTE)

### Problem 4
ps1.data.clean$lowbrwt <- as.numeric(ps1.data.clean$dbrwt < 2500)

blocklowbrwt <- ddply(ps1.data.clean, .(blocknumber), summarize, smokers = sum(tobacco.rescale == 1), nonsmokers = sum(tobacco.rescale == 0), lowbrwtprob.sm = mean(lowbrwt[tobacco.rescale == 1]), lowbrwtprob.nosm = mean(lowbrwt[tobacco.rescale == 0]))

blocklowbrwt$badbin <- with(blocklowbrwt, as.numeric(smokers == 0 | nonsmokers == 0))

cleaned.blocks.lowbrwt <- subset(blocklowbrwt, badbin == 0)
cleaned.blocks.lowbrwt$ATE <- with(cleaned.blocks.lowbrwt, lowbrwtprob.sm - lowbrwtprob.nosm)
cleaned.blocks.lowbrwt$weight <- with(cleaned.blocks.lowbrwt, (smokers + nonsmokers)/sum(smokers + nonsmokers))
cleaned.blocks.lowbrwt$weightedTE <- with(cleaned.blocks.lowbrwt, weight * ATE)

blocks.lowbrwt.ATE <- sum(cleaned.blocks.lowbrwt$weightedTE)



### Output values that need to be typed in to TeX:
print(paste("The estimated average treatment effect using the reweighting approach is", round(weightingestimator, digits=0)))
print(paste("The estimated average treatment on the treated using the reweighting approach is", round(ATTweight, digits = 0)))
print(paste("ATE is", round(tobacco.effects$fit[1] - tobacco.effects$fit[2], digits=0), "based on regression adjustment with p-score."))
print(paste("The Average Treatment Effect predicted by the blocking method with birthweight treated as a continuous variable is", round(blocksATE, digits=0)))
print(paste("The Average Treatment Effect predicted by the blocking method of birthweights falling into the 'low' category of less than 2500 grams is a probability of", round(blocks.lowbrwt.ATE, digits = 4),". That is, smokers are approximately", round(100*blocks.lowbrwt.ATE, digits = 0), "percent more likely to have babies with weights less than 2500 grams."))

