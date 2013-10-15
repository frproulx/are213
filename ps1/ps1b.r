# PROBLEM SET 1B
# ARE 213 Fall 2013


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

sm.flex <- with(ps1.data.clean, smooth.spline(cigar, y=dbrwt, nknots=10, spar = 0.7, tol = 0.0001)) # Fits a smooth line to the data
sm.flex.df <- data.frame(sm.flex$x, sm.flex$y) #converts the fitted values into a data frame for ggplot

splineplot <- ggplot(sm.flex.df, aes(x = sm.flex.x, y=sm.flex.y))
splineplot <- splineplot +
  geom_point(data=ps1.data.clean, aes(x = cigar, y = dbrwt), pch = 1) +
  geom_line(color='red') +
  labs(x = 'Cigarettes smoked per day by mother', y= 'Birthweight')
splineplot

ggsave(filename = 'img/splineplot.pdf')

# Using Series estimator with splines on maternal age.

ps1.data.clean$tobacco <- as.factor(ps1.data.clean$tobacco)
ps1.data.clean$dmar <- as.factor(ps1.data.clean$dmar)

wsp.ps1a <- lm(dbrwt ~ tobacco + dmage + dmar, data=ps1.data.clean)
wsp <- lm(dbrwt ~ tobacco + ns(dmage, df=3) + dmar, data=ps1.data.clean)
wsp.int <- lm(dbrwt ~ tobacco * ns(dmage, df=3) * dmar, data=ps1.data.clean)

stargazer(wsp.ps1a, wsp, wsp.int, type="text")

# This is the ATE with a splines regression on Age
summary(effect("tobacco", wsp))

# This is the ATE with a complex, interacting splines regression on AGe
summary(effect("tobacco", wsp.int))

# Problem 2a --------
ps1.data.clean$tobacco.rescale <- with(ps1.data.clean, recode(tobacco, "2='0'", as.numeric.result=TRUE)) #rescales the tobacco use variable to be 0/1, where 0=no and 1 = yes
ps1.data.clean$dmar.rescale <- with(ps1.data.clean, recode(dmar, "2='0'"))

smoke.propensity.all <- glm(tobacco.rescale ~ as.factor(mrace3) + dmeduc + dmar.rescale + dfage + dfeduc + as.factor(orfath) + dplural + csex + dmage, data=ps1.data.clean, family = binomial()) ## Did I miss any predetermined covariates here?

smoke.propensity.reduced <- glm(tobacco.rescale ~ as.factor(mrace3) + dmeduc + dmar.rescale + dfage + dfeduc + as.factor(orfath), data=ps1.data.clean, family = binomial())


### THIS TABLE IS PROBLEMATIC AND I'M NOT SURE WHY
#stargazer(smoke.propensity.all, smoke.propensity.reduced,
#           type = "latex",
#           covariate.labels = c("Mother's Race not White or Black", "Mother's Years of Education", "Marital status", "Father's age", "Father's Years of Education", "Father Mexican", "Father Puerto Rican", "Father Cuban", "Father Central or South American", "Father Race Other or Unknown Hispanic", "Plurality of Infant", "Sex of Infant", "Mother's age"),
#           style ="qje",
#           align = TRUE,
#          label = "tab:propensities",
#          title = "Propensity scores calculated for mother's smoking status \n
#logit model specification",
#           dep.var.labels = "Mother Tobacco-Use Status",
#           out = "propensityscores.tex"
#           )

ps1.data.clean$propensityfull <- predict(smoke.propensity.all, type = "response")
ps1.data.clean$propensityreduced <- predict(smoke.propensity.reduced, type = "response")

sink(file = "lrtest.tex", append = FALSE)
lrtest(smoke.propensity.all, smoke.propensity.reduced) #Test whether the two scores are statistically different
sink()
print("Works through 2a")

#Problem 2b - Estimating a regression model using propensity scores --------

sm.propensityregression <- lm(dbrwt ~ tobacco.rescale + (propensityreduced * tobacco.rescale) + propensityreduced, ps1.data.clean)

#calculation of average treatment effect:
coefficients(sm.propensityregression)[2] + coefficients(sm.propensityregression)[4]*mean(ps1.data.clean$propensityreduced)


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

term1 <- with(ps1.data.clean, sum((tobacco.rescale*dbrwt)/propensityreduced)/sum(tobacco.rescale/propensityreduced))
term2 <- with(ps1.data.clean, sum(((1-tobacco.rescale)*dbrwt)/(1-propensityreduced))/sum((1-tobacco.rescale)/(1-propensityreduced)))

weightingestimator <- term1-term2 #This should be the average treatment effect

term1.T <- with(subset(ps1.data.clean, tobacco.rescale=1), sum((tobacco.rescale*dbrwt)/propensityreduced)/sum(tobacco.rescale/propensityreduced))
#term2.T <- with(subset(ps1.data.clean, tobacco.rescale=1), sum(((1-tobacco.rescale)*dbrwt)/(1-propensityreduced))/sum((1-tobacco.rescale)/(1-propensityreduced)))

weightingestimator.T <- term1.T#-term2.T #This should be the average treatment on treated



# Problem 2d - Kernel Density Estimator
tot.propensity.nosm <- with(subset(ps1.data.clean, tobacco.rescale == 0), sum(propensityreduced))
tot.propensity.sm <- with(subset(ps1.data.clean, tobacco.rescale == 1), sum(propensityreduced))
h <- 35 # This is to play with the bandwidth

kerndensity.nosm <- with(subset(ps1.data.clean, tobacco.rescale == 0), density(dbrwt, #if nobody smoked
                                                                               kernel = "epanechnikov",
                                                                               bw = h,
                                                                               weights = propensityreduced/tot.propensity.nosm))
kerndensity.nosm.df <- data.frame(kerndensity.nosm[1], kerndensity.nosm[2])

kerndensity.sm <- with(subset(ps1.data.clean, tobacco.rescale == 1), density(dbrwt, #if everybody smoked
                                                                             kernel = "epanechnikov",
                                                                             bw = h,
                                                                             weights = propensityreduced/tot.propensity.sm))
kerndensity.sm.df <- data.frame(kerndensity.sm[1], kerndensity.sm[2])

kerndensity.plot <- ggplot(kerndensity.nosm.df, aes(x, y))
kerndensity.plot <- kerndensity.plot +
  geom_line(linetype = 'dotted') + 
  geom_line(data = kerndensity.sm.df, aes(x, y)) +
  labs(title = paste("Density of birthweights estimated using \n propensity score-weighted kernel regression \n Bandwidth=", as.factor(h)), x = "Birthweight (grams)", y = "Density") + 
  guides(linetype = "Legend") # Having trouble getting a legend.

kerndensity.plot

ggsave(file = 'img/kerndensity.pdf', plot = kerndensity.plot)

#Problem 2d - calculating kernel value by 'hand' at dbrwt = 3000 --------
# y_pred = sum_onj(K(pi-pj/h)*yj)/sum_onj(K(pi-pj/h))

#h <- 30
#kernel.epa <- function(u){
#return(if (abs(u) < 1){0.75*(1-u*u))}
#}
#propensity3000 <- # I'm not sure how we get the estimated propensity score at dbrwt = 3000


#Problem 2e --------
## This is in progress
#print("Works as far as through 2d")
#kerndensity.plot.bws <- ggplot() # Kernel density plot at varied bandwidths
# = 1
#for(h in seq(from = 15, to = 40, by = 5)) {
#  kerndensity.by.bw[i] <- list(h, with(subset(ps1.data.clean, tobacco.rescale == 1), density(dbrwt, #if everybody smoked
#        kernel = "epanechnikov",
#        bw = h,
#        weights = propensityreduced/tot.propensity.sm)))
#paste0('kerndensity.sm.df.', h) <- data.frame(paste0('kerndensity.sm.', h,'[1]'), paste0('kerndensity.sm.', h, '[2]'))
#kerndensity.plot.bws <- kerndensity.plot.bws +
#  geom_line(data = paste0('kerndensity.sm.df.', h), aes(x, y)) +
#  labs(title = "Density of birthweights estimated using \n propensity score-weighted kernel regression", x = "Birthweight (grams)", y = "Density")
#i = i + 1
#}

#kerndensity.plot.bws



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

blocksATE <- sum(cleaned.blocks$weightedTE)
print(paste("The Average Treatment Effect predicted by the blocking method is", blocksATE))
