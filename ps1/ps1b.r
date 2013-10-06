setwd("/media/frank/Data/documents/school/berkeley/fall13/are213/are213/ps1")

# Packages
library(foreign) #this is to read in Stata data
library(Hmisc)
library(psych)
library(stargazer)
library(ggplot2) # for neato plotting tools
library(plyr) # for nice data tools like ddply
library(car) # "companion for applied regression" - recode fxn, etc.
library(gmodels) #for Crosstabs
library(splines) # for series regression

# Homebrewed functions
source("../util/are213-func.R")
source("../util/watercolor.R") # for watercolor plots

# Data
ps1.data <- read.dta(file="ps1.dta")

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

# Problem 1a is only in ps1b.tex
# Problem 1b
# This is using a series estimator. I think smooth.spline() is the right function to use, but let me know if you think we should be doing kernel regression instead.


sm.flex <- with(ps1.data.clean, smooth.spline(cigar, y=dbrwt, nknots=10, spar = 0.7, tol = 0.0001)) # Fits a smooth line to the data
sm.flex.df <- data.frame(sm.flex$x, sm.flex$y) #converts the fitted values into a data frame for ggplot

splineplot <- ggplot(sm.flex.df, aes(x = sm.flex.x, y=sm.flex.y))
splineplot <- splineplot +
  geom_point(data=ps1.data.clean, aes(x = cigar, y = dbrwt), pch = 1) +
  geom_line(color='red') +
  labs(x = 'Cigarettes smoked per day by mother', y= 'Birthweight')
splineplot

ggsave(filename = 'img/splineplot.pdf')

