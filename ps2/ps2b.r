## Frank's wd
## setwd("/media/frank/Data/documents/school/berkeley/fall13/are213/are213/ps2")
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
library(Synth) #for synthetic control

source("../util/are213-func.R")

ps2a.data <- read.dta('traffic_safety2.dta')
ps2a.datakey <- data.frame(var.name=names(ps2a.data), var.labels = attr(ps2a.data, "var.labels"))
ps2a.data$logfatalpc <- with(ps2a.data, log(fatalities/population))
ps2a.data$sqyears <- with(ps2a.data, year^2)

## Problem 1

## Part a
## Subpart i
                                        # Average pre-period log fatalities per cap.
pre.treatment.avg <- with(subset(ps2a.data, (state == 99 & primary == 0)), mean(logfatalpc))
pre.control.avg <- with(subset(ps2a.data, (state != 99 & primary == 0)), mean(logfatalpc))
                                        # It seems like we should maybe be excluding CT, IA, NM, and TX from the control group (if they aren't already)
pre.period.data <- ddply(ps2a.data, .(year, treatment = as.integer(state == 99)), summarize,
                         logfatalpc = mean(logfatalpc))

                                        # Plot of average fatality rates pre-1986. This could be made prettier for sure
preperiod.plot <- ggplot(pre.period.data, aes(x = year, y = logfatalpc))
preperiod.plot <- preperiod.plot +
    geom_line(data = subset(pre.period.data, year < 1986 & treatment == 0), linetype = "dashed") +
    geom_line(data = subset(pre.period.data, year < 1986 & treatment == 1), linetype = "solid")
preperiod.plot

## Subpart ii
                                        # Compare logfatalpc in year before treatment
treatment.fatalities.85 <- with(ps2a.data, logfatalpc[which(state == 99 & year == 1985)])
## control.fatalities.85 <- subset(ps2a.data, year == 1985 & 



## Part B - Synthetic control method

## subpart ii

syntheticdata <- dataprep(ps2a.data,
	      predictors = c("college", "beer", "secondary", "unemploy", "totalvmt", "precip", "snow32", "rural_speed", "urban_speed", "sqyears"),
	      treatment.identifier = primary, 
	      	 dependent = logfatalpc, unit.variable = state, time.variable = year) #This is to get the data intothe proper format for the synth package

seatbelts.synth <- synth(data.prep.obj = syntheticdata)