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

state.labels <- attr(ps2a.data, "label.table")


## Problem 1 --------------------

## Part a -------------
## Subpart i ------------
                                        # Average pre-period log fatalities per cap.
pre.treatment.avg <- with(subset(ps2a.data, (state == 99 & primary == 0)), mean(logfatalpc))
pre.control.avg <- with(subset(ps2a.data, (state != 99 & primary == 0)), mean(logfatalpc))

                                        # Define state types
ps2a.data$type <- "Control"

for(i in 1:length(ps2a.data$year)){
  if(ps2a.data$state[i] == 99){
    ps2a.data$type[i] <- "Composite Treatment"}
    
  if(ps2a.data$state[i] %in% c(4,10,30,41)){
    ps2a.data$type[i] <- "Treatment State"}
}

use.rows <- which(ps2a.data$type != "Treatment State")

pre.period.data <- ddply(ps2a.data[use.rows,], .(year, type), summarize,
                         logfatalpc = mean(logfatalpc))

# Plot of average fatality rates pre-1986.
preperiod.plot <- ggplot(data = subset(pre.period.data, year < 1986), aes(x = year, y = logfatalpc, color = type))
preperiod.plot <- preperiod.plot +
                           geom_line() + 
                           theme_bw() +
                           ylab("log(fatalities per capita)")

pdf(file="img-p2b-1.pdf", width = 6, height = 4)
preperiod.plot
dev.off()

## Subpart ii
# Compare logfatalpc in year before treatment

year.before <- ddply(subset(ps2a.data, year == 1985 & type != "Treatment State"), .(state), summarize, 
                     type = type, 
                     logfatalpc = logfatalpc)

target.fatal <- year.before$logfatalpc[which(year.before$type== "Composite Treatment")]
year.before$distance <- abs(year.before$logfatalpc - target.fatal)
year.before$distance[which(year.before$type== "Composite Treatment")] <- NA

best.yb.match <- which(year.before$distance == min(year.before$distance, na.rm=T))

print(paste("The state number for the closest year before match is", year.before$state[best.yb.match]))

# The best match is Alabama.  

# Tables comparing Alabama to the composite treatment group
stargazer(subset(ps2a.data, state==99),
          out = "tab-ps2b-1a.tex",
          title = "Composite Treatment Group Summary")

stargazer(subset(ps2a.data, state==1),
          out = "tab-ps2b-1b.tex",
          title = "Closest match for pre-policy fatalities: Alabama")



## Part B - Synthetic control method

## subpart ii

# Not working yet...

syntheticdata.1 <- dataprep(ps2a.data,
	      predictors = c("college", "beer", "secondary", "unemploy", "totalvmt", "precip", "snow32", "rural_speed", "urban_speed", "sqyears"),
	      treatment.identifier = "primary", 
	      dependent = "logfatalpc", 
        unit.variable = "state", 
        time.variable = "year") 

seatbelts.synth <- synth(data.prep.obj = syntheticdata)