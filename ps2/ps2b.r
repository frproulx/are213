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
library(reshape)

source("../util/are213-func.R")

ps2a.data <- read.dta('traffic_safety2.dta')
ps2a.datakey <- data.frame(var.name=names(ps2a.data), var.labels = attr(ps2a.data, "var.labels"))
ps2a.data$logfatalpc <- with(ps2a.data, log(fatalities/population))
ps2a.data$sqyears <- with(ps2a.data, year^2)

# make state labels dataframe
state.labels <- attr(ps2a.data, "label.table")
state.labels$state.name <- attr(state.labels$state_number,"names")
state.labels <- as.data.frame(state.labels)
state.labels$state <- state.labels$state_number
state.labels$state.name <- as.character(state.labels$state.name)
state.labels$state_number <- NULL
state.labels <- rbind(state.labels, c("TU", 99))



tu.label <- data.frame(state_number = 99, state.name = "TU")


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

pdf(file="img-p2b-logfatTrend.pdf", width = 6, height = 4)
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


                                        # plot all states and TU
pdf("img-p2b-compareStates.pdf", width = 4, height = 4)

ggplot(year.before, aes(logfatalpc)) + 
    geom_histogram(binwidth=0.1) +
    geom_histogram(data=subset(year.before, state==1), aes(logfatalpc), binwidth=0.1, fill="red") +
    theme_bw() +
    xlab("log(fatalities per capita)") +
    geom_vline(aes(xintercept=target.fatal), size = 1, color = "blue") 

dev.off()
  

best.yb.match <- which(year.before$distance == min(year.before$distance, na.rm=T))

print(paste("The state number for the closest year before match is", year.before$state[best.yb.match]))

                                        # The best match is Alabama.  

                                        # Tables comparing Alabama to the composite treatment group
stargazer(subset(ps2a.data, state==99),
          out = "tab-ps2b-1a.tex",
          title = "Composite Treatment Group Summary",
          label = "tab:a21")

stargazer(subset(ps2a.data, state==1),
          out = "tab-ps2b-1b.tex",
          title = "Closest match for pre-policy fatalities: Alabama",
          label = "tab:a22")


# graphical comparison between states

prep.gg.compare <- melt.data.frame(subset(ps2a.data, state==99 | state ==1), id.vars = c("state","year"))

prep.gg.compare$value <- as.numeric(prep.gg.compare$value)
prep.gg.compare$state <- as.factor(prep.gg.compare$state)

vars.we.care.about <- c("beer", "college", "primary", "secondary", "unemploy", "logfatalpc", "rural_speed", "urban_speed", "precip")
rows.we.care.about <- which(prep.gg.compare$variable %in% vars.we.care.about)

gg.compare.states <- ggplot(prep.gg.compare[rows.we.care.about,], aes(x=year, y=value, color=state)) + 
    geom_point() + 
    facet_wrap("variable", scales = "free")

pdf("img-ps2b-compareStatesFacets.pdf", width=7, height = 6)
gg.compare.states
dev.off()

## Part B - Synthetic control method


                                        # Identify which states can be "control" (i.e., those that never have a primary seatbelt law)

state.policy <- ddply(ps2a.data, .(state), summarize, primary.max = max(primary))
good.states <- which(state.policy$primary.max < 1)
control.states <- state.policy$state[good.states]
all.states <- c(control.states, 99)

## subpart ii


# add to dataset
ps2a.data$vmt_percapita <- with(ps2a.data, totalvmt / population)
ps2a.data <- join(ps2a.data, state.labels)

#helper function: extract useful data from synth results
get.plot.data <- function(synth.res, dataprep.res, label="unlabeled", type = "unknown"){
    out <- data.frame(time=dataprep.res$tag$time.plot)
  
    synthetic.trend <- dataprep.res$Y0plot %*% synth.res$solution.w
    treatment.trend <- dataprep.res$Y1plot
    gap <- treatment.trend - synthetic.trend
  
    out$synth <- as.numeric(synthetic.trend)
    out$treat <- as.numeric(treatment.trend)
    out$gap <- as.numeric(gap)
    out$spe <- out$gap^2
    out$label <- label
    out$type <- type
  
  
    return(out)
}

#helper function: run dataprep, synth, and get.plot.data for a set of inputs.
run.syn <- function(predictor.set, time.prior, treatment.state = std.treatment.state, control.states, label = "unlabeled", type = "unknown"){

# synthetic controls dataprep with specified set of tractable predictors
    dataprep.results <- dataprep(foo = ps2a.data,
                                 predictors = predictor.set,
                                 predictors.op = c("mean"),
                                 dependent = "logfatalpc",
                                 unit.variable = "state",
                                 time.variable = "year",
                                 treatment.identifier = treatment.state,
                                 controls.identifier = control.states,
                                 time.predictors.prior = time.prior,
                                 time.optimize.ssr = time.prior,
                                 time.plot = c(1981:2003),
                                 unit.names.variable = "state.name"
                                 ) 

    synth.results <- synth(data.prep.obj = dataprep.results)
    
    output <- get.plot.data(synth.results, dataprep.results, label, type)

    return(output)
}

## helper function: makes a gap plot so we can get them to look consistent 
make.gap.plot <- function(syntheticresults, finame){
    gap.plot <- ggplot(data = syntheticresults, aes(y = gap, x = time))
    gap.plot <- gap.plot +
        geom_path() +
            geom_path(aes(y = synth), alpha = 0.8) +
                geom_path(aes(y = treat), alpha = 0.6) +
                    geom_vline(xintercept = 1986, linetype = 'dotted')
    ggsave(filename = paste0('img-gap-', finame, '.pdf'), plot = gap.plot)
    return(gap.plot)
}


                                        # default entries to run.syn function
std.treatment.state <- 99
full.predictor.set <- c("college", "precip", "snow32", "beer", "vmt_percapita", "unemploy")
full.time.prior <- c(1981:1985)

## Part C: Pretty pictures
## Part (i)
full.synthesis <- run.syn(full.predictor.set, full.time.prior, 99, control.states)
full.gap.plot <- make.gap.plot(full.synthesis, 'full')

full.synthesis1984 <- run.syn(full.predictor.set, c(1981:1984), 99, control.states)
full.1984.gap.plot <- make.gap.plot(full.synthesis1984, 'full1984')

vmt.synthesis <- run.syn("vmt_percapita", full.time.prior, 99, control.states)
vmt.gap.plot <- make.gap.plot(vmt.synthesis, 'vmt')

# Script to generate a placebo test plot
## Part (ii)

placebo.test <- run.syn(full.predictor.set, full.time.prior, treatment.state = 99, control.states, label = 99, type = "treatment")

for(state in control.states){
    updated.control <- control.states[which(control.states!=state)]
    placebo.additional.result <- run.syn(full.predictor.set, full.time.prior, state, updated.control, label = state, type = "placebo")
    placebo.test <- rbind(placebo.test, placebo.additional.result)
    print(paste("Finished with state number", state, "and you should be patient for the rest to finish :)"))
}

# plot all the lines
ggplot(placebo.test, aes(time, gap, group=label))+geom_line(aes(color=type))+geom_vline(aes(xintercept = 1985))

placebo.test$treated <- (placebo.test$time > 1985)
## Part(iii)

MPSEs <- ddply(placebo.test, .(label), summarize,
               preMPSE = mean(spe[!treated]),
               postMPSE = mean(spe[treated]))
MPSEs$ratio <- with(MPSEs, postMPSE/preMPSE)

MPSE.plot <- ggplot(MPSEs, aes(x=ratio))
MPSE.plot <-  MPSE.plot +
    geom_histogram()

# # Visual exploration of plots....only works if you have the native dataprep and synth objects.  We would rather make our own plots :)
# gaps.plot(seatbelts.synth, syn.data.full)
# path.plot(seatbelts.synth, syn.data.full, Ylim = c(-1.3, -2))

# # Tables for synthetic controls results
# syn.table <- synth.tab(seatbelts.synth, syn.data.full,3)



