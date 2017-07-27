
source("~/Documents/ResPECT-public/ALERT-simulations-fxns.R")

library("dplyr")
library("lubridate")


dates <- read.csv("~/Desktop/applied-ALERT-data/chco-trigger-dates.csv", stringsAsFactors = F)

dates$startDate <- ymd(dates$startDate)

dates$endDate <- ymd(dates$endDate)

##alternatively, use the following.
#real data
##load the data
fluA <- read.csv("~/Desktop/applied-ALERT-data/CHCO-fluA.csv", 
                 stringsAsFactors=F)
fluA$Date <- ymd(fluA$Date)
fluA <- arrange(fluA, Date)

dates <- ymd(fluA$Date) ##need this later for the simulations

fluB <- read.csv("~/Desktop/applied-ALERT-data/CHCO-fluB.csv", 
                 stringsAsFactors=F)
fluB$Date <- ymd(fluB$Date)
fluB <- arrange(fluB, Date)

RSV <- read.csv("~/Desktop/applied-ALERT-data/CHCO-RSV.csv", 
                stringsAsFactors=F)
RSV$Date <- ymd(RSV$Date)
RSV <- arrange(RSV, Date)

Cases <- fluA$Cases + fluB$Cases + RSV$Cases
Date <- fluA$Date

data <- data.frame(Date, Cases)
all <- data



#bugnames <- colnames(all[2:4])

#all <- all[1:4]

#all$date <- mdy(all$Date..Month.Year.)

all <- arrange(all, by=Date)

all$state <- 0

#specify the model for endemic and autoregressive components
f.end <- surveillance::addSeason2formula(f = ~ 1+t, S=1, period=52)
model1 <- list(ar = list(f = ~ 1), end = list(f =f.end),
               family = "NegBinM", subset=2:469)

DisProg.dat <- surveillance::create.disProg(week = 1:nrow(all), 
                             observed = as.matrix(all[2]),
                             state = all$state, start = c(2001, 35))# convert to sts class
chco.dat <- surveillance::disProg2sts(DisProg.dat)# convert to sts class

#  # run model
chco.dat_res <- surveillance::hhh4(chco.dat, model1)
#  j <- as.integer(i-1)
holder <- chco.dat_res$coefficients


all <- data.frame(data.table::rbindlist(lapply(holder,as.list)))

colnames(all[1]) <- "coefficients derived from CHCO dataset"

coefnames <- names(holder)
#rownames(all) <- bugnames

print(xtable(all, type="html", digits=4))

upper <- all[1]+(2*chco.dat_res$se)
lower <- all[1]-(2*chco.dat_res$se)

#upper <- rep(1.75, nrow(all))
#lower <- rep(-3.0, nrow(all))

limits <- data.frame(lower, upper)

#seq(from=limits[1,1], to=limits[1,2], length.out=50)

holder <- list()

for (i in 1:nrow(limits)){
  holder[[i]] <- seq(from=limits[i,1], to=limits[i,2], length.out=100
                     )
}

ranges <- t(data.frame(data.table::rbindlist(lapply(holder,as.list))))

names(holder) <- coefnames

ranges <- data.frame(ranges)

#holder <- list()

#names(holder) <- coefnames

coefs <- chco.dat_res$coefficients
colnames(ranges) <- coefnames

#######################################
########   GET PARAMS READY!!  ########
#######################################


###just change one param at a time.

## first end.1
alpha <- ranges
alpha$ar.1 <- coefs[1]  ## not going to mess with the AR param lambda
alpha$end.t <- coefs[3]  ###or the slope beta
alpha[4] <- coefs[4] ##or this seasonal term gamma
alpha[5] <- coefs[5] ##delta 
alpha[6] <- coefs[6] ##psi
alpha$param <- "alpha"

## overdisp
psi <- ranges
psi$ar.1 <- coefs[1]  ## not going to mess with the AR param lambda
psi$end.1 <- coefs[2]  ## alpha
psi$end.t <- coefs[3]  ###or the slope beta
psi[4] <- coefs[4] ##or this seasonal term gamma
psi[5] <- coefs[5] ##delta 
psi$param <- "psi"

## the delta cos term
delta <- ranges
delta$ar.1 <- coefs[1]  ## not going to mess with the AR param lambda
delta$end.1 <- coefs[2]  ## alpha
delta$end.t <- coefs[3]  ###or the slope beta
delta[4] <- coefs[4] ##or this seasonal term gamma
delta[6] <- coefs[6] ##psi
delta$param <- "delta"

##lambda
lambda <- ranges
lambda$end.1 <- coefs[2]  ## alpha
lambda$end.t <- coefs[3]  ###or the slope beta
lambda[4] <- coefs[4] ##or this seasonal term gamma
lambda[5] <- coefs[5] ##delta 
lambda[6] <- coefs[6] ##psi
lambda$param <- "lambda"

##beta
beta <- ranges
beta$ar.1 <- coefs[1]  ## not going to mess with the AR param lambda
beta$end.1 <- coefs[2]  ## alpha
beta[4] <- coefs[4] ##or this seasonal term gamma
beta[5] <- coefs[5] ##delta 
beta[6] <- coefs[6] ##psi
beta$param <- "beta"

##gamma
gamma <- ranges
gamma$ar.1 <- coefs[1]  ## not going to mess with the AR param lambda
gamma$end.1 <- coefs[2]  ## alpha
gamma$end.t <- coefs[3]  ###or the slope beta
gamma[5] <- coefs[5] ##delta 
gamma[6] <- coefs[6] ##psi
gamma$param <- "gamma"

ranges <- rbind(lambda, alpha, beta, delta, gamma, psi)

##########################################
######## PREP FOR SIMULATION #############
##########################################

#dates <- chco$Date

library("ALERT")

#this is for when there is a firstMonth error. It will be filtered in the following steps.
filler <- (createALERT(data, firstMonth=9))

#choose the number of simulations to perform

snum <- 100

simulations <- list()

#create the empty data.frame to allocate memory.

num <- (nrow(ranges)*snum)

alertstats2 <- data.frame(threshold=rep(NA, num),
                          median.dur=rep(NA, num),
                          median.pct.cases.captured=rep(NA, num),
                          min.pct.cases.captured=rep(NA, num),
                          max.pct.cases.captured=rep(NA, num),
                          pct.peaks.captured=rep(NA, num),
                          pct.ext.peaks.captured=rep(NA, num),
                          mean.low.weeks.incl=rep(NA, num),
                          parameter=rep(NA, num),
                          value=rep(NA, num),
                          success=rep(NA, num),
                          firstMonth=rep(NA, num))


alerttest <- data.frame(test.threshold=rep(NA, num),
                          test.median.dur=rep(NA, num),
                          test.median.pct.cases.captured=rep(NA, num),
                        test.min.pct.cases.captured=rep(NA, num),
                        test.max.pct.cases.captured=rep(NA, num),
                          test.pct.peaks.captured=rep(NA, num),
                        test.pct.ext.peaks.captured=rep(NA, num), 
                        test.mean.low.weeks.incl=rep(NA, num))


##Make a column with the changed values

ranges$value <- NA

ranges$value[ranges$param=="lambda"] <- ranges$ar.1[ranges$param=="lambda"]
ranges$value[ranges$param=="alpha"] <- ranges$end.1[ranges$param=="alpha"]
ranges$value[ranges$param=="beta"] <- ranges$end.t[ranges$param=="beta"]
ranges$value[ranges$param=="delta"] <- ranges$`end.cos(2 * pi * t/52)`[ranges$param=="delta"]
ranges$value[ranges$param=="gamma"] <- ranges$`end.sin(2 * pi * t/52)`[ranges$param=="gamma"]
ranges$value[ranges$param=="psi"] <- ranges$`-log(overdisp)`[ranges$param=="psi"]

########################################
##########  SIMULATE DATA!  ############
########################################

for (i in 1:nrow(ranges)){
  #print(i)
  params <- c(ranges$param[i], ranges$value[i])
  toycoef <- ranges[i,1:6]
    res2 <- surveillance::hhh4(chco.dat, model1)
    res2$coefficients <- unlist(c(toycoef))
     simulation <- as.vector(simulate(res2, nsim=snum, seed=123,
                                     y.start=NULL, simplify=TRUE))
     minisim <- split(simulation, as.factor(rep(seq(1,snum,1), each=length(dates))))
     for(k in 1:snum){
      simset <- data.frame(minisim[[k]], dates)
      colnames(simset) <- c("Cases", "Date")
      write.csv(simset, file=paste("~/Desktop/applied-ALERT-data/simulated-data/", 
                                   ranges$param[i], ranges$value[i],
                                   "_sim#", k, ".csv", sep=""))
      simset_train <- simset[1:200,]
      simset_test <- simset[201:nrow(simset),]
      ##to get the FirstMonth
      rho <- as.numeric(res2$coefficients[4])
      omega <- as.numeric(res2$coefficients[5])
      end.deriv <- function(t) 2*pi*rho*cos(2*pi*t)-2*pi*omega*sin(2*pi*t)
      xmin <- optimize(f=end.deriv, lower=0, upper=1, maximum=FALSE)
      startM <- month(data$Date[round(xmin$minimum*52)])
      alertstats2[((i-1)*snum)+k,] <- c(get_stats(simset_train, firstMonth=startM,
                                          params=params), startM)
      #print(alertstats2[((i-1)*snum)+k,])
      alerttest[((i-1)*snum)+k,] <- thresholdtestALERT(simset_test, 
                                                       whichThreshold=as.numeric(alertstats2[((i-1)*snum)+k,1]),
                                                       firstMonth=startM)$out
      #print(alerttest[((i-1)*snum)+k,])
     }
}

alertstats <- data.frame(alertstats2, alerttest)

alertstats <- alertstats %>% group_by(parameter, value) %>% 
  filter(median.pct.cases.captured>85) %>% group_by(parameter, value) %>% 
         filter(median.dur==min(median.dur)) %>% data.frame()

#filter(alertstats, threshold!=test.threshold)

med.stats <- alertstats %>% mutate(threshold=as.numeric(threshold), 
                        train.median.dur=as.numeric(median.dur),
                        median.pct.cases.captured = as.numeric(median.pct.cases.captured),
    test.median.pct.cases.captured=as.numeric(test.median.pct.cases.captured), 
    test.median.dur=as.numeric(test.median.dur),
    test.mean.low.weeks.incl=as.numeric(test.mean.low.weeks.incl),
    mean.low.weeks.incl=as.numeric(mean.low.weeks.incl)) %>% 
  filter(success==TRUE) %>% group_by(parameter, value) %>%
    summarise(test.median.pct.cases.captured=median(test.median.pct.cases.captured), 
            threshold=median(threshold),
            test.median.dur=median(test.median.dur), 
            train.median.dur=median(train.median.dur),
            median.pct.cases.captured = median(median.pct.cases.captured),
            median.test.low.weeks=median(test.mean.low.weeks.incl),
            median.train.low.weeks=median(mean.low.weeks.incl)) %>%
  mutate(duration.diff=train.median.dur-test.median.dur,
         median.pct.diff=median.pct.cases.captured-test.median.pct.cases.captured,
         median.low.weeks.diff=median.train.low.weeks-median.test.low.weeks) %>%
  data.frame()

##alertstats2 holds the uncollapsed ALERT results
## med.stats has the collapsed results
write.csv(med.stats, "~/Desktop/applied-ALERT-data/simulated-data/med_stats.csv")
write.csv(alertstats2, "~/Desktop/applied-ALERT-data/simulated-data/alertstats2.csv")

med.stats <- read.csv("~/Desktop/applied-ALERT-data/simulated-data/med_stats.csv", stringsAsFactors = F)
alertstats2 <- read.csv("~/Desktop/applied-ALERT-data/simulated-data/alertstats2.csv", stringsAsFactors = F)


#it looks like ar.1, end.1, and end.t are going to be the most interesting.
#maybe overdispersion?

#plot this bit with each parameter adjusted while holding the others constant.

#I'm filtering the last row (overdispersion) because the threshold is crazy high
#and medians are either NA or 0.

#med.stats <- med.stats[-60,]

library("ggplot2")
#library("gridExtra")

alpha.plot <- ggplot(filter(alertstats2, parameter=="alpha")) +
  geom_point(aes(x=as.numeric(value), y=median.pct.cases.captured, color=success)) +
##  scale_color_gradient2(guide=guide_colorbar(direction="vertical"),
  #                     limits=c(3, (8)), low = "white", mid = "black", high = "black", midpoint = 6, na.value='grey') +
  scale_size(range = c(1, 6)) +
  #facet_wrap(~threshold, ncol=3)+
  theme_classic() +
  geom_hline(aes(yintercept=80), linetype="longdash", show.legend=FALSE) +
  xlab("parameter value")+
  ylab("median percent cases captured")

delta.plot <- ggplot(filter(alertstats2, parameter=="delta")) +
  geom_point(aes(x=as.numeric(value), y=median.pct.cases.captured, color=success)) +
  ##  scale_color_gradient2(guide=guide_colorbar(direction="vertical"),
  #                     limits=c(3, (8)), low = "white", mid = "black", high = "black", midpoint = 6, na.value='grey') +
  scale_size(range = c(1, 6)) +
  #facet_wrap(~threshold, ncol=3)+
  theme_classic() +
  geom_hline(aes(yintercept=80), linetype="longdash", show.legend=FALSE) +
  xlab("parameter value")+
  ylab("median percent cases captured")


nu.plot <- ggplot(filter(alertstats2, parameter=="nu")) +
  geom_point(aes(x=as.numeric(value), y=median.pct.cases.captured, color=success)) +
  ##  scale_color_gradient2(guide=guide_colorbar(direction="vertical"),
  #                     limits=c(3, (8)), low = "white", mid = "black", high = "black", midpoint = 6, na.value='grey') +
  scale_size(range = c(1, 6)) +
  #facet_wrap(~threshold, ncol=3)+
  theme_classic() +
  geom_hline(aes(yintercept=80), linetype="longdash", show.legend=FALSE) +
  xlab("parameter value")+
  ylab("median percent cases captured")


#The sin term manages the width of the seasonal epidemic/peak height/baseline noise

#cos is the width of the sinus, Larger cos term should make the sinus more narrow. 
#smaller makes the sinus wider (less baseline noise)
med.stats$parameter2 <- factor(med.stats$parameter, 
                              labels = c(unique(med.stats$parameter)))

med.stats$value <- as.numeric(med.stats$value)

med.perc.diff.performance <- ggplot(med.stats, group=parameter2) +
 # geom_point(aes(x=value, y=median.low.weeks.diff, color=threshold)) +
  #scale_colour_gradient2(guide=guide_colorbar(direction="vertical"),
   #                      limits=c(min(med.stats$threshold), (max(med.stats$threshold))), 
    #                     low = "green", mid = "purple", high = "purple", 
     #                    midpoint = max(med.stats$threshold)-1.5, na.value='grey') +
  geom_smooth(aes(x=value, y=median.pct.diff), colour="black") +
  facet_wrap(~parameter2, ncol=2, scales = "free_x", labeller = label_parsed)+
  theme_classic() +
  geom_hline(aes(yintercept=0), linetype="longdash", show.legend=FALSE, alpha=0.35) +
  xlab("parameter value")+
  ylab("median percent cases captured difference")

med.perc.diff.performance

med.lowweeks.diff.performance <- ggplot(med.stats, group=parameter2) +
  # geom_point(aes(x=value, y=median.low.weeks.diff, color=threshold)) +
  #scale_colour_gradient2(guide=guide_colorbar(direction="vertical"),
  #                      limits=c(min(med.stats$threshold), (max(med.stats$threshold))), 
  #                     low = "green", mid = "purple", high = "purple", 
  #                    midpoint = max(med.stats$threshold)-1.5, na.value='grey') +
  geom_smooth(aes(x=value, y=median.low.weeks.diff), colour="black") +
  facet_wrap(~parameter2, ncol=2, scales = "free_x", labeller = label_parsed)+
  theme_classic() +
  geom_hline(aes(yintercept=0), linetype="longdash", show.legend=FALSE, alpha=0.35) +
  xlab("parameter value")+
  ylab("median low weeks captured difference")

med.lowweeks.diff.performance

######################################
###### PLOT SOME FAKE DATA!!!! #######
######################################

holder <- read.csv("~/Desktop/applied-ALERT-data/simulated-data/gamma-1.31716908734946_sim#26.csv")

threshold <- filter(med.stats, parameter=="gamma" & value==-1.31716908734946)$threshold

##make sure Dates are Dates
holder$Date <- as.Date(holder$Date)

##is this an alert period?
holder$smooththres <- ifelse (lowess(holder$Cases, f=0.001)$y>=threshold, TRUE, FALSE)

##make the year factor column
A <- rep(1, times=52)
for(i in 2:11){
  A <- append(rep(i, 52), A)
}
A <- append(rep(12, nrow(holder)-length(A)), A)
holder$year <- as.factor(rev(A))

ALERT_dates <- holder %>% group_by(year, smooththres) %>% arrange(Date) %>%
  summarise(xstop=last(Date)-1, xstart=first(Date)-1) %>% 
  data.frame() %>% 
  filter(smooththres==TRUE) %>% select(-year, -smooththres)

ymin <- -2

threstrig <- ggplot() + #this is the ALERT dates
  theme_classic() +
  labs(y = "Simulated cases", x = "Threshold-based intervention periods") +
  geom_bar(aes(y=holder$Cases, x=holder$Date), stat="identity") +
  geom_vline(aes(xintercept=as.numeric(holder$Date[200])), linetype="dashed",
             alpha=0.5, show.legend = FALSE) +
  geom_rect(aes(xmin = ALERT_dates$xstart[1], 
                xmax = ALERT_dates$xstop[1], 
                ymin = -40, ymax = -15), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates$xstart[2], 
                xmax = ALERT_dates$xstop[2], 
                ymin = -40, ymax = -15), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates$xstart[3], 
                xmax = ALERT_dates$xstop[3], 
                ymin = -40, ymax = -15), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates$xstart[4], 
                xmax = ALERT_dates$xstop[4], 
                ymin = -40, ymax = -15), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates$xstart[5], 
                xmax = ALERT_dates$xstop[5], 
                ymin = -40, ymax = -15), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates$xstart[6], 
                xmax = ALERT_dates$xstop[6], 
                ymin = -40, ymax = -15), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates$xstart[7], 
                xmax = ALERT_dates$xstop[7], 
                ymin = -40, ymax = -15), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates$xstart[8], 
                xmax = ALERT_dates$xstop[8], 
                ymin = -40, ymax = -15), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates$xstart[9], 
                xmax = ALERT_dates$xstop[9], 
                ymin = -40, ymax = -15), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates$xstart[10], 
                xmax = ALERT_dates$xstop[10], 
                ymin = -40, ymax = -15), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates$xstart[11], 
                xmax = ALERT_dates$xstop[11], 
                ymin = -40, ymax = -15), alpha = 0.2) 
 
threstrig 
#gridExtra::grid.arrange(threstrig, performance, heights=c(1, 3))

