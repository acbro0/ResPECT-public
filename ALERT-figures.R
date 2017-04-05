#### figures for the applied ALERT paper

require(ALERT)
require(surveillance)
require(lubridate); year <- lubridate::year
require(ggplot2)
library(dplyr)

#real data
##load the data
fluA <- read.csv("~/Desktop/applied-ALERT-data/CHCO-fluA.csv", 
                 stringsAsFactors=F)
fluA$Date <- ymd(fluA$Date)
fluA <- arrange(fluA, Date)

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
fulldata <- data

###get the dates that CHCO actually used
dates <- read.csv("~/Desktop/applied-ALERT-data/chco-trigger-dates.csv", stringsAsFactors = F)
dates$startDate <- ymd(dates$startDate)
dates$endDate <- ymd(dates$endDate)
#truncate to get years we have cutoffs for and cutoffs we have years for.
trainingdata <- filter(fulldata, Date >= head(dates$startDate, 1)-weeks(2))
testingdata <- filter(fulldata, Date < head(dates$startDate, 1)-weeks(2))
dates <- filter(dates, endDate <= tail(data$Date, 1)+weeks(6))


############################################
#### ALERT DEMO FIGURE   ###################
############################################

rawdat <- ggplot(testingdata, aes(x=Date, y=Cases)) +
  geom_bar(stat="identity") +
  theme_classic()+
# ggtitle("Training years") +
  geom_hline(aes(yintercept=25), linetype="dashed", 
             alpha=0.5, show.legend = FALSE) +
  theme(axis.title.y=element_blank())
  


  ggplot(trainingdata, aes(x=Date, y=Cases)) +
  geom_bar(stat="identity") +
  theme_classic()+
  ggtitle("Testing years") 

density(trainingdata$Cases)

##zeros are dropped because that's what ALERT does
dens <- ggplot(data=filter(testingdata, Cases>0), 
       aes(filter(testingdata, Cases>0)$Cases)) +
  theme_classic()+
  geom_histogram(binwidth=1) +
  coord_flip() +
  geom_vline(aes(xintercept=25), linetype="dashed", 
             alpha=0.5, show.legend = FALSE) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank()) +
  labs(y = "Count") 

dens <- density(filter(testingdata, Cases>0)$Cases, c(0.1, 0.3, 0.6))
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0.1, 0.20, 0.5, 0.75, 0.9)
quantiles <- quantile(df$x, prob=probs)
df$quant <- factor(findInterval(x=df$x, vec=probs))
ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant))

quantile(filter(testingdata, Cases>0)$Cases, c(0.1, 0.3, 0.6))

grid.arrange(rawdat, dens, ncol=2, left="LCRI incidence")


#######################################################
###### ALERT THRESHOLD COMPARISON FIGURE  #############
#######################################################

##Run ALERT

alerttrain <- createALERT(testingdata, firstMonth=8, lag=0)

############################### use later

alertholder <- createALERT(testingdata, firstMonth=8, lag=0)

#alertholder <- createALERT(trainingdata, firstMonth=8, lag=0)

dates$yearIdx <- 1:nrow(dates)

vecs <- list()

for (i in 1:nrow(dates)){
  print(i)
  vecs[[i]] <- ifelse(dates$startDate[i] < trainingdata$Date & dates$endDate[i] > trainingdata$Date, TRUE, FALSE)
}

holder <- t(data.frame(Reduce(rbind, vecs)))

alertperiod <- list()

for (i in 1:nrow(holder)){
  alertperiod[[i]] <- any(holder[i,]==TRUE)
}

trainingdata$alertperiod <- as.vector(Reduce(rbind, alertperiod))

A <- rep(1, 42)

for(i in 2:7){
  A <- append(rep(i, 51), A)
}

A <- append(rep(8, nrow(trainingdata)-length(A)), A)

trainingdata$year <- as.factor(rev(A))

tot <- trainingdata %>% group_by(year) %>% summarise(total_cases=sum(Cases)) #total cases per year

alertcases <- trainingdata %>% filter(alertperiod==TRUE) %>% group_by(year) %>% 
  summarise(alert_cases=sum(Cases)) #"ALERT" cases per year

calc <- full_join(tot, alertcases)

calc[is.na(calc)] <- 0

calc$perc_alert <- calc$alert_cases/calc$total_cases *100 #percent alert cases captured

median(calc$perc_alert) #median cases captured

alert_dura <- trainingdata %>% filter(alertperiod==TRUE) %>% group_by(year) %>% #duration
  summarise(duration=as.numeric(length(Date)))

calc <- unique(full_join(calc, alert_dura))

holder <- trainingdata %>% group_by(year) %>% summarise(Cases=max(Cases)) %>%
  data.frame()

holder <- left_join(holder, trainingdata)

holder <- holder %>% group_by(year) %>% summarise(Date=first(Date))

holder <- left_join(holder, trainingdata) %>% data.frame()

calc$peak_captured <- holder$alertperiod

peaky <- table(calc$peak_captured) %>% data.frame() 

if(nrow(peaky)==2){
  perc_peaks_captured <- peaky$Freq[2] / peaky$Freq[1]*100
} else {
  if (peaky$Var1==TRUE){
    perc_peaks_captured <- 100
  } else {
    perc_peaks_captured <- 0
  }
}

holder <- trainingdata %>% group_by(year) %>% summarise(Cases=min(Cases)) %>%
  data.frame()

holder <- left_join(holder, trainingdata)

#number of 0 weeks included
holder <- filter(holder, alertperiod==TRUE) %>% group_by(year) %>% summarise(low_weeks_incl=length(alertperiod))

calc <- left_join(calc, holder) %>% data.frame()

calc$year <- NULL

calc[is.na(calc)] <- 0

cutoffs <- dates %>% arrange(-yearIdx)

calc <- cbind(cutoffs, calc)

calc$yearIdx <- NULL

#get the colnames for eval statistics

alertholder <- data.frame(alertholder$out)

snames <- colnames(alertholder)[-7]

stats_real <- t(data.frame(c(NA, 
                             round(median(calc$duration), 1),
                             round(median(calc$perc_alert), 1), 
                             round(min(calc$perc_alert), 1), 
                             round(max(calc$perc_alert), 1), 
                             round(perc_peaks_captured, 3), 
                             round(mean(calc$low_weeks_incl), 1))))

colnames(stats_real) <- snames

comparison <- full_join(data.frame(stats_real), alertholder)

require(xtable)

print(xtable(comparison[1:7]), include.rownames=F)

str(stats_real)

#plotting
#choose a threshold

ymin <- -2

datetrig <- ggplot() + #this is the real dates
  theme_classic() +
  labs(y = "LCRI", x = "Actual intervention periods") +
  geom_bar(aes(y=trainingdata$Cases, x=trainingdata$Date), stat="identity") +
  geom_rect(aes(xmin = dates$startDate[1], 
                xmax = dates$endDate[1], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = dates$startDate[2], 
                xmax = dates$endDate[2], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = dates$startDate[3], 
                xmax = dates$endDate[3], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = dates$startDate[4], 
                xmax = dates$endDate[4], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = dates$startDate[5], 
                xmax = dates$endDate[5], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = dates$startDate[6], 
                xmax = dates$endDate[6], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = dates$startDate[7], 
                xmax = dates$endDate[7], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = dates$startDate[8], 
                xmax = dates$endDate[8], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank()
  ) +
  annotate("text", x = as.Date("2004-10-05"), y = 140, label = "A)", size=5)


#####ALERT thresholds

alerttrain$out
#choosing 1, 3, 12, 25

##Here's where I left off

threshold <- 1
trainingdata$smooththres <- ifelse (lowess(trainingdata$Cases, f=0.001)$y>=threshold, TRUE, FALSE)
ALERT_dates1 <- trainingdata %>% group_by(year, smooththres) %>% arrange(Date) %>%
  summarise(xstop=last(Date)-1, xstart=first(Date)-1) %>% 
  data.frame() %>% 
  filter(smooththres==TRUE) %>% select(-year, -smooththres)

threstrig1 <- ggplot() + #this is the ALERT dates
  theme_classic() +
  labs(y = "Influenza A cases", x = "Threshold-based intervention periods") +
  geom_bar(aes(y=trainingdata$Cases, x=trainingdata$Date), stat="identity") +
  #geom_hline(aes(yintercept=threshold), linetype="dashed", 
  #           alpha=0.5, show.legend = FALSE) +
  geom_rect(aes(xmin = ALERT_dates1$xstart[1], 
                xmax = ALERT_dates1$xstop[1], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates1$xstart[2], 
                xmax = ALERT_dates1$xstop[2], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates1$xstart[3], 
                xmax = ALERT_dates1$xstop[3], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates1$xstart[4], 
                xmax = ALERT_dates1$xstop[4], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates1$xstart[5], 
                xmax = ALERT_dates1$xstop[5], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates1$xstart[6], 
                xmax = ALERT_dates1$xstop[6], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates1$xstart[7], 
                xmax = ALERT_dates1$xstop[7], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates1$xstart[8], 
                xmax = ALERT_dates1$xstop[8], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank()
        )+
  annotate("text", x = as.Date("2004-10-05"), y = 140, label = "B)", size=5)


##threshold 4


threshold <- 3
trainingdata$smooththres <- ifelse (lowess(trainingdata$Cases, f=0.001)$y>=threshold, TRUE, FALSE)
ALERT_dates3 <- trainingdata %>% group_by(year, smooththres) %>% arrange(Date) %>%
  summarise(xstop=last(Date)-1, xstart=first(Date)-1) %>% 
  data.frame() %>% 
  filter(smooththres==TRUE) %>% select(-year, -smooththres)

threstrig3 <- ggplot() + #this is the ALERT dates
  theme_classic() +
  labs(y = "Influenza A cases", x = "Threshold-based intervention periods") +
  geom_bar(aes(y=trainingdata$Cases, x=trainingdata$Date), stat="identity") +
  #geom_hline(aes(yintercept=threshold), linetype="dashed", 
  #           alpha=0.5, show.legend = FALSE) +
  geom_rect(aes(xmin = ALERT_dates3$xstart[1], 
                xmax = ALERT_dates3$xstop[1], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates3$xstart[2], 
                xmax = ALERT_dates3$xstop[2], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates3$xstart[3], 
                xmax = ALERT_dates3$xstop[3], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates3$xstart[4], 
                xmax = ALERT_dates3$xstop[4], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates3$xstart[5], 
                xmax = ALERT_dates3$xstop[5], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates3$xstart[6], 
                xmax = ALERT_dates3$xstop[6], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates3$xstart[7], 
                xmax = ALERT_dates3$xstop[7], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates3$xstart[8], 
                xmax = ALERT_dates3$xstop[8], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate("text", x = as.Date("2004-10-05"), y = 140, label = "C)", size=5)

####threhold 12

threshold <- 12
trainingdata$smooththres <- ifelse (lowess(trainingdata$Cases, f=0.001)$y>=threshold, TRUE, FALSE)
ALERT_dates12 <- trainingdata %>% group_by(year, smooththres) %>% arrange(Date) %>%
  summarise(xstop=last(Date)-1, xstart=first(Date)-1) %>% 
  data.frame() %>% 
  filter(smooththres==TRUE) %>% select(-year, -smooththres)

threstrig12 <- ggplot() + #this is the ALERT dates
  theme_classic() +
  labs(y = "Influenza A cases", x = "Threshold-based intervention periods") +
  geom_bar(aes(y=trainingdata$Cases, x=trainingdata$Date), stat="identity") +
  #geom_hline(aes(yintercept=threshold), linetype="dashed", 
  #           alpha=0.5, show.legend = FALSE) +
  geom_rect(aes(xmin = ALERT_dates12$xstart[1], 
                xmax = ALERT_dates12$xstop[1], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates12$xstart[2], 
                xmax = ALERT_dates12$xstop[2], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates12$xstart[3], 
                xmax = ALERT_dates12$xstop[3], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates12$xstart[4], 
                xmax = ALERT_dates12$xstop[4], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates12$xstart[5], 
                xmax = ALERT_dates12$xstop[5], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates12$xstart[6], 
                xmax = ALERT_dates12$xstop[6], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates12$xstart[7], 
                xmax = ALERT_dates12$xstop[7], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates12$xstart[8], 
                xmax = ALERT_dates12$xstop[8], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())+
  annotate("text", x = as.Date("2004-10-05"), y = 140, label = "D)", size=5)

####threhold 25

threshold <- 25
trainingdata$smooththres <- ifelse (lowess(trainingdata$Cases, f=0.001)$y>=threshold, TRUE, FALSE)
ALERT_dates25 <- trainingdata %>% group_by(year, smooththres) %>% arrange(Date) %>%
  summarise(xstop=last(Date)-1, xstart=first(Date)-1) %>% 
  data.frame() %>% 
  filter(smooththres==TRUE) %>% select(-year, -smooththres)

threstrig25 <- ggplot() + #this is the ALERT dates
  theme_classic() +
  labs(y = "Influenza A cases", x = "Threshold-based intervention periods") +
  geom_bar(aes(y=trainingdata$Cases, x=trainingdata$Date), stat="identity") +
#  geom_hline(aes(yintercept=threshold), linetype="dashed", 
#             alpha=0.5, show.legend = FALSE) +
  geom_rect(aes(xmin = ALERT_dates25$xstart[1], 
                xmax = ALERT_dates25$xstop[1], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates25$xstart[2], 
                xmax = ALERT_dates25$xstop[2], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates25$xstart[3], 
                xmax = ALERT_dates25$xstop[3], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates25$xstart[4], 
                xmax = ALERT_dates25$xstop[4], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates25$xstart[5], 
                xmax = ALERT_dates25$xstop[5], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates25$xstart[6], 
                xmax = ALERT_dates25$xstop[6], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates25$xstart[7], 
                xmax = ALERT_dates25$xstop[7], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  geom_rect(aes(xmin = ALERT_dates25$xstart[8], 
                xmax = ALERT_dates25$xstop[8], 
                ymin = ymin, ymax = Inf), alpha = 0.2) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  annotate("text", x = as.Date("2004-10-05"), y = 140, label = "E)", size=5)


library(gridExtra)

grid.arrange(datetrig,threstrig1, threstrig3, threstrig12, threstrig25, ncol=1, left="LCRI incidence", 
             bottom="Date")

