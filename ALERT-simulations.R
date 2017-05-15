
library("dplyr")
library("surveillance")
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
f.end <- addSeason2formula(f = ~ 1+t, S=1, period=52)
model1 <- list(ar = list(f = ~ 1), end = list(f =f.end),
               family = "NegBinM", subset=2:469)

DisProg.dat <- create.disProg(week = 1:nrow(all), 
                             observed = as.matrix(all[2]),
                             state = all$state, start = c(2001, 35))# convert to sts class
chco.dat <- disProg2sts(DisProg.dat)# convert to sts class

#  # run model
chco.dat_res <- hhh4(chco.dat, model1)
#  j <- as.integer(i-1)
holder <- chco.dat_res$coefficients


all <- data.frame(data.table::rbindlist(lapply(holder,as.list)))

colnames(all[1]) <- "coefficients derived from CHCO dataset"

coefnames <- names(holder)
#rownames(all) <- bugnames

print(xtable(all, type="html", digits=4))

upper <- all[1]+(all[1]*0.2)
lower <- all[1]-(all[1]*0.2)

#upper <- rep(1.75, nrow(all))
#lower <- rep(-3.0, nrow(all))

limits <- data.frame(lower, upper)

seq(from=limits[1,1], to=limits[1,2], length.out=100)

holder <- list()

for (i in 1:nrow(limits)){
  holder[[i]] <- seq(from=limits[i,1], to=limits[i,2], length.out=100)
}

ranges <- t(data.frame(data.table::rbindlist(lapply(holder,as.list))))

names(holder) <- coefnames

ranges <- data.frame(ranges)

#holder <- list()

#names(holder) <- coefnames

coefs <- chco.dat_res$coefficients
colnames(ranges) <- coefnames

ranges$ar.1 <- coefs[1]  ## not going to mess with the AR param
ranges$end.t <- coefs[3]  ###or the slopeend.sin(2 * pi * t/52)
ranges[4] <- coefs[4] ##or this seasonal term
rownames(ranges) <- NULL


###just change one at a time.

## first end.1
alpha <- ranges
alpha[5] <- coefs[5]
alpha[6] <- coefs[6]
alpha$param <- "alpha"

## second overdisp
nu <- ranges
nu[5] <- coefs[5]
nu[2] <- coefs[2]
nu$param <- "nu"

## last the delta cos term
delta <- ranges
delta[6] <- coefs[6]
delta[2] <- coefs[2]
delta$param <- "delta"

ranges <- rbind(alpha, delta, nu)

#dates <- chco$Date

library("ALERT")

#this is for when there is a firstMonth error. It will be filtered in the following steps.
filler <- (createALERT(data, firstMonth=9))

get_stats <- function (j, params) {
  result <- try(createALERT(j, firstMonth=9))
  if (class(result)=='try-error') {
    alert_stats <- filler$out[,1:3]
    alert_summaries <- (c(apply(alert_stats,
                                2, function (x) {max(x)*0}), params))
    success <- FALSE
  } else {
    alert_stats <- result$out[,1:3]
    alert_summaries <- try((c(apply(alert_stats,
                                    2, function (x) {median(x)}), params)))
    if (class(alert_summaries)=='try-error'){
      alert_stats <- filler$out[,1:3]
      alert_summaries <- (c(apply(alert_stats,
                                  2, function (x) {max(x)*0}), params))
      success <- FALSE
    } else {
      success <- TRUE
    }
  }
  alert_summaries['success'] <- success
  return(alert_summaries)
}


#choose the number of simulations to perform

snum <- 10

simulations <- list()

#create the empty data.frame to allocate memory.

num <- (nrow(ranges)*snum)


alertstats2 <- data.frame(threshold=rep(NA, num),
                          median.dur=rep(NA, num),
                          median.pct.cases.captured=rep(NA, num),
                          parameter=rep(NA, num),
                          value=rep(NA, num),
                          success=rep(NA, num))

for (i in 1:nrow(ranges)){
  print(i)
  toycoef <- ranges[i,1:6]
    res2 <- hhh4(chco.dat, model1)
    res2$coefficients <- unlist(c(toycoef))
     simulation <- as.vector(simulate(res2, nsim=snum, seed=NULL,
                                     y.start=NULL, simplify=TRUE))
     minisim <- split(simulation, as.factor(rep(seq(1,snum,1), each=length(dates))))
}
    
    


for(k in 1:snum){
      simset <- data.frame(minisim[[k]], dates)
      colnames(simset) <- c("Cases", "Date")
      alertstats2[j+(i-1)*length(holder[[i]])+
                    (length(holder)*length(holder[[i]])*(k-1)),] <- get_stats(simset, 
                                                                              params=params) 
    }
  }
}


#fill the data.frame using the power of indexing
for(i in 1:length(names(holder))){
for (j in 1:length(holder[[i]])){
  print(c(j))
  params <- c(names(holder)[i], holder[[i]][[j]])
  toycoef <- coefs
  toycoef[i] <- holder[[i]][[j]]
  res2 <- hhh4(chco.dat, model1)
  res2$coefficients <- toycoef
  simulation <- as.vector(simulate(res2, nsim=snum, seed=NULL,
                                   y.start=NULL, simplify=TRUE))
  minisim <- split(simulation, as.factor(rep(seq(1,snum,1), each=length(dates))))
  for(k in 1:snum){
    simset <- data.frame(minisim[[k]], dates)
    colnames(simset) <- c("Cases", "Date")
    alertstats2[j+(i-1)*length(holder[[i]])+
                  (length(holder)*length(holder[[i]])*(k-1)),] <- get_stats(simset, 
                                                                            params=params) 
  }
}
}

med.stats <- alertstats2 %>% mutate(median.pct.cases.captured=as.numeric(median.pct.cases.captured), 
                                    threshold=as.numeric(threshold), 
                                    median.dur=as.numeric(median.dur)) %>% 
  filter(success==TRUE) %>% group_by(parameter, value) %>%
  summarise(median=median(median.pct.cases.captured), 
            threshold=median(threshold),
            median.dur=median(median.dur)) %>%
  data.frame()

#it looks like ar.1, end.1, and end.t are going to be the most interesting.
#maybe overdispersion?

#plot this bit with each parameter adjusted while holding the others constant.

#I'm filtering the last row (overdispersion) because the threshold is crazy high
#and medians are either NA or 0.

#med.stats <- med.stats[-60,]

library("ggplot2")
#library("gridExtra")

ar.1 <- ggplot(filter(med.stats, parameter=="ar.1")) +
  geom_point(aes(x=as.numeric(value), y=median, size=median.dur, color=threshold)) +
  scale_color_gradient2(guide=guide_colorbar(direction="vertical"),
                       limits=c(3, (8)), low = "white", mid = "black", high = "black", midpoint = 6, na.value='grey') +
  scale_size(range = c(1, 6)) +
  #facet_wrap(~threshold, ncol=3)+
  theme_classic() +
  geom_hline(aes(yintercept=80), linetype="longdash", show.legend=FALSE) +
  xlab("parameter value")+
  ylab("median percent cases captured")

end.1 <- ggplot(filter(med.stats, parameter=="end.1")) +
  geom_point(aes(x=as.numeric(value), y=median, size=median.dur, color=threshold)) +
  scale_colour_gradient2(guide=guide_colorbar(direction="vertical"),
                         limits=c(3, (8)), low = "blue", mid = "green", high = "green", midpoint = 6, na.value='grey') +
  #facet_wrap(~threshold, ncol=3)+
  theme_classic() +
  geom_hline(aes(yintercept=80), linetype="longdash", show.legend=FALSE) +
  xlab("parameter value")+
  ylab("median percent cases captured")

end.cos <- ggplot(filter(med.stats, parameter=="end.cos(2 * pi * t/52)")) +
  geom_point(aes(x=as.numeric(value), y=median, size=median.dur, color=threshold)) +
  scale_colour_gradient2(guide=guide_colorbar(direction="vertical"),
                         limits=c(3, (8)), low = "blue", mid = "green", high = "green", midpoint = 6, na.value='grey') +
  #facet_wrap(~threshold, ncol=3)+
  theme_classic() +
  geom_hline(aes(yintercept=80), linetype="longdash", show.legend=FALSE) +
  xlab("parameter value")+
  ylab("median percent cases captured")

end.sin <- ggplot(filter(med.stats, parameter=="end.sin(2 * pi * t/52)")) +
  geom_point(aes(x=as.numeric(value), y=median, size=median.dur, color=threshold)) +
  scale_colour_gradient2(guide=guide_colorbar(direction="vertical"),
                         limits=c(3, (8)), low = "blue", mid = "green", high = "green", midpoint = 6, na.value='grey') +
  #facet_wrap(~threshold, ncol=3)+
  theme_classic() +
  geom_hline(aes(yintercept=80), linetype="longdash", show.legend=FALSE) +
  xlab("parameter value")+
  ylab("median percent cases captured")

end.t <- ggplot(filter(med.stats, parameter=="end.t")) +
  geom_point(aes(x=as.numeric(value), y=median, size=median.dur, color=threshold)) +
  scale_colour_gradient2(guide=guide_colorbar(direction="vertical"),
                         limits=c(3, (8)), low = "blue", mid = "green", high = "green", midpoint = 6, na.value='grey') +
  #facet_wrap(~threshold, ncol=3)+
  theme_classic() +
  geom_hline(aes(yintercept=80), linetype="longdash", show.legend=FALSE) +
  xlab("parameter value")+
  ylab("median percent cases captured")

overdisp <- ggplot(filter(med.stats, parameter=="-log(overdisp)")) +
  geom_point(aes(x=as.numeric(value), y=median, size=median.dur, color=threshold)) +
  scale_colour_gradient2(guide=guide_colorbar(direction="vertical"),
                         limits=c(3, (8)), low = "blue", mid = "green", high = "green", midpoint = 6, na.value='grey') +
  #facet_wrap(~threshold, ncol=3)+
  theme_classic() +
  geom_hline(aes(yintercept=80), linetype="longdash", show.legend=FALSE) +
  xlab("parameter value")+
  ylab("median percent cases captured")


library("grid")

grid.newpage()
pushViewport(viewport(layout = 
                        grid.layout(nrow = 3, ncol = 2, 
                        widths = unit(c(4, 4), "null"))))

print(ar.1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(end.1, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(end.sin, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(end.cos, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(end.t, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(overdisp, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))

#grid.text("A)", hjust=-0.25, vjust=-5.5, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#grid.text("B)", hjust=-0.25, vjust=-5.5, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
#grid.text("C)", hjust=-0.25, vjust=-5.5, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))


#The sin term manages the width of the seasonal epidemic/peak height/baseline noise

#cos is the width of the sinus, Larger cos term should make the sinus more narrow. 
#smaller makes the sinus wider (less baseline noise)
med.stats$parameter2 <- factor(med.stats$parameter, 
                              labels = c("lambda", "alpha", "cos(2*pi/52)*delta",
                                         "sin(2*pi/52)*gamma", "beta", 
                                         "-log(psi)"))

holder <- ggplot(med.stats, group=parameter) +
  geom_point(aes(x=as.numeric(value), y=median, size=median.dur, color=threshold)) +
  scale_colour_gradient2(guide=guide_colorbar(direction="vertical"),
                         limits=c(3, (8)), low = "blue", mid = "green", high = "dark green", midpoint = 6, na.value='grey') +
  facet_wrap(~parameter2, ncol=2, scales = "free_x", labeller = label_parsed)+
  theme_classic() +
  geom_hline(aes(yintercept=80), linetype="longdash", show.legend=FALSE) +
  xlab("parameter value")+
  ylab("median percent cases captured")

holder
