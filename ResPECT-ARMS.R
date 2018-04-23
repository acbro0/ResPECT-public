
models <- readRDS("/home/alexandria/Desktop/ARMS-final-results.rds")

nums <- readRDS("/home/alexandria/Desktop/outcomes-final-numbers.rds")

library("mice")
library("dplyr")
library("lme4")
library("ggplot2")
library("xtable")


resultstable <- data.frame(model_name=names(models),
                        OR_RR=rep(NA, length(names(models))),
                        CI_lower=rep(NA, length(names(models))),
                        CI_upper=rep(NA, length(names(models))))



for (i in 1:length(names(models))){
  holder <- summary(models[[i]])
  estimate <- holder[2,1]
  resultstable[i,2] <- exp(holder[2,1])
  se <- holder[2,2]
  resultstable[i,3] <- exp(estimate-(1.96*se))
  resultstable[i,4] <- exp(estimate+(1.96*se))
}

##reorder by number of observations.

##Lew doesn't want superscripts anymore. 
#resultstable$model_name2 <- c(bquote(paste("LC", I^2, " Outcome: ITT Cohort, Unadjusted", sep="")),
#                               "Adjusted",
#                               "LCI Outcome: PP Cohort, Unadjusted",
#                               "Adjusted",
#                               bquote(paste("IL", I^6, " Outcome: ITT Cohort, Unadjusted", sep="")),
#                               "Adjusted",
#                               "ILI Outcome: PP Cohort, Unadjusted",
#                               "Adjusted",
#                               bquote(paste("AR", I^3, " Outcome: ITT Cohort, Unadjusted", sep="")),
#                               "Adjusted",
#                               "ARI Outcome: PP Cohort, Unadjusted",
#                               "Adjusted",
#                               bquote(paste("LCR", I^5, " Outcome: ITT Cohort, Unadjusted", sep="")),
#                               "Adjusted",
#                              "LCRI Outcome: PP Cohort, Unadjusted",
#                             "Adjusted",
#                               bquote(paste("LDR", I^4, " Outcome: ITT Cohort, Unadjusted", sep="")),
#                               "Adjusted",
#                               "LDRI Outcome: PP Cohort, Unadjusted",
#                               "Adjusted")


resultstable$model_name2 <- c("LCI Outcome: ITT Cohort, Unadjusted",
                               "Adjusted",
                               "LCI Outcome: PP Cohort, Unadjusted",
                               "Adjusted",
                               "ILI Outcome: ITT Cohort, Unadjusted",
                               "Adjusted",
                               "ILI Outcome: PP Cohort, Unadjusted",
                               "Adjusted",
                               "ARI Outcome: ITT Cohort, Unadjusted",
                               "Adjusted",
                               "ARI Outcome: PP Cohort, Unadjusted",
                               "Adjusted",
                               "LCRI Outcome: ITT Cohort, Unadjusted",
                               "Adjusted",
                              "LCRI Outcome: PP Cohort, Unadjusted",
                             "Adjusted",
                               "LDRI Outcome: ITT Cohort, Unadjusted",
                               "Adjusted",
                               "LDRI Outcome: PP Cohort, Unadjusted",
                               "Adjusted")

resultstable$model_name <- factor(resultstable$model_name, 
                                  levels = resultstable$model_name[order(rev(resultstable$model_name2))])

OR <- resultstable[1:4,]
RR <- resultstable[5:20,]

rownames(RR) <- seq(1:nrow(RR))

RR <- rbind(RR[5:8,], RR[13:16,], RR[9:12,], RR[1:4,])

OR_plot <- ggplot(OR, aes(x=model_name, y = OR_RR)) +                     
  theme_bw() + 
  geom_pointrange (aes (ymin = CI_lower, ymax = CI_upper)) +              
  coord_flip() +               
  geom_hline(aes(yintercept=1), lty=2) +           
  labs(y = NULL, x = NULL) +              
  scale_x_discrete(limits=rev(OR$model_name), labels=rev(OR$model_name2)) +
  theme(axis.text = element_text(size=10),  
        axis.title=element_text(size=11,face="bold"),
        plot.title = element_text(size=12, hjust = 0.5)) +
  ggtitle("Influenza (Primary) Outcomes Odds Ratios")+
  theme(panel.grid.minor=element_blank())+
  annotate("text", x=1.5, y=0.64, label=paste("n=", as.numeric(nums$n_FLU_pp[3]), sep=""))+
  annotate("text", x=3.5, y=0.64, label=paste("n=", as.numeric(nums$n_FLU_itt[3]), sep="")) +
  scale_y_log10(limits= c(0.63, 1.6), breaks=c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6))

RR_plot <- ggplot(RR, aes(x=model_name, y = OR_RR)) +                     
  theme_bw() + 
  geom_pointrange (aes (ymin = CI_lower, ymax = CI_upper)) +              
  coord_flip() +               
  geom_hline(aes(yintercept=1), lty=2) +           
  labs(y = NULL, x = NULL) +  
  scale_x_discrete(limits=rev(RR$model_name), labels=rev(RR$model_name2)) +
  theme(axis.text = element_text(size=10),  
        axis.title=element_text(size=11,face="bold"), 
        plot.title = element_text(size=12, hjust = 0.5)) +
  ggtitle("All Viral Respiratory Infections and Illnesses (Secondary) Outcomes Relative Rates")+
  theme(panel.grid.minor=element_blank()) +
  annotate("text", x=1.5, y=0.651, label=paste("n=", as.numeric(nums$n_ILI_pp), sep=""))+
  annotate("text", x=3.5, y=0.651, label=paste("n=", as.numeric(nums$n_ILI_itt), sep="")) +
  annotate("text", x=5.5, y=0.651, label=paste("n=", as.numeric(nums$n_LCRI_pp), sep=""))+
  annotate("text", x=7.5, y=0.651, label=paste("n=", as.numeric(nums$n_LCRI_itt), sep="")) +
  annotate("text", x=9.5, y=0.651, label=paste("n=", as.numeric(nums$n_LDI_pp), sep=""))+
  annotate("text", x=11.5, y=0.651, label=paste("n=", as.numeric(nums$n_LDI_itt), sep="")) +
  annotate("text", x=13.5, y=0.651, label=paste("n=", as.numeric(nums$n_ARI_pp), sep=""))+
  annotate("text", x=15.5, y=0.651, label=paste("n=", as.numeric(nums$n_ARI_itt), sep="")) +
  scale_y_log10(limits= c(0.63, 1.6), breaks=c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6))

library("grid")
#library(gridExtra)

#grid.arrange(OR_plot, RR_plot, ncol=1)


grid.newpage()
pushViewport(viewport(layout = 
                        grid.layout(nrow = 2, ncol = 1, 
                        heights = unit(c(0.5, 1), "null"))))

print(OR_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(RR_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))

resultstable

names(nums)

n <- c(as.numeric(nums$n_FLU_itt[3]), as.numeric(nums$n_FLU_pp[3]),
       as.numeric(nums$n_ILI_itt), as.numeric(nums$n_ILI_pp),
       as.numeric(nums$n_ARI_itt), as.numeric(nums$n_ARI_pp),
       as.numeric(nums$n_LCRI_itt), as.numeric(nums$n_LCRI_pp),
       as.numeric(nums$n_LDI_itt), as.numeric(nums$n_LDI_pp))

n <- as.character(rep(n, each=2))

#denom <- c(5180)

resultstable$n <- n

resultstable$CI <- paste("[", format(round(resultstable$CI_lower, 2), nsmall=2), 
                         ", ", 
                         format(round(resultstable$CI_upper, 2), nsmall=2), "]", sep="")

tab <- select(resultstable, model_name2, n, OR_RR, CI)

tab <- rbind(tab[1:4,], tab[9:12,], tab[17:20,], tab[13:16,], tab[5:8,])

print(xtable(tab, type="html", digits=2))
tab

#Compliance data

ppe <- read.csv("/home/alexandria/Desktop/handyaudit-data/PPE_CSV.csv")

head(ppe)

ppe2 <- ppe %>% filter(N.R.B=="Participant" & Respiratory.Illness=="true")

ppe2 <- ppe2[9:13]

q <- ifelse(ppe2[1]=="true", 1, 0)

for (i in 2:length(ppe2)){
  o  <- ifelse(ppe2[i]=="true", 1, 0)
  q <- o+q
}

q

sum(q)/length(q)*100

nrow(ppe2)


### try putting the table on the plot.

OR_plot <- ggplot(OR, aes(x=model_name, y = OR_RR)) +                     
  theme_bw() + 
  geom_pointrange (aes (ymin = CI_lower, ymax = CI_upper)) +              
  coord_flip() +               
  geom_hline(aes(yintercept=1), lty=2) +           
  labs(y = "Odds Ratio", x = NULL) +              
  scale_x_discrete(limits=rev(OR$model_name), labels=rev(OR$model_name2)) +
  theme(axis.text = element_text(size=10),  axis.title=element_text(size=12,face="bold")) +
  ggtitle("Risk Estimates for N95 vs. MM (reference)")+
  labs(size = 1) +
  theme(panel.grid.minor=element_blank())+
  annotate("text", x=1.5, y=0.651, label=paste("n=", as.numeric(nums$n_FLU_pp[3]), sep=""))+
  annotate("text", x=3.5, y=0.651, label=paste("n=", as.numeric(nums$n_FLU_itt[3]), sep="")) +
  annotate("text", x=4, y=1.57, label=paste(round(tab$OR_RR[1], digits=2), "; ", tab$CI[1], sep="")) +
  annotate("text", x=3, y=1.57, label=paste(round(tab$OR_RR[2], digits=2), "; ", tab$CI[2], sep="")) +
  annotate("text", x=2, y=1.57, label=paste(round(tab$OR_RR[3], digits=2), "; ", tab$CI[3], sep="")) +
  annotate("text", x=1, y=1.57, label=paste(sprintf("%.2f", round(tab$OR_RR[4], digits=2)), "; ", tab$CI[4], sep="")) +
  scale_y_log10(limits= c(0.63, 1.6), breaks=c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6))

RR_plot <- ggplot(RR, aes(x=model_name, y = OR_RR)) +                   
  theme_bw() + 
  geom_pointrange (aes (ymin = CI_lower, ymax = CI_upper)) +              
  coord_flip() +               
  geom_hline(aes(yintercept=1), lty=2) +           
  labs(y = "Relative Rate", x = NULL) +  
  scale_x_discrete(limits=rev(RR$model_name), labels=rev(RR$model_name2)) +
  theme(axis.text = element_text(size=10),  axis.title=element_text(size=12,face="bold")) +
  #ggtitle("Relative Risk for N95 vs. MM (reference)")+
  theme(panel.grid.minor=element_blank()) +
  annotate("text", x=1.5, y=0.651, label=paste("n=", as.numeric(nums$n_ILI_pp), sep=""))+
  annotate("text", x=3.5, y=0.651, label=paste("n=", as.numeric(nums$n_ILI_itt), sep="")) +
  annotate("text", x=5.5, y=0.651, label=paste("n=", as.numeric(nums$n_LCRI_pp), sep=""))+
  annotate("text", x=7.5, y=0.651, label=paste("n=", as.numeric(nums$n_LCRI_itt), sep="")) +
  annotate("text", x=9.5, y=0.651, label=paste("n=", as.numeric(nums$n_LDI_pp), sep=""))+
  annotate("text", x=11.5, y=0.651, label=paste("n=", as.numeric(nums$n_LDI_itt), sep="")) +
  annotate("text", x=13.5, y=0.651, label=paste("n=", as.numeric(nums$n_ARI_pp), sep=""))+
  annotate("text", x=15.5, y=0.651, label=paste("n=", as.numeric(nums$n_ARI_itt), sep="")) +
  annotate("text", x=16, y=1.57, label=paste(round(tab$OR_RR[5], digits=2), "; ", tab$CI[5], sep="")) +
  annotate("text", x=15, y=1.57, label=paste(round(tab$OR_RR[6], digits=2), "; ", tab$CI[6], sep="")) +
  annotate("text", x=14, y=1.57, label=paste(round(tab$OR_RR[7], digits=2), "; ", tab$CI[7], sep="")) +
  annotate("text", x=13, y=1.57, label=paste(sprintf("%.2f", round(tab$OR_RR[8], digits=2)), "; ", tab$CI[8], sep="")) +
  annotate("text", x=12, y=1.57, label=paste(round(tab$OR_RR[9], digits=2), "; ", tab$CI[9], sep="")) +
  annotate("text", x=11, y=1.57, label=paste(round(tab$OR_RR[10], digits=2), "; ", tab$CI[10], sep="")) +
  annotate("text", x=10, y=1.57, label=paste(round(tab$OR_RR[11], digits=2), "; ", tab$CI[11], sep="")) +
  annotate("text", x=9, y=1.57, label=paste(round(tab$OR_RR[12], digits=2), "; ", tab$CI[12], sep="")) +
  annotate("text", x=8, y=1.57, label=paste(round(tab$OR_RR[13], digits=2), "; ", tab$CI[13], sep="")) +
  annotate("text", x=7, y=1.57, label=paste(round(tab$OR_RR[14], digits=2), "; ", tab$CI[14], sep="")) +
  annotate("text", x=6, y=1.57, label=paste(round(tab$OR_RR[15], digits=2), "; ", tab$CI[15], sep="")) +
  annotate("text", x=5, y=1.57, label=paste(round(tab$OR_RR[16], digits=2), "; ", tab$CI[16], sep="")) +
  annotate("text", x=4, y=1.57, label=paste(round(tab$OR_RR[17], digits=2), "; ", tab$CI[17], sep="")) +
  annotate("text", x=3, y=1.57, label=paste(round(tab$OR_RR[18], digits=2), "; ", tab$CI[18], sep="")) +
  annotate("text", x=2, y=1.57, label=paste(round(tab$OR_RR[19], digits=2), "; ", tab$CI[19], sep="")) +
  annotate("text", x=1, y=1.57, label=paste(round(tab$OR_RR[20], digits=2), "; ", tab$CI[20], sep="")) +
  scale_y_log10(limits= c(0.63, 1.6), breaks=c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6))




grid.newpage()
pushViewport(viewport(layout = 
                        grid.layout(nrow = 2, ncol = 1, 
                                    heights = unit(c(0.5, 1), "null"))))

print(OR_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(RR_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))


ppe <- read.csv("/home/lex/Desktop/handyaudit-data/PPE_CSV.csv")
clinic_key <- read.csv("/home/lex/Desktop/clinic-names-matched.csv")
demog <- read.csv("/home/lex/Desktop/ResPECT-final-datasets/ResPECT-demographics.csv")

library(dplyr)
colnames(ppe)

#ObservationID year 1: 53346:85856, year2: 129563:169191, 
#year 3: 224450:270152, year 4: 352407:388423

year1 <- ifelse(as.integer(ppe$ObservationID)>=53346 & 
         as.integer(ppe$ObservationID)<=85856, 1, 0)

year2 <- ifelse(as.integer(ppe$ObservationID)>=129563 & 
                  as.integer(ppe$ObservationID)<=169191, 2, 0)

year3 <- ifelse(as.integer(ppe$ObservationID)>=224450 & 
                  as.integer(ppe$ObservationID)<=270152, 3, 0)

year4 <- ifelse(as.integer(ppe$ObservationID)>=352407 & 
                  as.integer(ppe$ObservationID)<=388423, 4, 0)

ppe$year <- year1+year2+year3+year4


colnames(clinic_key) <- c("clinic", "Location")

ha <- left_join(demog, clinic_key)

key <- unique(select(ha, clinic, Location, year, ARM))

ppe2 <- left_join(ppe, key)

colnames(ha)

colnames(ppe2)

ppe2 <- filter(ppe2, Respiratory.Illness=="true")

ppe2$complied <- 0

ppe2$complied[ppe2$N95.3M.1860=="true" | 
               ppe2$N95.3M.1870=="true" |
               ppe2$N95.KC.PFR95.170.174..duckbill.=="true" | 
               ppe2$Precept.15320..medical.mask..ties.=="true" |
               ppe2$KC.47107..medical.mask..loops..=="true" ] <- 1

ppe3 <- ppe2 %>% filter(N.R.B=='Participant' & !is.na(ARM)) 




#chisq.test(tab1)

#tab1

#692/(752+692)*100

totparti <- ppe3 %>% group_by(ARM) %>% 
  summarise(total=length(complied))

parti <- ppe3 %>% group_by(ARM, complied) %>% 
  summarise(n=length(complied)) 

parti <- left_join(parti, totparti)# %>% mutate(rate=n/total) #%>% filter(complied==1)

compl <- t(matrix(parti$n, ncol = 2))

dimnames(compl) <- list(ARM=c("MM", "N95"),
                        complied=c("no", "yes"))

chisq.test(compl)


#wilcox.test(parti$rate[1:4], parti$rate[5:8])


parti
nonparti <- filter(parti, N.R.B=="Non-Participant")

totarm <- ppe2 %>% filter(N.R.B=="Participant") %>%
  group_by(ARM, year) %>% 
  summarise(total=length(complied)) %>% 
  filter(ARM=="N95" | ARM=="MM" ) 

arm <- ppe2 %>% filter(N.R.B=="Participant") %>% 
  group_by(ARM, complied, year) %>% 
  summarise(n=length(complied)) %>% 
  filter(ARM=="MM" | ARM=="N95" )

armi <- left_join(arm, totarm) %>% mutate(rate=n/total) %>% filter(complied==1)
armi

wilcox.test(armi$rate[1:4], armi$rate[5:8])

#ppe2 <- filter(ppe, N.R.B=="Participant")

#ppe2 <- unique(ppe2$Location)

write.csv(ppe2, "/home/lex/Desktop/ppe_handyaudit.csv")

#substr(ppe$Datetime, 1, 4)

#tail(ppe$Datetime)

parti
armi


########################################
#### SENSITIVITY ANALYSIS  #############
########################################

sens <- readRDS("~/Desktop/sensitivity-grid.rds")

library(mice)
summary(sens[[1]][[1]])

holder <- list()
metaholder1 <- list()

for(j in 1:11){
for(i in 1:11){
holder[[i]] <- summary(sens[[j]][[i]])[[2,1]]
}
metaholder1[[j]] <- unlist(holder)
}

library(reshape2)
library(ggplot2)

sens[[12]] #N95
sens[[13]] #MM

holder1 <- data.frame(cbind(rep(sens[[13]], 11),
                rep(sens[[12]], each = 11),
                unlist(metaholder1)))



ggplot(data = holder1, aes(x=holder1[,1], y=holder1[,2], fill=exp(holder1[,3]))) + 
  theme_bw() + 
  labs(y = "N95", x = "MM") +    
  geom_tile() +
  scale_fill_gradient2(low = "white",
                       mid="white",
                       high = "blue",
                       midpoint = 1,
                       space = "Lab", 
                       name="Odds Ratio")

head(holder)

holder <- list()
metaholder2 <- list()

##is it significant?

for(j in 1:11){
  for(i in 1:11){
    holder[[i]] <- summary(sens[[j]][[i]])[[2,5]]
  }
  metaholder2[[j]] <- unlist(holder)
}

holder2 <- data.frame(cbind(rep(sens[[13]], 11),
                           rep(sens[[12]], each = 11),
                           unlist(metaholder2)))

head(holder1)
head(holder2)

holder1$pval <- holder2$X3

holder <- holder1

holder$X4 <- p.adjust(holder$pval, method = "holm")

holder$sig <- as.factor(ifelse(holder$X4<0.05, "*", NA))

###plot it

ggplot(data = holder, aes(x=holder[,1], y=holder[,2], shape=holder$sig)) + 
  theme_bw() + 
  labs(y = "N95", x = "MM") +    
  scale_shape_identity() +
  geom_point(size=5)


ggplot(data = holder1, aes(x=holder[,1], y=holder[,2], shape=holder$sig, fill=exp(holder$X3))) + 
  theme_bw() + 
  labs(y = "assumed N95 influenza attack rate", 
       x = "assumed MM influenza attack rate") +    
  geom_tile() +
  scale_fill_gradient2(low = "white",
                       mid="white",
                       high = "blue",
                       midpoint = 1,
                       space = "Lab", 
                       name="Odds Ratio") +
  scale_shape_identity() +
  geom_point(size=8)



