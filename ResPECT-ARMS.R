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

resultstable$model_name2 <- c("Influenza Outcome: ITT Cohort, Unadjusted",
                                                                        "Adjusted",
                                                                        "Influenza Outcome: PP Cohort, Unadjusted",
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
                                                                        "LDI Outcome: ITT Cohort, Unadjusted",
                                                                        "Adjusted",
                                                                        "LDI Outcome: PP Cohort, Unadjusted",
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
  labs(y = "Odds Ratio", x = NULL) +              
  scale_x_discrete(limits=rev(OR$model_name), labels=rev(OR$model_name2)) +
  theme(axis.text = element_text(size=10),  axis.title=element_text(size=12,face="bold")) +
  #ggtitle("Risk Estimates for MM versus N95")+
  theme(panel.grid.minor=element_blank())+
  annotate("text", x=1.5, y=0.651, label=paste("n=", as.numeric(nums$n_FLU_pp[3]), sep=""))+
  annotate("text", x=3.5, y=0.651, label=paste("n=", as.numeric(nums$n_FLU_itt[3]), sep="")) +
  scale_y_log10(limits= c(0.65, 1.6), breaks=c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6))

RR_plot <- ggplot(RR, aes(x=model_name, y = OR_RR)) +                     
  theme_bw() + 
  geom_pointrange (aes (ymin = CI_lower, ymax = CI_upper)) +              
  coord_flip() +               
  geom_hline(aes(yintercept=1), lty=2) +           
  labs(y = "Relative Rate", x = NULL) +  
  scale_x_discrete(limits=rev(RR$model_name), labels=rev(RR$model_name2)) +
  theme(axis.text = element_text(size=10),  axis.title=element_text(size=12,face="bold")) +
  #ggtitle("Adjusted relative rate of HCW absenteeism")+
  theme(panel.grid.minor=element_blank()) +
  annotate("text", x=1.5, y=0.651, label=paste("n=", as.numeric(nums$n_ILI_itt), sep=""))+
  annotate("text", x=3.5, y=0.651, label=paste("n=", as.numeric(nums$n_ILI_pp), sep="")) +
  annotate("text", x=5.5, y=0.651, label=paste("n=", as.numeric(nums$n_LCRI_itt), sep=""))+
  annotate("text", x=7.5, y=0.651, label=paste("n=", as.numeric(nums$n_LCRI_pp), sep="")) +
  annotate("text", x=9.5, y=0.651, label=paste("n=", as.numeric(nums$n_LDI_itt), sep=""))+
  annotate("text", x=11.5, y=0.651, label=paste("n=", as.numeric(nums$n_LDI_pp), sep="")) +
  annotate("text", x=13.5, y=0.651, label=paste("n=", as.numeric(nums$n_ARI_itt), sep=""))+
  annotate("text", x=15.5, y=0.651, label=paste("n=", as.numeric(nums$n_ARI_pp), sep="")) +
  scale_y_log10(limits= c(0.65, 1.6), breaks=c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6))

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
  as.numeric(nums$n_ARI_itt), as.numeric(nums$n_ILI_pp),
  as.numeric(nums$n_LCRI_itt), as.numeric(nums$n_LCRI_pp),
  as.numeric(nums$n_LDI_itt), as.numeric(nums$n_LDI_pp))

n <- as.character(rep(n, each=2))

#denom <- c(5180)

resultstable$n <- n

resultstable$CI <- paste("[", format(round(resultstable$CI_lower, 3), nsmall=3), 
                         ", ", 
                         format(round(resultstable$CI_upper, 3), nsmall=3), "]", sep="")

tab <- select(resultstable, model_name2, n, OR_RR, CI)


print(xtable(tab, type="html", digits=3))

