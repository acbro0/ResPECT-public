require(diagram)


par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
##initialize new grphics device
openplotmat()
##number of elements per row
elpos<-coordinates (c(1,2,2,1 ))
elpos

##draw arrows from each row to next row
treearrow(from=elpos[1,],to=elpos[2:3,],lwd=6)  
treearrow(from=elpos[2,],to=elpos[4:5,],lwd=6) 
treearrow(from=elpos[4,],to=elpos[6,],lwd=6)  
#treearrow(from=elpos[4,],to=elpos[8,],lwd=6)  

require(dplyr)

##get the data.
demog <- read.csv("/users/nreich/respect/all-years-data-current/ResPECT-final-datasets/ResPECT-demographics.csv", stringsAsFactors=FALSE)

nrow(demog)

##create a generic 3-lined label for each textbox
labels = vector(length=6)
labels[1] = paste(c("Assessed for eligibility (n=5266)"), collapse="") 

labels[3] = paste(c("Ineligible (n=86)", "\n", "Failed to meet inclusion criteria"), collapse="")

#nrow(demog)

labels[2] = paste(c("Recieved intervention (n=5180)", "\n", "Intention to treat cohort"), collapse="") 

labels[4] = paste(c("Completed study (n=4689)", "\n", "Per protocol cohort"), collapse="") 

labels[5] = paste(c("Discontinued intervention", "\n", "Change in work location (n=256)", "\n", "Other reason (n=235)" ), collapse="") 

labels[6] <-  paste("Outcomes analyzed")

labels

##plot text boxes
for ( i in 1:6) textrect (elpos[i,], radx=0.2, rady=0.06, lab=labels[i], shadow.size = 0)




