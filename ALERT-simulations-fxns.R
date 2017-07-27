###  homemade functions for use with ALERT

get_stats <- function (j, params, firstMonth) {
  result <- try(createALERT(j, firstMonth=firstMonth))
  if (class(result)=='try-error') {
    alert_stats <- filler$out[,1:8]
    alert_summaries <- (c(apply(alert_stats,
                                2, function (x) {max(x)*0}), params))
    success <- FALSE
  } else {
    alert_stats <- result$out[,1:8]
    alert_summaries <- try((c(apply(alert_stats,
                                    2, function (x) {median(x)}), params)))
    if (class(alert_summaries)=='try-error'){
      alert_stats <- filler$out[,1:8]
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

### chose a threshold and apply across a dataset.

thresholdtestALERT <- function(data, firstMonth=firstMonth, lag=7, minWeeks=8, whichThreshold=4, k=0, target.pct=NULL, caseColumn='Cases', lastDate=NULL) {
  ## check for correct column headers
  if( !("Date" %in% colnames(data)))
    stop("data needs Date columns.")
  if( !(caseColumn %in% colnames(data)) )
    stop(paste("column named", caseColumn, "not found in data."))
  
  ## subset data if required
  if(!is.null(lastDate))
    data <- subset(data, Date<as.Date(lastDate))
  
  ## create a list where each element of the list contains the indices for rows from that season. 
  years <- unique(year(data$Date))
  idxs <- vector("list", length(years)-1) 
  for(i in 1:length(idxs)) {
    startDate <- as.Date(paste0(years[i], "-", firstMonth, "-01"))
    endDate <- as.Date(paste0(years[i]+1, "-", firstMonth, "-01"))
    idxs[[i]] <- which(data$Date >= startDate & data$Date < endDate)   
  }
  
  ## threshold to test
  thresholds <- whichThreshold
  
  ## for each threshold and year, calculate metrics
  cnames <- c("threshold",
              "median.dur",
              "median.pct.cases.captured",
              "min.pct.cases.captured",
              "max.pct.cases.captured",
              "pct.peaks.captured",
              "pct.ext.peaks.captured",
              "mean.low.weeks.incl")
  if(!is.null(target.pct)) cnames <- c(cnames, "mean.duration.diff")
  out <- matrix(NA, nrow=length(thresholds), ncol=length(cnames))
  colnames(out) <- cnames
  details <- vector("list", length(thresholds))
  ## run a sample to get dim and dimnames
  samp.num <- ifelse(length(idxs[[1]])==0, 2, 1) # Used for evalALERT, if first season missing (i.e is test season) then use second season for sampleRun
  sampleRun <- applyALERT(data[idxs[[samp.num]],], threshold=thresholds[1], k=k, lag=lag, minWeeks=minWeeks, target.pct=target.pct, caseColumn=caseColumn)
  for(i in 1:length(thresholds)){
    tmp <- matrix(NA, nrow=length(idxs), ncol=length(sampleRun))
    colnames(tmp) <- names(sampleRun)
    for(j in 1:length(idxs)){
      if(length(idxs[[j]])==0) next # Used for evalALERT, skips missing (test) season
      tmp[j,] <- applyALERT(data[idxs[[j]],], threshold=thresholds[i], k=k, lag=lag, minWeeks=minWeeks, target.pct=target.pct, caseColumn=caseColumn)
    }
    details[[i]] <- tmp
    out[i,"threshold"] <- thresholds[i] ## threshold used
    out[i,"median.dur"] <- median(tmp[,"duration"], na.rm=TRUE) ## median duration
    out[i,"median.pct.cases.captured"] <- round(100*median(tmp[,"ALERT.cases.pct"], na.rm=TRUE),1) ## median % of cases captured
    out[i,"min.pct.cases.captured"] <- round(100*min(tmp[,"ALERT.cases.pct"], na.rm=TRUE),1) ## min % of cases captured
    out[i,"max.pct.cases.captured"] <- round(100*max(tmp[,"ALERT.cases.pct"], na.rm=TRUE),1) ## max % of cases captured
    out[i,"pct.peaks.captured"] <- round(100*sum(tmp[,"peak.captured"])/nrow(tmp),1) ## % of times peak captured
    out[i,"pct.ext.peaks.captured"] <- round(100*sum(tmp[,"peak.ext.captured"])/nrow(tmp),1) ## % of times peak +/- k weeks captured
    out[i,"mean.low.weeks.incl"] <- mean(tmp[,"low.weeks.incl"], na.rm=TRUE)
    if(!is.null(target.pct)) out[i,"mean.duration.diff"] <- mean(tmp[,"duration.diff"], na.rm=TRUE)
  }
  return(list(out=out, details=details))
}
