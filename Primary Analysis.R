#Functions ----

#Onset function -- Progresses backwards from peak through Bothsmooth data until threshold is reached and returns its position.

OnsetAlg <- function(a, x, Threshold, i) {  for (a in a:1) {if ((Experiment[[i]]$Trials[[x]]$Data[a*10, "BothSmooth"] <= Threshold)  | (a <= 1)) {return(a)}}}

#Offset function -- Progesses forwards from peak through Bothsmmoth data until threshold(1.5sd above baseline) is reached and returns it

OffsetAlg <- function(a, x, Threshold, i) {  for (a in a:200) {if ((Experiment[[i]]$Trials[[x]]$Data[a*10, "BothSmooth"] <= Threshold) | (a >= (200))){return(a)}}}


#Extract Value function -- Used to extract particular matching values from Experiment[[i]]$Trials.
#Two paramaters, first passes a condition to check eg "f$Distance == 1" includes all stimuli data that is Distance==1. Default is 1==1, therefore includes all
#Second parameter refers to the matching data to return eg ""f$Data$BothSmooth" returns all matching "BothSmooth" data columns.
#Also applies a column name
#If it fails and creates no data, it returns a dataframe containing a single '0' to stop errors

ExtractVal <- function(condition="1==1", target, i) {
  
  a <- lapply(Experiment[[i]]$Trials, function(f){if(eval(parse(text=condition))) {eval(parse(text=target))}})
  a <- as.data.frame(a[!sapply(a, is.null)])
  if (!(nrow(a) == 0)){ colnames(a) <- substring(target, 3)
  }else{a[1,1] <- 0}
  return(a)
}

#Used for adding a title to the contrast analysis tables.

AddTitle <- function(title, table) {
  
  a <- textGrob(as.character(title), gp = gpar(fontsize = 13))
  
  return(gtable_add_grob(gtable_add_rows(tableGrob(table, rows = NULL, cols = NULL), heights = grobHeight(a) + unit(0.5,"line"), pos = 0), list(a),t = 1, l = 1, r = ncol(table)))
}

# Init ----

library(data.table)
library(zoo)
library(MESS)
library(ggplot2)
library(gtable)
library(grid)
library(gplots)
library(afex)
library(knitr)
library(gridExtra)
library(ggpubr)
library(GGally)
library(abind)
library(R.utils)
library(emmeans)
setwd("D:")
options(scipen=999)

#Optional code to upscale participant 1s data.

# for (j in 1:64) {
#   
#   Data4 <- data.frame()
#   for (i in 1:400)
#   {
#     a <- Experiment[[1]]$Trials[[j]]$Data[i,]
#     Data4 <- rbind(Data4,a,a,a,a,a,a,a,a,a,a)
#     
#   }
#   Data4$Timepoint <- seq(from = -99.9, by = 0.1, to = 300.0)
#   Experiment[[1]]$Trials[[j]]$Data <- Data4
#   
# }

#if(!exists("Experiment")) 
Experiment <- Experiment.backup

Code.Format <- readline(prompt="All or specific? (All/#): ")
plot <- as.numeric(readline(prompt="Plot (1=Yes,0=No): "))
  
for (i in 1:length(Experiment)){
  
  if (Code.Format != "All") {if (as.numeric(Code.Format) != i ) {next}}
  if (length(Experiment[[i]]$Trials)==0) {next}
  
  for (x in 1:(length(Experiment[[i]]$Trials))){
    
    
    #Onset/offset threshold Calculation
    #If no threshold has been calculated, it loops through all 64 trials and rectifies and smoothes.
    #Then takes the mean Threshold across trials of 2sd above mean data at -100ms to -80ms.
    #It has to do this extra loop early because trial calculations are done one after another, and to calculate the threshold, all data must be rectified/smoothed first.
    #Awkward, but neccessary.
    
    if(is.null(Experiment[[i]]$Threshold)) {
      
      for (a in 1:(length(Experiment[[i]]$Trials))){
        
        #Smoothing left/right, then averaging Left/Right.
        
        Experiment[[i]]$Trials[[a]]$Data$LeftSmooth <- round(rollmean(Experiment[[i]]$Trials[[a]]$Data$LeftRect, 101, fill = 0), digits = 3)
        
        Experiment[[i]]$Trials[[a]]$Data$RightSmooth <- round(rollmean(Experiment[[i]]$Trials[[a]]$Data$RightRect, 101, fill = 0), digits = 3)
        
        Experiment[[i]]$Trials[[a]]$Data$BothSmooth <- round(((Experiment[[i]]$Trials[[a]]$Data$LeftSmooth + Experiment[[i]]$Trials[[a]]$Data$RightSmooth)/2), digits = 3)
        
      }
      
      #Threshold - Currently 2sd above mean of -100ms to -20ms
      
      Experiment[[i]]$Threshold <- mean(sapply(Experiment[[i]]$Trials, function(f) mean(f$Data$BothSmooth[1:(80*10)]) + (2 *sd(f$Data$BothSmooth[1:(80*10)]))))
      
      #More "correct" alternative threshold. It excludes the data destroyed via the smoothing function.
      #Experiment[[i]]$Threshold <- mean(sapply(Experiment[[i]]$Trials, function(f) mean(f$Data$BothSmooth[52:(80*10)]) + (2 *sd(f$Data$BothSmooth[52:(80*10)]))))
      
      #Experiment[[i]]$Threshold <- mean(sapply(Experiment[[i]]$Trials, function(f) mean(f$Data$BothSmooth)))
      
    }
    
    #Data trimming
    
    Experiment[[i]]$Trials[[x]]$Data <- Experiment[[i]]$Trials[[x]]$Data[1001:3000,]
      
    #Hand labelling
      
    if (x <= 32)  {if (Experiment[[i]]$StartingHand == "L") { Experiment[[i]]$Trials[[x]]$Hand <- 1}else {Experiment[[i]]$Trials[[x]]$Hand <- 2}
        Experiment[[i]]$Trials[[x]]$Block <- 1}
      
    else          {if (Experiment[[i]]$StartingHand == "L") { Experiment[[i]]$Trials[[x]]$Hand <- 2}else {Experiment[[i]]$Trials[[x]]$Hand <- 1}
        Experiment[[i]]$Trials[[x]]$Block <- 2}
    
    #Wave1 creation.
    
    a <- ceiling((which.max(Experiment[[i]]$Trials[[x]]$Data[(10*10):(150*10), "BothSmooth"])+(10*10-1))/10)
    
    Experiment[[i]]$Trials[[x]]$Wave1 <- list()
    Experiment[[i]]$Trials[[x]]$Wave1$Peak.ms <- a
    Experiment[[i]]$Trials[[x]]$Wave1$Peak.uv <- max(Experiment[[i]]$Trials[[x]]$Data[(10*10):(150*10), "BothSmooth"])
    Experiment[[i]]$Trials[[x]]$Wave1$Onset <- OnsetAlg(a,x,Experiment[[i]]$Threshold, i)
    Experiment[[i]]$Trials[[x]]$Wave1$Offset <- OffsetAlg(a,x, Experiment[[i]]$Threshold, i)
    
    #If Wave1 onset and offset exist, calculate duration
    
    if (!is.na(Experiment[[i]]$Trials[[x]]$Wave1$Onset) & !is.na(Experiment[[i]]$Trials[[x]]$Wave1$Offset)) {Experiment[[i]]$Trials[[x]]$Wave1$Duration <- Experiment[[i]]$Trials[[x]]$Wave1$Offset-Experiment[[i]]$Trials[[x]]$Wave1$Onset}
    
    #If Wave1 offset less than 200ms and a peak exceeding threshold exists between Wave1Offset and 200ms, then create Wave2
    
    if (Experiment[[i]]$Trials[[x]]$Wave1$Offset < (200) & (max(Experiment[[i]]$Trials[[x]]$Data[((Experiment[[i]]$Trials[[x]]$Wave1$Offset*10):(200*10)), "BothSmooth"]) >= Experiment[[i]]$Threshold))
    {
      a <- ceiling(which.max(Experiment[[i]]$Trials[[x]]$Data[((Experiment[[i]]$Trials[[x]]$Wave1$Offset*10):(200*10)), "BothSmooth"])/10 + Experiment[[i]]$Trials[[x]]$Wave1$Offset - 1)
      Experiment[[i]]$Trials[[x]]$Wave2 <- list()
      Experiment[[i]]$Trials[[x]]$Wave2$Peak.ms <- a
      Experiment[[i]]$Trials[[x]]$Wave2$Peak.uv <- max(Experiment[[i]]$Trials[[x]]$Data[((Experiment[[i]]$Trials[[x]]$Wave1$Offset*10):(200*10)), "BothSmooth"])
      Experiment[[i]]$Trials[[x]]$Wave2$Onset <- OnsetAlg(a,x, Experiment[[i]]$Threshold, i)
      Experiment[[i]]$Trials[[x]]$Wave2$Offset <- OffsetAlg(a,x, Experiment[[i]]$Threshold, i)
      if (!is.na(Experiment[[i]]$Trials[[x]]$Wave2$Onset) & !is.na(Experiment[[i]]$Trials[[x]]$Wave2$Offset)) {Experiment[[i]]$Trials[[x]]$Wave2$Duration <- Experiment[[i]]$Trials[[x]]$Wave2$Offset-Experiment[[i]]$Trials[[x]]$Wave2$Onset}
    }
    
    #If Wave1 onset greater than 1ms and a peak exceeding threshold exists between 1ms and Wave1Onset, then create Wave3
    
    if (Experiment[[i]]$Trials[[x]]$Wave1$Onset > 1 & (max(Experiment[[i]]$Trials[[x]]$Data[1:(Experiment[[i]]$Trials[[x]]$Wave1$Onset*10), "BothSmooth"]) >= Experiment[[i]]$Threshold))
    {
      a <- ceiling(which.max(Experiment[[i]]$Trials[[x]]$Data[1:(Experiment[[i]]$Trials[[x]]$Wave1$Onset*10), "BothSmooth"])/10)
      Experiment[[i]]$Trials[[x]]$Wave3 <- list()
      Experiment[[i]]$Trials[[x]]$Wave3$Peak.ms <- a
      Experiment[[i]]$Trials[[x]]$Wave3$Peak.uv <- max(Experiment[[i]]$Trials[[x]]$Data[1:(Experiment[[i]]$Trials[[x]]$Wave1$Onset*10), "BothSmooth"])
      Experiment[[i]]$Trials[[x]]$Wave3$Onset <- OnsetAlg(a,x, Experiment[[i]]$Threshold, i)
      Experiment[[i]]$Trials[[x]]$Wave3$Offset <- OffsetAlg(a,x, Experiment[[i]]$Threshold, i)
      if (!is.na(Experiment[[i]]$Trials[[x]]$Wave3$Onset) & !is.na(Experiment[[i]]$Trials[[x]]$Wave3$Offset)) {Experiment[[i]]$Trials[[x]]$Wave3$Duration <- Experiment[[i]]$Trials[[x]]$Wave3$Offset-Experiment[[i]]$Trials[[x]]$Wave3$Onset}
    }
    
    #If Wave2 or Wave3 are equal to Wave1, delete them.
    
    if (all(unlist(Experiment[[i]]$Trials[[x]]$Wave1) == unlist(Experiment[[i]]$Trials[[x]]$Wave3))) Experiment[[i]]$Trials[[x]]$Wave3 <- NULL
    if (all(unlist(Experiment[[i]]$Trials[[x]]$Wave1) == unlist(Experiment[[i]]$Trials[[x]]$Wave2))) Experiment[[i]]$Trials[[x]]$Wave2 <- NULL
    
    #Wave re-ordering. Reorders waves so Wave1 is Wave closest to expected peak. ----
    #DISABLED
    
    # a <- c(Experiment[[i]]$Trials[[x]]$Wave1$Peak.ms, 
    #        (if(!is.null(Experiment[[i]]$Trials[[x]]$Wave2)) Experiment[[i]]$Trials[[x]]$Wave2$Peak.ms else{1000}), 
    #        (if(!is.null(Experiment[[i]]$Trials[[x]]$Wave3)) Experiment[[i]]$Trials[[x]]$Wave3$Peak.ms else{1000}))
    # 
    # b <- 75  #Expected peak
    # 
    # if(which(abs(a-b)== min(abs(a-b))) == 3) { temp <- Experiment[[i]]$Trials[[x]]$Wave1
    # Experiment[[i]]$Trials[[x]]$Wave1 <- Experiment[[i]]$Trials[[x]]$Wave3
    # Experiment[[i]]$Trials[[x]]$Wave3 <- temp}
    # 
    # if (which(abs(a-b)== min(abs(a-b))) == 2) {temp <- Experiment[[i]]$Trials[[x]]$Wave1
    # Experiment[[i]]$Trials[[x]]$Wave1 <- Experiment[[i]]$Trials[[x]]$Wave2
    # Experiment[[i]]$Trials[[x]]$Wave2 <- temp}
    
    
    #Inter-StimuliInterval(ISI) labelling ----
    
    if (x >=2) { Experiment[[i]]$Trials[[x]]$ISI <- round((Experiment[[i]]$Trials[[x]]$Data[1, "Time"] - Experiment[[i]]$Trials[[x-1]]$Data[1, "Time"]), digits = 0)}
    
    
    #ExclusionReason labelling ----
    
    Experiment[[i]]$Trials[[x]]$Valid <- 1
    Experiment[[i]]$Trials[[x]]$ExclusionReason <- ""
    
    #If stimulator interference exists
    #DISABLED - Very inconsistent
    # if(mean(Experiment[[i]]$Trials[[x]]$Data[1:(5*10), "BothSmooth"]) > (Experiment[[i]]$Threshold)) {Experiment[[i]]$Trials[[x]]$ExclusionReason <- paste(Experiment[[i]]$Trials[[x]]$ExclusionReason, "ReactionDuringTrigger ")
    # 
    # }else{
    
    #If Onset is too late
    if(Experiment[[i]]$Trials[[x]]$Wave1$Onset > (120)) {Experiment[[i]]$Trials[[x]]$ExclusionReason <- paste(Experiment[[i]]$Trials[[x]]$ExclusionReason, "TooLate ")}
    
    #If Onset is too early
    if(Experiment[[i]]$Trials[[x]]$Wave1$Onset < (20)) {Experiment[[i]]$Trials[[x]]$ExclusionReason <- paste(Experiment[[i]]$Trials[[x]]$ExclusionReason, "TooEarly ")}
    
    #If duration is too short
    if(Experiment[[i]]$Trials[[x]]$Wave1$Duration < (10)) {Experiment[[i]]$Trials[[x]]$ExclusionReason <- paste(Experiment[[i]]$Trials[[x]]$ExclusionReason, "TooShort ")}
    
    #If duration is too long
    if(Experiment[[i]]$Trials[[x]]$Wave1$Duration > (120)) {Experiment[[i]]$Trials[[x]]$ExclusionReason <- paste(Experiment[[i]]$Trials[[x]]$ExclusionReason, "TooLong ")}
    
    #If Wave1 does not exceed threshold, then no reaction occured.
    if(Experiment[[i]]$Trials[[x]]$Wave1$Peak.uv < Experiment[[i]]$Threshold) {Experiment[[i]]$Trials[[x]]$ExclusionReason <- "NoReaction"}
    
    #If no exclusion reason, Wave is Valid, else wave is invalid.
    if(Experiment[[i]]$Trials[[x]]$ExclusionReason == "") {Experiment[[i]]$Trials[[x]]$Valid <- 1}else{Experiment[[i]]$Trials[[x]]$Valid <- 0}
    
    
    # AUC Calculation ----
    
    #Standard linear inerpolation AUC calc
    Experiment[[i]]$Trials[[x]]$AUCFULL <- auc(1:(200*10), Experiment[[i]]$Trials[[x]]$Data$BothSmooth, type = "linear")
    
 
    #Constrained AUC. AUC only between onset/offset value.
    Experiment[[i]]$Trials[[x]]$AUCCONS <- auc((Experiment[[i]]$Trials[[x]]$Wave1$Onset*10):(Experiment[[i]]$Trials[[x]]$Wave1$Offset*10),
                                            Experiment[[i]]$Trials[[x]]$Data[(Experiment[[i]]$Trials[[x]]$Wave1$Onset*10):(Experiment[[i]]$Trials[[x]]$Wave1$Offset*10), "BothSmooth"],
                                            type = "linear")
    

    cat( "Participant: ", i, " - Trial: ", x, " \r")
    flush.console()
    
    
  }
  
  cat( "Participant: ", i, " - Overall Processing \r")
  flush.console()
  
  #Overall aggregating - Prep ----
  
  Experiment[[i]]$TrialData$Overall <- data.frame(matrix(nrow = 200*10, ncol = 12))
  names(Experiment[[i]]$TrialData$Overall) <- c("Timepoint", "All", "Distance1", "Distance2", "Distance3", "Distance4", "Left", "Right", "Contra", "Ipsil", "Block1", "Block2")
  
  
  #Timepoint
  
  Experiment[[i]]$TrialData$Overall$Timepoint <- seq( from = 0.1, by = 0.1, to =200)
  
  
  #All aggrgating
  
  Experiment[[i]]$TrialData$Overall$All <- rowMeans(ExtractVal("1==1", "f$Data$BothSmooth", i))
  
  
  #Left/Right aggregating ----
  
  Experiment[[i]]$TrialData$Overall$Left <- rowMeans(ExtractVal("1==1", "f$Data$LeftSmooth", i))
  Experiment[[i]]$TrialData$Overall$Right <- rowMeans(ExtractVal("1==1", "f$Data$RightSmooth", i))
  
  
  #Distance aggregating ----
  
  Experiment[[i]]$TrialData$Overall$Distance1 <- rowMeans(ExtractVal("f$Distance == 1", "f$Data$BothSmooth", i))
  
  Experiment[[i]]$TrialData$Overall$Distance2 <- rowMeans(ExtractVal("f$Distance == 2", "f$Data$BothSmooth", i))
  
  Experiment[[i]]$TrialData$Overall$Distance3 <- rowMeans(ExtractVal("f$Distance == 3", "f$Data$BothSmooth", i))
  
  Experiment[[i]]$TrialData$Overall$Distance4 <- rowMeans(ExtractVal("f$Distance == 4", "f$Data$BothSmooth", i))
  
  
  #Ipsil/Contra aggregating ----
  
  if (length(Experiment[[i]]$Trials) > 32) {
    
    Experiment[[i]]$TrialData$Overall$Ipsil <- rowMeans(cbind(ExtractVal("f$Hand == 1", "f$Data$LeftSmooth", i),ExtractVal("f$Hand == 2", "f$Data$RightSmooth", i)))
    
    Experiment[[i]]$TrialData$Overall$Contra <- rowMeans(cbind(ExtractVal("f$Hand == 2", "f$Data$LeftSmooth", i),ExtractVal("f$Hand == 1", "f$Data$RightSmooth", i)))
    
    
    #Block1/Block2 aggregating ----
    
    Experiment[[i]]$TrialData$Overall$Block1 <- rowMeans(ExtractVal("f$Block == 1", "f$Data$BothSmooth", i))
    
    Experiment[[i]]$TrialData$Overall$Block2 <- rowMeans(ExtractVal("f$Block == 2", "f$Data$BothSmooth", i))
    
  } else { Experiment[[i]]$TrialData$Overall$Block2 <- Experiment[[i]]$TrialData$Overall$Block1 <- Experiment[[i]]$TrialData$Overall$Contra <- Experiment[[i]]$TrialData$Overall$Ipsil <- 0}
  
  
  #AUC collecting and scaling ----
  
  #Constructs data frame of Stimuli id, sub-stimuli id for repeated measures(for both "all" and "valid" tests)
  #Includes Distance, trial validity, raw AUC, Scaled(All) AUC and Scaled (valid) AUCs
  Experiment[[i]]$TrialData$AUC <- data.frame(Stimuli = as.factor(seq(from = 1, to = length(Experiment[[i]]$Trials), by = 1)),
                                              SubStimuli.All = 1,
                                              SubStimuli.Valid = 1,
                                              Distance = as.factor(sapply(Experiment[[i]]$Trials, function(f) f$Distance)),
                                              Valid = sapply(Experiment[[i]]$Trials, function(f) f$Valid),
                                              Raw.Full = sapply(Experiment[[i]]$Trials, function(f) f$AUCFULL),
                                              Raw.Cons = sapply(Experiment[[i]]$Trials, function(f) f$AUCCONS),
                                              Scaled.All.Full = sapply(Experiment[[i]]$Trials, function(f) f$AUCFULL),
                                              Scaled.All.Cons = sapply(Experiment[[i]]$Trials, function(f) f$AUCCONS),
                                              Scaled.Valid.Full = sapply(Experiment[[i]]$Trials, function(f) f$AUCFULL),
                                              Scaled.Valid.Cons = sapply(Experiment[[i]]$Trials, function(f) f$AUCCONS))
  
  
  #Removing invalid AUC in appropriate colomns
  
  if(sum(Experiment[[i]]$TrialData$AUC$Valid) < nrow(Experiment[[i]]$TrialData$AUC)) {
    
  Experiment[[i]]$TrialData$AUC[Experiment[[i]]$TrialData$AUC$Valid == 0,]$Scaled.Valid.Full <- NA
  Experiment[[i]]$TrialData$AUC[Experiment[[i]]$TrialData$AUC$Valid == 0,]$Scaled.Valid.Cons <- NA
  Experiment[[i]]$TrialData$AUC[Experiment[[i]]$TrialData$AUC$Valid == 0,]$SubStimuli.Valid <- NA
  
  }
  
  #Scaling all
  Experiment[[i]]$TrialData$AUC$Scaled.All.Full <- scale(Experiment[[i]]$TrialData$AUC$Scaled.All.Full)
  Experiment[[i]]$TrialData$AUC$Scaled.All.Cons <- scale(Experiment[[i]]$TrialData$AUC$Scaled.All.Cons)
  
  #Scaling valid
  Experiment[[i]]$TrialData$AUC$Scaled.Valid.Full <- scale(Experiment[[i]]$TrialData$AUC$Scaled.Valid.Full)
  Experiment[[i]]$TrialData$AUC$Scaled.Valid.Cons <- scale(Experiment[[i]]$TrialData$AUC$Scaled.Valid.Cons)
  
  #Sub-id for "All" test
  Experiment[[i]]$TrialData$AUC$SubStimuli.All <- with(Experiment[[i]]$TrialData$AUC, ave(rep(1, nrow(Experiment[[i]]$TrialData$AUC)), Distance, FUN = seq_along))
  
  
  
  #Sub-id for valid tests. Requires a temp dataframe to re-label sub-id with removed invalid trials, then copying in new corrected column
  temp <- Experiment[[i]]$TrialData$AUC

  temp$Distance <- ifelse(temp$Valid == 0, NA, temp$Distance)
  
  Experiment[[i]]$TrialData$AUC$SubStimuli.Valid <- ifelse(temp$Valid == 0, NA, with(temp, ave(rep(1, nrow(temp)), Distance, FUN = seq_along)))
  
  
  #Constructing mean AUC table
  Experiment[[i]]$TrialData$AUCMean <- as.data.frame(t(data.frame(a = rowMeans(t(sapply(Experiment[[i]]$TrialData$AUC[Experiment[[i]]$TrialData$AUC$Distance == 1,], as.numeric)), na.rm =TRUE),
                                                                 b = rowMeans(t(sapply(Experiment[[i]]$TrialData$AUC[Experiment[[i]]$TrialData$AUC$Distance == 2,], as.numeric)), na.rm =TRUE),
                                                                 c = rowMeans(t(sapply(Experiment[[i]]$TrialData$AUC[Experiment[[i]]$TrialData$AUC$Distance == 3,], as.numeric)), na.rm =TRUE),
                                                                 d = rowMeans(t(sapply(Experiment[[i]]$TrialData$AUC[Experiment[[i]]$TrialData$AUC$Distance == 4,], as.numeric)), na.rm =TRUE))))[,4:11]
  
  
  
  #Contrast analysis
  
  Contrasts <- data.frame(Linear = c(-3,-1,1,3),
                          Small = c(-1,-1,-1,3),
                          LargeR = c(-1.5,-1.5,0.5,2.5),
                          LargeS = c(-1,-1,1,1),
                          ELarge = c(-3,1,1,1))
  
  #tryCatch( XXXXXX ,error = function (e) paste("StupidProgram"))
  #Seems to stop random errors
  
  tryCatch(Experiment[[i]]$TrialData$Contrast$Between.All.Full <- summary(contrast(emmeans(aov_ez(id = "Stimuli", dv = "Scaled.All.Full", data = Experiment[[i]]$TrialData$AUC, between = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None")),error = function (e) paste("StupidProgram"))
  
  tryCatch(Experiment[[i]]$TrialData$Contrast$Between.All.Cons <- summary(contrast(emmeans(aov_ez(id = "Stimuli", dv = "Scaled.All.Cons", data = Experiment[[i]]$TrialData$AUC, between = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None")),error = function (e) paste("StupidProgram"))
  
  tryCatch(Experiment[[i]]$TrialData$Contrast$Within.All.Full <- summary(contrast(emmeans(aov_ez(id = "SubStimuli.All", dv = "Scaled.All.Full", data = Experiment[[i]]$TrialData$AUC, within = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None")),error = function (e) paste("StupidProgram"))
  
  tryCatch(Experiment[[i]]$TrialData$Contrast$Within.All.Cons <- summary(contrast(emmeans(aov_ez(id = "SubStimuli.All", dv = "Scaled.All.Cons", data = Experiment[[i]]$TrialData$AUC, within = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None")),error = function (e) paste("StupidProgram"))
  
  tryCatch(Experiment[[i]]$TrialData$Contrast$Between.Valid.Full <- summary(contrast(emmeans(aov_ez(id = "Stimuli", dv = "Scaled.Valid.Full", data = Experiment[[i]]$TrialData$AUC, between = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None")),error = function (e) paste("StupidProgram"))
  
  tryCatch(Experiment[[i]]$TrialData$Contrast$Between.Valid.Cons <- summary(contrast(emmeans(aov_ez(id = "Stimuli", dv = "Scaled.Valid.Cons", data = Experiment[[i]]$TrialData$AUC, between = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None")),error = function (e) paste("StupidProgram"))
  
  tryCatch(Experiment[[i]]$TrialData$Contrast$Within.Valid.Full <- summary(contrast(emmeans(aov_ez(id = "SubStimuli.All", dv = "Scaled.Valid.Full", data = Experiment[[i]]$TrialData$AUC, within = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None")),error = function (e) paste("StupidProgram"))
  
  tryCatch(Experiment[[i]]$TrialData$Contrast$Within.Valid.Cons <- summary(contrast(emmeans(aov_ez(id = "SubStimuli.All", dv = "Scaled.Valid.Cons", data = Experiment[[i]]$TrialData$AUC, within = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None")),error = function (e) paste("StupidProgram"))
  
  
  #Validity table
  
  Experiment[[i]]$TrialData$Validity <- data.frame(V1=c("Valid", "Invalid", "TooEarly", "TooLate", "TooShort", "TooLong", "NoReaction"),
                                                   V2=c(sum(ExtractVal("1==1", "f$Valid", i)),
                                                        length(Experiment[[i]]$Trials) - sum(ExtractVal("1==1", "f$Valid", i)),
                                                        sum(sapply(Experiment[[i]]$Trials, function(f) if(grepl("TooEarly",f$ExclusionReason)) 1 else{0})),
                                                        sum(sapply(Experiment[[i]]$Trials, function(f) if(grepl("TooLate",f$ExclusionReason)) 1 else{0})),
                                                        sum(sapply(Experiment[[i]]$Trials, function(f) if(grepl("TooShort",f$ExclusionReason)) 1 else{0})),
                                                        sum(sapply(Experiment[[i]]$Trials, function(f) if(grepl("TooLong",f$ExclusionReason)) 1 else{0})),
                                                        sum(sapply(Experiment[[i]]$Trials, function(f) if(grepl("NoReaction",f$ExclusionReason)) 1 else{0}))))
  

  #Average table of Mean/sd Onset, Peak(ms), Offset, Duration and Peak(uV)
  
  temp <- as.data.frame(matrix(ncol=2, nrow=5))
  colnames(temp) <- c("Mean", "SD")
  rownames(temp) <- c("Onset(ms)", "Peak(ms)", "Offset(ms)", "Duration(ms)", "Peak(uV)")
  
  temp$Mean <- c(round(rowMeans(ExtractVal("f$Valid == 1","f$Wave1$Onset", i)), digits = 3),
              round(rowMeans(ExtractVal("f$Valid == 1","f$Wave1$Peak.ms", i)), digits = 3),
              round(rowMeans(ExtractVal("f$Valid == 1","f$Wave1$Offset", i)), digits = 3),
              round(rowMeans(ExtractVal("f$Valid == 1","f$Wave1$Duration", i)), digits = 3),
              round(rowMeans(ExtractVal("f$Valid == 1","f$Wave1$Peak.uv", i)), digits = 3))
  
  temp$SD <- c(round(sd(ExtractVal("f$Valid == 1","f$Wave1$Onset", i)), digits = 4),
            round(sd(ExtractVal("f$Valid == 1","f$Wave1$Peak.ms", i)), digits = 4),
            round(sd(ExtractVal("f$Valid == 1","f$Wave1$Offset", i)), digits = 4),
            round(sd(ExtractVal("f$Valid == 1","f$Wave1$Duration", i)), digits = 4),
            round(sd(ExtractVal("f$Valid == 1","f$Wave1$Peak.uv", i)), digits = 4))
  
  Experiment[[i]]$TrialData$Averages <- temp
  
  

  # Participant level plotting ----
  
  cat( "Participant: ", i, " - Plotting           \r")
  flush.console()
  
  if (!(plot == 1)) next
  
  pdf(paste0("Participant ", i , ".pdf"),width = 10, height = 10, onefile = TRUE)
  xrange = seq(from = 1/10, to = 200, by = 1/10)
  ymax1 <- max(max(ExtractVal("1==1", "f$Data$BothSmooth", i)), 30)
  #ymax1 <- 30
  graph.list <- list()

  #Creates trial graphs in a 4x4 formation over multiple pages
  for (x in 1:length(Experiment[[i]]$Trials))
  {
    suppressWarnings(
    graph.list[x] <- grob(ggplot(Experiment[[i]]$Trials[[x]]$Data) + 
                              geom_line(aes(x=Timepoint, y=BothSmooth), color = "black") +
                              geom_line(aes(x=Timepoint, y=LeftSmooth), color = "red") +
                              geom_line(aes(x=Timepoint, y=RightSmooth), color = "green") +
                              geom_hline(yintercept = Experiment[[i]]$Threshold, color = "orange") +
                              annotate("rect", ymin = 0, ymax =Experiment[[i]]$Trials[[x]]$Wave1$Peak.uv, xmin = Experiment[[i]]$Trials[[x]]$Wave1$Onset, xmax = Experiment[[i]]$Trials[[x]]$Wave1$Offset, alpha = .2) +
                              annotate("text", label = paste("Trial: ", x , " - Distance: ", Experiment[[i]]$Trials[[x]]$Distance, "\n" ,
                                                             Experiment[[i]]$Trials[[x]]$Wave1$Onset , "-", Experiment[[i]]$Trials[[x]]$Wave1$Offset, "ms - Dur: ", Experiment[[i]]$Trials[[x]]$Wave1$Duration, "\n",
                                                             "ISI:", Experiment[[i]]$Trials[[x]]$ISI, "-Status:", Experiment[[i]]$Trials[[x]]$ExclusionReason ), 
                                       y=Inf, x=Inf, vjust=1, hjust=1, size = 3.5) +
                              scale_y_continuous(expand = c(0,0), limits = c(0,ymax1))+
                              scale_x_continuous(expand = c(0,0))+
                              ylab("")+
                              xlab("")+
                              theme_classic()))
      
    
    
  }
  
  #Had issues of plotting over various pages. Yes there is a package for it, no it didnt work.
  
  grid.draw(arrangeGrob(grobs = graph.list[1:16]))
  grid.newpage()
  
  grid.draw(arrangeGrob(grobs = graph.list[17:32]))
  grid.newpage()
  
  if(length(Experiment[[i]]$Trials)>32){
    
  grid.draw(arrangeGrob(grobs = graph.list[33:48]))
  grid.newpage()
  
  grid.draw(arrangeGrob(grobs = graph.list[49:(length(Experiment[[i]]$Trials))]))
  grid.newpage()}
  
  
  #Plots the average waveform for each hand distance, then an overlayed one of all 4.
  ymax2 <- c(0,max((1.3* max(Experiment[[i]]$TrialData$Overall[, 2:12])), 20))
  
  graph.list <-list()
  
  
  suppressWarnings({
  graph.list[1] <- grob(ggplot(Experiment[[i]]$TrialData$Overall, aes(x=Timepoint, y = Distance1)) + 
                            geom_line(color = "red") +
                            scale_y_continuous(expand = c(0,0), limits = c(ymax2))+
                            scale_x_continuous(expand = c(0,0))+
                            ylab("")+
                            xlab("")+
                            ggtitle("Distance 1")+
                            theme_classic())
  
  graph.list[2] <- grob(ggplot(Experiment[[i]]$TrialData$Overall, aes(x=Timepoint, y = Distance2)) + 
                            geom_line(color = "green") +
                            scale_y_continuous(expand = c(0,0), limits = c(ymax2))+
                            scale_x_continuous(expand = c(0,0))+
                            ylab("")+
                            xlab("")+
                            ggtitle("Distance 2")+
                            theme_classic())
  
  graph.list[3] <- grob(ggplot(Experiment[[i]]$TrialData$Overall, aes(x=Timepoint, y = Distance3)) + 
                            geom_line(color = "blue") +
                            scale_y_continuous(expand = c(0,0), limits = c(ymax2))+
                            scale_x_continuous(expand = c(0,0))+
                            ylab("")+
                            xlab("")+
                            ggtitle("Distance 3")+
                            theme_classic())
  

  graph.list[4] <- grob(ggplot(Experiment[[i]]$TrialData$Overall, aes(x=Timepoint, y = Distance4)) + 
                            geom_line(color = "orange") +
                            scale_y_continuous(expand = c(0,0), limits = c(ymax2))+
                            scale_x_continuous(expand = c(0,0))+
                            ylab("")+
                            xlab("")+
                            ggtitle("Distance 4")+
                            theme_classic())
  
  graph.list[5] <- grob(ggplot(Experiment[[i]]$TrialData$Overall) + 
                            geom_line(aes(x=Timepoint, y = Distance1),color = "red") +
                            geom_line(aes(x=Timepoint, y = Distance2),color = "green") +
                            geom_line(aes(x=Timepoint, y = Distance3),color = "blue") +
                            geom_line(aes(x=Timepoint, y = Distance4),color = "orange") +
                            scale_y_continuous(expand = c(0,0), limits = c(ymax2))+
                            scale_x_continuous(expand = c(0,0))+
                            ylab("")+
                            xlab("")+
                            ggtitle("Distance 1,2,3,4")+
                            theme_classic())})
  
  grid.draw(arrangeGrob(grobs = graph.list, layout_matrix = rbind(c(1,2,3,4),c(5,5,5,5),c(5,5,5,5))))
  grid.newpage()

  
  
  # AUC plots of Full vs Cons. (Raw, All and Valid) then Ipsil vs Contra, Left vs Right(eyes), Block1 vs Block2
  
  graph.list <-list()
  
  suppressWarnings({
    graph.list[1] <- grob(ggplot(Experiment[[i]]$TrialData$AUCMean) + 
                            geom_line(aes(x=c(1:4), y=Raw.Full), color = "red") +
                            geom_line(aes(x=c(1:4), y=Raw.Cons), color = "green") +
                            ylab("")+
                            xlab("")+
                            ggtitle("Raw AUC")+
                            theme_classic())
      
    
    graph.list[2] <- grob(ggplot(Experiment[[i]]$TrialData$AUCMean) + 
                            geom_line(aes(x=c(1:4), y=Scaled.All.Full), color = "red") +
                            geom_line(aes(x=c(1:4), y=Scaled.All.Cons), color = "green") +
                            ylab("")+
                            xlab("")+
                            ggtitle("Scaled ALL AUC")+
                            theme_classic())
    
    graph.list[3] <- grob(ggplot(Experiment[[i]]$TrialData$AUCMean) + 
                            geom_line(aes(x=c(1:4), y=Scaled.Valid.Full), color = "red") +
                            geom_line(aes(x=c(1:4), y=Scaled.Valid.Cons), color = "green") +
                            ylab("")+
                            xlab("")+
                            ggtitle("Scaled VALID AUC")+
                            theme_classic())
    
    graph.list[4] <- grob(ggplot(Experiment[[i]]$TrialData$Overall) + 
                            geom_line(aes(x=Timepoint, y = Ipsil), color = "red") +
                            geom_line(aes(x=Timepoint, y = Contra), color = "green") +
                            ylab("")+
                            xlab("")+
                            scale_y_continuous(expand = c(0,0), limits = c(ymax2))+
                            scale_x_continuous(expand = c(0,0))+
                            ggtitle("Ipsilateral vs Contralateral")+
                            theme_classic())
    
    graph.list[5] <- grob(ggplot(Experiment[[i]]$TrialData$Overall) + 
                            geom_line(aes(x=Timepoint, y = Left), color = "red") +
                            geom_line(aes(x=Timepoint, y = Right), color = "green") +
                            ylab("")+
                            xlab("")+
                            scale_y_continuous(expand = c(0,0), limits = c(ymax2))+
                            scale_x_continuous(expand = c(0,0))+
                            ggtitle("Left vs Right")+
                            theme_classic())
    
    graph.list[6] <- grob(ggplot(Experiment[[i]]$TrialData$Overall) + 
                            geom_line(aes(x=Timepoint, y = Block1), color = "red") +
                            geom_line(aes(x=Timepoint, y = Block2), color = "green") +
                            ylab("")+
                            xlab("")+
                            scale_y_continuous(expand = c(0,0), limits = c(ymax2))+
                            scale_x_continuous(expand = c(0,0))+
                            ggtitle("Block1 vs Block2")+
                            theme_classic())})
  
  grid.draw(arrangeGrob(grobs = graph.list, nrow= 2, ncol =3))
  grid.newpage()
  
  
  
  #Habituation plots
  #Gradient plot of each trial over 200ms  to show changes over time.
  #Also includes scatterplot+regression of peak.uv for each block
  
  temp <- melt(sapply(Experiment[[i]]$Trials, function(f){f$Data$BothSmooth}))
  temp2 <- melt(sapply(Experiment[[i]]$Trials, function(f){f$Wave1$Peak.uv}))
  temp2$value2 <- as.numeric(rownames(temp2))
  
  graph.list <- list()
  
  suppressWarnings({
  graph.list[1] <- grob(ggplot(temp, aes(Var1,Var2,fill = value)) +
                          geom_tile() +
                          scale_fill_gradient(low = "white", high = "red") +
                          scale_x_continuous(limits=c(0, 200*10), expand = c(0, 0)) +
                          scale_y_continuous(breaks = seq(from=0,by=4,to=64)) +
                          theme(legend.position="none") +
                          theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                                 axis.title.y=element_blank()))
  
  graph.list[2] <- grob(ggplot(temp2[33:64,], aes(value2,value)) +
                           geom_point() +
                           geom_smooth(method = "lm") +
                           scale_x_continuous(breaks = seq(from=32,by=4,to=64)) +
                           theme(axis.title.x=element_blank(),
                                 axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()))
  
  graph.list[3] <- grob(ggplot(temp2[1:32,], aes(value2,value)) +
                           geom_point() +
                           geom_smooth(method = "lm") +
                           scale_x_continuous(breaks = seq(from=0,by=4,to=32)) +
                           theme(axis.title.x=element_blank(),
                                 axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()))})
    
  grid.draw(arrangeGrob(grobs=graph.list,layout_matrix = rbind(c(1,2),c(1,3))))
  grid.newpage()
  
  
  #Displays Validity table, averages table and Contrast analysis tables.
  
  graph.list <- list()

  graph.list[[1]] <- tableGrob(Experiment[[i]]$TrialData$Validity, rows = NULL, cols = NULL)
  
  graph.list[[2]] <- tableGrob(Experiment[[i]]$TrialData$Averages)
  
  if (!is.null(Experiment[[i]]$TrialData$Contrast$Between.All.Full)){
    graph.list[[3]] <- AddTitle("Between All Full", Experiment[[i]]$TrialData$Contrast$Between.All.Full[,c(1,4,6)])}
  
  if (!is.null(Experiment[[i]]$TrialData$Contrast$Between.All.Cons)){
    graph.list[[4]] <- AddTitle("Between All Cons", Experiment[[i]]$TrialData$Contrast$Between.All.Cons[,c(1,4,6)])}
  
  if (!is.null(Experiment[[i]]$TrialData$Contrast$Within.All.Full)){
    graph.list[[5]] <- AddTitle("Within All Full", Experiment[[i]]$TrialData$Contrast$Within.All.Full[,c(1,4,6)])}
  
  if (!is.null(Experiment[[i]]$TrialData$Contrast$Within.All.Cons)){
    graph.list[[6]] <- AddTitle("Within All Cons", Experiment[[i]]$TrialData$Contrast$Within.All.Cons[,c(1,4,6)])}
  
  if (!is.null(Experiment[[i]]$TrialData$Contrast$Between.Valid.Full)){
    graph.list[[7]] <- AddTitle("Between Valid Full", Experiment[[i]]$TrialData$Contrast$Between.Valid.Full[,c(1,4,6)])}
  
  if (!is.null(Experiment[[i]]$TrialData$Contrast$Between.Valid.Cons)){
    graph.list[[8]] <- AddTitle("Between Valid Cons", Experiment[[i]]$TrialData$Contrast$Between.Valid.Cons[,c(1,4,6)])}
  
  if (!is.null(Experiment[[i]]$TrialData$Contrast$Within.Valid.Full)){
    graph.list[[9]] <- AddTitle("Withn Valid Full", Experiment[[i]]$TrialData$Contrast$Within.Valid.Full[,c(1,4,6)])}
  
  if (!is.null(Experiment[[i]]$TrialData$Contrast$Within.Valid.Cons)){
    graph.list[[10]] <- AddTitle("Within Valid Cons", Experiment[[i]]$TrialData$Contrast$Within.Valid.Cons[,c(1,4,6)])}
  
  graph.list<- lapply(graph.list, function(f) if(is.null(f)) tableGrob(data.frame("N/A"), rows = NULL, cols = NULL) else f)


  grid.draw(arrangeGrob(grobs=graph.list, layout_matrix = cbind(c(1,3,4,5,6),c(2,7,8,9,10)), padding = 2, left = TRUE))

  dev.off()
  
}


#graph.list <- Overall <- temp <- temp2 <- a <- b <- i <- ID <- x <- Contrasts <- NULL
