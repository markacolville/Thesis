#http://www.clinicalgaitanalysis.com/faq/emgonset.html
#opinions on onset/offset analysis

#https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
#ggplot cheatsheet

#Functions ----

#Onset function -- Progresses backwards from peak through Bothsmooth data until threshold is reached and returns its position.

OnsetAlg <- function(a, x, Threshold) {  for (a in a:1) {if ((Data3[[x]]$Data[a*SampRate, "BothSmooth"] <= Threshold)  | (a <= 1)) {return(a)}}}

#Offset function -- Progesses forwards from peak through Bothsmmoth data until threshold(1.5sd above baseline) is reached and returns it

OffsetAlg <- function(a, x, Threshold) {  for (a in a:200) {if ((Data3[[x]]$Data[a*SampRate, "BothSmooth"] <= Threshold) | (a >= (200))){return(a)}}}


#Extract Value function -- Used to extract particular matching values from Data3.
#Two paramaters, first passes a condition to check eg "f$Distance == 1" includes all stimuli data that is Distance==1. Default is 1==1, therefore includes all
#Second parameter refers to the data type to return eg ""f$Data$BothSmooth" returns all qualifying stimulis "BothSmooth" data columns.
#Also applies a column name
#If it fails and creates no data, it returns a dataframe containing a single '0'

ExtractVal <- function(condition="1==1", target) {
  
  a <- sapply(Data3, function(f){if(eval(parse(text=condition))) {eval(parse(text=target))}})
  a <- as.data.frame(a[!sapply(a, is.null)])
  if (!(nrow(a) == 0)){ colnames(a) <- substring(target, 3)
  }else{a[1,1] <- 0}
  return(a)
}

# Init ----

library(data.table)
library(zoo)
library("MESS")
library(ggplot2)
library(gtable)
library(grid)
library(gplots)
library(afex)
library(knitr)


# Data import# ----

# Lets you choose multiple files
filename <- choose.files(filters = Filters[c("txt"),])

#Starts time recording for program debugging
start.time <- Sys.time()

#Takes list of chosen filenames, reads.tables them all into a list of data-frames, then rbinds them together into one
Data1 <- do.call(rbind,lapply(filename, function(f){read.delim(f, col.names = paste0("V", seq_len(5)), header = FALSE, skip = 5, na.strings = "", stringsAsFactors = TRUE)}))

#Captures filename of first file
filename <- basename(filename[1])


#Filename processing# ----

filename <- sub('\\.txt', '', filename)
filename <- as.list(unlist(strsplit(filename, " - ")))

#Capures components of filename. Filename format is seperated by " - "
#First part "Participant #", labbeling whos data it is
#Second part "RS" for responder, or "NR" for non-responder
#Third part "L" or R", left or right hand for starting hand
#Forth part "T" for tested, "C" for control
#Fifth part for "Block 1" or "Block 2" (not used)
#Fifth part "1k" for 1k sampling rate, "10k" for 10k sampling rate

Participant <- filename[[1]]
if (filename[2] == "RS") {Responder <- 1}else{Responder <- 2}
if (filename[3] == "L") {Beginhand <- 1}else{Beginhand <- 2}
if (filename[4] == "T") {Tested <- 1}else{Tested <- 2}
if (filename[6] == "1k") {SampRate <- 1}else{SampRate <- 10}


#Old. Fixes NA values and removes excess cols

#suppressWarnings(Data1 <- Data1[!is.na(as.numeric(Data1$V3)),])
#Data1 <- Data1[c(1, 2, 3, 4, ncol(Data1))]



names(Data1) <- c("Time", "Marker", "LeftRaw", "RightRaw", "Distance") #Coloumn labelling

Data2 <- Data1[which(Data1$Marker >= 1),] #Grab marker values ----

#Excludes excess marker values
#This cycles throguh the marker values in reverse order, excluding the later multiples within 10 seconds.

for (x in nrow(Data2):2){ if (abs(Data2[x, "Time"] - Data2[x-1,"Time"]) <= 10) Data2 <<- Data2[-c(x),]}


#Data structuring. This creates a list of each Stimuli, each Stimuli contains a Dataframe of 200ms of data and relevant Stimuli level info.

Data2 <- rbind(Data2,subset(Data1,!(is.na(Distance))))
Data2 <- Data2[order(as.integer(row.names(Data2))), ]

#Throw mismatch error if number of Stim Markers != Distance labels
if (nrow(subset(Data2, is.na(Distance))) != nrow(subset(Data2, !is.na(Distance)))) {print("ERROR::::DISTANCE MARKER/STIM MISMATCH::::ERROR")}

x <- 1
Data3 <- list()

for (row in 1:(nrow(Data2))){
  
  if (!is.na(Data2[row, "Distance"])) 
    
  { dist <- Data2[row, "Distance"]
  if (grepl("-Far", dist)) dist <- 1
  if (grepl(" Far", dist)) dist <- 2
  if (grepl(" Near", dist)) dist <- 3
  if (grepl("-Near", dist)) dist <- 4
  
  }else{
    
    c <- as.integer(rownames(Data2[row, ]))
    
    Data3[[x]] <- list()
    
    #Extract -100ms to +300ms of data around stim trigger
    
    Data3[[x]]$Data <- Data1[(c-(100*SampRate)):(c+(300*SampRate - 1)), 1:4] 
    
    #Rectify
    
    Data3[[x]]$Data$LeftRect <-  round(abs(Data3[[x]]$Data$LeftRaw - mean(Data3[[x]]$Data$LeftRaw)), digits = 2)
    Data3[[x]]$Data$RightRect <- round(abs(Data3[[x]]$Data$RightRaw - mean(Data3[[x]]$Data$RightRaw)), digits = 2)
    
    #Smooth
    
    if (SampRate == 1) {smoothfunc <- 15}else {smoothfunc <- 151}
    Data3[[x]]$Data$LeftSmooth <-round(rollmedian( Data3[[x]]$Data$LeftRect, smoothfunc, fill = 0), digits = 2)
    Data3[[x]]$Data$RightSmooth <-round(rollmedian( Data3[[x]]$Data$RightRect, smoothfunc, fill = 0), digits = 2)
    
    #Average across both sides
    
    Data3[[x]]$Data$BothSmooth <-  round(((Data3[[x]]$Data$LeftSmooth + Data3[[x]]$Data$RightSmooth) / 2), digits = 2)
    
    #Threshold
    
    Data3[[x]]$Threshold <- mean(Data3[[x]]$Data$BothSmooth[1:1000]) + 3 * sd(Data3[[x]]$Data$BothSmooth[1:1000])
    
    #Adding timepoints
    
    Data3[[x]]$Data$Timepoint <- seq(from = (1/SampRate-100), to = 300, by = 1/SampRate)
    
    #Removing excess
    
    Data3[[x]]$Data <- Data3[[x]]$Data[((100*SampRate)+1):(300*SampRate),]
    
    
    if (x <= 32)  {Data3[[x]]$Block <- 1; Data3[[x]]$Hand <- Beginhand}
    else          {Data3[[x]]$Block <- 2; if (Beginhand == 1) {Data3[[x]]$Hand <- 2} else {Data3[[x]]$Hand <- 1}}
    
    Data3[[x]]$Distance <- dist
    
    #Wave labelling
    
    a <- ceiling(which.max(Data3[[x]]$Data[1:(150*SampRate), "BothSmooth"])/SampRate)
    
    Data3[[x]]$Wave1 <- list()
    Data3[[x]]$Wave1$Peak.ms <- a
    Data3[[x]]$Wave1$Peak.uv <- max(Data3[[x]]$Data[1:(150*SampRate), "BothSmooth"])
    Data3[[x]]$Wave1$Onset <- OnsetAlg(a,x, Data3[[x]]$Threshold)
    Data3[[x]]$Wave1$Offset <- OffsetAlg(a,x, Data3[[x]]$Threshold)
    
    #If Wave1 onset and offset exist, calculate duration
    
    if (!is.na(Data3[[x]]$Wave1$Onset) & !is.na(Data3[[x]]$Wave1$Offset)) {Data3[[x]]$Wave1$Duration <- Data3[[x]]$Wave1$Offset-Data3[[x]]$Wave1$Onset}
    
    #If Wave1 offset less than 200 and a peak exceeding threshold exists between Wave1Offset 200, then make wave2
    
    if (Data3[[x]]$Wave1$Offset < (200) & (max(Data3[[x]]$Data[((Data3[[x]]$Wave1$Offset*SampRate):(200*SampRate)), "BothSmooth"]) >= Data3[[x]]$Threshold))
    {
      a <- ceiling(which.max(Data3[[x]]$Data[((Data3[[x]]$Wave1$Offset*SampRate):(200*SampRate)), "BothSmooth"])/SampRate + Data3[[x]]$Wave1$Offset - 1)
      Data3[[x]]$Wave2 <- list()
      Data3[[x]]$Wave2$Peak.ms <- a
      Data3[[x]]$Wave2$Peak.uv <- max(Data3[[x]]$Data[((Data3[[x]]$Wave1$Offset*SampRate):(200*SampRate)), "BothSmooth"])
      Data3[[x]]$Wave2$Onset <- OnsetAlg(a,x, Data3[[x]]$Threshold)
      Data3[[x]]$Wave2$Offset <- OffsetAlg(a,x, Data3[[x]]$Threshold)
      if (!is.na(Data3[[x]]$Wave2$Onset) & !is.na(Data3[[x]]$Wave2$Offset)) {Data3[[x]]$Wave2$Duration <- Data3[[x]]$Wave2$Offset-Data3[[x]]$Wave2$Onset}
    }
    
    #If Wave1 onset greater than 1 and a peak exceeding threshold exists between 1 and Wave1Onset, then make wave3
    
    if (Data3[[x]]$Wave1$Onset > 1 & (max(Data3[[x]]$Data[1:(Data3[[x]]$Wave1$Onset*SampRate), "BothSmooth"]) >= Data3[[x]]$Threshold))
    {
      a <- ceiling(which.max(Data3[[x]]$Data[1:(Data3[[x]]$Wave1$Onset*SampRate), "BothSmooth"])/SampRate)
      Data3[[x]]$Wave3 <- list()
      Data3[[x]]$Wave3$Peak.ms <- a
      Data3[[x]]$Wave3$Peak.uv <- max(Data3[[x]]$Data[1:(Data3[[x]]$Wave1$Onset*SampRate), "BothSmooth"])
      Data3[[x]]$Wave3$Onset <- OnsetAlg(a,x, Data3[[x]]$Threshold)
      Data3[[x]]$Wave3$Offset <- OffsetAlg(a,x, Data3[[x]]$Threshold)
      if (!is.na(Data3[[x]]$Wave3$Onset) & !is.na(Data3[[x]]$Wave3$Offset)) {Data3[[x]]$Wave3$Duration <- Data3[[x]]$Wave3$Offset-Data3[[x]]$Wave3$Onset}
    }
    
    #Wave re-ordering ----
    
    a <- c(Data3[[x]]$Wave1$Peak.ms, Data3[[x]]$Wave3$Peak.ms)
    b <- 80  #Expected peak
    
    if (which(abs(a-b)== min(abs(a-b))) == 2) { temp <- Data3[[x]]$Wave1
    Data3[[x]]$Wave1 <- Data3[[x]]$Wave3
    Data3[[x]]$Wave3 <- temp}
    
    
    #ISI labelling ----
    
    if (x >=2) { Data3[[x]]$ISI <- round((Data3[[x]]$Data[1, "Time"] - Data3[[x-1]]$Data[1, "Time"]), digits = 0)}
    
    
    #ExclusionReason labelling ----
    
    Data3[[x]]$Valid <- 1
    Data3[[x]]$ExclusionReason <- ""
    
    # if(mean(Data3[[x]]$Data[1:(5*SampRate), "BothSmooth"]) > (Data3[[x]]$Threshold)) {Data3[[x]]$ExclusionReason <- paste(Data3[[x]]$ExclusionReason, "ReactionDuringTrigger ")
    # 
    # }else{
    
    if(Data3[[x]]$Wave1$Onset > (120)) {Data3[[x]]$ExclusionReason <- paste(Data3[[x]]$ExclusionReason, "TooLate ")}
    if(Data3[[x]]$Wave1$Onset < (20)) {Data3[[x]]$ExclusionReason <- paste(Data3[[x]]$ExclusionReason, "TooEarly ")}
    if(Data3[[x]]$Wave1$Duration < (10)) {Data3[[x]]$ExclusionReason <- paste(Data3[[x]]$ExclusionReason, "TooShort ")}
    if(Data3[[x]]$Wave1$Duration > (130)) {Data3[[x]]$ExclusionReason <- paste(Data3[[x]]$ExclusionReason, "TooLong ")}
    
    
    if(Data3[[x]]$Wave1$Peak.uv < Data3[[x]]$Threshold) {Data3[[x]]$ExclusionReason <- "NoReaction"}
    
    if(Data3[[x]]$ExclusionReason == "") {Data3[[x]]$Valid <- 1}else{Data3[[x]]$Valid <- 0}
    
    cat( "Trial: ", x, " \r")
    flush.console()
    
    x <- x+1}
}




#AUC calculation ----

temp1 <-  scale(sapply(Data3, function(f) {auc(1:(200*SampRate), f$Data$BothSmooth, type = "linear")}))

temp2 <-  scale(sapply(Data3, function(f) {auc(1:(200*SampRate), f$Data$BothSmooth, type = "linear", absolutearea = "true")}))

temp3 <-  scale(sapply(Data3, function(f) {auc(f$Wave1$Onset:f$Wave1$Offset, f$Data[f$Wave1$Onset:f$Wave1$Offset, "BothSmooth"], type = "linear", absolutearea = "true")}))

temp1exc <- scale(sapply(Data3, function(f) {if(f$Valid == 1){auc(1:(200*SampRate), f$Data$BothSmooth, type = "linear")}else{NA}}))

temp2exc <- scale(sapply(Data3, function(f) {if(f$Valid == 1){auc(1:(200*SampRate), f$Data$BothSmooth, type = "linear", absolutearea = "true")}else{NA}}))

temp3exc <- scale(sapply(Data3, function(f) {if(f$Valid == 1){auc(f$Wave1$Onset:f$Wave1$Offset, f$Data[f$Wave1$Onset:f$Wave1$Offset, "BothSmooth"], type = "linear", absolutearea = "true")}else{NA}}))

sapply(seq_along(temp1), function(f) {Data3[[f]]$AUC <<- temp1[f]
Data3[[f]]$AUC2 <<- temp2[f]
Data3[[f]]$AUC3 <<- temp3[f]
Data3[[f]]$AUCexc <<- temp1exc[f]
Data3[[f]]$AUC2exc <<- temp2exc[f]
Data3[[f]]$AUC3exc <<- temp3exc[f]})


#Overall aggregating - Prep ----
print("Aggregating")

Overall <- data.frame(matrix(nrow = 200*SampRate, ncol = 11))
names(Overall) <- c("Overall", "Distance1", "Distance2", "Distance3", "Distance4", "Left", "Right", "Contra", "Ipsil", "Block1", "Block2")

ymax1 <- max(max(sapply(Data3, function(f){f$Data[, "BothSmooth"]}),30))
Overall$Overall <- rowMeans(sapply(Data3, function(f){f$Data[, "BothSmooth"]}))


#Left/Right aggregating ----

Overall$Left <- rowMeans(sapply(Data3, function(f){f$Data[, "LeftSmooth"]}))
Overall$Right <- rowMeans(sapply(Data3, function(f){f$Data[, "RightSmooth"]}))


#Distance aggregating ----

Overall$Distance1 <- rowMeans(ExtractVal("f$Distance == 1", "f$Data$BothSmooth"))

Overall$Distance2 <- rowMeans(ExtractVal("f$Distance == 2", "f$Data$BothSmooth"))

Overall$Distance3 <- rowMeans(ExtractVal("f$Distance == 3", "f$Data$BothSmooth"))

Overall$Distance4 <- rowMeans(ExtractVal("f$Distance == 4", "f$Data$BothSmooth"))


#Ipsil/Contra aggregating ----

if (length(Data3) > 32) {
  
  Overall$Ipsil <- rowMeans(cbind(ExtractVal("f$Hand == 1", "f$Data$LeftSmooth"),ExtractVal("f$Hand == 2", "f$Data$RightSmooth")))
  
  Overall$Contra <- rowMeans(cbind(ExtractVal("f$Hand == 2", "f$Data$LeftSmooth"),ExtractVal("f$Hand == 1", "f$Data$RightSmooth")))
  
  
  #Block1/Block2 aggregating ----
  
  Overall$Block1 <- rowMeans(ExtractVal("f$Block == 1", "f$Data$BothSmooth"))
  
  Overall$Block2 <- rowMeans(ExtractVal("f$Block == 2", "f$Data$BothSmooth"))
  
} else { Overall$Block2 <- Overall$Block1 <- Overall$Contra <- Overall$Ipsil <- 0}


#AUC aggregating ----


AUCVec <- c(rowMeans(ExtractVal("f$Distance == 1", "f$AUC")),
            rowMeans(ExtractVal("f$Distance == 2", "f$AUC")),
            rowMeans(ExtractVal("f$Distance == 3", "f$AUC")),
            rowMeans(ExtractVal("f$Distance == 4", "f$AUC")))

AUCVec2 <- c(rowMeans(ExtractVal("f$Distance == 1", "f$AUC2")),
             rowMeans(ExtractVal("f$Distance == 2", "f$AUC2")),
             rowMeans(ExtractVal("f$Distance == 3", "f$AUC2")),
             rowMeans(ExtractVal("f$Distance == 4", "f$AUC2")))

AUCVecExc <- c(rowMeans(ExtractVal("f$Distance == 1 & f$Valid == 1", "f$AUCexc")),
               rowMeans(ExtractVal("f$Distance == 2 & f$Valid == 1", "f$AUCexc")),
               rowMeans(ExtractVal("f$Distance == 3 & f$Valid == 1", "f$AUCexc")),
               rowMeans(ExtractVal("f$Distance == 4 & f$Valid == 1", "f$AUCexc")))

AUCVecExc2 <- c(rowMeans(ExtractVal("f$Distance == 1 & f$Valid == 1", "f$AUC2exc")),
                rowMeans(ExtractVal("f$Distance == 2 & f$Valid == 1", "f$AUC2exc")),
                rowMeans(ExtractVal("f$Distance == 3 & f$Valid == 1", "f$AUC2exc")),
                rowMeans(ExtractVal("f$Distance == 4 & f$Valid == 1", "f$AUC2exc")))



AUCRestrVec <- c(rowMeans(ExtractVal("f$Distance == 1 & f$Valid == 1", "f$AUC3")),
                 rowMeans(ExtractVal("f$Distance == 2 & f$Valid == 1", "f$AUC3")),
                 rowMeans(ExtractVal("f$Distance == 3 & f$Valid == 1", "f$AUC3")),
                 rowMeans(ExtractVal("f$Distance == 4 & f$Valid == 1", "f$AUC3")))

AUCRestrVecExc <- c(rowMeans(ExtractVal("f$Distance == 1 & f$Valid == 1", "f$AUC3exc")),
                    rowMeans(ExtractVal("f$Distance == 2 & f$Valid == 1", "f$AUC3exc")),
                    rowMeans(ExtractVal("f$Distance == 3 & f$Valid == 1", "f$AUC3exc")),
                    rowMeans(ExtractVal("f$Distance == 4 & f$Valid == 1", "f$AUC3exc")))



# # Inidividual data gathering ----
# 
# temp <- list(Trials=Data3, Overall=Overall, Vec=list(AUCVec=AUCVec, AUCVec2=AUCVec2, AUCVecExc=AUCVecExc, AUCVecExc2=AUCVecExc2, AUCRestrVec=AUCRestrVec, AUCRestrVecExc=AUCRestrVecExc))
# 
# if (!exists("Experiment")) {
#   
#   Experiment <- list(temp)
#   names(Experiment)[1] <- Participant
# 
# }else{
#   
#   Experiment[[length(Experiment)+1]] <- temp
#   names(Experiment)[length(Experiment)] <- Participant
#   
# }


end.time <- Sys.time()
print(end.time - start.time)


# Plotting ----
if (readline(prompt = "plot(y/n):  ") == "y") {
  
  #pdf(readline(prompt="Pdf name:  "), width = 10, height=10, onefile = TRUE )
  pdf(paste0(Participant, "-",filename[[6]], ".pdf"),width = 10, height = 10,onefile = TRUE)
  par(mfrow = c(4, 4))
  xrange = seq(from = 1/SampRate, to = 200, by = 1/SampRate)
  
  for (x in 1:length(Data3))
  {
    
    plot(x=xrange, y=Data3[[x]]$Data$BothSmooth, type = "l", ylim = c(0, ymax1), ylab = "uV", xlab = "Time (ms)", 
         main = paste("Trial: ", x , " - Distance: ", Data3[[x]]$Distance, "\n" , 
                      Data3[[x]]$Wave1$Onset , "-", Data3[[x]]$Wave1$Offset, "ms - Dur: ", Data3[[x]]$Wave1$Duration, " - ISI: ", Data3[[x]]$ISI, "\n Status: ", Data3[[x]]$ExclusionReason ))
    
    
    if (Data3[[x]]$Valid == 1) {
      
      rect(Data3[[x]]$Wave1$Onset, 0, Data3[[x]]$Wave1$Offset, Data3[[x]]$Wave1$Peak.uv, border = "red")
      
      if(!is.null(Data3[[x]]$Wave2$Onset)) {rect(Data3[[x]]$Wave2$Onset, 0, Data3[[x]]$Wave2$Offset, Data3[[x]]$Wave2$Peak.uv, border = "green")}
      
      if(!is.null(Data3[[x]]$Wave3$Onset)) {rect(Data3[[x]]$Wave3$Onset, 0, Data3[[x]]$Wave3$Offset, Data3[[x]]$Wave3$Peak.uv, border = "blue")}
      
      
    }else {abline(c(0, (ymax1)/(200)), col = "red");abline(c(ymax1, -(ymax1/(200))), col = "red")}
  
    
    #Left/Right secondary graph
    
    #plot(Data3[[x]]$Data$LeftSmooth, type = "l", col = "red", ylim = c(0, ymax1), ylab = "uV", xlab = "Time (ms)", 
    #    main = paste("Trial: ", x , " - Distance: ", Data3[[x]]$Distance, "\n", "Left vs Right"))
    #lines(Data3[[x]]$Data$RightSmooth, type = "l", col= "green")
    
    

  }
  
  layout(matrix(c(1, 2, 3, 4 , 5 , 5 , 5, 5 , 5, 5, 5, 5),nrow = 3, byrow = TRUE))
  ymax2 <- c(0,max((1.3* max(Overall[, ])), 20))
  
  plot(x=xrange, y=Overall$Distance1,  type = "l",  ylim = ymax2,  ylab = "uV",  xlab = "Time (ms)",  main = "Pos1",  col = "red")
  plot(x=xrange, y=Overall$Distance2,  type = "l",  ylim = ymax2,  ylab = "uV",  xlab = "Time (ms)",  main = "Pos2",  col = "green")
  plot(x=xrange, y=Overall$Distance3,  type = "l",  ylim = ymax2,  ylab = "uV",  xlab = "Time (ms)",  main = "Pos3",  col = "blue")
  plot(x=xrange, y=Overall$Distance4,  type = "l",  ylim = ymax2,  ylab = "uV",  xlab = "Time (ms)",  main = "Pos4",  col = "orange")
  
  plot(x=xrange, y=Overall$Distance1,  type = "l",  ylim = ymax2,  ylab = "uV",  xlab = "Time (ms)",  main = "Combined",  col = "red")
  lines(x=xrange, y=Overall$Distance2, type = "l", col = "green")
  lines(x=xrange, y=Overall$Distance3, type = "l", col = "blue")
  lines(x=xrange, y=Overall$Distance4, type = "l", col = "orange")
  
  
  par(mfrow = c(2, 3))
  
  plot(AUCVec, ylab = "AUC", ylim = range(c(AUCVec,if(is.na(sum(AUCVecExc))){AUCVec}else{AUCVecExc})), xlim = c(0, 4), xlab = "Distance",  main = "Distance comparison", type = "o", col = "red")
  if(!anyNA(AUCVecExc )) lines(AUCVecExc, type = "o", col = "green")
  legend("topleft", legend=c("All", "Valid"), col=c("red", "green"), lty = 1:1, cex = 0.9)
  
  plot(AUCVec2, ylab = "AUC2", ylim = range(c(AUCVec2,if(is.na(sum(AUCVecExc2))){AUCVec2}else{AUCVecExc2})), xlim = c(0, 4), xlab = "Distance",  main = "Distance comparison", type = "o", col = "red")
  if(!anyNA(AUCVecExc2 ))lines(AUCVecExc2, type = "o", col = "green")
  legend("topleft", legend=c("All", "Valid"), col=c("red", "green"), lty = 1:1, cex = 0.9)
  
  if(!anyNA(AUCRestrVec)){ plot(AUCRestrVec, ylab = "AUC",  ylim = range(c(AUCRestrVec, AUCRestrVecExc)), xlim = c(0, 4), xlab = "Distance",  main = "AUC Restr comp", type = "o", col = "red")
    lines(AUCRestrVecExc, type = "o", col = "green")
    legend("topleft", legend=c("All", "Valid"), col=c("red", "green"), lty = 1:1, cex = 0.9)}
  
  plot(Overall$Ipsil,  type = "l",  ylim = ymax2,  ylab = "uV",  xlab = "Time (ms)",  main = "Ipsilateral vs Contralateral",  col = "red")
  lines(Overall$Contra, type = "l", col = "green")
  legend("topleft", legend=c("Ipsil", "Contra"), col=c("red", "green"), lty = 1:1, cex = 0.9)
  
  plot(Overall$Left,  type = "l",  ylim = ymax2,  ylab = "uV",  xlab = "Time (ms)",  main = "Left vs Right",  col = "red")
  lines(Overall$Right, type = "l", col = "green")
  legend("topleft", legend=c("Left", "Right"), col=c("red", "green"), lty = 1:1, cex = 0.9)
  
  plot(Overall$Block1,  type = "l",  ylim = ymax2,  ylab = "uV",  xlab = "Time (ms)",  main = "Block 1 vs Block 2",  col = "red")
  lines(Overall$Block2, type = "l", col = "green")
  legend("topleft", legend=c("Block 1", "Block 2"), col=c("red", "green"), lty = 1:1, cex = 0.9)
  
  
  
  
  temp <- melt(sapply(Data3, function(f){f$Data[, "BothSmooth"]}))
  temp2 <- melt(sapply(Data3, function(f){f$Wave1$Peak.uv}))
  temp2$Var1 <- 1
  temp2$Var2 <- row.names(temp2)
  
  
  a <- ggplotGrob(ggplot(temp, aes(Var1,Var2,fill = value)) + geom_tile() + scale_fill_gradient(low = "white", high = "red") + scale_x_continuous(limits=c(0, 200*SampRate), expand = c(0, 0)) + scale_y_continuous(limits=c(0, length(Data3)), expand = c(0, 0))
                  + theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                          axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()))
  b <- ggplotGrob(ggplot(temp2, aes(Var1, Var2, fill = value)) + geom_tile() + scale_fill_gradient(low = "white", high = "red") + theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                                                                                                                                        axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()))
  g <- gtable_matrix(name = "test",grobs = matrix( list(a,b), ncol=2), widths = unit(c(8,2), "in"), heights = (10))
  grid.newpage()
  grid.draw(g)
  
  # Habituation graphs
  
  par(mfrow = c(1, 2))
  temp2$Var2 <- as.numeric(temp2$Var2)
  plot(temp2$value[1:32] ~ temp2$Var2[1:32])
  abline(lm(temp2$value[1:32] ~ temp2$Var2[1:32]))
  
  plot(temp2$value[33:64] ~ temp2$Var2[33:64])
  abline(lm(temp2$value[33:64] ~ temp2$Var2[33:64]))
  
  #Ipsil/Contra distance pairing graphs
  
  Ipsil1 <- auc(1:(200*SampRate),rowMeans(cbind(ExtractVal("f$Distance == 1 & f$Hand == 1", "f$Data$LeftSmooth"),ExtractVal("f$Distance == 1 & f$Hand == 2", "f$Data$RightSmooth"))), type = "linear")
  Ipsil2 <- auc(1:(200*SampRate),rowMeans(cbind(ExtractVal("f$Distance == 2 & f$Hand == 1", "f$Data$LeftSmooth"),ExtractVal("f$Distance == 2 & f$Hand == 2", "f$Data$RightSmooth"))), type = "linear")
  Ipsil3 <- auc(1:(200*SampRate),rowMeans(cbind(ExtractVal("f$Distance == 3 & f$Hand == 1", "f$Data$LeftSmooth"),ExtractVal("f$Distance == 3 & f$Hand == 2", "f$Data$RightSmooth"))), type = "linear")
  Ipsil4 <- auc(1:(200*SampRate),rowMeans(cbind(ExtractVal("f$Distance == 4 & f$Hand == 1", "f$Data$LeftSmooth"),ExtractVal("f$Distance == 4 & f$Hand == 2", "f$Data$RightSmooth"))), type = "linear")
  
  Contra1 <- auc(1:(200*SampRate),rowMeans(cbind(ExtractVal("f$Distance == 1 & f$Hand == 2", "f$Data$LeftSmooth"),ExtractVal("f$Distance == 1 & f$Hand == 1", "f$Data$RightSmooth"))), type = "linear")
  Contra2 <- auc(1:(200*SampRate),rowMeans(cbind(ExtractVal("f$Distance == 2 & f$Hand == 2", "f$Data$LeftSmooth"),ExtractVal("f$Distance == 2 & f$Hand == 1", "f$Data$RightSmooth"))), type = "linear")
  Contra3 <- auc(1:(200*SampRate),rowMeans(cbind(ExtractVal("f$Distance == 3 & f$Hand == 2", "f$Data$LeftSmooth"),ExtractVal("f$Distance == 3 & f$Hand == 1", "f$Data$RightSmooth"))), type = "linear")
  Contra4 <- auc(1:(200*SampRate),rowMeans(cbind(ExtractVal("f$Distance == 4 & f$Hand == 2", "f$Data$LeftSmooth"),ExtractVal("f$Distance == 4 & f$Hand == 1", "f$Data$RightSmooth"))), type = "linear")
  
  
  plot(c(Contra1,Ipsil1), ylim = range(Ipsil1,Ipsil2,Ipsil3,Ipsil4,Contra1,Contra2,Contra3,Contra4), type = "o", col = "red", xlim = c(1, 2))
  lines(c(Contra2,Ipsil2), type = "o",  col = "green", xlim = c(1, 2))
  lines(c(Contra3,Ipsil3), type = "o",  col = "blue", xlim = c(1, 2))
  lines(c(Contra4,Ipsil4), type = "o",  col = "orange", xlim = c(1, 2))
  
  #Calculation text dump
  
  a <- NULL
  a <- as.data.frame(cbind(sapply(Data3, function(f){f$Distance}),
                           sapply(Data3, function(f){f$AUC}),
                           sapply(Data3, function(f){f$AUC2}),
                           sapply(Data3, function(f){f$AUC3}),
                           sapply(Data3, function(f){f$AUCexc}),
                           sapply(Data3, function(f){f$AUC2exc}),
                           sapply(Data3, function(f){f$AUC3exc}), 
                           c(1:64)))
  colnames(a) <- c("Distance", "AUC", "AUC2", "AUC3", "AUCexc", "AUC2exc", "AUC3exc", "Stimuli")
  a$Stimuli <- as.factor(a$Stimuli)
  a$Distance <- as.factor(a$Distance)
  
  Contrasts <- data.frame(Linear = c(-3,-1,1,3),
                          Small = c(-1,-1,-1,3),
                          LargeR = c(-1.5,-1.5,0.5,2.5),
                          LargeS = c(-1,-1,1,1),
                          ELarge = c(-3,1,1,1),
                          Custom = c(-1,0,0,1))
  
  sink(paste0(Participant, "-",filename[[6]], ".txt"))
  
  b <- melt(table(unlist(ExtractVal("1==1","f$ExclusionReason"))))
  b$Var1 <- as.character(b$Var1)
  b[1,1] <- "Valid"
  
  colnames(b) <- NULL
  print.data.frame(b[,], right = FALSE, row.names = FALSE)
  cat("\n")
  cat(paste0("==== ",round(b[[1,2]]/0.64, digits=0), "% Valid ====\n\n"))
  
  cat(paste0("Mean Onset = ", round(rowMeans(ExtractVal("f$Valid == 1","f$Wave1$Onset")), digits = 0), "\n"))
  cat(paste0("Mean Peak Onset = ", round(rowMeans(ExtractVal("f$Valid == 1","f$Wave1$Peak.ms")), digits = 0), "\n"))
  cat(paste0("Mean Offset = ", round(rowMeans(ExtractVal("f$Valid == 1","f$Wave1$Offset")), digits = 0), "\n\n"))
  
  cat(paste0("Mean Duration = ", round(rowMeans(ExtractVal("f$Valid == 1","f$Wave1$Duration")), digits = 0), "\n"))
  cat(paste0("Mean Peak uV = ", round(rowMeans(ExtractVal("f$Valid == 1","f$Wave1$Peak.uv")), digits = 0), "\n\n"))
  
  cat(paste0("AUC", "\n"))
  print(contrast(emmeans(aov_ez(id = "Stimuli", dv = "AUC", data = a, between = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None"))
  
  cat(paste0("AUC2", "\n"))
  print(contrast(emmeans(aov_ez(id = "Stimuli", dv = "AUC2", data = a, between = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None"))
  
  cat(paste0("AUC3", "\n"))
  print(contrast(emmeans(aov_ez(id = "Stimuli", dv = "AUC3", data = a, between = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None"))
  
  cat(paste0("AUCexc", "\n"))
  print(contrast(emmeans(aov_ez(id = "Stimuli", dv = "AUCexc", data = a, between = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None"))
  
  cat(paste0("AUCexc2", "\n"))
  print(contrast(emmeans(aov_ez(id = "Stimuli", dv = "AUC2exc", data = a, between = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None"))
  
  cat(paste0("AUCexc3", "\n"))
  print(contrast(emmeans(aov_ez(id = "Stimuli", dv = "AUC3exc", data = a, between = "Distance"), ~Distance, weights = "proportional"), Contrasts, adjust = "None"))
  
  sink()
  
  
  
  dev.off()
}



a <- b <- g <- temp <- temp1 <- temp2 <- temp3 <-  NULL


