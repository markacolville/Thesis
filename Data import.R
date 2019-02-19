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

for (i in 1:32) {
  
  if (identical(filename[unlist(grepl((paste0("Participant ", i, " ")), filename))], character(0))) next()
    
    filename2 <- filename[unlist(grepl((paste0("Participant ", i, " ")), filename))]
    
    
    #Starts time recording for program debugging
    start.time <- Sys.time()
    
    #Takes list of chosen filenames, reads.tables them all into a list of data-frames, then rbinds them together into one
    Data1 <- do.call(rbind,lapply(filename2, function(f){read.delim(f, col.names = paste0("V", seq_len(5)), header = FALSE, skip = 5, na.strings = "", stringsAsFactors = TRUE)}))
    
    #Captures filename of first file
    filename2 <- basename(filename2[1])
    
    
    #Filename processing# ----
    
    filename2 <- sub('\\.txt', '', filename2)
    filename2 <- as.list(unlist(strsplit(filename2, " - ")))
    
    #Capures components of filename. Filename format is seperated by " - "
    #First part "Participant #", labbeling whos data it is
    #Second part "RS" for responder, or "NR" for non-responder
    #Third part "L" or R", left or right hand for starting hand
    #Forth part "T" for tested, "C" for control
    #Fifth part for "Block 1" or "Block 2" (not used)
    #Fifth part "1k" for 1k sampling rate, "10k" for 10k sampling rate
    
    Participant <- filename2[[1]]
    if (filename2[2] == "RS") {Responder <- 1}else{Responder <- 2}
    if (filename2[3] == "L") {Beginhand <- 1}else{Beginhand <- 2}
    if (filename2[4] == "T") {Tested <- 1}else{Tested <- 2}
    if (filename2[6] == "1k") {SampRate <- 1}else{SampRate <- 10}
    
    
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
        
        #Adding timepoints
        
        Data3[[x]]$Data$Timepoint <- seq(from = (1/SampRate-100), to = 300, by = 1/SampRate)
        
        Data3[[x]]$Distance <- dist
        
        cat( "Trial: ", x, " \r")
        flush.console()
        
        x <- x+1
      }}
    
    # Inidividual data gathering ----
    
    a <- as.integer(unlist(strsplit(filename2[[1]], " "))[2])
    
    Experiment[[a]]$Trials <- Data3
    

    end.time <- Sys.time()
    print(paste0("Participant ", a, " - ", end.time - start.time))
}



##Participant specific info entering. Afterwards, need to integrate the EMG+info together
##This isnt included but easy to do.

test <- data.frame(matrix(nrow = 32, ncol = 13))
colnames(test) <- c("Participant", "Age", "Gender", "Responder", "Hand", "Condition",
                    "LeftResis", "RightResis", "ReferResis", "Block1Twitch", "Block1Working", "Block2Twitch", "Block2Working",
                    "LeftTwitch", "LeftWorking", "RightTwitch", "RightWorking")

for (i in 1:32)
{
  cat( "Participant: ", i, " \r")
  flush.console()
  
  test[i, "Participant"] <- i
  test[i, "Age"] <- readline(prompt = "Age: ")
  test[i, "Gender"] <- readline(prompt = "Gender: ")
  test[i, "Responder"] <- readline(prompt = "Responder: ")
  test[i, "Hand"] <- readline(prompt = "Hand: ")
  test[i, "Condition"] <- readline(prompt = "Condition: ")
  test[i, "LeftResis"] <- readline(prompt = "LeftResis: ")
  test[i, "RightResis"] <- readline(prompt = "RightResis: ")
  test[i, "ReferResis"] <- readline(prompt = "ReferResis: ")
  test[i, "Block1Twitch"] <- readline(prompt = "Block1Twitch: ")
  test[i, "Block1Working"] <- readline(prompt = "Block1Working: ")
  test[i, "Block2Twitch"] <- readline(prompt = "Block2Twitch: ")
  test[i, "Block2Working"] <- readline(prompt = "Block2Working: ")
  
  if (test[i, "Hand"] == "L") {
    
    test[i, "LeftTwitch"] <- test[i,"Block1Twitch"]
    test[i, "RightTwitch"] <- test[i,"Block2Twitch"]
    test[i, "LeftWorking"] <- test[i,"Block1Working"]
    test[i, "RightWorking"] <- test[i,"Block2Working"]                                                
    
  }else{
    
    test[i, "LeftTwitch"] <- test[i,"Block2Twitch"]
    test[i, "RightTwitch"] <- test[i,"Block1Twitch"]
    test[i, "LeftWorking"] <- test[i,"Block2Working"]
    test[i, "RightWorking"] <- test[i,"Block1Working"]
  }
}

for (i in 1:32)
{
  cat( "Participant: ", i, " \r")
  flush.console()
  
  test2[i, "Q1"] <- as.numeric(readline(prompt = "Q1: "))
  test2[i, "Q2"] <- as.numeric(readline(prompt = "Q2: "))
  test2[i, "Q3"] <- as.numeric(readline(prompt = "Q3: "))
  test2[i, "Q4"] <- as.numeric(readline(prompt = "Q4: "))
  test2[i, "Q5"] <- as.numeric(readline(prompt = "Q5: "))
  test2[i, "Q6"] <- as.numeric(readline(prompt = "Q6: "))
  
  test2[i, "Score"] <- (5 - test2[i, "Q1"]) + test2[i, "Q2"] + test2[i, "Q3"] + 
    (5 - test2[i, "Q4"]) + (5 - test2[i, "Q5"]) + test2[i, "Q6"]
  
}


test3 <- list()

for (i in 1:32)
{
  test3[[i]] <- list()
  
  test3[[i]]$Age <- test$Age[i] 
  test3[[i]]$Gender <- test$Gender[i]
  test3[[i]]$Responder <- test$Responder[i]
  test3[[i]]$StartingHand <- test$Hand[i]
  test3[[i]]$Condition <- test$Condition[i]
  
  test3[[i]]$Impedance$Left <- test$LeftResis[i]
  test3[[i]]$Impedance$Right <- test$RightResis[i]
  test3[[i]]$Impedance$Reference <- test$ReferResis[i]
  
  test3[[i]]$Intensity$Block1$Twitch <- test$Block1Twitch[i]
  test3[[i]]$Intensity$Block1$Working <- test$Block1Working[i]
  
  test3[[i]]$Intensity$Block2$Twitch <- test$Block2Twitch[i]
  test3[[i]]$Intensity$Block2$Working <- test$Block2Working[i]
  
  test3[[i]]$Intensity$Left$Twitch <- test$LeftTwitch[i]
  test3[[i]]$Intensity$Left$Working <- test$LeftWorking[i]
  
  test3[[i]]$Intensity$Right$Twitch <- test$RightTwitch[i]
  test3[[i]]$Intensity$Right$Working <- test$RightWorking[i]
  
  
  test3[[i]]$WritingComponent$Q1 <- test2$Q1[i]
  test3[[i]]$WritingComponent$Q2 <- test2$Q2[i]
  test3[[i]]$WritingComponent$Q3 <- test2$Q3[i]
  test3[[i]]$WritingComponent$Q4 <- test2$Q4[i]
  test3[[i]]$WritingComponent$Q5 <- test2$Q5[i]
  test3[[i]]$WritingComponent$Q6 <- test2$Q6[i]
  test3[[i]]$WritingComponent$Score <- test2$Score[i]
  
}
