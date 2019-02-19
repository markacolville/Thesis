####################################== DEMOGRAPHIC AND GROUP == ####################################

mean(sapply(Experiment, function (f){f$Age}),na.rm = TRUE)
sd(sapply(Experiment, function (f){f$Age}),na.rm = TRUE)

table(sapply(Experiment, function (f){f$Gender}))

table(sapply(Experiment, function (f){f$StartingHand}))

table(sapply(Experiment, function (f){f$Condition}))


####################################== IMPEDANCE == ####################################

data.frame(Left = c(mean(sapply(Experiment, function (f){f$Impedance$Left}),na.rm = TRUE),
                    mean(sapply(Experiment, function (f)if(f$Responder == "RS") f$Impedance$Left else NA), na.rm = TRUE),
                    mean(sapply(Experiment, function (f)if(f$Responder == "NR") f$Impedance$Left else NA), na.rm = TRUE)),
           Right = c(mean(sapply(Experiment, function (f){f$Impedance$Right}),na.rm = TRUE),
                     mean(sapply(Experiment, function (f)if(f$Responder == "RS") f$Impedance$Right else NA), na.rm = TRUE),
                     mean(sapply(Experiment, function (f)if(f$Responder == "NR") f$Impedance$Right else NA), na.rm = TRUE)),
           Reference = c(mean(sapply(Experiment, function (f){f$Impedance$Reference}),na.rm = TRUE),
                         mean(sapply(Experiment, function (f)if(f$Responder == "RS") f$Impedance$Reference else NA), na.rm = TRUE),
                         mean(sapply(Experiment, function (f)if(f$Responder == "NR") f$Impedance$Reference else NA), na.rm = TRUE)),
           RangeStart = c(range(sapply(Experiment, function (f){f$Impedance}),na.rm = TRUE)[1],
                     range(sapply(Experiment, function (f)if(f$Responder == "RS") f$Impedance else NA), na.rm = TRUE)[1],
                     range(sapply(Experiment, function (f)if(f$Responder == "NR") f$Impedance else NA), na.rm = TRUE)[1]),
           RangeEnd = c(range(sapply(Experiment, function (f){f$Impedance}),na.rm = TRUE)[2],
                          range(sapply(Experiment, function (f)if(f$Responder == "RS") f$Impedance else NA), na.rm = TRUE)[2],
                          range(sapply(Experiment, function (f)if(f$Responder == "NR") f$Impedance else NA), na.rm = TRUE)[2]))


####################################== INTENSITY == ####################################

print("Twitch")
data.frame(Block1 = c(mean(sapply(Experiment, function (f)f$Intensity$Block1$Twitch),na.rm = TRUE),
                      mean(sapply(Experiment, function (f)if(f$Responder == "RS") f$Intensity$Block1$Twitch else NA), na.rm = TRUE),
                      mean(sapply(Experiment, function (f)if(f$Responder == "NR") f$Intensity$Block1$Twitch else NA), na.rm = TRUE)),
           Block2 = c(mean(sapply(Experiment, function (f)f$Intensity$Block2$Twitch),na.rm = TRUE),
                      mean(sapply(Experiment, function (f)if(f$Responder == "RS") f$Intensity$Block2$Twitch else NA), na.rm = TRUE),
                      mean(sapply(Experiment, function (f)if(f$Responder == "NR") f$Intensity$Block2$Twitch else NA), na.rm = TRUE)),
           LeftHand = c(mean(sapply(Experiment, function (f)f$Intensity$Left$Twitch),na.rm = TRUE),
                        mean(sapply(Experiment, function (f)if(f$Responder == "RS") f$Intensity$Left$Twitch else NA), na.rm = TRUE),
                        mean(sapply(Experiment, function (f)if(f$Responder == "NR") f$Intensity$Left$Twitch else NA), na.rm = TRUE)),
           RightHand = c(mean(sapply(Experiment, function (f)f$Intensity$Right$Twitch),na.rm = TRUE),
                         mean(sapply(Experiment, function (f)if(f$Responder == "RS") f$Intensity$Right$Twitch else NA), na.rm = TRUE),
                         mean(sapply(Experiment, function (f)if(f$Responder == "NR") f$Intensity$Right$Twitch else NA), na.rm = TRUE)),
           Both = c(mean(sapply(Experiment, function (f) mean(c(f$Intensity$Left$Twitch,f$Intensity$Right$Twitch))),na.rm = TRUE),
                         mean(sapply(Experiment, function (f)if(f$Responder == "RS" & !all(is.na(c(f$Intensity$Left$Twitch,f$Intensity$Right$Twitch))))
                           mean(c(f$Intensity$Left$Twitch,f$Intensity$Right$Twitch), na.rm = TRUE) else NA), na.rm = TRUE),
                         mean(sapply(Experiment, function (f)if(f$Responder == "NR" & !all(is.na(c(f$Intensity$Left$Twitch,f$Intensity$Right$Twitch)))) 
                           mean(c(f$Intensity$Left$Twitch,f$Intensity$Right$Twitch), na.rm = TRUE) else NA), na.rm = TRUE)))

print("Working")
data.frame(Block1 = c(mean(sapply(Experiment, function (f)f$Intensity$Block1$Working),na.rm = TRUE),
                      mean(sapply(Experiment, function (f)if(f$Responder == "RS") f$Intensity$Block1$Working else NA), na.rm = TRUE),
                      mean(sapply(Experiment, function (f)if(f$Responder == "NR") f$Intensity$Block1$Working else NA), na.rm = TRUE)),
           Block2 = c(mean(sapply(Experiment, function (f)f$Intensity$Block2$Working),na.rm = TRUE),
                      mean(sapply(Experiment, function (f)if(f$Responder == "RS") f$Intensity$Block2$Working else NA), na.rm = TRUE),
                      mean(sapply(Experiment, function (f)if(f$Responder == "NR") f$Intensity$Block2$Working else NA), na.rm = TRUE)),
           LeftHand = c(mean(sapply(Experiment, function (f)f$Intensity$Left$Working),na.rm = TRUE),
                        mean(sapply(Experiment, function (f)if(f$Responder == "RS") f$Intensity$Left$Working else NA), na.rm = TRUE),
                        mean(sapply(Experiment, function (f)if(f$Responder == "NR") f$Intensity$Left$Working else NA), na.rm = TRUE)),
           RightHand = c(mean(sapply(Experiment, function (f)f$Intensity$Right$Working),na.rm = TRUE),
                         mean(sapply(Experiment, function (f)if(f$Responder == "RS") f$Intensity$Right$Working else NA), na.rm = TRUE),
                         mean(sapply(Experiment, function (f)if(f$Responder == "NR") f$Intensity$Right$Working else NA), na.rm = TRUE)),
           Both = c(mean(sapply(Experiment, function (f) mean(c(f$Intensity$Left$Working,f$Intensity$Right$Working))),na.rm = TRUE),
                    mean(sapply(Experiment, function (f)if(f$Responder == "RS" & !all(is.na(c(f$Intensity$Left$Working,f$Intensity$Right$Working))))
                      mean(c(f$Intensity$Left$Working,f$Intensity$Right$Working), na.rm = TRUE) else NA), na.rm = TRUE),
                    mean(sapply(Experiment, function (f)if(f$Responder == "NR" & !all(is.na(c(f$Intensity$Left$Working,f$Intensity$Right$Working))))
                      mean(c(f$Intensity$Left$Working,f$Intensity$Right$Working), na.rm = TRUE) else NA), na.rm = TRUE)))


t.test(unlist(c(sapply(Experiment, function (f)if(f$Responder == "RS") f$Intensity$Block1$Working else NA),
                sapply(Experiment, function (f)if(f$Responder == "RS") f$Intensity$Block2$Working else NA))),
       unlist(c(sapply(Experiment, function (f)if(f$Responder == "NR") f$Intensity$Block1$Working else NA),
                sapply(Experiment, function (f)if(f$Responder == "NR") f$Intensity$Block2$Working else NA))))


####################################== TRIAL ONSET/OFFSET ETC AVERAGES == ####################################

rowMeans(abind(sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Averages), along = 3), dims = 2)

rowMeans(abind(sapply(Experiment, function(f) if(f$Responder == "NR") f$TrialData$Averages), along = 3), dims = 2)


t.test(na.omit(sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Averages$Mean[5] else {NA})),
       na.omit(sapply(Experiment, function(f) if(f$Responder == "NR" & (!is.null(f$TrialData))) f$TrialData$Averages$Mean[5] else {NA})))


temp2 <- list()

for (i in 1:32){
  
  if (Experiment[[i]]$Responder == "NR") {next}
  
  temp <- as.data.frame(matrix(ncol=2, nrow=5))
  colnames(temp) <- c("Mean", "SD")
  rownames(temp) <- c("Onset(ms)", "Peak(ms)", "Offset(ms)", "Duration(ms)", "Peak(uV)")
  
  temp$Mean <- c(round(rowMeans(ExtractVal("f$Valid == 1 & f$Distance == 1","f$Wave1$Onset", i)), digits = 3),
                 round(rowMeans(ExtractVal("f$Valid == 1 & f$Distance == 1","f$Wave1$Peak.ms", i)), digits = 3),
                 round(rowMeans(ExtractVal("f$Valid == 1 & f$Distance == 1","f$Wave1$Offset", i)), digits = 3),
                 round(rowMeans(ExtractVal("f$Valid == 1 & f$Distance == 1","f$Wave1$Duration", i)), digits = 3),
                 round(rowMeans(ExtractVal("f$Valid == 1 & f$Distance == 1","f$Wave1$Peak.uv", i)), digits = 3))
  
  temp$SD <- c(round(sd(ExtractVal("f$Valid == 1 & f$Distance == 1","f$Wave1$Onset", i)), digits = 4),
               round(sd(ExtractVal("f$Valid == 1 & f$Distance == 1","f$Wave1$Peak.ms", i)), digits = 4),
               round(sd(ExtractVal("f$Valid == 1 & f$Distance == 1","f$Wave1$Offset", i)), digits = 4),
               round(sd(ExtractVal("f$Valid == 1 & f$Distance == 1","f$Wave1$Duration", i)), digits = 4),
               round(sd(ExtractVal("f$Valid == 1 & f$Distance == 1","f$Wave1$Peak.uv", i)), digits = 4))
  
  temp2[[i]] <- temp
  
}


a <- rowMeans(abind(sapply(temp2, function(f) if(!is.null(f)) f), along = 3), dims = 2)


####################################== OVERALL CLASSIFICATION == ####################################

data.frame(V1 = c("Valid", "Invalid", "TooEarly", "TooLate", "TooShort", "TooLong", "NoReaction"),
           RS = rowSums(as.data.frame(abind(sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Validity$V2), along = 2))),
           NR = rowSums(as.data.frame(abind(sapply(Experiment, function(f) if(f$Responder == "NR") f$TrialData$Validity$V2), along = 2))))

t.test(unlist(sapply(Experiment, function(f) if (f$Responder == "RS") sapply(f$Trials, function(g) if(g$Valid == 1) g$Wave1$Peak.ms else NA) else NA)),
       unlist(sapply(Experiment, function(f) if (f$Responder == "NR") sapply(f$Trials, function(g) if(g$Valid == 1)g$Wave1$Peak.ms else NA) else NA)))

t.test(unlist(sapply(Experiment, function(f) if (f$Responder == "RS") sapply(f$Trials, function(g) if(g$Valid == 1) g$Wave1$Peak.uv else NA) else NA)),
       unlist(sapply(Experiment, function(f) if (f$Responder == "NR") sapply(f$Trials, function(g) if(g$Valid == 1)g$Wave1$Peak.uv else NA) else NA)))

t.test(unlist(sapply(Experiment, function(f) if (f$Responder == "RS") sapply(f$Trials, function(g) if(g$Valid == 1) g$Wave1$Onset else NA) else NA)),
       unlist(sapply(Experiment, function(f) if (f$Responder == "NR") sapply(f$Trials, function(g) if(g$Valid == 1)g$Wave1$Onset else NA) else NA)))

t.test(unlist(sapply(Experiment, function(f) if (f$Responder == "RS") sapply(f$Trials, function(g) if(g$Valid == 1) g$Wave1$Offset else NA) else NA)),
       unlist(sapply(Experiment, function(f) if (f$Responder == "NR") sapply(f$Trials, function(g) if(g$Valid == 1)g$Wave1$Offset else NA) else NA)))




####################################== HABITUATION == ####################################

a <- b <- NULL
graph.list <- list()

for (i in 1:64){
  
  #a <- sapply(Experiment, function (f) if((length(f$Trials) >= i) & (f$Responder == "RS"))  f$Trials[[i]]$Data$BothSmooth else NA)
  #a <- sapply(Experiment, function (f) if((length(f$Trials) >= i) & (f$Responder == "NR"))  f$Trials[[i]]$Data$BothSmooth else NA)
  a <- lapply(Experiment, function (f) if((length(f$Trials) >= i) & (f$Responder == "RS")) {if(f$Trials[[i]]$Valid == 1)  f$Trials[[i]]$Data$BothSmooth else NA}else NA)
  
  a <- rowMeans(as.data.frame(a[!sapply(a, is.null)]), na.rm = TRUE)
  
  b <- cbind(b, a)}

colnames(b) <- seq(from=1, to=64, by=1)
c <- melt(b)

graph.list[1] <-grob(ggplot(c, aes(Var1/10,Var2,fill = value)) +
                       geom_tile() +
                       scale_fill_gradient(low = "white", high = "red") +
                       scale_x_continuous(expand = c(0, 0)) +
                       scale_y_continuous(breaks = seq(from=0,by=4,to=64), expand = c(0, 0)) +
                       theme(legend.position="none") +
                       ylab("Trial #")+
                       xlab("Time (ms)"))
#theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(),axis.title.y=element_blank())

a <- b <- NULL

for (i in 1:64){
  
  a <- lapply(Experiment, function (f) if((length(f$Trials) >= i) & (f$Responder == "RS")) {if(f$Trials[[i]]$Valid == 1)  f$Trials[[i]]$Wave1$Peak.uv else NA} else NA)
  a <- mean(unlist(a[!sapply(a, is.null)]), na.rm = TRUE)
  
  b <- rbind(b, a)}

rownames(b) <- seq(from=1, by=1, to=64)

b <- as.data.frame(b)

b$value2 <- as.numeric(rownames(b))

graph.list[2] <- grob(ggplot(b[1:32,], aes(value2,V1)) +
                        geom_point() +
                        geom_smooth(method = "lm") +
                        scale_x_continuous(breaks = seq(from=0,by=4,to=32)) +
                        theme(axis.title.x=element_blank(),
                              axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()))


graph.list[3] <- grob(ggplot(b[33:64,], aes(value2,V1)) +
                        geom_point() +
                        geom_smooth(method = "lm") +
                        scale_x_continuous(breaks = seq(from=32,by=4,to=64)) +
                        theme(axis.title.x=element_blank(),
                              axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()))

grid.draw(arrangeGrob(grobs=graph.list,layout_matrix = rbind(c(1,2),c(1,3))))


####################################== SIMPLE REGRESSION ==#############################################


a <- do.call(rbind,sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.All.Cons) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.All.Cons else NA))

colnames(a) <- c(1:4)
a <- as.data.frame(na.omit(a))
b <- melt(a)
b$variable <- as.numeric(b$variable)

summary(lm(data=b, variable ~ value))



####################################== Distance comparison across responders ==#############################################

a <- as.data.frame(cbind(rowMeans(rbind(do.call(cbind,sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Overall$Distance1)))),
                         rowMeans(rbind(do.call(cbind,sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Overall$Distance2)))),
                         rowMeans(rbind(do.call(cbind,sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Overall$Distance3)))),
                         rowMeans(rbind(do.call(cbind,sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Overall$Distance4))))))
colnames(a) <- c("Distance1", "Distance2", "Distance3", "Distance4")
a$timepoint <- seq(from = 0.1, by = 0.1, to = 200)

a <- melt(a, id = "timepoint")

ggplot(a, aes(timepoint, value)) + 
  geom_line(aes(color = variable, group = variable), size = 0.8) +
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  ylab("Microvolts (uV")+
  xlab("Time (ms)")+
  ggtitle("HBR magnitude across 4 positions")+
  theme_classic()+
  scale_color_manual(values=c("red", "green", "blue", "orange"))+
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

####################################== Ipsil/Contra Position1 and 4 comparison graph ==#############################################

a <- data.frame('Pos4 Ipsilateral' = rowMeans(sapply(Experiment, function (f) if(f$Responder == "RS") rowMeans(sapply(f$Trials, function(g) if(g$Distance == 4) {if(g$Hand == 1) g$Data$LeftSmooth else g$Data$RightSmooth} else rep(NA, 2000)), na.rm = TRUE) else rep(NA, 2000)), na.rm = TRUE),
                'Pos1 Ipsilateral' = rowMeans(sapply(Experiment, function (f) if(f$Responder == "RS") rowMeans(sapply(f$Trials, function(g) if(g$Distance == 1) {if(g$Hand == 1) g$Data$LeftSmooth else g$Data$RightSmooth} else rep(NA, 2000)), na.rm = TRUE) else rep(NA, 2000)), na.rm = TRUE),
                'Pos4 Contralateral' = rowMeans(sapply(Experiment, function (f) if(f$Responder == "RS") rowMeans(sapply(f$Trials, function(g) if(g$Distance == 4) {if(g$Hand == 2) g$Data$LeftSmooth else g$Data$RightSmooth} else rep(NA, 2000)), na.rm = TRUE) else rep(NA, 2000)), na.rm = TRUE),
                'Pos1Contralateral' = rowMeans(sapply(Experiment, function (f) if(f$Responder == "RS") rowMeans(sapply(f$Trials, function(g) if(g$Distance == 1) {if(g$Hand == 2) g$Data$LeftSmooth else g$Data$RightSmooth} else rep(NA, 2000)), na.rm = TRUE) else rep(NA, 2000)), na.rm = TRUE)
                     
                     )

a$timepoint <- seq(from = 0.1, by = 0.1, to = 200)

a <- melt(a, id = "timepoint")
#a$group <- as.factor(c(rep("Position 4", 2000), rep("Position 1", 2000), rep("Position 4", 2000),rep("Position 1", 2000)))
#a$distance <- as.factor(c(rep("Ipsilateral", 4000), rep("Contralateral", 4000)))

ggplot(a, aes(timepoint, value)) + 
  geom_line(aes(color = variable, linetype = variable), size = 0.8) +
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  ylab("Microvolts (uV)")+
  xlab("Time (ms)")+
  ggtitle("Ipsilateral/Contralateral comparison of Position 1 and Position 4")+
  scale_linetype_manual(values=c(1, 1, 2, 2))+
  scale_color_manual(values=c("red", "blue", "red", "blue"))+
  
  theme_classic() +



  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))



####################################== BASIC ANOVA ==#############################################

a <- data.frame(Position1 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Full) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Full[1] else NA),
                Position2 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Full) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Full[2] else NA),
                Position3 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Full) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Full[3] else NA),
                Position4 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Full) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Full[4] else NA))


a <- na.omit(a)
a$id <- as.factor(seq(from=1, by=1, to=nrow(a)))
b <- melt(a)
colnames(b) <- c("id", "distance", 'AUC')

summary(aov_ez(data = b,id = "id", dv = "AUC", within = "distance"))


####################################== BASIC T-TESTS ==#############################################


a <- data.frame(Position1 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Full) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Full[1] else NA),
                Position2 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Full) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Full[2] else NA),
                Position3 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Full) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Full[3] else NA),
                Position4 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Full) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Full[4] else NA))

b <- data.frame(Position1 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Cons) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Cons[1] else NA),
                Position2 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Cons) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Cons[2] else NA),
                Position3 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Cons) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Cons[3] else NA),
                Position4 = sapply(Experiment, function (f) if(!is.null(f$TrialData$AUCMean$Scaled.Valid.Cons) & f$Responder == "RS") f$TrialData$AUCMean$Scaled.Valid.Cons[4] else NA))


t.test(a$Position1, a$Position2, paired = TRUE)

t.test(a$Position2, a$Position3, paired = TRUE)

t.test(a$Position3, a$Position4, paired = TRUE)

t.test(a$Position1, a$Position4, paired = TRUE)

t.test(a$Position2, a$Position4, paired = TRUE)

####################################== Point by Point ANOVA ==#############################################

b <- as.data.frame(matrix(ncol = 3, nrow=2000))
colnames(b) <- c("fvalue", "pvalue", "valid")
b$valid <- 0
for( x in 1:2000){
  
  a <- melt(rbind(do.call(cbind,sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Overall$Distance1[x])),
                  do.call(cbind,sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Overall$Distance2[x])),
                  do.call(cbind,sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Overall$Distance3[x])),
                  do.call(cbind,sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Overall$Distance4[x]))))
  
  temp <- aov_ez(id = "Var2", dv = "value", data = a, within = "Var1")
  
  b$fvalue[x] <- temp$anova_table$F
  b$pvalue[x] <- temp$anova_table$`Pr(>F)`
  
}

for(x in 1:1900){  if (all(b[x:(x+100), 2] < 0.05)) b[x:(x+100), 3] <- 1}

a <- seqToIntervals(as.numeric(row.names(b[b$valid == 1,])))/10

ggplot(b, aes(y=fvalue, x=(as.numeric(rownames(b))/10))) + 
  annotate("rect", xmin=a[1,1], xmax=a[1,2], ymin=0,ymax=15, alpha=0.6, fill="lightblue") +
  annotate("rect", xmin=a[2,1], xmax=a[2,2], ymin=0,ymax=15, alpha=0.6, fill="lightblue") +
  geom_line(aes(color = valid, group=1)) + 
  ylab("F value")+
  xlab("Time (ms)")+
  #ggtitle("Point-by-Point ANOVA of HBR magnitude")+
  scale_color_manual(values = c("0" = "gray", "1" = "black")) +
  theme_classic() +
  theme(legend.position="none") + 
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))
  

seqToHumanReadable(as.numeric(row.names(b[b$valid == 1,])))


####################################== Point by Point T-Tests ==#############################################

b <- as.data.frame(matrix(ncol = 4, nrow=2000))
colnames(b) <- c("tvalue", "pvalue", "signif", "valid")
b$signif <- 0
b$valid <- 0
b$valid <- as.numeric(b$valid) 

## Make sure to alter each Distance for what you want to test.

for( x in 1:2000){
  
  a <- t(rbind(do.call(cbind,sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Overall$Distance1[x])),
    do.call(cbind,sapply(Experiment, function(f) if(f$Responder == "RS") f$TrialData$Overall$Distance4[x]))))
  
  temp <- t.test(a[,1],a[,2], paired = TRUE, alternative="less")
  
  b$tvalue[x] <- abs(as.numeric(temp$statistic[1]))
  b$pvalue[x] <- temp$p.value
  
}

for(x in 1:1900){  
  
  if(b[x,2] <0.05) b[x,3] <- 1
  if (all(b[x:(x+100),2] < 0.05)) b[x:(x+100),4] <- 1}

b$valid <- as.factor(b$valid)

a <- seqToIntervals(as.numeric(row.names(b[b$valid == 1,])))/10

ggplot(b, aes(y=tvalue, x=(as.numeric(rownames(b))/10))) + 
  annotate("rect", xmin=a[1,1], xmax=a[1,2], ymin=0,ymax=7, alpha=0.6, fill="lightblue")+
  annotate("rect", xmin=a[2,1], xmax=a[2,2], ymin=0,ymax=7, alpha=0.6, fill="lightblue")+
  annotate("rect", xmin=a[3,1], xmax=a[3,2], ymin=0,ymax=7, alpha=0.6, fill="lightblue")+
  geom_line(aes(color = valid, group=1)) + 
  ylab("T value")+
  xlab("Time (ms)")+
  ggtitle("Point-by-Point T-Test of Position 1 vs Position 4")+
  scale_color_manual(values = c("0" = "gray", "1" = "black")) +
  theme_classic() +
  theme(legend.position="none") + 
  scale_y_continuous(limits = c(0,7))+
  scale_x_continuous(expand = c(0,0))

seqToHumanReadable(as.numeric(row.names(b[b$signif == 1,])))
seqToHumanReadable(as.numeric(row.names(b[b$valid == 1,])))

####################################== INDIVIDUAL CLASSIFICATION == ####################################

#All
temp <- data.frame(Responder = sapply(Experiment, function(f) f$Responder),
                   PercValid = sapply(Experiment, function(f) if(!is.null(f$TrialData)) round(f$TrialData$Validity$V2[1]/0.64, digits=1) else 0),
                   Valid = sapply(Experiment, function(f) if(!is.null(f$TrialData)) f$TrialData$Validity$V2[1] else 0),
                   Invalid = sapply(Experiment, function(f) if(!is.null(f$TrialData)) f$TrialData$Validity$V2[2] else 0),
                   TooEarly = sapply(Experiment, function(f) if(!is.null(f$TrialData)) f$TrialData$Validity$V2[3] else 0),
                   TooLate = sapply(Experiment, function(f) if(!is.null(f$TrialData)) f$TrialData$Validity$V2[4] else 0),
                   TooShort = sapply(Experiment, function(f) if(!is.null(f$TrialData)) f$TrialData$Validity$V2[5] else 0),
                   TooLong = sapply(Experiment, function(f) if(!is.null(f$TrialData)) f$TrialData$Validity$V2[6] else 0),
                   Noreaction = sapply(Experiment, function(f) if(!is.null(f$TrialData)) f$TrialData$Validity$V2[7] else 0))

#Responder
temp[temp$Responder == "RS",]

#Non-Responder
temp[temp$Responder == "NR",]


####################################== CONTRAST ANALYSIS PLOTTING == ####################################


Overall <- list()

Overall[[1]] <- as.data.frame(t(sapply(Experiment, function(f) if(!is.null(f$TrialData$Contrast$Between.All.Full$p.value)) f$TrialData$Contrast$Between.All.Full$p.value else c(NA,NA,NA,NA,NA))))
Overall[[2]] <- as.data.frame(t(sapply(Experiment, function(f) if(!is.null(f$TrialData$Contrast$Between.All.Cons$p.value)) f$TrialData$Contrast$Between.All.Cons$p.value else c(NA,NA,NA,NA,NA))))
Overall[[3]] <- as.data.frame(t(sapply(Experiment, function(f) if(!is.null(f$TrialData$Contrast$Within.All.Full$p.value)) f$TrialData$Contrast$Within.All.Full$p.value else c(NA,NA,NA,NA,NA))))
Overall[[4]] <- as.data.frame(t(sapply(Experiment, function(f) if(!is.null(f$TrialData$Contrast$Within.All.Cons$p.value)) f$TrialData$Contrast$Within.All.Cons$p.value else c(NA,NA,NA,NA,NA))))
Overall[[5]] <- as.data.frame(t(sapply(Experiment, function(f) if(!is.null(f$TrialData$Contrast$Between.Valid.Full$p.value)) f$TrialData$Contrast$Between.Valid.Full$p.value else c(NA,NA,NA,NA,NA))))
Overall[[6]] <- as.data.frame(t(sapply(Experiment, function(f) if(!is.null(f$TrialData$Contrast$Between.Valid.Cons$p.value)) f$TrialData$Contrast$Between.Valid.Cons$p.value else c(NA,NA,NA,NA,NA))))
Overall[[7]] <- as.data.frame(t(sapply(Experiment, function(f) if(!is.null(f$TrialData$Contrast$Within.Valid.Full$p.value)) f$TrialData$Contrast$Within.Valid.Full$p.value else c(NA,NA,NA,NA,NA))))
Overall[[8]] <- as.data.frame(t(sapply(Experiment, function(f) if(!is.null(f$TrialData$Contrast$Within.Valid.Cons$p.value)) f$TrialData$Contrast$Within.Valid.Cons$p.value else c(NA,NA,NA,NA,NA))))

for (i in 1:8) {
  
  Overall[[i]] <- as.data.frame(t(sapply(Experiment, function(f) if(!is.null(f$TrialData$Contrast[i][[1]]$p.value)) f$TrialData$Contrast[i][[1]]$p.value else c(NA,NA,NA,NA,NA))))
  colnames(Overall[[i]]) <- c("Linear", "Small", "LargeR", "LargeS", "ELarge")
  rownames(Overall[[i]]) <- 1:32
  Overall[[i]]$Responder <- as.character(lapply(Experiment, function(f) f$Responder))
  #Overall[[i]]$Validity <- as.character(lapply(Experiment, function(f) paste0(round(f$TrialData$Validity$V2[1]/0.64, digits=1), "%")))
  Overall[[i]]$Validity <- lapply(Experiment, function(f) if(!is.null(f$TrialData$Validity)) round(f$TrialData$Validity$V2[1]/0.64, digits = 1) else 0)
  Overall[[i]][,1:5] <- round(Overall[[i]][,1:5], digits = 4)
}

temp <- lapply(Overall, function(f) { 
  
  reference <- na.omit(f[which(f$Responder == "NR"),])
  reference[, 1:5] <- lapply(reference[, 1:5], function(f) as.numeric(as.character(f)))
  reference$Responder <- NULL
  temp <- ggtexttable(reference)
  
  for (i in 1:nrow(reference)){
    
    for (a in 1:6) {
      
      if(is.na(reference[i,a])) next
      
      #temp <- table_cell_bg(temp, (i+1),(1+a), fill = "lightblue1")
      temp <- table_cell_bg(temp, (i+1),(1+(which.min(reference[i,1:5]))), fill = "lightblue1")}}
  
  
  return(temp)
  
  
})

pdf("All-Between-Within.pdf",width = 10, height = 10,onefile = TRUE)
grid.draw(arrangeGrob(grobs=temp[1:4],layout_matrix = rbind(c(1,2),c(3,4))))
grid.newpage()
dev.off()

pdf("Valid-Between-Within.pdf",width = 10, height = 10,onefile = TRUE)
grid.draw(arrangeGrob(grobs=temp[5:8],layout_matrix = rbind(c(1,2),c(3,4))))
dev.off()




####################################== Graph Matrix plot ==#############################################

a <- data.frame(do.call(rbind,lapply(Experiment, function(f) (if(is.null(f$TrialData$AUCMean$Scaled.All.Full)) {
  c(NA, NA, NA, NA)}else{ f$TrialData$AUCMean$Scaled.All.Full}))),
  do.call(rbind,lapply(Experiment, function(f) (if(is.null(f$TrialData$AUCMean$Scaled.All.Full)) {
    c(NA, NA, NA, NA)}else{ f$TrialData$AUCMean$Scaled.All.Cons}))),
  do.call(rbind,lapply(Experiment, function(f) (if(is.null(f$TrialData$AUCMean$Scaled.Valid.Full)) {
    c(NA, NA, NA, NA)}else{ f$TrialData$AUCMean$Scaled.Valid.Full}))),
  do.call(rbind,lapply(Experiment, function(f) (if(is.null(f$TrialData$AUCMean$Scaled.Valid.Cons)) {
    c(NA, NA, NA, NA)}else{ f$TrialData$AUCMean$Scaled.Valid.Cons}))))

colnames(a) <- c("All Full 1", "All Full 2", "All Full 3", "All Full 4", "All Con 1", "All Con 2", "All Con 3", "All Con 4",
                 "Valid Full 1", "Valid Full 2", "Valid Full 3", "Valid Full 4", "Valid Con 1", "Valid Con 2", "Valid Con 3", "Valid Con 4")


a <- data.frame(do.call(rbind,lapply(Experiment, function(f) (if(is.null(f$TrialData$AUCMean$Raw.Full)) {
  c(NA, NA, NA, NA)}else{ f$TrialData$AUCMean$Raw.Full}))),
  do.call(rbind,lapply(Experiment, function(f) (if(is.null(f$TrialData$AUCMean$Raw.Cons)) {
    c(NA, NA, NA, NA)}else{ f$TrialData$AUCMean$Raw.Cons}))))

colnames(a) <- c("Raw Full 1", "Raw Full 2", "Raw Full 3", "Raw Full 4", "Raw Con 1", "Raw Con 2", "Raw Con 3", "Raw Con 4")

a <- cbind(a,
           data.frame(Onset = sapply(Experiment, function(f) if(!is.null(f$TrialData$Averages))f$TrialData$Averages$Mean[[1]] else NA),
                Peak = sapply(Experiment, function(f) if(!is.null(f$TrialData$Averages))f$TrialData$Averages$Mean[[2]] else NA),
                Offset = sapply(Experiment, function(f) if(!is.null(f$TrialData$Averages))f$TrialData$Averages$Mean[[3]] else NA),
                Duration = sapply(Experiment, function(f) if(!is.null(f$TrialData$Averages))f$TrialData$Averages$Mean[[4]] else NA),
                Peak.uV = sapply(Experiment, function(f) if(!is.null(f$TrialData$Averages))f$TrialData$Averages$Mean[[5]] else NA),
                Score = sapply(Experiment, function(f) f$WritingComponent$Score),
                Age = sapply(Experiment, function(f) f$Age),
                Validity = sapply(Experiment, function(f) if(!is.null(f$TrialData$Validity)) as.numeric(as.character(f$TrialData$Validity[1,2]))/64 else NA),
                Impedance = sapply(Experiment, function(f) mean(c(f$Impedance$Left,f$Impedance$Right))),
                Intensity = sapply(Experiment, function(f) mean(c(f$Intensity$Block1$Working, f$Intensity$Block2$Working))),
                Threshold = sapply(Experiment, function(f) if(!is.null(f$Threshold)) f$Threshold else NA),
                #pvalue = as.numeric(sapply(Experiment, function(f) if(is.null(f$TrialData$Contrast1)) NA else{ as.character(f$TrialData$Contrast1$p.value[[which(f$TrialData$Contrast1$p.value == min(f$TrialData$Contrast1$p.value))]])})),
                FinalGroup = (c(2,4,1,4,4,4,3,3,4,2,1,2,1,2,1,1,2,2,2,2,3,1,4,2,3,2,4,4,4,2,2,1)),
                Responder = sapply(Experiment, function(f) f$Responder),
                Gender = sapply(Experiment, function(f) f$Gender),
                Hand = sapply(Experiment, function(f) f$StartingHand),
                Condition = sapply(Experiment, function(f) f$Condition)
                ))




ggpairs(data=a)


a <- data.frame(Lefteye = sapply(Experiment, function(f) if(!is.null(f$TrialData$Overall)) auc(1:(200*SampRate), f$TrialData$Overall$Left, type = "linear") else NA),
                Righteye = sapply(Experiment, function(f) if(!is.null(f$TrialData$Overall)) auc(1:(200*SampRate), f$TrialData$Overall$Right, type = "linear") else NA),
                Eyediff = sapply(Experiment, function(f) if(!is.null(f$TrialData$Overall)) auc(1:(200*SampRate), f$TrialData$Overall$Right, type = "linear") - auc(1:(200*SampRate), f$TrialData$Overall$Left, type = "linear") else NA))
                

####################################== Writing Task Statistics == ####################################

data.frame(V1 = c("Overall", "Tested", "Control"),
           Mean = c(mean(sapply(Experiment, function (f) f$WritingComponent$Score), na.rm = TRUE),
                    mean(sapply(Experiment, function (f) if(f$Condition == "T") f$WritingComponent$Score else NA), na.rm = TRUE),
                    mean(sapply(Experiment, function (f) if(f$Condition == "C")f$WritingComponent$Score else NA), na.rm = TRUE)),
           SD = c(sd(sapply(Experiment, function (f) f$WritingComponent$Score), na.rm = TRUE),
                  sd(sapply(Experiment, function (f) if(f$Condition == "T") f$WritingComponent$Score else NA), na.rm = TRUE),
                  sd(sapply(Experiment, function (f) if(f$Condition == "C")f$WritingComponent$Score else NA), na.rm = TRUE)),
           RangeStart = c(range(sapply(Experiment, function (f) f$WritingComponent$Score), na.rm = TRUE)[1],
                          range(sapply(Experiment, function (f) if(f$Condition == "T") f$WritingComponent$Score else NA), na.rm = TRUE)[1],
                          range(sapply(Experiment, function (f) if(f$Condition == "C") f$WritingComponent$Score else NA), na.rm = TRUE)[1]),
           RangeEND = c(range(sapply(Experiment, function (f) f$WritingComponent$Score), na.rm = TRUE)[2],
                        range(sapply(Experiment, function (f) if(f$Condition == "T") f$WritingComponent$Score else NA), na.rm = TRUE)[2],
                        range(sapply(Experiment, function (f) if(f$Condition == "C") f$WritingComponent$Score else NA), na.rm = TRUE)[2]))

a <- data.frame(T = sapply(Experiment, function (f) if(f$Condition == "T") f$WritingComponent$Score else NA),
                C = sapply(Experiment, function (f) if(f$Condition == "C")f$WritingComponent$Score else NA))


####################################== Writing Task Plots == ####################################

c <- na.omit(data.frame(Score = sapply(Experiment, function(f) f$WritingComponent$Score), Status = sapply(Experiment, function(f) f$Condition)))
c$Status <- as.character(c$Status)
c[c$Status == "C",]$Status <- "Control"
c[c$Status == "T",]$Status <- "Tested"
ggplot(c, aes(x=Status, y = Score)) + 
  geom_boxplot(fill='#A4A4A4', color="black") + 
  xlab("Condition") + 
  ylab("STAI Score") +
  scale_y_continuous(limits=c(4, 24),breaks = seq(from=4,by=2,to=24), expand = c(0,0)) +
  theme_classic()




####################################== HERE LIES DRAGONS == ####################################

#Block1 vs Block2 AUC comparison chart

a <- data.frame( Block1 = sapply(Experiment, function(f) if(!is.null(f$TrialData) & f$Condition == "T") mean(f$TrialData$AUC$Scaled.Valid.Full[1:32], na.rm = TRUE) else NA),
                 Block2 = sapply(Experiment, function(f) if(!is.null(f$TrialData) & f$Condition == "T") {if (nrow(f$TrialData$AUC) > 32) mean(f$TrialData$AUC$Scaled.Valid.Full[33:64], na.rm = TRUE) else NA} else NA))

a$id <- as.factor(seq(from=1, to=32, by=1))
a$Group <- 1
a[(a$Block1 > a$Block2) & (!is.na(a$Block1) & (!is.na(a$Block2))),]$Group <- 1
a[(a$Block1 < a$Block2) & (!is.na(a$Block1) & (!is.na(a$Block2))),]$Group <- 2
a$Group <- as.factor(a$Group)

b <- melt(a)


ggplot(b, aes(x=variable, y = value, linetype = Group, col = Group, group = id)) + 
  geom_boxplot(aes(group = variable),position=position_dodge(width = 0.05), outlier.shape = NA) +
  geom_point(position=position_dodge(width = 0.05)) + 
  theme_classic() +
  geom_line(position=position_dodge(width = 0.05)) +
  theme(legend.position="none") +
  ylab("Response z-score") +
  xlab("") +
  ggtitle("Control condition - Block 1 vs Block 2")


a <- data.frame( Pos1 = c(sapply(Experiment, function(f) if(!is.null(f$TrialData) & f$Condition == "C") mean(f$TrialData$AUC[(f$TrialData$AUC$Distance == 1) & (as.numeric(f$TrialData$AUC$Stimuli) <=32),]$Scaled.All.Full, na.rm = TRUE) else NA),
                          sapply(Experiment, function(f) if(!is.null(f$TrialData) & f$Condition == "C") mean(f$TrialData$AUC[(f$TrialData$AUC$Distance == 1) & (as.numeric(f$TrialData$AUC$Stimuli) >=32),]$Scaled.All.Full, na.rm = TRUE) else NA)),
                 Pos2 = c(sapply(Experiment, function(f) if(!is.null(f$TrialData) & f$Condition == "C") mean(f$TrialData$AUC[(f$TrialData$AUC$Distance == 2) & (as.numeric(f$TrialData$AUC$Stimuli) <=32),]$Scaled.All.Full, na.rm = TRUE) else NA),
                          sapply(Experiment, function(f) if(!is.null(f$TrialData) & f$Condition == "C") mean(f$TrialData$AUC[(f$TrialData$AUC$Distance == 2) & (as.numeric(f$TrialData$AUC$Stimuli) >=32),]$Scaled.All.Full, na.rm = TRUE) else NA)),
                 Pos3 = c(sapply(Experiment, function(f) if(!is.null(f$TrialData) & f$Condition == "C") mean(f$TrialData$AUC[(f$TrialData$AUC$Distance == 3) & (as.numeric(f$TrialData$AUC$Stimuli) <=32),]$Scaled.All.Full, na.rm = TRUE) else NA),
                          sapply(Experiment, function(f) if(!is.null(f$TrialData) & f$Condition == "C") mean(f$TrialData$AUC[(f$TrialData$AUC$Distance == 3) & (as.numeric(f$TrialData$AUC$Stimuli) >=32),]$Scaled.All.Full, na.rm = TRUE) else NA)),
                 Pos4 = c(sapply(Experiment, function(f) if(!is.null(f$TrialData) & f$Condition == "C") mean(f$TrialData$AUC[(f$TrialData$AUC$Distance == 4) & (as.numeric(f$TrialData$AUC$Stimuli) <=32),]$Scaled.All.Full, na.rm = TRUE) else NA),
                          sapply(Experiment, function(f) if(!is.null(f$TrialData) & f$Condition == "C") mean(f$TrialData$AUC[(f$TrialData$AUC$Distance == 4) & (as.numeric(f$TrialData$AUC$Stimuli) >=32),]$Scaled.All.Full, na.rm = TRUE) else NA)),
                 Block = as.factor(rep(1:2, each=32)),
                 id = as.factor(c(1:32,1:32)))

colnames(a) <- c("1","2","3","4", "Block", "id")

b <- melt(a)

           
ggplot(b, aes(x=Block, y = value, group = interaction(Block,variable), col = variable)) + 
  geom_boxplot(position=position_dodge(width = 1), outlier.shape = NA) +
  geom_point(position=position_dodge(width = 1)) + 
  theme_classic() +
  #geom_line() +
  #theme(legend.position="none") +
  ylab("Response z-score") +
  xlab("") +
  ggtitle("Anxiety condition - Pre-Post Distance Comparison")

ggplot(b, aes(x=variable, y = value, group = interaction(Block,variable), col = Block)) + 
  geom_boxplot(position=position_dodge(width = 1)) +
  geom_point(position=position_dodge(width = 1)) + 
  theme_classic() +
  #geom_line(aes(group = id)) +
  #theme(legend.position="none") +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 2.5) +
  geom_vline(xintercept = 3.5) +
  geom_vline(xintercept = 4.5) +
  ylab("Response z-score") +
  xlab("Distance") +
  ggtitle("Control condition - Pre-Post Distance Comparison")
  
c <- melt(a[a$Block == 1,])
c$id <- as.factor(c(1:128))
summary(contrast(emmeans(aov_ez(id = "id", dv = "value", data = c, between = "variable"),
                         ~variable, weights = "proportional"), Contrasts, adjust = "None"))

###################################################################################################################################

##Validity vs pvalue comparisons 

a <- Overall[[5]][(Overall[[5]]$Responder == "RS"),]
a$Validity <- as.numeric(a$Validity)

a <- Overall[[5]]
a$Validity <- as.numeric(a$Validity)



b <- as.data.frame(rbind(cbind(a$Linear,a$Validity),
                         cbind(a$Small, a$Validity),
                         cbind(a$LargeR, a$Validity),
                         cbind(a$LargeS, a$Validity),
                         cbind(a$ELarge, a$Validity)))

plot(b$V1, b$V2)



b <- melt(cbind(a$ELarge,as.numeric(a$Validity)))

plot(a$ELarge, a$Validity)
summary(lm(data=b, Var1 ~ value))



boxplot(a, ylim = c(0,1.0)) + abline(h = 0.05, col = "red")
boxplot(a, ylim = c(0,1.0)) + abline(h = 0.05, col = "red")

structure(list(Linear = c(0.962856, 0.012708, 0.731336, 0.010802, 
                          0.63938, 0.019496, 0.055758, 0.261634, 0.926184, 0.346157, 0.00883, 
                          0.013447, 0.019961, 0.867955, 0.154055, 0.000018), Small = c(0.872303, 
                                                                                       0.003072, 0.614508, 0.000654, 0.106848, 0.001486, 0.072682, 0.070964, 
                                                                                       0.585681, 0.871475, 0.000888, 0.028755, 0.001523, 0.402558, 0.280594, 
                                                                                       0), LargeR = c(0.932312, 0.009182, 0.982861, 0.002649, 0.186377, 
                                                                                                      0.006395, 0.059834, 0.1043, 0.540179, 0.414748, 0.004168, 0.019147, 
                                                                                                      0.004231, 0.861054, 0.288505, 0.000001), LargeS = c(0.78642, 
                                                                                                                                                          0.076776, 0.691181, 0.04576, 0.434187, 0.075298, 0.105299, 0.259324, 
                                                                                                                                                          0.589912, 0.227984, 0.072254, 0.03984, 0.032589, 0.673997, 0.399028, 
                                                                                                                                                          0.001962), ELarge = c(0.781688, 0.152414, 0.373306, 0.398036, 
                                                                                                                                                                                0.167196, 0.447565, 0.186991, 0.795171, 0.3429, 0.389052, 0.171043, 
                                                                                                                                                                                0.050996, 0.32681, 0.928153, 0.106056, 0.101759)), row.names = c(1L, 
                                                                                                                                                                                                                                                 3L, 10L, 11L, 12L, 13L, 14L, 15L, 18L, 19L, 22L, 24L, 26L, 30L, 
                                                                                                                                                                                                                                                 31L, 32L), class = "data.frame")

structure(list(Linear = c(0.0855, 0.1084, 0.1222, 0.1753, 0.0591, 
                          0.1018, 0.1321, 0.0994, 0.1347, 0.0866, 0.081, 0.1143, 0.137,
                          0.125, 0.1229), Small = c(0.0777, 0.0256, 0.0176, 0.4825, 0.1097, 
                                                    0.2374, 0.0223, 0.2012, 0.178, 0.0407, 0.0492, 0.0492, 0.0128, 
                                                    0.0212, 0.0263), LargeR = c(0.0077, 0.0446, 0.0596, 0.1328, 0.0009, 
                                                                                0.0239, 0.0458, 0.0152, 0.0221, 0.0292, 0.0217, 0.0224, 0.0655, 
                                                                                0.0476, 0.099), LargeS = c(0.1532, 0.2594, 0.2913, 0.0101, 0.1142, 
                                                                                                           0.0417, 0.2643, 0.0587, 0.0902, 0.2209, 0.201, 0.2022, 0.3052, 
                                                                                                           0.2682, 0.3491), ELarge = c(0.5388, 0.5274, 0.5342, 0.5312, 0.4837, 
                                                                                                                                       0.5499, 0.5862, 0.5604, 0.6384, 0.5006, 0.5005, 0.5852, 0.5611, 
                                                                                                                                       0.5646, 0.4607)), row.names = c(1L, 3L, 10L, 11L, 12L, 13L, 14L, 
                                                                                                                                                                       15L, 18L, 19L, 22L, 24L, 26L, 30L, 31L), class = "data.frame")





#TEST IDEA

test4 <- NULL
for (i in 1:32) {

test <- data.frame(Valid = sapply(Experiment[[i]]$Trials, function(f) f$Valid),
           Distance = sapply(Experiment[[i]]$Trials, function(f) f$Distance))

test2 <- test[test$Valid == 1,]

test3 <- as.data.frame(table(test2$Distance))$Freq

if(is.null(test4)) test4 <- test3 else rbind(test4, test3)}





b <- data.frame("1" = sapply(Experiment, function(f) if(length(f$Trials) == 64) sum(sapply(f$Trials, function(g) if(g$Distance == 1 && g$Valid == 1) 1 else 0)) else NA),
                "2" = sapply(Experiment, function(f) if(length(f$Trials) == 64) sum(sapply(f$Trials, function(g) if(g$Distance == 2 && g$Valid == 1) 1 else 0)) else NA),
                "3" = sapply(Experiment, function(f) if(length(f$Trials) == 64) sum(sapply(f$Trials, function(g) if(g$Distance == 3 && g$Valid == 1) 1 else 0)) else NA),
                "4" = sapply(Experiment, function(f) if(length(f$Trials) == 64) sum(sapply(f$Trials, function(g) if(g$Distance == 4 && g$Valid == 1) 1 else 0)) else NA),
                "Responder" = sapply(Experiment, function(f) f$Responder))

c <- b[b$Responder == "NR",]

########################################## == Shaded Plot for presentations == ######################################################

ymax1 <- max(max(ExtractVal("1==1", "f$Data$BothSmooth", i)), 30)
ggplot(Experiment[[i]]$Trials[[x]]$Data) + 
  geom_line(aes(x=Timepoint, y=BothSmooth), color = "black") +
  #geom_area(data=Experiment[[i]]$Trials[[x]]$Data[(Experiment[[i]]$Trials[[x]]$Wave1$Onset*10):(Experiment[[i]]$Trials[[x]]$Wave1$Offset*SampRate),], aes(x=Timepoint, y=BothSmooth)) +
  geom_area(aes(x=Timepoint, y=BothSmooth)) +
  #geom_line(aes(x=Timepoint, y=LeftSmooth), color = "red") +
  #geom_line(aes(x=Timepoint, y=RightSmooth), color = "green") +
  #annotate("rect", ymin = 0, ymax =Experiment[[i]]$Trials[[x]]$Wave1$Peak.uv, xmin = Experiment[[i]]$Trials[[x]]$Wave1$Onset, xmax = Experiment[[i]]$Trials[[x]]$Wave1$Offset, alpha = .2) +
  #annotate("text", label = paste("Trial: ", x , " - Distance: ", Experiment[[i]]$Trials[[x]]$Distance, "\n" ,
  #                               Experiment[[i]]$Trials[[x]]$Wave1$Onset , "-", Experiment[[i]]$Trials[[x]]$Wave1$Offset, "ms - Dur: ", Experiment[[i]]$Trials[[x]]$Wave1$Duration, "\n",
  #                               "ISI:", Experiment[[i]]$Trials[[x]]$ISI, "-Status:", Experiment[[i]]$Trials[[x]]$ExclusionReason ), 
  #         y=Inf, x=Inf, vjust=1, hjust=1, size = 3.5) +
  scale_y_continuous(expand = c(0,0), limits = c(0,45))+
  scale_x_continuous(expand = c(0,0))+
  #  ylab("Microvolts(uV)")+
  #  xlab("Milliseconds(ms)")+
  #ylab("")+
  #xlab("")+
  theme_classic()  +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())  
#ggtitle("Participant 4 - Trial 13 - Both Smoothed") 
#expand_limits(x = c(1:200)) 
#  geom_hline(aes(colour = "red", yintercept = Experiment[[i]]$Trials[[x]]$Threshold),show.legend = FALSE )

########################################## == Raw unrectified plot for presentation == ######################################################

ggplot(Experiment[[5]]$Trials[[4]]$Data) + 
  geom_line(aes(x=Timepoint, y=LeftRaw), color = "black") +
  #geom_line(aes(x=Timepoint, y=LeftSmooth), color = "red") +
  #geom_line(aes(x=Timepoint, y=RightSmooth), color = "green") +
  #annotate("rect", ymin = 0, ymax =Experiment[[i]]$Trials[[x]]$Wave1$Peak.uv, xmin = Experiment[[i]]$Trials[[x]]$Wave1$Onset, xmax = Experiment[[i]]$Trials[[x]]$Wave1$Offset, alpha = .2) +
  #annotate("text", label = paste("Trial: ", x , " - Distance: ", Experiment[[i]]$Trials[[x]]$Distance, "\n" ,
  #                               Experiment[[i]]$Trials[[x]]$Wave1$Onset , "-", Experiment[[i]]$Trials[[x]]$Wave1$Offset, "ms - Dur: ", Experiment[[i]]$Trials[[x]]$Wave1$Duration, "\n",
  #                               "ISI:", Experiment[[i]]$Trials[[x]]$ISI, "-Status:", Experiment[[i]]$Trials[[x]]$ExclusionReason ), 
  #         y=Inf, x=Inf, vjust=1, hjust=1, size = 3.5) +
  #scale_y_continuous(expand = c(0,0), limits = c(0,ymax1))+
  #scale_x_continuous(expand = c(0,0))+
  #ylab("Microvolts(uV)")+
  #xlab("Milliseconds(ms)")+
#ylab("")+
#xlab("")+
theme_classic()  +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())  
#ggtitle("Participant 4 - Trial 1 - Both Smoothed") +
#expand_limits(x = c(1:200)) +
#  geom_hline(aes(colour = "red", yintercept = Experiment[[i]]$Trials[[x]]$Threshold),show.legend = FALSE )


########################################## == Specific trial plot for presentation == ######################################################


ymax1 <- max(max(ExtractVal("1==1", "f$Data$BothSmooth", i)), 30)
temp <- Experiment[[i]]$Trials[[x]]$Data[(Experiment[[i]]$Trials[[x]]$Wave1$Onset*SampRate):(Experiment[[i]]$Trials[[x]]$Wave1$Offset*SampRate),]
temp1<- Experiment[[i]]$Trials[[x]]$Data

ggplot(temp1) + 
  geom_line(aes(x=Timepoint, y=BothSmooth), color = "black") +
  #geom_line(aes(x=Timepoint, y=LeftSmooth), color = "red") +
  #geom_line(aes(x=Timepoint, y=RightSmooth), color = "green") +
  geom_hline(yintercept = Experiment[[i]]$Threshold, color = "orange") +
  annotate("rect", ymin = 0, ymax =Experiment[[i]]$Trials[[x]]$Wave1$Peak.uv, xmin = Experiment[[i]]$Trials[[x]]$Wave1$Onset, xmax = Experiment[[i]]$Trials[[x]]$Wave1$Offset, alpha = .2) +
  #annotate("text", label = paste("Trial: ", x , " - Distance: ", Experiment[[i]]$Trials[[x]]$Distance, "\n" ,
  #                               Experiment[[i]]$Trials[[x]]$Wave1$Onset , "-", Experiment[[i]]$Trials[[x]]$Wave1$Offset, "ms - Dur: ", Experiment[[i]]$Trials[[x]]$Wave1$Duration, "\n",
  #                               "ISI:", Experiment[[i]]$Trials[[x]]$ISI, "-Status:", Experiment[[i]]$Trials[[x]]$ExclusionReason ), 
  #         y=Inf, x=Inf, vjust=1, hjust=1, size = 3.5) +
  scale_y_continuous(expand = c(0,0), limits = c(0,ymax1))+
  scale_x_continuous(expand = c(0,0))+
  ylab("Microvolts(uV)")+
  xlab("Milliseconds(ms)")+
  #ylab("")+
  #xlab("")+
  theme_classic()  +
  # theme(axis.line=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.ticks=element_blank(),
  #       axis.title.x=element_blank(),
  #       axis.title.y=element_blank(),
  #       legend.position="none",
  #       panel.background=element_blank(),
  #       panel.border=element_blank(),
  #       panel.grid.major=element_blank(),
  #       panel.grid.minor=element_blank(),
#       plot.background=element_blank())
ggtitle(paste0("Participant: ", i, " - Trial: ", x, " - Both Smoothed")) 
#expand_limits(x = c(1:200)) +
#geom_hline(aes(colour = "red", yintercept = Experiment[[i]]$Trials[[x]]$Threshold),show.legend = FALSE )

