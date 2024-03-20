#Data on Module1
web_scores <- data.frame(
  row.names = c("Participant.1",
                "Participant.2",
                "Participant.3",
                "Participant.4",
                "Participant.5",
                "Participant.6",
                "Participant.7",
                "Participant.8",
                "Participant.9",
                "Participant.10"),
  Q1 = c(5,5,5,5,5,5,5,5,5,5),
  Q2 = c(1,1,1,1,2,1,1,1,1,1),
  Q3 = c(4,4,4,4,5,4,4,4,4,4),  
  Q4 = c(1,1,1,1,1,1,1,1,1,1),
  Q5 = c(4,5,5,5,5,4,5,5,5,5),
  Q6 = c(1,1,1,2,1,1,1,1,1,2),
  Q7 = c(5,5,5,3,5,5,5,5,5,5),
  Q8 = c(5,4,5,5,5,3,5,5,5,5),   
  Q9 = c(5,5,5,5,5,5,5,5,5,5),
  Q10= c(5,5,3,4,4,5,5,5,5,5)   
)

#Data on Module2
mobile_scores <- data.frame(
  row.names = c("Participant 1",
                "Participant.2",
                "Participant.3",
                "Participant.4",
                "Participant.5",
                "Participant.6",
                "Participant.7",
                "Participant.8",
                "Participant.9",
                "Participant.10"),
  Q1 = c(5,5,5,5,5,5,5,5,5,5),
  Q2 = c(1,2,1,1,1,1,1,1,1,1),
  Q3 = c(5,4,4,4,5,4,4,4,4,5),
  Q4 = c(1,1,1,1,1,1,1,1,1,1),
  Q5 = c(5,5,5,5,5,5,5,5,5,5),
  Q6 = c(1,1,1,2,1,1,1,2,1,1),
  Q7 = c(5,5,5,3,5,5,5,5,5,5),
  Q8 = c(5,4,5,5,5,4,5,5,5,5),
  Q9 = c(5,5,5,5,5,5,5,5,5,5),
  Q10= c(5,5,5,4,4,5,4,5,5,5)
)

labels <- data.frame(
  row.names = c("web-based (Module1)",
                "mobile (Module2)")
)
labels

web_scores
mobile_scores

#module1
web_Qadj <- data.frame(
  row.names = c("Participant 1",
                "P2",
                "P3",
                "P4",
                "P5",
                "P6",
                "P7",
                "P8",
                "P9",
                "P10"),
  Q1 = web_scores$Q1 - 1,
  Q2 = 5 - web_scores$Q2,
  Q3 = web_scores$Q3 - 1,
  Q4 = 5 - web_scores$Q4, 
  Q5 = web_scores$Q5 - 1,
  Q6 = 5 - web_scores$Q6,
  Q7 = web_scores$Q7 - 1,
  Q8 = 5 - web_scores$Q8,
  Q9 = web_scores$Q9 - 1,
  Q10= 5 - web_scores$Q10
)
web_Qadj

#module2
mobile_Qadj <- data.frame(
  row.names = c("Participant 1",
                "Participant 2",
                "Participant 3",
                "Participant 4",
                "Participant 5",
                "Participant 6",
                "Participant 7",
                "Participant 8",
                "Participant 9",
                "Participant 10"),
  Q1 = mobile_scores$Q1 - 1,
  Q2 = 5 - mobile_scores$Q2,
  Q3 = mobile_scores$Q3 - 1,
  Q4 = 5 - mobile_scores$Q4, 
  Q5 = mobile_scores$Q5 - 1,
  Q6 = 5 - mobile_scores$Q6,
  Q7 = mobile_scores$Q7 - 1,
  Q8 = 5 - mobile_scores$Q8,
  Q9 = mobile_scores$Q9 - 1,
  Q10= 5 - mobile_scores$Q10
)
mobile_Qadj

#sum multiplied by 2.5 to get adjustment
web_adj <- rowSums(web_Qadj) * 2.5
web_adj

mobile_adj <- rowSums(mobile_Qadj) * 2.5
mobile_adj

total_score <- data.frame(
  row.names = c("Participant.1",
                "Participant.2",
                "Participant.3",
                "Participant.4",
                "Participant.5",
                "Participant.6",
                "Participant.7",
                "Participant.8",
                "Participant.9",
                "Participant.10"),
  Module1 = web_adj,
  Module2 = mobile_adj
)
total_score


#filling data
myData <- data.frame(
  row.names = c("P.1","P.2","P.3","P.4","P.5","P.6","P.7","P.8","P.9","P.10"),
  web = total_score$Module1,
  mob = total_score$Module2
)
myData

summaryData <- data.frame(
  row.names = c("web",
                "mobile"),
  name = c("web","mobile"),
  mean = c(mean(myData$web),
           mean(myData$mob)),
  std = c(sd(myData$web),
          sd(myData$mob))
)
summaryData


#plotting
library(ggplot2)
myPlot <- ggplot(summaryData,
                 aes(x=name,
                     y=mean)) +
  geom_bar(stat="identity", 
           color="black",
           fill="grey65",
           width=0.3,
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-std,
                    ymax=mean+std),
                width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label=round(mean,2)),size=4,
            position=position_dodge(0.85),
            vjust = -2.5)

myPlot + coord_cartesian(ylim=c(0,100)) + labs(title="Mean Diferences in user's perception", 
                                               subtitle="error bars ± 1 standard deviation",
                                               caption = "Data collected on 10 participants",
                                               x = NULL,
                                               y = "SUS total score (0-100)")


#=========T-TEST on OUR STUDY ==============
dataOur <- data.frame(
  row.names = c("p1",
                "p2",
                "p3",
                "p4",
                "p5",
                "p6",
                "p7",
                "p8",
                "p9",
                "p10"),
  Module1 = web_adj,
  Module2 = mobile_adj
)
dataOur

mySample1 <- web_adj
mySample2 <- mobile_adj


summary <- data.frame(
  row.names = c("Module1",
                "Module2"),
  mean = c(mean(dataOur$Module1),
           mean(dataOur$Module2)),
  sd = c(sd(dataOur$Module1),
         sd(dataOur$Module2))
)

#data our Study
dataOur
summary
var(dataOur$Module1)
var(dataOur$Module2)

#2tailed t-test 95% CI on Our study
t.test(mySample1,mySample2,
       var.equal = FALSE,
       alternative="two.sided",
       paired=FALSE)

#1tailed t-test 95% CI on Our study
t.test(mySample1,mySample2,
       var.equal = FALSE,
       alternative="less",
       paired=FALSE)