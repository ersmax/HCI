#create data for SUS total
# odd  Q  -> subtract 1 from score
# even Q  -> subtract their value from 5
# sum total score, then multiply by 2.5

#install package. useful info at: https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/
#install.packages("fmsb")
#load the package
library(fmsb)
library(ggplot2)

#helper function to customise radar chart
pretty_radard <- function(data,
                          color="#00AFBB",
                          vlabels = colnames(data),
                          vlcex = 0.7,
                          caxislabels = NULL,
                          title = NULL, ...) {
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    seg=5,
    caxislabels = caxislabels, title = title, ...
  )
}


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
  Q1 = c(5,4,5,5,5,5,4,4,5,5),
  Q2 = c(2,1,2,2,2,1,2,2,1,1),
  Q3 = c(2,4,4,3,3,4,2,4,4,4),
  Q4 = c(1,1,1,1,1,1,1,1,1,1),
  Q5 = c(3,3,4,3,2,4,3,4,3,3),
  Q6 = c(4,4,3,4,5,3,3,3,4,5),
  Q7 = c(4,5,4,5,4,4,5,4,5,4),
  Q8 = c(2,3,3,2,3,1,2,2,1,2),
  Q9 = c(5,5,5,5,5,5,5,5,5,5),
  Q10= c(3,3,2,3,3,3,3,3,2,2)
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
  Q1 = c(5,5,4,5,5,5,5,5,4,5),
  Q2 = c(1,1,1,1,1,1,1,1,1,1),
  Q3 = c(5,5,5,5,5,5,5,5,5,5),
  Q4 = c(1,1,1,1,1,1,1,1,1,1),
  Q5 = c(5,5,5,5,5,5,5,5,5,5),
  Q6 = c(1,1,1,1,1,1,1,1,1,1),
  Q7 = c(5,5,5,5,4,5,4,5,5,4),
  Q8 = c(2,3,3,3,2,2,3,4,3,2),
  Q9 = c(3,2,3,2,3,3,3,2,3,2),
  Q10= c(3,4,5,4,3,3,4,5,5,4)
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

#define standard error
stderror <- function(x) sd(x)/sqrt(length(x))

#computing mean, sd deviation, SE for Module1
summary(total_score$Module1)
mean(total_score$Module1)
sd(total_score$Module1)
stderror(total_score$Module1)


#computing mean, sd deviation, SE for Module2
summary(total_score$Module2)
mean(total_score$Module2)
sd(total_score$Module2)
stderror(total_score$Module2)


summaryData <- data.frame(
  row.names = c("web",
                "mobile"),
  name = c("Module 1","Module 2"),
  mean = c(mean(total_score$Module1),
           mean(total_score$Module2)),
  stderror = c(stderror(total_score$Module1),
               stderror(total_score$Module2))
  )
summaryData


myPlot <- ggplot(summaryData,
                 aes(x=name,
                 y=mean)) +
  geom_bar(stat="identity", 
           color="black",
           fill="firebrick",
           width=0.4,
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-stderror,
                    ymax=mean+stderror),
                width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label=round(mean,2)),size=5,
            position=position_dodge(0.85),
            vjust = -0.5)
  
myPlot + coord_cartesian(ylim=c(70,90)) + labs(title="Modules Mean Diferences in SUS Final Scores", 
                                               subtitle="Our study - Masarin, Chen, Wang, Herrmann",
                                               caption = "Data collected on 10 participants",
                                               x = NULL,
                                               y = "SUS Final Score (mean)")




#====== T-test on YUEN ===========
dataYuen <- data.frame(
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
  Module1 = c(90,82.5,92.5,80,90,52.5,57.5,97.5,77.5,87.5),
  Module2 = c(75,92.5,82.5,80,92.5,55,57.5,95,77.5,90)
  )

a<- c(90,82.5,92.5,80,90,52.5,57.5,97.5,77.5,87.5)
b<- c(75,92.5,82.5,80,92.5,55,57.5,95,77.5,90)
summaryYuen <- data.frame(
  row.names = c("Module1",
                "Module2"),
  mean = c(mean(dataYuen$Module1),
           mean(dataYuen$Module2)),
  sd = c(sd(dataYuen$Module1),
         sd(dataYuen$Module2))
)

#data Yuen
dataYuen
summaryYuen
var(dataYuen$Module1)
var(dataYuen$Module2)

#2tailed t-test 95% CI on Yuen study
t.test(a,b,
       var.equal = FALSE,
       alternative="two.sided",
       paired=FALSE)

#1tailed t-test 95% CI on Yuen study
t.test(a,b,
       var.equal = FALSE,
       alternative="greater",
       paired=FALSE)

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



