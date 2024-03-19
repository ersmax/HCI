#import data
task1web <- c(18,15,18,20,17,17,18,18,19,17)
task1mob <- c(16,18,18,19,17,18,17,17,18,19)
task2web <- c(20,16,19,19,20,21,20,19,20,21)
task2mob <- c(20,15,17,19,19,20,19,18,22,21)
task3web <- c(10,9,9,10,11,10,9,8,10,9)
task3mob <- c(9,9,8,10,10,8,9,7,9,8)
task4web <- c(15,16,18,14,15,14,13,15,17,15)
task4mob <- c(16,16,17,15,15,15,12,16,18,17)
task5web <- c(10,9,9,8,10,11,9,10,12,9)
task5mob <- c(10,8,9,9,10,10,9,11,10,9)
task6web <- c(5,4,3,3,4,6,5,5,5,4)
task6mob <- c(6,3,4,4,4,7,5,6,6,5)
totweb <- task1web+task2web+task3web+task4web+task5web+task6web
totmob <- task1mob+task2mob+task3mob+task4mob+task5mob+task6mob

#filling data
myData <- data.frame(
  row.names = c("P.1","P.2","P.3","P.4","P.5","P.6","P.7","P.8","P.9","P.10"),
  web = totweb,
  mob = totmob
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

myPlot + coord_cartesian(ylim=c(0,100)) + labs(title="Mean Diferences in seconds", 
                                              subtitle="error bars ± 1 standard deviation",
                                              caption = "Data collected on 10 participants",
                                              x = NULL,
                                              y = "completion time (seconds)")


# ANOVA
# fast-way
Group1 <- totweb 
Group2 <- totmob
Combined_Groups <- data.frame(cbind(Group1,Group2))
Combined_Groups
Stacked_Groups <- stack(Combined_Groups)
Stacked_Groups
Anova_Results <- aov(values ~ind, data=Stacked_Groups)
summary(Anova_Results)
summaryData

# F(1,18) = 2.339, p>.05   k=groups=2  n=20  alfa=0.05
# F-critical = 4.4139
# conclusion: p >.05 and F< F critical. No significant difference

#ANOVA each task
Group1t1 <- task1web 
Group2t1 <- task1mob
Combined_Groups_t1 <- data.frame(cbind(Group1t1,Group2t1))
Stacked_Groups_t1 <- stack(Combined_Groups_t1)
Anova_Results_t1 <- aov(values ~ind, data=Stacked_Groups_t1)
summary(Anova_Results_t1)

Group1t2 <- task2web 
Group2t2 <- task2mob
Combined_Groups_t2 <- data.frame(cbind(Group1t2,Group2t2))
Stacked_Groups_t2 <- stack(Combined_Groups_t2)
Anova_Results_t2 <- aov(values ~ind, data=Stacked_Groups_t2)
summary(Anova_Results_t2)

Group1t3 <- task3web 
Group2t3 <- task3mob
Combined_Groups_t3 <- data.frame(cbind(Group1t3,Group2t3))
Stacked_Groups_t3 <- stack(Combined_Groups_t3)
Anova_Results_t3 <- aov(values ~ind, data=Stacked_Groups_t3)
summary(Anova_Results_t3)

Group1t4 <- task4web 
Group2t4 <- task4mob
Combined_Groups_t4 <- data.frame(cbind(Group1t4,Group2t4))
Stacked_Groups_t4 <- stack(Combined_Groups_t4)
Anova_Results_t4 <- aov(values ~ind, data=Stacked_Groups_t4)
summary(Anova_Results_t4)

Group1t5 <- task5web 
Group2t5 <- task5mob
Combined_Groups_t5 <- data.frame(cbind(Group1t5,Group2t5))
Stacked_Groups_t5 <- stack(Combined_Groups_t5)
Anova_Results_t5 <- aov(values ~ind, data=Stacked_Groups_t5)
summary(Anova_Results_t5)

Group1t6 <- task6web 
Group2t6 <- task6mob
Combined_Groups_t6 <- data.frame(cbind(Group1t6,Group2t6))
Stacked_Groups_t6 <- stack(Combined_Groups_t6)
Anova_Results_t6 <- aov(values ~ind, data=Stacked_Groups_t6)
summary(Anova_Results_t6)


# ANOVA
# fast-way
Group1 <- totweb 
Group2 <- totmob
Combined_Groups <- data.frame(cbind(Group1,Group2))
Combined_Groups
Stacked_Groups <- stack(Combined_Groups)
Stacked_Groups
Anova_Results <- aov(values ~ind, data=Stacked_Groups)
summary(Anova_Results)
summaryData
