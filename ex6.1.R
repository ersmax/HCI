# source theory ANOVA
# https://www.statology.org/one-way-vs-two-way-anova/
# https://www.youtube.com/watch?app=desktop&v=WUjsSB7E-ko 
# https://www.geeksforgeeks.org/how-to-calculate-the-p-value-of-an-f-statistic-in-r/
# https://www.youtube.com/watch?v=fT2No3Io72g 

#filling data
myData <- data.frame(
  row.names = c("P.1","P.2","P.3","P.4","P.5","P.6","P.7","P.8"),
  sitting = c(13,14,12,9,15,11,18,9),
  walking = c(10,11,9,13,14,8,9,11)
)
myData

summaryData <- data.frame(
  row.names = c("sitting",
                "walking"),
  name = c("sitting","walking"),
  mean = c(mean(myData$sitting),
           mean(myData$walking)),
  stderror = c(sd(myData$sitting),
               sd(myData$walking))
)
summaryData



#plotting
myPlot <- ggplot(summaryData,
                 aes(x=name,
                     y=mean)) +
  geom_bar(stat="identity", 
           color="black",
           fill="cyan",
           width=0.3,
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-stderror,
                    ymax=mean+stderror),
                width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label=round(mean,2)),size=5,
            position=position_dodge(0.85),
            vjust = -0.5)

myPlot + coord_cartesian(ylim=c(0,20)) + labs(title="Mean Diferences in wpm", 
                                               subtitle="exercise 6-1",
                                               caption = "Data collected on 8 participants",
                                               x = NULL,
                                               y = "texting speed (wpm)")


# ANOVA
# fast-way
Group1 <- c(13,14,12,9,15,11,18,9)
Group2 <- c(10,11,9,13,14,8,9,11)
Combined_Groups <- data.frame(cbind(Group1,Group2))
Combined_Groups
Stacked_Groups <- stack(Combined_Groups)
Stacked_Groups
Anova_Results <- aov(values ~ind, data=Stacked_Groups)
summary(Anova_Results)

# F(1,14) = 2.339, p>.05 
# F-critical = 4.6001

# long-way   k=groups=2  n=16  alfa=0.05
# mean each Group
meanSitting <- mean(myData$sitting)  
meanWalking <- mean(myData$walking)
# Grand mean
grandMean <- (meanSitting+meanWalking)/2
grandMean
# sum of squares by group
sumSquaresSitting <- sum((myData$sitting - meanSitting)^2)
sumSquaresWalking <- sum((myData$walking - meanWalking)^2)

# sum of squares within SSw
SSw <- sumSquaresSitting + sumSquaresWalking

# mean sum of squares within=SSw/(n-k)
MSSw <- SSw/14
MSSw

# mean sum of squares between=[nGroup1*(meanGroup1-grandMean)^2+nGroup2*(meanGroup2-grandMean)^2]/(k-1)
MSSb <- (8*(meanSitting-grandMean)^2+8*(meanSitting-grandMean)^2)/1
MSSb

# F-value = MSSb/MSSw
Fvalue <- MSSb/MSSw
Fvalue

# df1=k-1=1   df2=n-k=14  alfa=0.05
Fcrit <- 4.6001
Fcrit

pf(Fvalue,1,14,lower.tail = FALSE)
summary(Anova_Results)

sd(myData$sitting)
sd(myData$walking)