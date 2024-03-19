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


#check normality with Shapiro–Wilk test
shapiro.test(total_score$Module1)  # p-value >.05 web is normal  
shapiro.test(total_score$Module2)  # p-value >.05 mobile is normal

#check normality with qq-plot
qqnorm(total_score$Module1,main="Normal QQ-plot users' perception (Web)",ylim=c(75,85))                   
qqline(web_adj, col="red")
qqnorm(total_score$Module2,main="Normal QQ-plot of completion time (mobile)",ylim=c(75,85))
qqline(mobile_adj, col="blue")
# 
# #check normality with boxplot
boxplot(total_score$Module1, main = "BoxPlot of users'perception (web)",ylim=c(70,85))     # overall web
boxplot(total_score$Module2, main = "BoxPlot of users'perception (mobile)",ylim=c(70,85))  # overall mobile

#check normality with histogram
hist(total_score$Module1, main = "Histogram of users'perception (web)", xlab = "completion time (seconds)",xlim=c(60,90))     #overall web
hist(totmob, main = "Histogram of completion time (mobile)", xlab = "completion time (seconds)",xlim=c(60,90))  #overall mobile



