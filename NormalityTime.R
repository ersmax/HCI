#import data
task1web <- c(18,15,18,20,17,17,18,18,19,17)
task1mob <- c(16,18,18,19,17,18,17,17,18,19)
task2web <- c(20,16,19,19,20,21,20,19,20,21)
task2mob <- c(21,15,17,19,19,20,19,18,22,21)
task3web <- c(10,9,9,10,11,10,9,8,10,9)
task3mob <- c(9,8,8,10,10,8,9,7,9,8)
task4web <- c(15,16,18,14,15,14,13,15,17,15)
task4mob <- c(16,16,17,15,15,15,12,16,18,17)
task5web <- c(10,9,9,8,10,11,9,10,12,9)
task5mob <- c(10,8,9,9,10,10,9,11,10,9)
task6web <- c(5,4,3,3,4,6,5,5,5,4)
task6mob <- c(6,3,4,4,4,7,5,6,6,5)
totweb <- task1web+task2web+task3web+task4web+task5web+task6web
totmob <- task1mob+task2mob+task3mob+task4mob+task5mob+task6mob

#check normality with qq-plot
qqnorm(totweb,main="Normal QQ-plot of completion time (Web)",ylim=c(66,84))                   
qqline(totweb, col="red")
qqnorm(totmob,main="Normal QQ-plot of completion time (mobile)",ylim=c(66,84))
qqline(totmob, col="blue")
# 
# #check normality with histogram
hist(totweb, col="darkred",main = "Histogram of completion time (web)", xlab = "completion time (seconds)",xlim=c(60,90))     #overall web
hist(totmob, col="lightblue",main = "Histogram of completion time (mobile)", xlab = "completion time (seconds)",xlim=c(60,90))  #overall mobile

#check normality with boxplot
boxplot(totweb, col="darkred",main = "BoxPlot of Completion time (web)",ylim=c(68,83))     # overall web
boxplot(totmob, col="lightblue",main = "BoxPlot of Completion time (mobile)",ylim=c(68,83))  # overall mobile

#check normality with Shapiro–Wilk test
shapiro.test(totweb)  # p-value > .05     web is normal
shapiro.test(totmob)  # p-value > .05     mobile is normal
shapiro.test(task1web)  # p-value > .05 
shapiro.test(task1mob)  # p-value > .05 task1 is normal
shapiro.test(task2web)  # p-value > .05 
shapiro.test(task2mob)  # p-value > .05 task2 is normal
shapiro.test(task3web)  # p-value > .05 
shapiro.test(task3mob)  # p-value > .05 task3 is normal
shapiro.test(task4web)  # p-value > .05 
shapiro.test(task4mob)  # p-value > .05 task4 is normal
shapiro.test(task5web)  # p-value > .05 
shapiro.test(task5mob)  # p-value > .05 task5 is normal
shapiro.test(task6web)  # p-value > .05 
shapiro.test(task6mob)  # p-value > .05 task6 is normal



