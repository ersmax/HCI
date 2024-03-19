# #import data
# task1web <- c(18,15,18,20,17,17,18,18,19,17)
# task1mob <- c(16,18,18,19,17,18,17,17,18,19)
# task2web <- c(20,16,19,19,20,21,20,19,20,21)
# task2mob <- c(20,15,17,19,19,20,19,18,22,21)
# task3web <- c(10,9,9,10,11,10,9,8,10,9)
# task3mob <- c(9,9,8,10,10,8,9,7,9,8)
# task4web <- c(15,16,18,14,15,14,13,15,17,15)
# task4mob <- c(16,16,17,15,15,15,12,16,18,17)
# task5web <- c(10,9,9,8,10,11,9,10,12,9)
# task5mob <- c(10,8,9,9,10,10,9,11,10,9)
# task6web <- c(5,4,3,3,4,6,5,5,5,4)
# task6mob <- c(6,3,4,4,4,7,5,6,6,5)
# totweb <- task1web+task2web+task3web+task4web+task5web+task6web
# totmob <- task1mob+task2mob+task3mob+task4mob+task5mob+task6mob

web <- matrix(
  c(78,#participant1  web
    69,   #participant2  web
    76, #participant3  web
    74, #participant4  web
    77,  #participant5  web
    79,    #participant6  web
    74,    #participant7  web
    75,     #participant8  web
    83,    #participant9  web
    75     #participant10 web
  ),byrow=T,nrow=10)
web




mobile <- matrix(
  c(77,  #p1            mobile
    69,   #p2            mobile
    73,  #p3            mobile
    76,  #p4            mobile
    75,  #p5            mobile
    78,  #p6            mobile
    71, #p7            mobile
    75,  #p8            mobile
    83,   #p9            mobile
    79   #p10           mobile
  ),byrow=T,nrow=10
)
mobile

# set.seed(150)
# xvalues <- web
# yvalues <- mobile
# data <- data.frame(x = xvalues,y=yvalues)
# data$y <- data$x + data$y
# correlation <- cor(data$x, data$y,method='pearson')
# correlation

set.seed(150)
xvalues <- web
yvalues <- mobile
data <- data.frame(x=rep(xvalues,1),
                   y=rep(yvalues,1),
                   category=
                     rep(c("tot Reaction Time"),each=10))

# data$y[data$category=="tot Reaction Time"] <- 0 + 
#      data$x[data$category=="reactionTime"]/data$y[data$category=="reactionTime"]



correlation.tot <-
  cor(data$x[data$category=="tot Reaction Time"],
      data$y[data$category=="tot Reaction Time"],method='pearson')

correlation.tot



library(ggplot2)
gg <- ggplot(data,aes(x,y,colour=category))+xlim(65,85)+ylim(65,85)
gg <- gg + geom_point()
gg <- gg + geom_smooth(alpha=0.3,method="lm")


labs(
  title = waiver(),
  subtitle = waiver(),
  caption = waiver(),
  tag = waiver(),
  alt = waiver(),
  alt_insight = waiver()
)


gg + labs(x="Task time Module1 (seconds)") + labs(y="Task time Module2 (seconds)") + labs(title = "Correlation between Tasks time Module1 (web-based
and Module2 (mobile)") 

lmTasks = lm(data$y[data$category=="tot Reaction Time"]~data$x[data$category=="tot Reaction Time"],data=data)
summary(lmTasks)

