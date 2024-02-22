# value <- matrix(
#           c(44,64,10,39,21,10, #participant1  web
#            36,29,10,13,10,7,  #              mobile
#            47,39,9,35,22,9,   #participant2  web
#            37,30,9,15,9,10,   #              mobile
#            42,48,11,40,18,10, #participant3  web
#            34,28,11,11,10,9,  #              mobile
#            46,42,11,37,19,10, #participant4  web
#            35,31,10,10,10,7,  #              mobile
#            45,58,10,41,22,8,  #participant5  web
#            37,29,10,16,10,8,  #              mobile
#            43,21,10,4,6,4,    #participant6  web
#            33,18,12,27,11,9,  #              mobile
#            45,29,10,3,9,4,    #participant7  web
#            32,15,10,20,10,10, #              mobile
#            40,22,9,5,6,5,     #participant8  web
#            35,19,10,30,9,10,  #              mobile
#            42,25,10,4,8,5,    #participant9  web
#            33,18,11,22,9,8,   #              mobile
#            43,23,9,4,6,4,     #participant10 web
#            32,16,12,31,10,9   #              mobile
# ),byrow=T,nrow=20
# )
# value

web <- matrix(
    c(44,64,10,39,21,10,#participant1  web
     47,39,9,35,22,9,   #participant2  web
     42,48,11,40,18,10, #participant3  web
     46,42,11,37,19,10, #participant4  web
     45,58,10,41,22,8,  #participant5  web
     43,21,10,4,6,4,    #participant6  web
     45,29,10,3,9,4,    #participant7  web
     40,22,9,5,6,5,     #participant8  web
     42,25,10,4,8,5,    #participant9  web
     43,23,9,4,6,4     #participant10 web
),byrow=T,nrow=10)
web




mobile <- matrix(
  c(36,29,10,13,10,7,  #p1            mobile
    37,30,9,15,9,10,   #p2            mobile
    34,28,11,11,10,9,  #p3            mobile
    35,31,10,10,10,7,  #p4            mobile
    37,29,10,16,10,8,  #p5            mobile
    33,18,12,27,11,9,  #p6            mobile
    32,15,10,20,10,10, #p7            mobile
    35,19,10,30,9,10,  #p8            mobile
    33,18,11,22,9,8,   #p9            mobile
    32,16,12,31,10,9   #p10           mobile
  ),byrow=T,nrow=10
)
mobile


set.seed(150)
xvalues <- web
yvalues <- mobile
data <- data.frame(x=rep(xvalues,1),
                   y=rep(yvalues,1),
                   category=
rep(c("Task1","Task2","Task3","Task4","Task5","Task6"),each=10))

# data$y[data$category=="Task1"] <- 0 +
#   data$x[data$category=="Task1"]/data$y[data$category=="Task1"]
# data$y[data$category=="Task2"] <- 0 +
#   data$x[data$category=="Task2"]/data$y[data$category=="Task2"]
# data$y[data$category=="Task3"] <- 0 + 
#   data$x[data$category=="Task3"]/data$y[data$category=="Task3"]
# data$y[data$category=="Task4"] <- 0 +
#   data$x[data$category=="Task4"]/data$y[data$category=="Task4"]
# data$y[data$category=="Task5"] <- 0 + 
#   data$x[data$category=="Task5"]/data$y[data$category=="Task5"]
# data$y[data$category=="Task6"] <- 0 +
#   data$x[data$category=="Task6"]/data$y[data$category=="Task6"]

correlation.one <-
  cor(data$x[data$category=="Task1"],
      data$y[data$category=="Task1"],method='pearson')
correlation.two <-
  cor(data$x[data$category=="Task2"],
      data$y[data$category=="Task2"],method='pearson')
correlation.three <-
  cor(data$x[data$category=="Task3"],
      data$y[data$category=="Task3"],method='pearson')
correlation.four <-
  cor(data$x[data$category=="Task4"],
      data$y[data$category=="Task4"],method='pearson')
correlation.five <-
  cor(data$x[data$category=="Task5"],
      data$y[data$category=="Task5"],method='pearson')
correlation.six <-
  cor(data$x[data$category=="Task6"],
      data$y[data$category=="Task6"],method='pearson')

correlation.one
correlation.two
correlation.three
correlation.four
correlation.five
correlation.six

library(ggplot2)
gg <- ggplot(data,aes(x,y,colour=category))+xlim(0,70)+ylim(0,70)
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


gg + labs(x="Task time Module1 (seconds)") + labs(y="Task time Module2 (seconds)") + labs(title = "Correlation between Tasks time Module1 (web-based) and Module2 (mobile)")

lmTask1 = lm(data$y[data$category=="Task1"]~data$x[data$category=="Task1"],data=data)
lmTask2 = lm(data$y[data$category=="Task2"]~data$x[data$category=="Task2"],data=data)
lmTask3 = lm(data$y[data$category=="Task3"]~data$x[data$category=="Task3"],data=data)
lmTask4 = lm(data$y[data$category=="Task4"]~data$x[data$category=="Task4"],data=data)
lmTask5 = lm(data$y[data$category=="Task5"]~data$x[data$category=="Task5"],data=data)
lmTask6 = lm(data$y[data$category=="Task6"]~data$x[data$category=="Task6"],data=data)
summary(lmTask1)
summary(lmTask2)
summary(lmTask3)
summary(lmTask4)
summary(lmTask5)
summary(lmTask6)


