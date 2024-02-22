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
  c(188,#participant1  web
    161,   #participant2  web
    169, #participant3  web
    165, #participant4  web
    184,  #participant5  web
    88,    #participant6  web
    100,    #participant7  web
    87,     #participant8  web
    94,    #participant9  web
    89     #participant10 web
  ),byrow=T,nrow=10)
web




mobile <- matrix(
  c(105,  #p1            mobile
    110,   #p2            mobile
    103,  #p3            mobile
    103,  #p4            mobile
    110,  #p5            mobile
    110,  #p6            mobile
    97, #p7            mobile
    113,  #p8            mobile
    101,   #p9            mobile
    110   #p10           mobile
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
  


correlation.one <-
  cor(data$x[data$category=="tot Reaction Time"],
      data$y[data$category=="tot Reaction Time"],method='pearson')

correlation.one


library(ggplot2)
gg <- ggplot(data,aes(x,y,colour=category))+xlim(0,200)+ylim(0,200)
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

lmTasks = lm(data$y[data$category=="tot Reaction Time"]~data$x[data$category=="tot Reaction Time"],data=data)
summary(lmTasks)

