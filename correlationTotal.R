# value <- matrix(
#           c(44,64,10,39,21,10, #participant1  web
#            40,28,10,13,10,7,  #              mobile
#            47,39,9,35,22,9,   #participant2  web
#            39,25,10,15,9,10,   #              mobile
#            42,48,11,40,18,10, #participant3  web
#            43,29,10,11,10,9,  #              mobile
#            46,42,11,37,19,10, #participant4  web
#            41,26,10,10,10,7,  #              mobile
#            45,58,10,41,22,8,  #participant5  web
#            37,24,9,16,10,8,  #              mobile
#            38,21,10,16,6,4,    #participant6  web
#            42,29,10,27,11,9,  #              mobile
#            39,29,10,17,9,4,    #participant7  web
#            40,30,9,20,10,10, #              mobile
#            37,22,9,16,6,5,     #participant8  web
#            39,28,11,30,9,10,  #              mobile
#            36,25,10,14,8,5,    #participant9  web
#            44,31,10,22,9,8,   #              mobile
#            37,23,9,16,6,4,     #participant10 web
#            41,29,10,31,10,9   #              mobile
# ),byrow=T,nrow=20
# )
# value

web <- matrix(
  c(188,#participant1  web
    161,   #participant2  web
    169, #participant3  web
    165, #participant4  web
    184,  #participant5  web
    95,    #participant6  web
    108,    #participant7  web
    95,     #participant8  web
    98,    #participant9  web
    95     #participant10 web
  ),byrow=T,nrow=10)
web




mobile <- matrix(
  c(108,  #p1            mobile
    108,   #p2            mobile
    112,  #p3            mobile
    104,  #p4            mobile
    114,  #p5            mobile
    128,  #p6            mobile
    119, #p7            mobile
    127,  #p8            mobile
    124,   #p9            mobile
    130   #p10           mobile
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

