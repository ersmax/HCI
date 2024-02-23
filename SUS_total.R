#create data for SUS total
# odd  Q  -> subtract 1 from score
# even Q  -> subtract their value from 5
# sum total score, then multiply by 2.5

#install package. useful info at: https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/
#install.packages("fmsb")
#load the package
library(fmsb)

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


#define the variable ranges 
max_min_100_0 <- data.frame(
  row.names = c("Module.1",
                "Module.2"),
  PARTICIPANT.1 = c(100,0),
  PARTICIPANT.2 = c(100,0),
  PARTICIPANT.3 = c(100,0),
  PARTICIPANT.4 = c(100,0),
  PARTICIPANT.5 = c(100,0),
  PARTICIPANT.6 = c(100,0),
  PARTICIPANT.7 = c(100,0),
  PARTICIPANT.8 = c(100,0),  
  PARTICIPANT.9 = c(100,0),
  PARTICIPANT.10 = c(100,0)
)
max_min_100_0
rownames(max_min_100_0) <- c("Max","Min")

score <- rbind(max_min_100_0,web_adj,mobile_adj)
score


# Reduce plot margin
#reduce margin with par()
op <- par(mar = c(1,1,1,1))
par(mfrow=c(1,1))


# Create the radar charts
pretty_radard(
  data = score, 
  caxislabels = c(0, 20, 40, 60, 80, 100),
  color = c("#00AFBB", "#FC4E07"),
  title = "Participants perception"
)
# Add an horizontal legend
legend(
  x = "bottomright", 
  legend = rownames(labels[c(0)]), 
  bty = "n", 
  pch = 20 , 
  col = c("#00AFBB", "#FC4E07"),
  text.col = "black", 
  cex = 1, 
  pt.cex = 1.5
)



