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
    caxislabels = caxislabels, title = title, ...
  )
}

# Participants perception on the Prototypes. 
# Please note that we considered the perception instead of the response, 
# because even numbered questions are formulated negatively. 
# Data computed through R. Code available in the footnote.
# below the data from the responses:
#Data on Module1
# web_scores <- data.frame(
#   row.names = c("Participant.1",
#                 "Participant.2",
#                 "Participant.3",
#                 "Participant.4",
#                 "Participant.5",
#                 "Participant.6",
#                 "Participant.7",
#                 "Participant.8",
#                 "Participant.9",
#                 "Participant.10"),
#   Q1 = c(5,4,5,5,5,5,4,4,5,5),
#   Q2 = c(2,1,2,2,2,1,2,2,1,1),
#   Q3 = c(2,4,4,3,3,4,2,4,4,4),
#   Q4 = c(1,1,1,1,1,1,1,1,1,1),
#   Q5 = c(3,3,4,3,2,4,3,4,3,3),
#   Q6 = c(4,4,3,4,5,3,3,3,4,5),
#   Q7 = c(4,5,4,5,4,4,5,4,5,4),
#   Q8 = c(2,3,3,2,3,1,2,2,1,2),
#   Q9 = c(5,5,5,5,5,5,5,5,5,5),
#   Q10= c(3,3,2,3,3,3,3,3,2,2)
# )
# 
# #Data on Module2
# mobile_scores <- data.frame(
#   row.names = c("Participant.1",
#                 "Participant.2",
#                 "Participant.3",
#                 "Participant.4",
#                 "Participant.5",
#                 "Participant.6",
#                 "Participant.7",
#                 "Participant.8",
#                 "Participant.9",
#                 "Participant.10"),
#   Q1 = c(5,5,4,5,5,5,5,5,4,5),
#   Q2 = c(1,1,1,1,1,1,1,1,1,1),
#   Q3 = c(5,5,5,5,5,5,5,5,5,5),
#   Q4 = c(1,1,1,1,1,1,1,1,1,1),
#   Q5 = c(5,5,5,5,5,5,5,5,5,5),
#   Q6 = c(1,1,1,1,1,1,1,1,1,1),
#   Q7 = c(5,5,5,5,4,5,4,5,5,4),
#   Q8 = c(2,3,3,3,2,2,3,4,3,2),
#   Q9 = c(3,2,3,2,3,3,3,2,3,2),
#   Q10= c(3,4,5,4,3,3,4,5,5,4)
# )



# below the data we used to compute
# perception for the radar graph


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
  Q2 = c(4,5,4,4,4,5,4,4,5,5),
  Q3 = c(2,4,4,3,3,4,2,4,4,4),
  Q4 = c(5,5,5,5,5,5,5,5,5,5),
  Q5 = c(3,3,4,3,2,4,3,4,3,3),
  Q6 = c(2,2,3,2,1,3,3,3,2,1),
  Q7 = c(4,5,4,5,4,4,5,4,5,4),
  Q8 = c(4,3,3,4,3,5,4,4,5,4),
  Q9 = c(5,5,5,5,5,5,5,5,5,5),
  Q10= c(3,3,4,3,3,3,3,3,4,4)
)

#Data on Module2
mobile_scores <- data.frame(
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
  Q1 = c(5,5,4,5,5,5,5,5,4,5),
  Q2 = c(5,5,5,5,5,5,5,5,5,5),
  Q3 = c(5,5,5,5,5,5,5,5,5,5),
  Q4 = c(5,5,5,5,5,5,5,5,5,5),
  Q5 = c(5,5,5,5,5,5,5,5,5,5),
  Q6 = c(5,5,5,5,5,5,5,5,5,5),
  Q7 = c(5,5,5,5,4,5,4,5,5,4),
  Q8 = c(4,3,3,3,4,4,3,2,3,4),
  Q9 = c(3,2,3,2,3,3,3,2,3,2),
  Q10= c(3,2,1,2,3,3,2,1,1,2)
)

labels <- data.frame(
  row.names = c("web-b.",
               "mobile")
)
labels

web_scores
mobile_scores

#define the variable ranges 
max_min_5_1 <- data.frame(
  Q1 = c(5,1),Q2 = c(5,1),Q3 = c(5,1),
  Q4 = c(5,1),Q5 = c(5,1),Q6 = c(5,1),
  Q7 = c(5,1),Q8 = c(5,1),Q9 = c(5,1),
  Q10= c(5,1)
)
rownames(max_min_5_1) <- c("Max","Min")

#bind a variable to data with max$min 
web_eachQ <- rbind(max_min_5_1,web_scores)
mobile_eachQ <- rbind(max_min_5_1,mobile_scores)
web_eachQ
mobile_eachQ

#create data for radar
p <- rbind(max_min_5_1,web_scores,mobile_scores)
p

#define titles
titles <- c("Participant 1","Participant 2",
            "Participant 3","Participant 4",
            "Participant 5","Participant 6",
            "Participant 7","Participant 8",
            "Participant 9","Participant 10")

#reduce margin with par()
op <- par(mar = c(1,1,1,1))
par(mfrow=c(3,4))


#create my pretty radar for p
for (i in 1:10){
  pretty_radard(data = p[c(1,2,i+2,i+12),],
              caxislabels = c(1,2,3,4,5),
              color = c("#00AFBB","#FC4E07"),
              title = titles[i]
  )
  #add my legend
  legend(
    x = "bottomright",
    legend = rownames(labels[c(0)]),
    bty = "n",
    pch = 20,
    col = c("#00AFBB","#FC4E07"),
    text.col = "black",
    cex = 1,
    pt.cex = 1.5
  )
}
par(op)






