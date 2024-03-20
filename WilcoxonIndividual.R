# The data set
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

# Create a data frame
myData <- data.frame(
  group = rep(c("web", "mobile"), each = 10),
  Q1 = c(web_scores$Q1, mobile_scores$Q1)
)

# Print all data
print(myData)

# # Paired Samples Wilcoxon Test
resultQ1_1 = wilcox.test(web_scores$Q1, mobile_scores$Q1, paired = TRUE)
resultQ1_2 = wilcox.test(mobile_scores$Q1, web_scores$Q1, paired = TRUE)
resultQ2_1 = wilcox.test(web_scores$Q2, mobile_scores$Q2, paired = TRUE)
resultQ2_2 = wilcox.test(mobile_scores$Q2, web_scores$Q2, paired = TRUE)
resultQ3_1 = wilcox.test(web_scores$Q3, mobile_scores$Q3, paired = TRUE)
resultQ3_2 = wilcox.test(mobile_scores$Q3, web_scores$Q3, paired = TRUE)
resultQ4_1 = wilcox.test(web_scores$Q4, mobile_scores$Q4, paired = TRUE)
resultQ4_2 = wilcox.test(mobile_scores$Q4, web_scores$Q4, paired = TRUE)
resultQ5_1 = wilcox.test(web_scores$Q5, mobile_scores$Q5, paired = TRUE)
resultQ5_2 = wilcox.test(mobile_scores$Q5, web_scores$Q5, paired = TRUE)
resultQ6_1 = wilcox.test(web_scores$Q6, mobile_scores$Q6, paired = TRUE)
resultQ6_2 = wilcox.test(mobile_scores$Q6, web_scores$Q6, paired = TRUE)
resultQ7_1 = wilcox.test(web_scores$Q7, mobile_scores$Q7, paired = TRUE)
resultQ7_2 = wilcox.test(mobile_scores$Q7, web_scores$Q7, paired = TRUE)
resultQ8_1 = wilcox.test(web_scores$Q8, mobile_scores$Q8, paired = TRUE)
resultQ8_2 = wilcox.test(mobile_scores$Q8, web_scores$Q8, paired = TRUE)
resultQ9_1 = wilcox.test(web_scores$Q9, mobile_scores$Q9, paired = TRUE)
resultQ9_2 = wilcox.test(mobile_scores$Q9, web_scores$Q9, paired = TRUE)
resultQ10_1 = wilcox.test(web_scores$Q10, mobile_scores$Q10, paired = TRUE)
resultQ10_2 = wilcox.test(mobile_scores$Q10, web_scores$Q10, paired = TRUE)
resultQ1_1; resultQ1_2
resultQ2_1; resultQ2_2
resultQ3_1; resultQ3_2
resultQ4_1; resultQ4_2
resultQ5_1; resultQ5_2
resultQ6_1; resultQ6_2
resultQ7_1; resultQ7_2
resultQ8_1; resultQ8_2
resultQ9_1; resultQ9_2
resultQ10_1; resultQ10_2

