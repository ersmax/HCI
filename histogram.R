library(ggplot2)
set.seed(2022)


specie <- c(rep("Participant 1", 12),
            rep("Participant 2", 12),
            rep("Participant 3", 12),
            rep("Participant 4", 12),
            rep("Participant 5", 12),
            rep("Participant 6", 12),
            rep("Participant 7", 12),
            rep("Participant 8", 12),
            rep("Participant 9", 12),
            rep("Participant10", 12))
condition <- c(rep("Mod1",6),rep("Mod2",6),
               rep("Mod1",6),rep("Mod2",6),
               rep("Mod1",6),rep("Mod2",6),
               rep("Mod1",6),rep("Mod2",6),
               rep("Mod1",6),rep("Mod2",6),
               rep("Mod1",6),rep("Mod2",6),
               rep("Mod1",6),rep("Mod2",6),
               rep("Mod1",6),rep("Mod2",6),
               rep("Mod1",6),rep("Mod2",6),
               rep("Mod1",6),rep("Mod2",6))
Task <- rep(c("task 1", "task 2", "task 3", "task 4", "task 5","task 6"), 20)
value <- c(44,64,10,39,21,10, #participant1  web
           40,28,10,13,10,7,  #              mobile
           47,39,9,35,22,9,   #participant2  web
           39,25,10,15,9,10,  #              mobile
           42,48,11,40,18,10, #participant3  web
           43,29,10,11,10,9,  #              mobile
           46,42,11,37,19,10, #participant4  web
           41,26,10,10,10,7,  #              mobile
           45,58,10,41,22,8,  #participant5  web
           37,24,9,16,10,8,   #              mobile
           38,21,10,16,6,4,   #participant6  web
           42,29,10,27,11,9,  #              mobile
           39,29,10,17,9,4,   #participant7  web
           40,30,9,20,10,10,  #              mobile
           37,22,9,16,6,5,    #participant8  web
           39,28,11,30,9,10,  #              mobile
           36,25,10,14,8,5,   #participant9  web
           44,31,10,22,9,8,   #              mobile
           37,23,9,16,6,4,    #participant10 web
           41,29,10,31,10,9   #              mobile
           )
data <- data.frame(specie, condition, Task, value)
head(data)

myPlot <- ggplot(data) +
  geom_bar(aes(x = condition, y = value, fill = Task),
           position = "stack",
           stat = "identity") +
  facet_grid(~ specie, switch = "x") +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = "white"),
        panel.spacing = unit(0.15,"cm"))

labs(
  title = waiver(),
  subtitle = waiver(),
  caption = waiver(),
  tag = waiver(),
  alt = waiver(),
  alt_insight = waiver()
)

myPlot + labs(x = "Participant") + labs(y = "Tasks time (seconds)") + labs(title = "Reaction time by each participants for each prototype and task") 

