library(readxl)
library(tidyverse)
library(ggthemes)

#Riddle Totals
riddle_total <- read_excel("~/Summit/Summit Laptop/riddle_log.xlsx", 
                         range = "R1:S12", col_types = c("text", 
                                                         "numeric"))

#Riddles counter for plots!!!!
riddle_log <- read_excel("~/Summit/Summit Laptop/riddle_log.xlsx", 
                         sheet = "Sheet2")

#Colors as chosen by the interns!
tol_colors = c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933",   
               "#DDCC77", "#FF0000", "#CC6677", "#882255", "#AA4499")


riddle_log <- riddle_log %>%
  tidyr::gather("Analyst", "Wins", Brigid:Neil)


###EVERYBODY IN ONE
ggplot(riddle_log) + 
  geom_line(aes(x = date, y = Wins, color = Analyst, size = Analyst)) +
  scale_size_manual(values = seq(5, 1, length = 11)) +
  scale_color_manual(values = tol_colors) +
  ggtitle("Riddle Log Summit Internship Rankings 2019") +
  scale_x_datetime(breaks = "1 day", date_labels = "%d. %b") +
  theme(axis.text.x = element_text(hjust = 0, angle = -45))

##INDIVIDUAL with Riddle Names
riddle_log2 <- riddle_log %>%
  arrange(Analyst, date) %>%                # Make sure sortings is correct
  group_by(Analyst) %>%                     # 'Wins_increase' will be calculated for every Analyst 
  mutate(Wins_increase = Wins - lag(Wins))  # How much 'Wins' have increased since last day

ggplot(riddle_log, aes(x = date, y = Wins, color = Analyst)) + 
  geom_line(size = 2) +
  scale_color_manual(values = tol_colors) + 
  facet_wrap(~Analyst) +
  scale_x_datetime(breaks = "2 day", date_labels = "%d. %b") +     # as before
  theme(axis.text.x = element_text(hjust = 0, angle = -45)) +      # as before
  geom_text(data = riddle_log2 %>% filter(Wins_increase > 0),      # Pick only where "Wins" is increasing
            aes(y = Wins+ .3, label = riddle_name),                # We add 0.2 to lift the labels a bit
            hjust = "innward",vjust="inward", angle = 0, size = 3, nudge_x = TRUE, 
            color = "black")                                       # Left-adjust and rotate labels
