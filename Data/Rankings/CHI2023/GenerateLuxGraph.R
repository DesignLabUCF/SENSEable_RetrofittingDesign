#install.packages("readxl")

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)

data <- read_excel("conf_Pos1.xlsx", sheet = "Pos1_12_louver_hor_20_30") # Pos1_12_facade1_60 #Pos1_12_fin_10_75_5_30 #Pos1_12_facade1_90
data$lux <- data$CellData * 122000.0

daylight <- subset(data, lux >= 100 & lux <= 5000)

glare <- subset(data, lux >= 5000.0 & lux <= 60000)
glare_above_60000 <- subset(data, lux > 60000)

# https://www.engineeringtoolbox.com/light-level-rooms-d_708.html
light_indicators <- data.frame("Task" = c("Office", "Drawing Work", "Low Contrast Visual Tasks"),
                               "LuxLowerLimit" = c(250.0, 1000.0, 2000.0),
                               "LuxUpperLimit" = c(1000.0, 2000.0, 5000.0))

color_louver <- "#e78ac3"
color_kinetic <- "#8da0cb"
color_fritt <- "#fc8d62"
color_fin <-  "#66c2a5"

#toString(nrow(glare_above_60000))
lux_p <- ggplot(daylight, aes(x=lux)) +
  ## Zones
  #geom_vline(data=light_indicators, aes(xintercept=LuxLowerLimit, color=Task)) + 
  geom_rect(inherit.aes = FALSE, data=light_indicators, aes(xmin=LuxLowerLimit, xmax=LuxUpperLimit, fill=Task), ymin=0, ymax=200, alpha=0.5) +
  ## Histogram
  geom_histogram(binwidth=100, color="black", fill=color_louver) +
  ## Labels
  #geom_label(data=NULL, position = "identity", x=3500, y=50, label=38) +
  ## Aesthetics
  theme_minimal() + 
  scale_fill_brewer(palette="Set1", limits = c("Office", "Drawing Work", "Low Contrast Visual Tasks")) +
  ylim(0, 200) +
  labs(#title="Daylighting",
       #subtitle="Conference Room - 12 P.M. - Back Wall Window",
       x="Lux",
       y="Count",
       fill="Ideal Task Lighting") +
  theme(text=element_text(size=14, family="sans"),
        legend.position="bottom")
print(lux_p)