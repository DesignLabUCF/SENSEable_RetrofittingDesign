#install.packages('KraljicMatrix')
library(KraljicMatrix)

#install.packages('emoa')
#library(emoa)

#install.packages("plot3D")
#library(plot3D)
#library(rgl)
#library(rglwidget)
#library(knitr)


library(dplyr)
library(ggplot2)
library(RColorBrewer)



data <- read.csv("FacadeRankings.csv",
                 skip=1,
                 header=TRUE,
                 sep=",")[1:14]

#data$VF_Thru <- as.numeric()
#data$VF_Blocked <- as.numeric()
#data$VF_Percentage <-as.numeric()
#data$G_Within <- as.numeric()
#data$G_Total <- as.numeric()
#data$G_Percentage <- as.numeric()
#data$DL_Within <- as.numeric()
#data$DL_Total <- as.numeric()
#data$DL_Percentage <- as.numeric()
data[, c(5:14)] <- sapply(data[, c(5:14)], as.numeric) 
 
# Add facade type column
data$Type <- ifelse(grepl("1_", data$Facade, fixed=TRUE), 'Kinetic',
                    ifelse(grepl("fin", data$Facade, fixed=TRUE), 'Fin',
                           ifelse(grepl("fritt", data$Facade, fixed=TRUE), 'Fritting',
                                  ifelse(grepl("louver", data$Facade, fixed=TRUE), 'Louver', 'ERROR-FACADENOTFOUND'))))

louvers_extra <- tail(data, -144)
#louvers_extra$Type = "Louver (Add.)"
louvers_extra <- subset(louvers_extra, Time==12) # Get specific Louver (12 pm)
louvers_extra <- subset(louvers_extra, Facade=="louver_hor_20_75_10_20" | Facade=="louver_hor_10_30_10" | Facade=="louver_hor_10_75_5" | Facade=="louver_hor_10_75_5_30")
louvers_extra <- subset(louvers_extra, Facade!="louver_hor_20_75_10_20") # Remove outlier that messed up Pareto
#louvers_extra <- subset(louvers_extra, Facade=="louver_hor_10_30_5_20" & Time==12) # Get specific Louver

set3 <- tail(data, -240)
set3 <- subset(set3, !grepl("fritt", set3$Facade, fixed=TRUE))
#set3$Type <- paste(set3$Type, " (Add.)", sep="")


data <- head(data,144) # take only original facades
#data <- rbind(data, louvers_extra, set3)
data <- rbind(data, louvers_extra, set3) # Add back other facades

pareto_data <- subset(data, Time==12)
#pareto_data <- subset(rbind(data, louvers_extra), Time==12)
glare_vf_pareto <- get_frontier(pareto_data, VF_Percentage, G_Percentage, quadrant="bottom.right", decreasing = FALSE)

#grouped <- data %>% group_by(Room, Position, Time)
conf_9_Pos1 <- data[data$Room == "Conf" & data$Position == 1 & data$Time == 9, ]
conf_12_Pos1 <- data[data$Room == "Conf" & data$Position == 1 & data$Time == 12, ]
conf_3_Pos1 <- data[data$Room == "Conf" & data$Position == 1 & data$Time == 3, ]
conf_9_Pos2 <- data[data$Room == "Conf" & data$Position == 2 & data$Time == 9, ]
conf_12_Pos2 <- data[data$Room == "Conf" & data$Position == 2 & data$Time == 12, ]
conf_3_Pos2 <- data[data$Room == "Conf" & data$Position == 2 & data$Time == 3, ]
office_9_Pos1 <- data[data$Room == "Office" & data$Position == 1 & data$Time == 9, ]
office_12_Pos1 <- data[data$Room == "Office" & data$Position == 1 & data$Time == 12, ]
office_3_Pos1 <- data[data$Room == "Office" & data$Position == 1 & data$Time == 3, ]

#louvers_12_Pos1 <- subset(conf_12_Pos1, conf_12_Pos1$Type=="Louver")
#louvers_12_Pos1 <- subset(louvers_12_Pos1, Facade!="louver_10_30" )
#louvers_12_Pos1 <- subset(louvers_12_Pos1, Facade!="louver_10_75" )
#louvers_12_Pos1 <- subset(louvers_12_Pos1, Facade!="louver_20_30" )
#louvers_12_Pos1 <- subset(louvers_12_Pos1, Facade!="louver_20_75" )

# Plots
  
dl_p <- ggplot(conf_12_Pos1, aes(x=VF_Percentage, y=DL_Percentage)) +
  ## DATA
  stat_smooth(method="loess", formula=y~x, size=1.5, color='black', linetype='dashed', alpha=0.5, se=FALSE) +
  geom_step(aes(color=Type), size=2.5, show.legend=FALSE) +
  geom_point(aes(fill=Type), color='black', size=4.5, stroke=1, shape=22) + 
  scale_x_reverse() +
  scale_y_reverse() +
  #geom_text(aes(label=Facade), size = 3, hjust=0, nudge_x=2, nudge_y=0.01) +
  ## LIMITS
  #xlim(0, 100.0)
  #ylim(0.15, 0.35) +
  ## AESTHETICS
  theme_light() + 
  labs(title="Optimal Facades",
       subtitle="Conference Room - 12 P.M. - Back Wall Window",
       x="View Factor (%)",
       y="Daylight Allowed (%)") +
  theme(text=element_text(size=14, family="sans"),
        axis.title.x=element_text(size=14, face = "plain"),
        axis.title.y=element_text(size=14, face = "plain"),
        axis.text.x=element_text(size=12, face = "plain"),
        axis.text.y=element_text(size=12, face = "plain"),
        axis.title=element_text(size=20, face = "bold"),
        legend.title=element_text(size=14, face = "bold.italic"),
        legend.text=element_text(size=12, face = "italic"),
        legend.background=element_rect(fill="lightgray", color="gray", size=0.5, linetype="solid"),
        legend.title.align=0.5,
        legend.key = element_rect(fill="lightgray")) 
print(dl_p)

glare_p <- ggplot(conf_12_Pos1, aes(x=VF_Percentage, y=G_Percentage)) +
  ## DATA (PARETO FRONTIER)
  geom_line(data=glare_vf_pareto, aes(x=VF_Percentage, y=G_Percentage), linetype="dotted", size=1.5) +
  ## DATA (MAIN)
  #stat_smooth(method="loess", formula=y~x, size=1.5, color='black', linetype='dashed', alpha=0.5, se=FALSE) +
  #geom_step(aes(color=Type), size=2.5, show.legend=FALSE) +
  geom_point(aes(fill=Type), color='black', size=4.5, stroke=1, shape=22) + 
  #geom_text(aes(label=Facade), size = 3, hjust=0, nudge_x=2, nudge_y=0.01) +
  ####### DATA (EXTRA LOUVERS)
  #####geom_point(data=louvers_extra, aes(x=VF_Percentage, y=G_Percentage, fill=Type), color='black', size=4.5, stroke=1, shape=22) + 
  #####geom_text(data=louvers_extra, aes(x=VF_Percentage, y=G_Percentage, label=Facade), size = 4, hjust=1, nudge_x=-0.015) +
  ## LABELS
  geom_text(x=0.07, y=-0.32, label="Less Optimal", size=6, angle=-45, fontface = "italic") +
  geom_text(x=0.91, y=-0.185, label="More Optimal", size=6, angle=-45, fontface = "italic") +
  geom_label(x=0.36, y=-0.325, label="Dotted line indicates optimal choices", size=3.5) +
  ## LIMITS
  #xlim(0, 100.0)
  #ylim(0.15, 0.35) +
  ## AESTHETICS
  theme_light() +
  #theme_classic() +
  #scale_y_reverse() +
  ylim(0.35, 0.15) +
  xlim(0.0, 1.0) +
  labs(title="Optimal Facades",
       subtitle="Conference Room - 12 P.M. - Back Wall Window",
       x="View Factor (%)",
       y="Brightness Discomfort (%)",
       fill="Facade Type") +
  theme(text=element_text(size=14, family="sans"),
        axis.title.x=element_text(size=14, face = "plain"),
        axis.title.y=element_text(size=14, face = "plain"),
        axis.text.x=element_text(size=12, face = "plain"),
        axis.text.y=element_text(size=12, face = "plain"),
        axis.title=element_text(size=20, face = "bold"),
        legend.title=element_text(size=14, face = "bold.italic"),
        legend.text=element_text(size=12, face = "italic"),
        legend.background=element_rect(fill="white", color="gray", size=0.5, linetype="solid"),
        legend.title.align=0.5,
        legend.key=element_rect(fill="white"),
        legend.position="bottom",
        axis.ticks=element_blank()) +
  scale_fill_brewer(palette="Set2") + 
  scale_color_brewer(palette="Set2")
print(glare_p)



