#install.packages('KraljicMatrix')
library(KraljicMatrix)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(gghalves) # For violin plot thingy
library(treemapify) # Treemap for GGPlot2

library(grid) # For gradient background
library(RColorBrewer) # For gradient background

#library(PASWR) # Sign test

## Read in CSV
facade_data <- read.csv("FacadeRankings.csv",
                 skip=1,
                 header=TRUE,
                 sep=",")[1:14]
facade_data <- na.omit(facade_data)
# Cast data to floats
facade_data$VF_Thru <- as.numeric(facade_data$VF_Thru)
facade_data$VF_Blocked <- as.numeric(facade_data$VF_Blocked)
facade_data$VF_Percentage <-as.numeric(facade_data$VF_Percentage)
facade_data$G_Within <- as.numeric(facade_data$G_Within)
facade_data$G_Total <- as.numeric(facade_data$G_Total)
facade_data$G_Percentage <- as.numeric(facade_data$G_Percentage)
facade_data$DL_Within <- as.numeric(facade_data$DL_Within)
facade_data$DL_Total <- as.numeric(facade_data$DL_Total)
facade_data$DL_Percentage <- as.numeric(facade_data$DL_Percentage)
# Add facade type column
facade_data$Type <- ifelse(grepl("1_", facade_data$Facade, fixed=TRUE), 'Kinetic',
                    ifelse(grepl("fin", facade_data$Facade, fixed=TRUE), 'Fin',
                           ifelse(grepl("fritt", facade_data$Facade, fixed=TRUE), 'Fritting',
                                  ifelse(grepl("louver", facade_data$Facade, fixed=TRUE), 'Louver', 'ERROR-FACADENOTFOUND'))))
## Subset data
pareto_data_conf <- subset(facade_data, Time==12 & Room=="Conf")
pareto_data_office <- subset(facade_data, Time==12 & Room=="Office")
glare_vf_pareto_conf <- get_frontier(pareto_data_conf, VF_Percentage, G_Percentage, quadrant="bottom.right", decreasing = FALSE)
glare_vf_pareto_office <- get_frontier(pareto_data_office, VF_Percentage, G_Percentage, quadrant="bottom.right", decreasing = FALSE)
## Sort
pareto_data_conf <- pareto_data_conf[order(pareto_data_conf$VF_Percentage, decreasing=TRUE),]
pareto_data_office <- pareto_data_office[order(pareto_data_office$VF_Percentage, decreasing=TRUE),]

## Score the facades by comparing their distance to the pareto line
# https://stackoverflow.com/questions/35194048/using-r-how-to-calculate-the-distance-from-one-point-to-a-line
# dist2d(a2,b2,c2) # distance of point a from line (b,c) in 2D (a2 <- c(0,2)) 
dist2d <- function(point, line_point_a, line_point_b) {
  v1 <- line_point_a - line_point_b
  v2 <- point - line_point_a
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
  return(d)
}
distance_to_pareto <- function(point, pareto_line) {
  minimum_distance <- 10000000.0
  for(row in 1:nrow(pareto_line)) {
    if(row == nrow(pareto_line)) break
    x1 <- pareto_line[row, "VF_Percentage"]
    y1 <- pareto_line[row, "G_Percentage"]
    x2 <- pareto_line[row + 1, "VF_Percentage"]
    y2 <- pareto_line[row + 1, "G_Percentage"]
    distance <- dist2d(point, c(x1, y1), c(x2, y2))
    if(distance < minimum_distance) minimum_distance = distance
  }
  return(minimum_distance)
}

# Computer optimal and distance
add_rating_columns <- function(pareto_data, pareto_line) {
  pareto_data$Distance <- -1
  pareto_data$Optimal <- NA
  for(i in 1:nrow(pareto_data))
  {
    x <- pareto_data[i, "VF_Percentage"]
    y <- pareto_data[i, "G_Percentage"]
    distance <-  distance_to_pareto(c(x,y), pareto_line)
    pareto_data$Distance[i] <- distance
    if(distance <= 0.001){
      pareto_data$Optimal[i] <- 1
    }
    else{
      pareto_data$Optimal[i] <- 0
    }
  }
  # Compute overall rank (Optimal ranked by highest daylight, nonoptimal ranked by distance to pareto line)
  optimal_facades <- subset(pareto_data, Optimal == TRUE)
  optimal_facades <- optimal_facades[with(optimal_facades, order(-DL_Percentage)),]
  optimal_facades$Rank <- 1:nrow(optimal_facades)
  nonoptimal_facades <- subset(pareto_data, Optimal == FALSE)
  nonoptimal_facades <- nonoptimal_facades[with(nonoptimal_facades, order(Distance)),]
  nonoptimal_facades$Rank <- nrow(optimal_facades) + (1:nrow(nonoptimal_facades))
  pareto_data <- rbind(optimal_facades, nonoptimal_facades)
  # Compute other rankings
  pareto_data <- pareto_data[with(pareto_data, order(-DL_Percentage)),]
  pareto_data$DL_Rank <- 1:nrow(pareto_data)
  pareto_data <- pareto_data[with(pareto_data, order(-VF_Percentage)),]
  pareto_data$VF_Rank <- 1:nrow(pareto_data)
  pareto_data <- pareto_data[with(pareto_data, order(G_Percentage)),]
  pareto_data$G_Rank <- 1:nrow(pareto_data)
  return(pareto_data)
}
pareto_data_conf <- add_rating_columns(pareto_data_conf, glare_vf_pareto_conf)
pareto_data_office <- add_rating_columns(pareto_data_office, glare_vf_pareto_office)

distance_to_optimal_point <- function(x, y, optimal_x, optimal_y)
{
  return(sqrt((optimal_x - x)^2 + (optimal_y - y)^2))
}
pareto_data_conf$FromOptimal <- distance_to_optimal_point(pareto_data_conf$VF_Percentage, pareto_data_conf$G_Percentage, 1.0, 0.0)
pareto_data_office$FromOptimal <- distance_to_optimal_point(pareto_data_office$VF_Percentage, pareto_data_office$G_Percentage, 1.0, 0.0)

# Save facade data to csv
write.csv(pareto_data_conf, "../Subjects/FacadesConference.csv", row.names = FALSE)
write.csv(pareto_data_office, "../Subjects/FacadesOffice.csv", row.names = FALSE)

## Subject selection scoring
# Subject hand-done log
subject_data <- read.csv("../Subjects/Log.csv",
                         skip=1,
                         header=TRUE,
                         sep=",")[1:12]
subject_data <- select(subject_data, Participant.ID, Condition, Room, Facade.Choice, Gender) # Drop useless columns
subject_data <- subset(subject_data, Condition == "AR" & !grepl("(CUT)", Room)) # Get data that wasn't cut
names(subject_data)[names(subject_data) == 'Facade.Choice'] <- "After" # Rename facade column to be the AFTER experiment choice
subject_data$After <- toupper(subject_data$After) # Capitalize for easier matching
# Make N for AR-Office match AR-Conference
#sample(subject_data$Participant.ID[subject_data$Condition=="AR" & subject_data$Room=="Office"], 22, replace=FALSE)
#sample(subject_data$Participant.ID[subject_data$Condition=="AR" & subject_data$Room=="Office"], length(subject_data$Participant.ID[subject_data$Condition=="AR" & subject_data$Room=="Conference"]), replace=FALSE)
office_ar_subjects <- c(14,12,32,26,27,20,24,1,22,11,25,8,3,16,33,4,21,31,23,17,10,19)
for(i in nrow(subject_data):1)
{
  # Subject is AR in office
  if(subject_data$Condition[i] == "AR" & subject_data$Room[i] == "Office")
  {
    # Subject is not one of the chosen ID's to stay 
    if(!(subject_data$Participant.ID[i] %in% office_ar_subjects))
    {
      subject_data <- subject_data[-i,]
    } 
  }
}

# Qualtrics data
qualtrics_data <- read.csv("../Subjects/Qualtrics_3_7_22.csv",
                           header=TRUE,
                           sep=",")
qualtrics_data <- qualtrics_data[-c(1, 2), ] # Remove alt-header
qualtrics_data <- select(qualtrics_data, c(18, 21, 22, 65, 66, 67, 68, 69)) # Participant ID, Room, Condition, All before selection columns
# Get before response facade from qualtrics
qualtrics_data$sub <- paste(qualtrics_data$X2a, qualtrics_data$X2b, qualtrics_data$X2c, qualtrics_data$X2d, sep="") # Combine into one col
qualtrics_data <- subset(qualtrics_data, select = -c(X2a, X2b, X2c, X2d))
qualtrics_data$Before <- NA
for(i in 1:nrow(qualtrics_data)){
  type <- qualtrics_data[i, "X1.6"]
  sub <- qualtrics_data[i, "sub"]
  if(sub == "Triangle Foldout Angle = 0 degrees"){
    qualtrics_data$Before[i] <- "dynamic_0"
  }
  else if(sub == "Triangle Foldout Angle = 30 degrees"){
    qualtrics_data$Before[i] <- "dynamic_30"
  }
  else if(sub == "Triangle Foldout Angle = 60 degrees"){
    qualtrics_data$Before[i] <- "dynamic_60"
  }
  else if(sub == "Triangle Foldout Angle = 90 degrees"){
    qualtrics_data$Before[i] <- "dynamic_90"
  }
  else if(sub == "Fin Quantity = 10\nFin Width     = 3.15 inches"){
    qualtrics_data$Before[i] <- "fin_10_30"
  }
  else if(sub == "Fin Quantity = 10\nFin Width     = 7.32 inches"){
    qualtrics_data$Before[i] <- "fin_10_70"
  }
  else if(sub == "Fin Quantity = 20\nFin Width     = 1.58 inches"){
    qualtrics_data$Before[i] <- "fin_20_30"
  }
  else if(sub == "Fin Quantity = 20\nFin Width     = 3.66 inches"){
    qualtrics_data$Before[i] <- "fin_20_70"
  }
  else if(sub == "Louver Quantity = 10\nLouver Width     = 3.15 inches"){
    qualtrics_data$Before[i] <- "louver_10_30"
  }
  else if(sub == "Louver Quantity = 10\nLouver Width     = 7.32 inches"){
    qualtrics_data$Before[i] <- "louver_10_70"
  }
  else if(sub == "Louver Quantity = 20\nLouver Width     = 1.58 inches"){
    qualtrics_data$Before[i] <- "louver_20_30"
  }
  else if(sub == "Louver Quantity = 20\nLouver Width     = 3.66 inches"){
    qualtrics_data$Before[i] <- "louver_20_70"
  }
  else if(sub == "Fritting Quantity = 15\nFritting Width     = 1.22 inches"){
    qualtrics_data$Before[i] <- "fritt_15_5"
  }
  else if(sub == "Fritting Quantity = 15\nFritting Width     = 2.44 inches"){
    qualtrics_data$Before[i] <- "fritt_15_10"
  }
  else if(sub == "Fritting Quantity = 30\nFritting Width     = 1.22 inches"){
    qualtrics_data$Before[i] <- "fritt_30_5"
  }
  else if(sub == "Fritting Quantity = 30\nFritting Width     = 2.44 inches"){
    qualtrics_data$Before[i] <- "fritt_30_10"
  }
}
qualtrics_data$Before <- toupper(qualtrics_data$Before) # Capitalize for easier matching
qualtrics_data <- subset(qualtrics_data, qualtrics_data$X1 %in% subject_data$Participant.ID) # Drop subjects with un-used data

# Facade scores (rename columns for easier matching as well)
scores_conf <- select(pareto_data_conf, Facade, Distance, Optimal, Rank, DL_Rank, VF_Rank, G_Rank)
scores_conf <- mutate(scores_conf, Facade = str_replace(Facade, "1_", "dynamic_"))
scores_conf <- mutate(scores_conf, Facade = str_replace(Facade, "louver_10_75", "louver_10_70"))
scores_conf <- mutate(scores_conf, Facade = str_replace(Facade, "louver_20_75", "louver_20_70"))
scores_conf$Facade <- toupper(scores_conf$Facade)
scores_office <- select(pareto_data_office, Facade, Distance, Optimal, Rank, DL_Rank, VF_Rank, G_Rank)
scores_office <- mutate(scores_office, Facade = str_replace(Facade, "1_", "dynamic_"))
scores_office <- mutate(scores_office, Facade = str_replace(Facade, "louver_10_75", "louver_10_70"))
scores_office <- mutate(scores_office, Facade = str_replace(Facade, "louver_20_75", "louver_20_70"))
scores_office$Facade <- toupper(scores_office$Facade)

# Match
subject_data$Before <- qualtrics_data$Before[match(qualtrics_data$X1, subject_data$Participant.ID)]
subjects_data_conf <- subset(subject_data, Room == "Conference")
subjects_data_conf$Distance <- scores_conf$Distance[match(subjects_data_conf$After, scores_conf$Facade)]
subjects_data_conf$Optimal <- scores_conf$Optimal[match(subjects_data_conf$After, scores_conf$Facade)]
subjects_data_conf$AfterRank <- scores_conf$Rank[match(subjects_data_conf$After, scores_conf$Facade)]
subjects_data_conf$BeforeRank <- scores_conf$Rank[match(subjects_data_conf$Before, scores_conf$Facade)]
subjects_data_conf$VF_BeforeRank <- scores_conf$VF_Rank[match(subjects_data_conf$Before, scores_conf$Facade)]
subjects_data_conf$VF_AfterRank <- scores_conf$VF_Rank[match(subjects_data_conf$After, scores_conf$Facade)]
subjects_data_conf$DL_BeforeRank <- scores_conf$DL_Rank[match(subjects_data_conf$Before, scores_conf$Facade)]
subjects_data_conf$DL_AfterRank <- scores_conf$DL_Rank[match(subjects_data_conf$After, scores_conf$Facade)]
subjects_data_conf$G_BeforeRank <- scores_conf$G_Rank[match(subjects_data_conf$Before, scores_conf$Facade)]
subjects_data_conf$G_AfterRank <- scores_conf$G_Rank[match(subjects_data_conf$After, scores_conf$Facade)]

subjects_data_office <- subset(subject_data, Room == "Office")
subjects_data_office$Distance <- scores_office$Distance[match(subjects_data_office$After, scores_office$Facade)]
subjects_data_office$Optimal <- scores_office$Optimal[match(subjects_data_office$After, scores_office$Facade)]
subjects_data_office$AfterRank <- scores_office$Rank[match(subjects_data_office$After, scores_office$Facade)]
subjects_data_office$BeforeRank <- scores_office$Rank[match(subjects_data_office$Before, scores_office$Facade)]
subjects_data_office$VF_BeforeRank <- scores_office$VF_Rank[match(subjects_data_office$Before, scores_office$Facade)]
subjects_data_office$VF_AfterRank <- scores_office$VF_Rank[match(subjects_data_office$After, scores_office$Facade)]
subjects_data_office$DL_BeforeRank <- scores_office$DL_Rank[match(subjects_data_office$Before, scores_office$Facade)]
subjects_data_office$DL_AfterRank <- scores_office$DL_Rank[match(subjects_data_office$After, scores_office$Facade)]
subjects_data_office$G_BeforeRank <- scores_office$G_Rank[match(subjects_data_office$Before, scores_office$Facade)]
subjects_data_office$G_AfterRank <- scores_office$G_Rank[match(subjects_data_office$After, scores_office$Facade)]

subject_data <- rbind(subjects_data_office, subjects_data_conf)

# Get difference between before and after
subject_data$Difference <- subject_data$BeforeRank - subject_data$AfterRank

# Save to csv
write.csv(subject_data, "../Subjects/SubjectScores.csv", row.names = FALSE)

## Subject Facade time spent
choice_data <- read.csv("../Subjects/FacadeTimes.csv",
                        header=TRUE,
                        sep=",")
names(choice_data)[names(choice_data) == 'X'] <- "Participant.ID" # Set name for subject ID column
for(i in nrow(choice_data):1) # Drop choices from unused subjects
{
  # Subject is not one of the chosen ID's to stay 
  if(!(choice_data$Participant.ID[i] %in% subject_data$Participant.ID))
  {
    choice_data <- choice_data[-i,]
  } 
}


## PLOTS

##########################################################################################################
###### PARETO PLOT
#optimal_color <- "#1E88E5"
optimal_color <- "#005AB5"
#poor_color <- "#D81B60"
poor_color <- "#DC3220"
mid_color <- "#bf9be4"
alt_color <- "#FFC107"
optimal_color_scale <- c(poor_color, optimal_color)

# Shapes to fill out the background

background_pareto_conf_min_x <- 0.1
background_pareto_conf_max_x <- 0.95
background_pareto_conf_min_y <- 0.06
background_pareto_conf_max_y <- 0.345
background_pareto_conf_optimal <- glare_vf_pareto_conf
background_pareto_conf_optimal[nrow(background_pareto_conf_optimal) + 1,] <- c(background_pareto_conf_max_x+0.025, background_pareto_conf_min_y-0.001)
background_pareto_conf_poor <- glare_vf_pareto_conf
background_pareto_conf_poor[nrow(background_pareto_conf_poor) + 1,] <- c(background_pareto_conf_min_x-0.015, background_pareto_conf_max_y-0.005)

make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}
optimal_gradient <- make_gradient(deg=315, n=100, cols=optimal_color_scale)

glare_p <- ggplot(pareto_data_conf, aes(x=VF_Percentage, y=G_Percentage)) +
  ## Background
    # annotate("Segment",
    #          x=0.1,
    #          y=0.25,
    #          xend=0.4,
    #          yend=0.1,
    #          size=1,
    #          color="black",
    #          arrow=arrow(length=unit(0.35,"cm"))) +
  annotate("text",x=0.25, y=0.275, label="Less Optimal", size=7, angle=0, fontface = "bold") +
  annotate("text",x=0.75, y=0.125, label="More Optimal", size=7, angle=0, fontface = "bold") +
  annotate("text",x=0.56, y=0.18, label="Most Optimal Facade Choices Available", size=6, angle=-30, fontface = "italic") +
  #geom_label(x=0.36, y=-0.335, label="Dotted line indicates optimal choices", size=3.5, angle=45) +
  # geom_polygon(data=background_pareto_conf_optimal, 
  #              mapping=aes(x=VF_Percentage, y=G_Percentage),
  #              fill=optimal_color,
  #              alpha=0.25) +
  # geom_polygon(data=background_pareto_conf_poor,
  #              mapping=aes(x=VF_Percentage, y=G_Percentage),
  #              fill=poor_color,
  #              alpha=0.25) +
  # annotation_custom(
  #   grob = optimal_gradient, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  # ) + 
  ## DATA (PARETO FRONTIER)
  # geom_line(data=glare_vf_pareto_conf,
  #           aes(x=VF_Percentage, y=G_Percentage),
  #           color=optimal_color,
  #           linetype="solid",
  #           size=1.5) +
  # geom_point(data=glare_vf_pareto_conf, 
  #            aes(x=VF_Percentage, y=G_Percentage), 
  #            color='black', 
  #            fill='red', 
  #            size=1.5, 
  #            stroke=1, 
  #            shape=22) +
  ## DATA (LINE OF BEST FIT)
  stat_smooth(data=glare_vf_pareto_conf,
              aes(x=VF_Percentage, y=G_Percentage),
              method="loess",
              formula=y~x,
              size=2,
              se=FALSE,
              color="black") +
  ## DATA (MAIN)
  geom_point(aes(fill=factor(Optimal),
                 shape=factor(Optimal)),
             color='black',
             size=6,
             #shape=21,
             stroke=2) +
  scale_shape_manual(values = c(22, 24)) +
  # geom_text(data = ~filter(pareto_data_conf, Optimal == 1),
  #           aes(label=Facade), 
  #           size=5, 
  #           hjust=0, 
  #           nudge_x=0.0, 
  #           nudge_y=0.02) +
  labs(title="Conference Room",
       #subtitle="Conference Room - 12 P.M. - Back Wall Window",
       x="View Factor (%)",
       y="Brightness Discomfort (%)",
       fill="Facade Type") +
  ## LIMITS
  #coord_cartesian(
  #  xlim=c(background_pareto_conf_min_x, background_pareto_conf_max_x),
  #  ylim=c(background_pareto_conf_max_y, background_pareto_conf_min_y),
  #  expand=FALSE,
  #  clip="off") +
  scale_y_reverse() + 
  ## AESTHETICS
  #theme_light() +
  theme_classic() +
  theme(
        # Grid
        panel.grid.major=element_line(size=1, color="gray"),
        panel.grid.minor=element_line(size=0.5, color="gray"),
        # Background
        #panel.background = element_rect(fill = 'green', colour = 'red'),
        #Legend
        legend.position = "none",
        # Title
        #text=element_text(size=16, family="bold"),
        #axis.title=element_text(size=16, face = "bold"),
        plot.title=element_text(size=16, face = "bold", hjust = 0.5),
        # Y Axis
        axis.title.y=element_text(size=16, face = "bold"),
        #axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.text.y=element_text(size=16, face = "bold"),
        axis.ticks.y=element_blank(),
        #axis.line.y=element_blank(),
        # X Axis
        axis.title.x=element_text(size=16, face = "bold"),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.text.x=element_text(size=16, face = "bold"),
        axis.ticks.x=element_blank()) +
        #axis.line.x=element_blank()) +
  ## Color
  scale_fill_manual(values=optimal_color_scale)
  #scale_fill_brewer(palette="Set2")
  #scale_color_brewer(palette="Set2")
print(glare_p)

# ##########################################################################################################
# ###### BEFORE AND AFTER FACADE PLOTS

subject_data_melted <- melt(subject_data, id="Room")
subject_data_melted <- subset(subject_data_melted, variable=="BeforeRank" | variable=="AfterRank")
subject_data_melted$value <- as.numeric(subject_data_melted$value)
subject_data_melted$Before <- grepl("Before", subject_data_melted$variable, fixed=TRUE) # Set Before column for coloring

before_color <- "#785EF0"
after_color <- "#FE6100"

# ranking_p <- ggplot(subject_data_melted, aes(x=variable, y=value)) +
#   # Data
#   ggdist::stat_halfeye(fill="gray",
#                        alpha=0.5,
#                        #width=0.5,
#                        width=0.75,
#                        adjust=0.5,
#                        #justification=-0.3,
#                        #justification=0.225) +
#                        justification=0.225) +
#   geom_point(color="black",
#              fill="gray",
#              shape=19,
#              size=1.5,
#              alpha=0.3,
#              position=position_jitter(seed=101, width=0.25, height=0)) +
#   geom_boxplot(color="black",
#                fill="white",
#                alpha=0,
#                #width=0.20,
#                width=0.3,
#                lwd=0.5) +
#   # Aesthetics
#   scale_x_discrete(labels=c("BeforeRank" = "Pre",
#                             "AfterRank" = "Post")) +
#   #scale_y_continuous(breaks=c(1,4,8,12,16)) +
#   scale_y_continuous(breaks=c(1,16),
#                      labels=c("1" = "Optimal",
#                               "16" = "Poor")) +
#   labs(
#     title="Both Room",
#     #subtitle="Conference Room - 12 P.M. - Back Wall Window",
#     x="",
#     y="Facade Rank") +
#   theme_classic() +
#   theme(
#     text=element_text(size=14, family="sans"),
#     # Title
#     axis.title=element_text(size=20, face = "bold"),
#     # Y Axis
#     #axis.title.x=element_text(size=14, face = "plain"),
#     axis.title.y=element_blank(),
#     axis.text.y=element_text(size=16, face = "bold"),
#     axis.ticks.y=element_blank(),
#     #axis.line.y=element_blank(),
#     # X Axis
#     axis.title.x=element_blank(),
#     axis.text.x=element_text(size=16, face = "bold"),
#     axis.ticks.x=element_blank(),
#     #axis.line.x=element_blank()
#     ) +
#   #theme_void() +
#   coord_flip()
# print(ranking_p)
#
# subject_data_melted_conf <- subset(subject_data_melted, Room == "Conference")
# ranking_conf_p <- ggplot(subject_data_melted_conf, aes(x=variable, y=value)) +
#   # Data
#   ggdist::stat_halfeye(fill="gray",
#                        alpha=0.5,
#                        #width=0.5,
#                        width=0.75,
#                        adjust=0.5,
#                        #justification=-0.3,
#                        #justification=0.225) +
#                        justification=0.225) +
#   geom_point(color="black",
#              fill="gray",
#              shape=19,
#              size=1.5,
#              alpha=0.3,
#              position=position_jitter(seed=101, width=0.25, height=0)) +
#   geom_boxplot(color="black",
#                fill="white",
#                alpha=0,
#                #width=0.20,
#                width=0.3,
#                lwd=0.5) +
#   # Aesthetics
#   scale_x_discrete(labels=c("BeforeRank" = "Pre",
#                             "AfterRank" = "Post")) +
#   #scale_y_continuous(breaks=c(1,4,8,12,16)) +
#   scale_y_continuous(breaks=c(1,16),
#                      labels=c("1" = "Optimal",
#                               "16" = "Poor")) +
#   labs(
#     title="Conference",
#     #subtitle="Conference Room - 12 P.M. - Back Wall Window",
#     x="",
#     y="Facade Rank") +
#   theme_classic() +
#   theme(
#     text=element_text(size=14, family="sans"),
#     # Title
#     axis.title=element_text(size=20, face = "bold"),
#     # Y Axis
#     #axis.title.x=element_text(size=14, face = "plain"),
#     axis.title.y=element_blank(),
#     axis.text.y=element_text(size=16, face = "bold"),
#     axis.ticks.y=element_blank(),
#     #axis.line.y=element_blank(),
#     # X Axis
#     axis.title.x=element_blank(),
#     axis.text.x=element_text(size=16, face = "bold"),
#     axis.ticks.x=element_blank(),
#     #axis.line.x=element_blank()
#   ) +
#   #theme_void() +
#   coord_flip()
# print(ranking_conf_p)
#
# subject_data_melted_office <- subset(subject_data_melted, Room == "Office")
# ranking_office_p <- ggplot(subject_data_melted_office, aes(x=variable, y=value)) +
#   # Data
#   ggdist::stat_halfeye(fill="gray",
#                        alpha=0.5,
#                        #width=0.5,
#                        width=0.75,
#                        adjust=0.5,
#                        #justification=-0.3,
#                        #justification=0.225) +
#                        justification=0.225) +
#   geom_point(color="black",
#              fill="gray",
#              shape=19,
#              size=1.5,
#              alpha=0.3,
#              position=position_jitter(seed=101, width=0.25, height=0)) +
#   geom_boxplot(color="black",
#                fill="white",
#                alpha=0,
#                #width=0.20,
#                width=0.3,
#                lwd=0.5) +
#   # Aesthetics
#   scale_x_discrete(labels=c("BeforeRank" = "Pre",
#                             "AfterRank" = "Post")) +
#   #scale_y_continuous(breaks=c(1,4,8,12,16)) +
#   scale_y_continuous(breaks=c(1,16),
#                      labels=c("1" = "Optimal",
#                               "16" = "Poor")) +
#   scale_fill_gradient(low=poor_color,
#                       high=optimal_color) +
#   labs(
#     title="Office",
#     #subtitle="Conference Room - 12 P.M. - Back Wall Window",
#     x="",
#     y="Facade Rank") +
#   theme_classic() +
#   theme(
#     text=element_text(size=14, family="sans"),
#     # Title
#     axis.title=element_text(size=20, face = "bold"),
#     # Y Axis
#     #axis.title.x=element_text(size=16, face = "bold"),
#     axis.title.y=element_blank(),
#     axis.text.y=element_text(size=16, face = "bold"),
#     axis.ticks.y=element_blank(),
#     #axis.line.y=element_blank(),
#     # X Axis
#     axis.title.x=element_blank(),
#     axis.text.x=element_text(size=16, face = "bold"),
#     axis.ticks.x=element_blank(),
#     #axis.line.x=element_blank()
#   ) +
#   #theme_void() +
#   coord_flip()
# print(ranking_office_p)

ranking_all_p <- ggplot(subject_data_melted, aes(x=factor(variable), y=value, fill=Before)) +
  # Data
  # ggdist::stat_halfeye(#fill="gray",
  #                      alpha=0.5,
  #                      #width=0.5,
  #                      width=0.75,
  #                      adjust=0.5,
  #                      #justification=-0.3,
  #                      #justification=0.225) +
  #                      justification=0.225) +
  ggdist::stat_halfeye(#fill="gray",
                       alpha=0.5,
                       width=0.75,
                       adjust=0.5,
                       justification=-0.4) +
  # geom_point(color="black",
  #            #fill="gray",
  #            shape=21,
  #            size=1.5,
  #            alpha=0.3,
  #            position=position_jitter(seed=101, width=0.25, height=0)) +
  geom_boxplot(color="black",
               fill="white",
               alpha=0,
               #width=0.20,
               width=0.3,
               lwd=0.5,
               position="dodge") +
  # Facet
  facet_grid(.~Room,
             space="fixed",
             margins = "vs") +
  # Labels
  annotate("label",
           x=0.525,
           y=2.5,
           label="More Optimal",
           size=3,
           angle=0,
           hjust=0.5,
           vjust=0.5,
           fill="white",
           fontface="bold") +
  annotate("label",
           x=0.525,
           y=15.475,
           label="Poorer",
           size=3,
           angle=0,
           hjust=0.5,
           vjust=0.5,
           fill="white",
           fontface="bold") +
  annotate("Segment",
           x=0.525,
           y=8.5,
           xend=0.525,
           yend=5,
           size=1,
           color="black",
           arrow=arrow(length=unit(0.35,"cm"))) +
  annotate("Segment",
           x=0.525,
           y=8.5,
           xend=0.525,
           yend=13.9,
           size=1,
           color="black",
           arrow=arrow(length=unit(0.35,"cm"))) +
  # Aesthetics
  scale_x_discrete(labels=c("BeforeRank" = "Pre-Experiment\nFacade Choice",
                            "AfterRank" = " Final\nFacade Choice")) +
  #scale_y_continuous(breaks=c(3.5,15),
  #                   labels=c("3.5" = "More Optimal",
  #                            "15" = "Poorer")) +
  scale_y_continuous(lim=c(1, 16),
                     breaks=c(1, 4, 7, 10, 13, 16)) +
  scale_fill_manual(values=c(before_color,
                             after_color)) +
  labs(
    title="All",
    #subtitle="Conference Room - 12 P.M. - Back Wall Window",
    x="",
    y="Facade Overall Rank") +
  #theme_classic() +
  theme_bw() +
  theme(
    # Grid
    panel.grid.major=element_line(size=1, color="gray"),
    #panel.grid.minor=element_line(size=1, color="gray"),
    # Legend
    legend.position = "none",
    # Title
    #plot.title=element_text(size=20, face = "bold"),
    plot.title=element_blank(),
    # Y Axis
    #axis.title.x=element_text(size=16, face = "bold"),
    axis.title.y=element_blank(),
    axis.text.y=element_text(size=16, face = "bold", hjust=0.5),
    axis.ticks.y=element_blank(),
    #axis.line.y=element_blank(),
    # X Axis
    axis.title.x=element_text(size=16, face = "bold"),
    axis.text.x=element_text(size=16, face = "bold"),
    axis.ticks.x=element_blank(),
    #axis.line.x=element_blank()
    # Facet
    strip.text.x=element_text(size=16, face = "bold"),
    strip.text.y=element_text(size=16, face = "bold"),
    strip.background = element_rect(color="black", fill="gray", size=1, linetype="solid"),
    panel.spacing = unit(2, "lines")
  ) +
  coord_flip()
print(ranking_all_p)

subject_data_melted2 <- melt(subject_data, id=c("Room"))
subject_data_melted2 <- subset(subject_data_melted2, variable=="VF_BeforeRank" | variable=="G_BeforeRank" | variable=="DL_BeforeRank" | variable=="VF_AfterRank" | variable=="G_AfterRank" | variable=="DL_AfterRank")
subject_data_melted2$value <- as.numeric(subject_data_melted2$value)
subject_data_melted2$Before <- grepl("Before", subject_data_melted2$variable, fixed=TRUE) # Set Before column for coloring

#ranking_alt_p <- ggplot(subject_data_melted2, aes(x=factor(variable), y=value, fill=factor(Room))) +
ranking_alt_p <- ggplot(subject_data_melted2, aes(x=variable, y=value, fill=Before)) +
  # Data
  # ggdist::stat_halfeye(#fill="gray",
  #                      alpha=0.5,
  #                      #width=0.5,
  #                      width=0.75,
  #                      adjust=0.5,
  #                      #justification=-0.3,
  #                      #justification=0.225) +
  #                      justification=0.225) +
  ggdist::stat_halfeye(#fill="gray",
                       alpha=0.5,
                       width=0.6,
                       adjust=0.5,
                       justification=-0.4) +
  # geom_point(color="black",
  #            #fill="gray",
  #            shape=21,
  #            size=1.5,
  #            alpha=0.3,
  #            position=position_jitter(seed=101, width=0.25, height=0)) +
  geom_boxplot(color="black",
               fill="white",
               alpha=0,
               #width=0.20,
               width=0.3,
               lwd=0.5,
               position="dodge") +
  # Facet
  facet_grid(.~Room,
             space="fixed",
             margins = "vs") +
  # Labels
  annotate("label",
           x=0.7,
           y=2.5,
           label="More Optimal",
           size=3,
           angle=0,
           hjust=0.5,
           vjust=0.5,
           fill="white",
           fontface="bold") +
  annotate("label",
           x=0.7,
           y=15.475,
           label="Poorer",
           size=3,
           angle=0,
           hjust=0.5,
           vjust=0.5,
           fill="white",
           fontface="bold") +
  annotate("Segment",
           x=0.7,
           y=8.5,
           xend=0.7,
           yend=5,
           size=1,
           color="black",
           arrow=arrow(length=unit(0.35,"cm"))) +
  annotate("Segment",
           x=0.7,
           y=8.5,
           xend=0.7,
           yend=13.9,
           size=1,
           color="black",
           arrow=arrow(length=unit(0.35,"cm"))) +
  # Aesthetics
  scale_x_discrete(labels=c("DL_BeforeRank" = "Pre-Experiment\nDaylighting",
                            "VF_BeforeRank" = "Pre-Experiment\nView Factor",
                            "G_BeforeRank" = " Pre-Experiment\nBrightness\nDiscomfort",
                            "DL_AfterRank" = "Final\nDaylighting",
                            "VF_AfterRank" = "Final\nView Factor",
                            "G_AfterRank" = " Final\nBrightness\nDiscomfort"),
                   limits=c("VF_AfterRank",
                            "VF_BeforeRank",
                            "G_AfterRank",
                            "G_BeforeRank",
                            "DL_AfterRank",
                            "DL_BeforeRank")) +
  #scale_y_continuous(breaks=c(3.5,15),
  #                   labels=c("3.5" = "More Optimal",
  #                            "15" = "Poorer")) +
  scale_y_continuous(lim=c(1, 16),
                     breaks=c(1, 4, 7, 10, 13, 16)) +
  scale_fill_manual(values=c(before_color,
                             after_color)) +
  labs(
    title="All",
    #subtitle="Conference Room - 12 P.M. - Back Wall Window",
    x="",
    y="Facade Overall Rank") +
  #theme_classic() +
  theme_bw() +
  theme(
    # Grid
    panel.grid.major=element_line(size=1, color="gray"),
    #panel.grid.minor=element_line(size=1, color="gray"),
    # Legend
    legend.position = "none",
    # Title
    #plot.title=element_text(size=20, face = "bold"),
    plot.title=element_blank(),
    # Y Axis
    #axis.title.x=element_text(size=16, face = "bold"),
    axis.title.y=element_blank(),
    axis.text.y=element_text(size=16, face = "bold", hjust=0.5),
    axis.ticks.y=element_blank(),
    #axis.line.y=element_blank(),
    # X Axis
    axis.title.x=element_text(size=16, face = "bold"),
    axis.text.x=element_text(size=16, face = "bold"),
    axis.ticks.x=element_blank(),
    #axis.line.x=element_blank()
    # Facet
    strip.text.x=element_text(size=16, face = "bold"),
    strip.text.y=element_text(size=16, face = "bold"),
    strip.background = element_rect(color="black", fill="gray", size=1, linetype="solid"),
    panel.spacing = unit(2, "lines")
  ) +
  coord_flip()
print(ranking_alt_p)

##########################################################################################################
###### CHOICE TIME PLOT

choice_conf <- choice_data
choice_office <- choice_data
for(i in nrow(choice_data):1)
{
  id <- choice_data[i, "Participant.ID"]
  
  if(!(id %in% subject_data[subject_data$Room=="Conference",]$Participant.ID))
  {
    choice_conf <- choice_conf[-i,]
  }
  if(!(id %in% subject_data[subject_data$Room=="Office",]$Participant.ID))
  {
    choice_office <- choice_office[-i,]
  }
}

sum_choice_data <- function(data)
{
  data_summed <- data[FALSE,] # Create dataframe with same columns names as data
  data_summed <- subset(data_summed, select = -c(Participant.ID)) # Drop participant ID column
  data_summed[1, "DYNAMIC_0"] <- sum(data$DYNAMIC_0)
  data_summed[1, "DYNAMIC_30"] <- sum(data$DYNAMIC_30)
  data_summed[1, "DYNAMIC_60"] <- sum(data$DYNAMIC_60)
  data_summed[1, "DYNAMIC_90"] <- sum(data$DYNAMIC_90)
  data_summed[1, "FIN_10_30"] <- sum(data$FIN_10_30)
  data_summed[1, "FIN_10_70"] <- sum(data$FIN_10_70)
  data_summed[1, "FIN_20_30"] <- sum(data$FIN_20_30)
  data_summed[1, "FIN_20_70"] <- sum(data$FIN_20_70)
  data_summed[1, "LOUVER_10_30"] <- sum(data$LOUVER_10_30)
  data_summed[1, "LOUVER_10_70"] <- sum(data$LOUVER_10_70)
  data_summed[1, "LOUVER_20_30"] <- sum(data$LOUVER_20_30)
  data_summed[1, "LOUVER_20_70"] <- sum(data$LOUVER_20_70)
  data_summed[1, "FRITT_15_5"] <- sum(data$FRITT_15_5)
  data_summed[1, "FRITT_15_10"] <- sum(data$FRITT_15_10)
  data_summed[1, "FRITT_30_5"] <- sum(data$FRITT_30_5)
  data_summed[1, "FRITT_30_10"] <- sum(data$FRITT_30_10)
  data_summed <- melt(data_summed)
  names(data_summed)[names(data_summed) == 'variable'] <- "Facade" # Rename column
  names(data_summed)[names(data_summed) == 'value'] <- "Time" # Rename column
  return(data_summed)
}

# Add optimal and rank
add_optimal_and_rank_cols <- function(data, room)
{
  data$Optimal <- -1
  data$Rank <- -1
  for(i in 1:nrow(data))
  {
    facade_name <- data[i, "Facade"]
    if(room == "Conference")
    {
      for(j in 1:nrow(scores_conf))
      {
        if(facade_name == scores_conf[j, "Facade"])
        {
          # Rank
          data$Rank[i] <- scores_conf[j, "Rank"]
          #Optimal
          if(scores_conf[j, "Optimal"] == 1)
          {
            data$Optimal[i] <- "Optimal"
          }
          else
          {
            data$Optimal[i] <- "Poor"
          }
          break
        }
      }
    }
    else
    {
      for(j in 1:nrow(scores_office))
      {
        if(facade_name == scores_office[j, "Facade"])
        {
          # Rank
          data$Rank[i] <- scores_office[j, "Rank"]
          #Optimal
          if(scores_office[j, "Optimal"] == 1)
          {
            data$Optimal[i] <- "Optimal"
          }
          else
          {
            data$Optimal[i] <- "Poor"
          }
          break
        }
      }
    }
  }
  return(data)
}

choice_conf <- sum_choice_data(choice_conf)
choice_conf <- add_optimal_and_rank_cols(choice_conf, "Conference")
choice_conf$Room <- "Conference"
choice_office <- sum_choice_data(choice_office)
choice_office <- add_optimal_and_rank_cols(choice_office, "Office")
choice_office$Room <- "Office"
choice_data_summed <- rbind(choice_conf, choice_office)

# Plot choice treemap
optimal_color_scale <- c(optimal_color, poor_color)

# https://r-charts.com/part-whole/treemapify/
choice_tree_p <- ggplot(choice_data_summed, aes(area=Time,
                                                fill=Rank,
                                                subgroup=Optimal)) +
  geom_treemap(color="black") +
  # geom_treemap_text(color="black",
  #                   place="centre",
  #                   fontface="italic",
  #                   size=12) +
  # geom_treemap_text(data=~filter(choice_bestworst_data, FinalChoice == TRUE),
  #                   aes(area=Time, fill=factor(Optimal), label=Facade),
  #                   #label="Final Choice",
  #                   color="white",
  #                   place="centre",
  #                   fontface="italic",
  #                   size=12) +
geom_treemap_subgroup_text(place = "centre",
                           grow = TRUE,
                           alpha = 0.30,
                           angle = 45,
                           colour = "black",
                           fontface = "bold") +
  facet_wrap(.~Room,
             #space="fixed",
             #margins="vs",
             nrow=2) +
  geom_treemap_subgroup_border(colour = "black", size = 2) +
  #scale_fill_manual(values=optimal_color_scale) +
  scale_fill_gradient2(name="Facade\nRank",
                       low=optimal_color,
                       mid="white",
                       high=poor_color,
                       space = "Lab",
                       aesthetics = "fill",
                       midpoint = 8,
                       breaks=c(1, 4, 7, 10, 13, 16)) +
  theme(# Legend
    #legend.position = "right",
    legend.position = "bottom",
    legend.key.height=unit(0.5, "cm"),
    #legend.key.height=unit(0.5, "cm"),
    legend.key.width=unit(3.25, "cm"),
    legend.background=element_rect(fill="white",
                                   color="gray",
                                   size=1.0),
    legend.title=element_text(size=14, face = "bold"),
    legend.title.align=0.5,
    legend.text=element_text(size=12, face = "bold", hjust=0.5, vjust=0),
    legend.spacing.y=unit(0.5, "cm"),
    # Facet
    strip.text.x=element_text(size=12, face = "bold"),
    strip.text.y=element_text(size=12, face = "bold"),
    strip.background = element_rect(color="black", fill="gray", size=1, linetype="solid"),
    panel.spacing = unit(0.5, "lines"))
print(choice_tree_p)

choice_bar_p <- ggplot(choice_data_summed, aes(x=Rank,
                                               y=Time,
                                               fill=Rank)) +
  geom_bar(stat="identity",
           color="black") +
  facet_wrap(.~Room,
             #space="fixed",
             #margins="vs",
             nrow=2) +
  scale_fill_gradient2(name="Facade\nRank",
                       low=optimal_color,
                       mid=mid_color,
                       high=poor_color,
                       space = "Lab",
                       aesthetics = "fill",
                       midpoint = 8,
                       breaks=c(1, 4, 7, 10, 13, 16),
                       guide = guide_colourbar(reverse = TRUE)) + # For reversing legend
  xlab("") + 
  ylab("Time (seconds)") +
  scale_x_reverse() +
  theme_bw() +
  theme(
    # Legend
    #legend.position = "right",
    legend.position = "bottom",
    legend.key.height=unit(0.5, "cm"),
    #legend.key.height=unit(0.5, "cm"),
    legend.key.width=unit(2.5, "cm"),
    legend.background=element_rect(fill="white",
                                   color="gray",
                                   size=1.0),
    legend.title=element_text(size=14, face = "bold"),
    legend.title.align=0.5,
    legend.text=element_text(size=12, face = "bold", hjust=0.5, vjust=0),
    legend.spacing.y=unit(0.5, "cm"),
    # Facet
    strip.text.x=element_text(size=12, face = "bold"),
    strip.text.y=element_text(size=12, face = "bold"),
    strip.background = element_rect(color="black", fill="gray", size=1, linetype="solid"),
    panel.spacing = unit(0.5, "lines"),
    # Background
    #panel.background=element_rect(fill="gray90",
    #                              color="gray90",
    #                              size=0.5,
    #                              linetype="solid"),
    panel.grid.major.y=element_line(size=1,
                                    linetype="solid",
                                    color="gray"),
    #panel.grid.minor.y=element_line(size=0.5,
    #                               linetype="solid",
    #                               color="black"),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x=element_blank(),
    # Axes
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_text(size=16, face = "bold", hjust=0.5),
    axis.text.y=element_text(size=10, face = "bold"))
print(choice_bar_p)

##########################################################################################################
###### BIGGEST RISERS LINE PLOT


#### OLD METHOD START ############################

# # Get biggset change subjects
# difference_threshold <- 8
# #subject_data$Difference <- subject_data$BeforeRank - subject_data$AfterRank
# subject_biggest_change_data <- subset(subject_data, abs(Difference) >= difference_threshold)
# subject_biggest_change_data_melted <- melt(subject_biggest_change_data, id=c("Participant.ID", "Room", "Condition"))
# subject_biggest_change_data_melted <- subset(subject_biggest_change_data_melted, variable=="AfterRank" | variable=="BeforeRank")
# subject_biggest_change_data_melted$value <- as.numeric(subject_biggest_change_data_melted$value)
# subject_biggest_change_data_melted$Biggest <- "Largest Change"
#
# # Get smallest change subjects
# subject_data <- subject_data[with(subject_data, order(Difference)),] # Reorder by distance value so we can go through and find the minimum sum chain
# biggest_change_n <- nrow(subject_biggest_change_data)
# minimum_sum <- 10000000
# minimum_index <- -1
# for(i in 1:nrow(subject_data)) # Get the smallest sequential sum of desired n
# {
#   #print(i)
#   distance_sum <- 0
#   for(j in i:(i+biggest_change_n-1)) # Check next N spots in list
#   {
#     #print(paste("Checking", toString(j)))
#     if(j > nrow(subject_data)) # Past end of data, so this cant be a valid section
#     {
#       distance_sum <- 0
#       break
#     }
#
#     distance_sum <- distance_sum + abs(subject_data[j, "Difference"])
#
#     if(j == (i+biggest_change_n-1)) # end of chain
#     {
#       if(distance_sum < minimum_sum) # set new minimum
#       {
#         minimum_index <- i
#         minimum_sum <- distance_sum
#         #print(paste(toString(i), "is smallest"))
#         #print(paste("Sum:", toString(minimum_sum)))
#       }
#     }
#   }
# }
# subject_smallest_change_data <- subject_data[minimum_index:(minimum_index+biggest_change_n-1),]
# subject_smallest_change_data_melted <- melt(subject_smallest_change_data, id=c("Participant.ID", "Room", "Condition"))
# subject_smallest_change_data_melted <- subset(subject_smallest_change_data_melted, variable=="AfterRank" | variable=="BeforeRank")
# subject_smallest_change_data_melted$value <- as.numeric(subject_smallest_change_data_melted$value)
# subject_smallest_change_data_melted$Biggest <- "Smallest Change"
#
# # Combine biggest and smallest into 1 dataframe
# subject_most_change_data <- rbind(subject_biggest_change_data_melted, subject_smallest_change_data_melted)

#### OLD METHOD END ############################

# Top 22 and bottom 22 most changed
subject_most_change_data <- head(subject_data[order(abs(subject_data$Difference), decreasing=TRUE),], 22)
#subject_most_change_data$Changed <- "Most Changed"
subject_least_change_data <- head(subject_data[order(abs(subject_data$Difference), decreasing=FALSE),], 22)
#subject_least_change_data$Changed <- "Least Changed"

subject_most_change_data_melted <- melt(subject_most_change_data, id=c("Participant.ID", "Room", "Condition"))
subject_most_change_data_melted <- subset(subject_most_change_data_melted, variable=="AfterRank" | variable=="BeforeRank")
subject_most_change_data_melted$value <- as.numeric(subject_most_change_data_melted$value)
subject_most_change_data_melted$Changed <- "Most Changed"
subject_least_change_data_melted <- melt(subject_least_change_data, id=c("Participant.ID", "Room", "Condition"))
subject_least_change_data_melted <- subset(subject_least_change_data_melted, variable=="AfterRank" | variable=="BeforeRank")
subject_least_change_data_melted$value <- as.numeric(subject_least_change_data_melted$value)
subject_least_change_data_melted$Changed <- "Least Changed"

# # Combine biggest and smallest into 1 dataframe
subject_mostleast_change_data <- rbind(subject_most_change_data_melted, subject_least_change_data_melted)

# Plot
conf_color <- "#DC267F"
office_color <- "#FFB000"
biggest_change_p <- ggplot(subject_mostleast_change_data, aes(x=variable, y=value, color=factor(Room), group=Participant.ID)) +
  ## DATA (MAIN)
  geom_line(linetype="solid",
            size=2) +
  geom_point(size=4,
             show.legend=FALSE) +
  labs(y="Facade Rank",
       fill="Facade Type") +
  # Facet
  facet_grid(.~Changed,
             space="fixed",
             margins = "vs") +
  ## LIMITS
  #scale_y_reverse() +
  scale_y_reverse(lim=c(16,1),
                  breaks=c(1, 4, 7, 10, 13, 16)) +
  scale_x_discrete(labels=c("AfterRank" = "Final",
                            "BeforeRank" = "Pre\nExperiment"),
                   limits=c("BeforeRank",
                            "AfterRank"),
                   #expand=c(0.075,0.075)) +
                   expand=c(0.03,0.03)) +
  ## AESTHETICS
  theme_classic() +
  theme(
    #Legend
    legend.position="bottom",
    #legend.position=c(0.875, 0.5),
    legend.background=element_rect(fill="white",
                                   color="gray",
                                   size=1.0),
    legend.title=element_blank(),
    legend.text=element_text(size=14, face = "italic"),
    # Facet
    strip.text.x=element_text(size=16, face = "bold"),
    strip.text.y=element_text(size=16, face = "bold"),
    strip.background = element_rect(color="black", fill="gray", size=1, linetype="solid"),
    panel.spacing = unit(2, "lines"),
    # Title
    #text=element_text(size=16, family="bold"),
    #axis.title=element_text(size=16, face = "bold"),
    plot.title=element_text(size=16, face = "bold", hjust = 0.5),
    # Y Axis
    axis.title.y=element_text(size=16, face = "bold"),
    #axis.title.y=element_blank(),
    #axis.text.y=element_blank(),
    axis.text.y=element_text(size=16, face = "bold"),
    axis.ticks.y=element_blank(),
    #axis.line.y=element_blank(),
    # X Axis
    axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.x=element_text(size=16, face = "bold"),
    axis.ticks.x=element_blank(),
    # Grid
    panel.grid.major=element_line(size=1, color="gray"),
    panel.grid.minor=element_line(size=1, color="gray")) +
  #axis.line.x=element_blank()) +
  ## Color
  scale_color_manual(values=c(office_color,
                              conf_color))
print(biggest_change_p)

##########################################################################################################
###### BIGGEST RISERS BOX AND WHISKER

optimal_x <- 0.6

biggest_change_box_p <- ggplot(subject_mostleast_change_data, aes(x=factor(variable), y=value, fill=factor(Room))) +
  # Data
  # ggdist::stat_halfeye(#fill="gray",
  #                      alpha=0.5,
  #                      #width=0.5,
  #                      width=0.75,
  #                      adjust=0.5,
  #                      #justification=-0.3,
  #                      #justification=0.225) +
  #                      justification=0.225) +
  ggdist::stat_halfeye(#fill="gray",
    alpha=0.5,
    width=0.6,
    adjust=0.5,
    justification=-0.4) +
  # geom_point(color="black",
  #            #fill="gray",
  #            shape=21,
  #            size=1.5,
  #            alpha=0.3,
  #            position=position_jitter(seed=101, width=0.25, height=0)) +
  geom_boxplot(color="black",
               fill="white",
               alpha=0,
               #width=0.20,
               width=0.3,
               lwd=0.5,
               position="dodge") +
  # Facet
  facet_wrap(.~Changed+Room,
             #space="fixed",
             #margins = "vs",
             ncol=2) +
  # Labels
  annotate("label",
           x=optimal_x,
           y=2.5,
           label="More Optimal",
           size=3,
           angle=0,
           hjust=0.5,
           vjust=0.5,
           fill="white",
           fontface="bold") +
  annotate("label",
           x=optimal_x,
           y=15.475,
           label="Poorer",
           size=3,
           angle=0,
           hjust=0.5,
           vjust=0.5,
           fill="white",
           fontface="bold") +
  annotate("Segment",
           x=optimal_x,
           y=8.5,
           xend=optimal_x,
           yend=5,
           size=1,
           color="black",
           arrow=arrow(length=unit(0.35,"cm"))) +
  annotate("Segment",
           x=optimal_x,
           y=8.5,
           xend=optimal_x,
           yend=13.9,
           size=1,
           color="black",
           arrow=arrow(length=unit(0.35,"cm"))) +
  # Aesthetics
  # scale_x_discrete(labels=c("DL_BeforeRank" = "Pre-Experiment\nDaylighting",
  #                           "VF_BeforeRank" = "Pre-Experiment\nView Factor",
  #                           "G_BeforeRank" = " Pre-Experiment\nBrightness\nDiscomfort",
  #                           "DL_AfterRank" = "Final\nDaylighting",
  #                           "VF_AfterRank" = "Final\nView Factor",
  #                           "G_AfterRank" = " Final\nBrightness\nDiscomfort"),
  #                  limits=c("VF_AfterRank",
  #                           "VF_BeforeRank",
  #                           "G_AfterRank",
  #                           "G_BeforeRank",
  #                           "DL_AfterRank",
  #                           "DL_BeforeRank")) +
  scale_y_continuous(lim=c(1, 16),
                     breaks=c(1, 4, 7, 10, 13, 16)) +
  scale_x_discrete(labels=c("AfterRank" = "Final",
                            "BeforeRank" = "Pre\nExperiment")) +
  scale_fill_manual(values=c(office_color,
                             conf_color)) +
  labs(
    title="All",
    #subtitle="Conference Room - 12 P.M. - Back Wall Window",
    x="",
    y="Facade Rank") +
  #theme_classic() +
  theme_bw() +
  theme(
    # Grid
    panel.grid.major=element_line(size=1, color="gray"),
    #panel.grid.minor=element_line(size=1, color="gray"),
    # Legend
    legend.position = "none",
    # Title
    #plot.title=element_text(size=20, face = "bold"),
    plot.title=element_blank(),
    # Y Axis
    #axis.title.x=element_text(size=16, face = "bold"),
    axis.title.y=element_blank(),
    axis.text.y=element_text(size=16, face = "bold", hjust=0.5),
    axis.ticks.y=element_blank(),
    #axis.line.y=element_blank(),
    # X Axis
    axis.title.x=element_blank(),
    axis.text.x=element_text(size=16, face = "bold"),
    axis.ticks.x=element_blank(),
    #axis.line.x=element_blank()
    # Facet
    strip.text.x=element_text(size=12, face = "bold"),
    strip.text.y=element_text(size=12, face = "bold"),
    strip.background = element_rect(color="black", fill="gray", size=1, linetype="solid"),
    panel.spacing = unit(0.5, "lines")
  ) +
  coord_flip()
print(biggest_change_box_p)

##########################################################################################################
###### BIGGEST RISERS TREEMAP

# START HERE

# Get time spent in each option for the most/least changed subjects
choice_most_change_data <- choice_data
choice_least_change_data <- choice_data
for(i in nrow(choice_data):1)
{
  id <- choice_data[i, "Participant.ID"]
  if(!(id %in% subject_most_change_data$Participant.ID))
  {
    choice_most_change_data <- choice_most_change_data[-i,]
  }
  if(!(id %in% subject_least_change_data$Participant.ID))
  {
    choice_least_change_data <- choice_least_change_data[-i,]
  }
}

# Add condition and room
add_condition_and_room_cols <- function(data)
{
  data$Room <- NA
  data$Condition <- NA
  for(i in 1:nrow(data))
  {
    data_id <- data[i, "Participant.ID"]
    for(j in 1:nrow(subject_data))
    {
      if(data_id == subject_data[j,"Participant.ID"])
      {
        data[i,"Room"] <- subject_data[j,"Room"]
        data[i,"Condition"] <- subject_data[j,"Condition"]
        break
      }
    }
  }
  return(data)
}
choice_most_change_data <- add_condition_and_room_cols(choice_most_change_data)
choice_least_change_data <- add_condition_and_room_cols(choice_least_change_data)
choice_most_change_conf_data <- subset(choice_most_change_data, Room=="Conference")
choice_most_change_office_data <- subset(choice_most_change_data, Room=="Office")
choice_least_change_conf_data <- subset(choice_least_change_data, Room=="Conference")
choice_least_change_office_data <- subset(choice_least_change_data, Room=="Office")

# Seperate, sum up, and recombine
choice_most_change_conf_data <- subset(choice_most_change_conf_data, select=-c(Room, Condition))
choice_most_change_office_data <- subset(choice_most_change_office_data, select=-c(Room, Condition))
choice_least_change_conf_data <- subset(choice_least_change_conf_data, select=-c(Room, Condition))
choice_least_change_office_data <- subset(choice_least_change_office_data, select=-c(Room, Condition))

choice_most_change_conf_data_summed <- sum_choice_data(choice_most_change_conf_data)
choice_most_change_conf_data_summed$Room <- "Conference"
choice_most_change_conf_data_summed$condition <- "AR"
choice_most_change_conf_data_summed$Change <- "Most Changed"
choice_most_change_office_data_summed <- sum_choice_data(choice_most_change_office_data)
choice_most_change_office_data_summed$Room <- "Office"
choice_most_change_office_data_summed$condition <- "AR"
choice_most_change_office_data_summed$Change <- "Most Changed"
choice_least_change_conf_data_summed <- sum_choice_data(choice_least_change_conf_data)
choice_least_change_conf_data_summed$Room <- "Conference"
choice_least_change_conf_data_summed$condition <- "AR"
choice_least_change_conf_data_summed$Change <- "Least Changed"
choice_least_change_office_data_summed <- sum_choice_data(choice_least_change_office_data)
choice_least_change_office_data_summed$Room <- "Office"
choice_least_change_office_data_summed$condition <- "AR"
choice_least_change_office_data_summed$Change <- "Least Changed"

choice_most_change_conf_data_summed <- add_optimal_and_rank_cols(choice_most_change_conf_data_summed, "Conference")
choice_most_change_office_data_summed <- add_optimal_and_rank_cols(choice_most_change_office_data_summed, "Office")
choice_least_change_conf_data_summed <- add_optimal_and_rank_cols(choice_least_change_conf_data_summed, "Conference")
choice_least_change_office_data_summed <- add_optimal_and_rank_cols(choice_least_change_office_data_summed, "Office")

add_is_final_choice_col <- function(summed_data, final_choice)
{
  summed_data$FinalChoice <- ""
  for(i in 1:nrow(summed_data))
  {
    if(summed_data[i,"Facade"] == final_choice)
    {
      summed_data[i,"FinalChoice"] <- "Final Choice"
    }
    else
    {
      summed_data[i,"FinalChoice"] <- ""
    }
  }
  return(summed_data)
}

choice_most_change_conf_data_summed <- add_is_final_choice_col(choice_most_change_conf_data_summed, "FIN_10_70")
choice_most_change_office_data_summed <- add_is_final_choice_col(choice_most_change_office_data_summed, "FIN_10_70")
choice_least_change_conf_data_summed <- add_is_final_choice_col(choice_least_change_conf_data_summed, "LOUVER_20_30")
choice_least_change_office_data_summed <- add_is_final_choice_col(choice_least_change_office_data_summed, "FIN_20_70")

choice_mostleast_data <- rbind(choice_most_change_conf_data_summed, choice_most_change_office_data_summed)
choice_mostleast_data <- rbind(choice_mostleast_data, choice_least_change_conf_data_summed)
choice_mostleast_data <- rbind(choice_mostleast_data, choice_least_change_office_data_summed)

optimal_color_scale <- c(optimal_color, poor_color)


# END HERE


biggest_change_tree_p <- ggplot(choice_mostleast_data, aes(area=Time,
                                                           fill=Rank,
                                                           #fill=factor(Optimal),
                                                           label=FinalChoice,
                                                           subgroup=Optimal)) +
  geom_treemap(color="black") +
  # geom_treemap_text(color="black",
  #                   place="centre",
  #                   fontface="italic",
  #                   size=12) +
  # geom_treemap_text(data=~filter(choice_bestworst_data, FinalChoice == TRUE),
  #                   aes(area=Time, fill=factor(Optimal), label=Facade),
  #                   #label="Final Choice",
  #                   color="white",
  #                   place="centre",
  #                   fontface="italic",
  #                   size=12) +
geom_treemap_subgroup_text(place = "centre",
                           grow = TRUE,
                           alpha = 0.30,
                           angle = 45,
                           colour = "black",
                           fontface = "bold") +
  facet_wrap(.~Change+Room,
             #space="fixed",
             #margins="vs",
             nrow=2) +
  geom_treemap_subgroup_border(colour = "black", size = 2) +
  #scale_fill_manual(values=optimal_color_scale) +
  scale_fill_gradient2(name="Facade\nRank",
                       low=optimal_color,
                       mid="white",
                       high=poor_color,
                       space = "Lab",
                       aesthetics = "fill",
                       midpoint = 8,
                       breaks=c(1, 4, 7, 10, 13, 16)) +
  theme(# Legend
    legend.position = "right",
    #legend.position = "bottom",
    legend.key.height=unit(2, "cm"),
    #legend.key.height=unit(0.5, "cm"),
    #legend.key.width=unit(4.05, "cm"),
    legend.background=element_rect(fill="white",
                                   color="gray",
                                   size=1.0),
    legend.title=element_text(size=14, face = "bold"),
    legend.title.align=0.5,
    legend.text=element_text(size=12, face = "bold", hjust=0.5, vjust=0),
    legend.spacing.y=unit(0.5, "cm"),
    # Facet
    strip.text.x=element_text(size=12, face = "bold"),
    strip.text.y=element_text(size=12, face = "bold"),
    strip.background = element_rect(color="black", fill="gray", size=1, linetype="solid"),
    panel.spacing = unit(0.5, "lines"))
print(biggest_change_tree_p)

##########################################################################################################
###### TOP/BOTTOM TREE MAP

#### OLD METHOD START ############################

# top_performance_threshold <- 1
# bottom_performance_threshold <- 14
# subject_bestworst_data <- subset(subject_data, AfterRank >= bottom_performance_threshold | AfterRank <= top_performance_threshold)
# subject_best_data <- subset(subject_data, AfterRank <= top_performance_threshold)
# subject_worst_data <- subset(subject_data, AfterRank >= bottom_performance_threshold)
#
# # Add best worst col
# #subject_bestworst_data$Best <- subject_bestworst_data$AfterRank <= top_performance_threshold
# subject_bestworst_data$Best <- NA
# for(i in 1:nrow(subject_bestworst_data))
# {
#   if(subject_bestworst_data[i,"AfterRank"] <= top_performance_threshold)
#   {
#     subject_bestworst_data[i, "Best"] <- "Best Performers"
#   }
#   else
#   {
#     subject_bestworst_data[i, "Best"] <- "Worst Performers"
#   }
# }

#### OLD METHOD END ############################

# Top 22 and bottom 22 best
subject_best_data <- head(subject_data[order(abs(subject_data$AfterRank), decreasing=FALSE),], 22)
subject_best_data$Best <- "Best Performers"
subject_worst_data <- head(subject_data[order(abs(subject_data$AfterRank), decreasing=TRUE),], 22)
subject_worst_data$Best <- "Worst Performers"

# Recombine
subject_bestworst_data <- rbind(subject_best_data, subject_worst_data)

# Get time spent in each option for the best/worst subjects
choice_best_data <- choice_data
choice_worst_data <- choice_data
for(i in nrow(choice_data):1)
{
  id <- choice_data[i, "Participant.ID"]
  if(!(id %in% subject_best_data$Participant.ID))
  {
    choice_best_data <- choice_best_data[-i,]
  }
  if(!(id %in% subject_worst_data$Participant.ID))
  {
    choice_worst_data <- choice_worst_data[-i,]
  }
}

# Add condition and room
add_condition_and_room_cols <- function(data)
{
  data$Room <- NA
  data$Condition <- NA
  for(i in 1:nrow(data))
  {
    data_id <- data[i, "Participant.ID"]
    for(j in 1:nrow(subject_data))
    {
      if(data_id == subject_data[j,"Participant.ID"])
      {
        data[i,"Room"] <- subject_data[j,"Room"]
        data[i,"Condition"] <- subject_data[j,"Condition"]
        break
      }
    }
  }
  return(data)
}
choice_best_data <- add_condition_and_room_cols(choice_best_data)
choice_worst_data <- add_condition_and_room_cols(choice_worst_data)
choice_best_conf_data <- subset(choice_best_data, Room=="Conference")
choice_best_office_data <- subset(choice_best_data, Room=="Office")
choice_worst_conf_data <- subset(choice_worst_data, Room=="Conference")
choice_worst_office_data <- subset(choice_worst_data, Room=="Office")

# Seperate, sum up, and recombine
choice_best_conf_data <- subset(choice_best_conf_data, select=-c(Room, Condition))
choice_best_office_data <- subset(choice_best_office_data, select=-c(Room, Condition))
choice_worst_conf_data <- subset(choice_worst_conf_data, select=-c(Room, Condition))
choice_worst_office_data <- subset(choice_worst_office_data, select=-c(Room, Condition))

choice_best_conf_data_summed <- sum_choice_data(choice_best_conf_data)
choice_best_conf_data_summed$Room <- "Conference"
choice_best_conf_data_summed$condition <- "AR"
choice_best_conf_data_summed$Best <- "Best Performers"
choice_best_office_data_summed <- sum_choice_data(choice_best_office_data)
choice_best_office_data_summed$Room <- "Office"
choice_best_office_data_summed$condition <- "AR"
choice_best_office_data_summed$Best <- "Best Performers"
choice_worst_conf_data_summed <- sum_choice_data(choice_worst_conf_data)
choice_worst_conf_data_summed$Room <- "Conference"
choice_worst_conf_data_summed$condition <- "AR"
choice_worst_conf_data_summed$Best <- "Worst Performers"
choice_worst_office_summed <- sum_choice_data(choice_worst_office_data)
choice_worst_office_summed$Room <- "Office"
choice_worst_office_summed$condition <- "AR"
choice_worst_office_summed$Best <- "Worst Performers"

choice_best_conf_data_summed <- add_optimal_and_rank_cols(choice_best_conf_data_summed, "Conference")
choice_best_office_data_summed <- add_optimal_and_rank_cols(choice_best_office_data_summed, "Office")
choice_worst_conf_data_summed <- add_optimal_and_rank_cols(choice_worst_conf_data_summed, "Conference")
choice_worst_office_summed <- add_optimal_and_rank_cols(choice_worst_office_summed, "Office")

add_is_final_choice_col <- function(summed_data, final_choice)
{
  summed_data$FinalChoice <- ""
  for(i in 1:nrow(summed_data))
  {
    if(summed_data[i,"Facade"] == final_choice)
    {
      summed_data[i,"FinalChoice"] <- "Final Choice"
    }
    else
    {
      summed_data[i,"FinalChoice"] <- ""
    }
  }
  return(summed_data)
}
# choice_best_conf_data_summed$FinalChoice <- choice_best_conf_data_summed$Facade == "FIN_10_70"
# choice_best_office_data_summed$FinalChoice <- choice_best_office_data_summed$Facade == "FIN_10_70"
# choice_worst_conf_data_summed$FinalChoice <- choice_worst_conf_data_summed$Facade == "LOUVER_20_30"
# choice_worst_office_summed$FinalChoice <- choice_worst_office_summed$Facade == "FIN_20_70"
choice_best_conf_data_summed <- add_is_final_choice_col(choice_best_conf_data_summed, "FIN_10_70")
choice_best_office_data_summed <- add_is_final_choice_col(choice_best_office_data_summed, "FIN_10_70")
choice_worst_conf_data_summed <- add_is_final_choice_col(choice_worst_conf_data_summed, "LOUVER_20_30")
choice_worst_office_summed <- add_is_final_choice_col(choice_worst_office_summed, "FIN_20_70")

choice_bestworst_data <- rbind(choice_best_conf_data_summed, choice_best_office_data_summed)
choice_bestworst_data <- rbind(choice_bestworst_data, choice_worst_conf_data_summed)
choice_bestworst_data <- rbind(choice_bestworst_data, choice_worst_office_summed)

optimal_color_scale <- c(optimal_color, poor_color)

bestworst_tree_p <- ggplot(choice_bestworst_data, aes(area=Time,
                                                      fill=Rank,
                                                      #fill=factor(Optimal),
                                                      label=FinalChoice,
                                                      subgroup=Optimal)) +
  geom_treemap(color="black") +
  # geom_treemap_text(color="black",
  #                   place="centre",
  #                   fontface="italic",
  #                   size=12) +
  # geom_treemap_text(data=~filter(choice_bestworst_data, FinalChoice == TRUE),
  #                   aes(area=Time, fill=factor(Optimal), label=Facade),
  #                   #label="Final Choice",
  #                   color="white",
  #                   place="centre",
  #                   fontface="italic",
  #                   size=12) +
  geom_treemap_subgroup_text(place = "centre",
                             grow = TRUE,
                             alpha = 0.30,
                             angle = 45,
                             colour = "black",
                             fontface = "bold") +
  facet_wrap(.~Best+Room,
             #space="fixed",
             #margins="vs",
             nrow=2) +
  geom_treemap_subgroup_border(colour = "black", size = 2) +
  #scale_fill_manual(values=optimal_color_scale) +
  scale_fill_gradient2(name="Facade\nRank",
                       low=optimal_color,
                       mid="white",
                       high=poor_color,
                       space = "Lab",
                       aesthetics = "fill",
                       midpoint = 8,
                       breaks=c(1, 4, 7, 10, 13, 16)) +
  theme(# Legend
        legend.position = "right",
        #legend.position = "bottom",
        legend.key.height=unit(2, "cm"),
        #legend.key.height=unit(0.5, "cm"),
        #legend.key.width=unit(4.05, "cm"),
        legend.background=element_rect(fill="white",
                                       color="gray",
                                       size=1.0),
        legend.title=element_text(size=14, face = "bold"),
        legend.title.align=0.5,
        legend.text=element_text(size=12, face = "bold", hjust=0.5, vjust=0),
        legend.spacing.y=unit(0.5, "cm"),
        # Facet
        strip.text.x=element_text(size=12, face = "bold"),
        strip.text.y=element_text(size=12, face = "bold"),
        strip.background = element_rect(color="black", fill="gray", size=1, linetype="solid"),
        panel.spacing = unit(0.5, "lines"))
print(bestworst_tree_p)






## Commented out below to update choice time plots on 6/2/2022





bestworst_bar_p <- ggplot(choice_bestworst_data, aes(x=Rank,
                                                     y=Time,
                                                     fill=Rank)) +
  geom_bar(stat="identity",
           color="black") +
  facet_wrap(.~Room+Best,
             #space="fixed",
             #margins="vs",
             nrow=2) +
  scale_fill_gradient2(name="Facade\nRank",
                       low=optimal_color,
                       mid=mid_color,
                       high=poor_color,
                       space = "Lab",
                       aesthetics = "fill",
                       midpoint = 8,
                       breaks=c(1, 4, 7, 10, 13, 16),
                       guide = guide_colourbar(reverse = TRUE)) + # For reversing legend
  xlab("") + 
  ylab("Time (seconds)") +
  scale_x_reverse() +
  theme_bw() +
  theme(
    # Legend
    #legend.position = "right",
    legend.position = "bottom",
    legend.key.height=unit(0.5, "cm"),
    #legend.key.height=unit(0.5, "cm"),
    legend.key.width=unit(2.5, "cm"),
    legend.background=element_rect(fill="white",
                                   color="gray",
                                   size=1.0),
    legend.title=element_text(size=14, face = "bold"),
    legend.title.align=0.5,
    legend.text=element_text(size=12, face = "bold", hjust=0.5, vjust=0),
    legend.spacing.y=unit(0.5, "cm"),
    # Facet
    strip.text.x=element_text(size=12, face = "bold"),
    strip.text.y=element_text(size=12, face = "bold"),
    strip.background = element_rect(color="black", fill="gray", size=1, linetype="solid"),
    panel.spacing = unit(0.5, "lines"),
    # Background
    #panel.background=element_rect(fill="gray90",
    #                              color="gray90",
    #                              size=0.5,
    #                              linetype="solid"),
    panel.grid.major.y=element_line(size=1,
                                    linetype="solid",
                                    color="gray"),
    #panel.grid.minor.y=element_line(size=0.5,
    #                               linetype="solid",
    #                               color="black"),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x=element_blank(),
    # Axes
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_text(size=16, face = "bold", hjust=0.5),
    axis.text.y=element_text(size=10, face = "bold"))
print(bestworst_bar_p)




##########################################################################################################
###### TOP/BOTTOM BOX PLOT

subject_bestworst_data_melted <- melt(subject_bestworst_data, id=c("Room", "Best"))
subject_bestworst_data_melted <- subset(subject_bestworst_data_melted, variable=="BeforeRank" | variable=="AfterRank")
subject_bestworst_data_melted$value <- as.numeric(subject_bestworst_data_melted$value)

optimal_x <- 0.6

bestworst_box_p <- ggplot(subject_bestworst_data_melted, aes(x=factor(variable), y=value, fill=factor(Room))) +
  # Data
  # ggdist::stat_halfeye(#fill="gray",
  #                      alpha=0.5,
  #                      #width=0.5,
  #                      width=0.75,
  #                      adjust=0.5,
  #                      #justification=-0.3,
  #                      #justification=0.225) +
  #                      justification=0.225) +
  ggdist::stat_halfeye(#fill="gray",
    alpha=0.5,
    width=0.6,
    adjust=0.5,
    justification=-0.4) +
  # geom_point(color="black",
  #            #fill="gray",
  #            shape=21,
  #            size=1.5,
  #            alpha=0.3,
  #            position=position_jitter(seed=101, width=0.25, height=0)) +
  geom_boxplot(color="black",
               fill="white",
               alpha=0,
               #width=0.20,
               width=0.3,
               lwd=0.5,
               position="dodge") +
  # Facet
  facet_wrap(.~Best+Room,
             #space="fixed",
             #margins = "vs",
             ncol=2) +
  # Labels
  annotate("label",
           x=optimal_x,
           y=2.5,
           label="More Optimal",
           size=3,
           angle=0,
           hjust=0.5,
           vjust=0.5,
           fill="white",
           fontface="bold") +
  annotate("label",
           x=optimal_x,
           y=15.475,
           label="Poorer",
           size=3,
           angle=0,
           hjust=0.5,
           vjust=0.5,
           fill="white",
           fontface="bold") +
  annotate("Segment",
           x=optimal_x,
           y=8.5,
           xend=optimal_x,
           yend=5,
           size=1,
           color="black",
           arrow=arrow(length=unit(0.35,"cm"))) +
  annotate("Segment",
           x=optimal_x,
           y=8.5,
           xend=optimal_x,
           yend=13.9,
           size=1,
           color="black",
           arrow=arrow(length=unit(0.35,"cm"))) +
  # Aesthetics
  # scale_x_discrete(labels=c("DL_BeforeRank" = "Pre-Experiment\nDaylighting",
  #                           "VF_BeforeRank" = "Pre-Experiment\nView Factor",
  #                           "G_BeforeRank" = " Pre-Experiment\nBrightness\nDiscomfort",
  #                           "DL_AfterRank" = "Final\nDaylighting",
  #                           "VF_AfterRank" = "Final\nView Factor",
  #                           "G_AfterRank" = " Final\nBrightness\nDiscomfort"),
  #                  limits=c("VF_AfterRank",
  #                           "VF_BeforeRank",
  #                           "G_AfterRank",
  #                           "G_BeforeRank",
#                           "DL_AfterRank",
#                           "DL_BeforeRank")) +
scale_y_continuous(lim=c(1, 16),
                   breaks=c(1, 4, 7, 10, 13, 16)) +
  scale_x_discrete(labels=c("AfterRank" = "Final",
                            "BeforeRank" = "Pre\nExperiment")) +
  scale_fill_manual(values=c(office_color,
                             conf_color)) +
  labs(
    title="All",
    #subtitle="Conference Room - 12 P.M. - Back Wall Window",
    x="",
    y="Facade Rank") +
  #theme_classic() +
  theme_bw() +
  theme(
    # Grid
    panel.grid.major=element_line(size=1, color="gray"),
    #panel.grid.minor=element_line(size=1, color="gray"),
    # Legend
    legend.position = "none",
    # Title
    #plot.title=element_text(size=20, face = "bold"),
    plot.title=element_blank(),
    # Y Axis
    #axis.title.x=element_text(size=16, face = "bold"),
    axis.title.y=element_blank(),
    axis.text.y=element_text(size=16, face = "bold", hjust=0.5),
    axis.ticks.y=element_blank(),
    #axis.line.y=element_blank(),
    # X Axis
    axis.title.x=element_blank(),
    axis.text.x=element_text(size=16, face = "bold"),
    axis.ticks.x=element_blank(),
    #axis.line.x=element_blank()
    # Facet
    strip.text.x=element_text(size=12, face = "bold"),
    strip.text.y=element_text(size=12, face = "bold"),
    strip.background = element_rect(color="black", fill="gray", size=1, linetype="solid"),
    panel.spacing = unit(0.5, "lines")
  ) +
  coord_flip()
print(bestworst_box_p)

##########################################################################################################
###### CHOICE LOG PLOT
#
# # Read in choice data from python script output
# choice_log_data <- read.csv("../Subjects/FacadeChoiceLog.csv",
#                             header=TRUE,
#                             sep=",")
# choice_log_data <- subset(choice_log_data, select = -c(X)) # Drop bunk index column
#
# # Get specific participant
# get_participant_room <- function(id)
# {
#   for(row in 1:nrow(subject_data))
#   {
#     if(subject_data[row,"Participant.ID"] == id)
#     {
#       return(subject_data[row,"Room"])
#     }
#   }
#   print("ERROR: Participant room unable to be found")
#   ?stop
#   return(-1)
# }
# get_participants_pre_and_post <- function(id) # returns list of of length 2: pre and then post
# {
#   for(row in 1:nrow(subject_data))
#   {
#     if(subject_data[row,"Participant.ID"] == id)
#     {
#       pre <- subject_data[row,"Before"]
#       post <- subject_data[row,"After"]
#       return(c(pre, post))
#     }
#   }
#   print("ERROR: Participant pre and choices unable to be found")
#   ?stop
#   return(-1)
# }
# get_room_optimal_count <- function(room)
# {
#   if(room == "Conference")
#   {
#     return(length(which(scores_conf$Optimal == 1)))
#   }
#   else if(room == "Office")
#   {
#     return(length(which(scores_office$Optimal == 1)))
#   }
#   else
#   {
#     print("ERROR: Incorrect room passed into get_room_optimal_count function")
#     print(room)
#     ?stop
#     return(-1)
#   }
# }
#
# #desired_participant <- 17 # Improved greatly, had bad pre, good post
# desired_participant <- 22
# participant_room <- get_participant_room(desired_participant)
# choice_log_data <- choice_log_data[choice_log_data$Subject == desired_participant, ]
# choice_log_data <- add_optimal_and_rank_cols(choice_log_data, participant_room)
# choice_log_data$IsBefore <- choice_log_data$Facade==get_participants_pre_and_post(desired_participant)[1]
# choice_log_data$IsAfter <- choice_log_data$Facade==get_participants_pre_and_post(desired_participant)[2]
#
# choice_line_p <- ggplot(choice_log_data, aes(x=TimeSwitchTo, y=Rank, fill=Rank)) +
#   ## BACKGROUND
#   annotate("rect",
#            xmin=min(choice_log_data$TimeSwitchTo),
#            xmax=max(choice_log_data$TimeSwitchTo),
#            ymin=1,
#            ymax=get_room_optimal_count(participant_room),
#            fill=optimal_color,
#            alpha=0.5) +
#   annotate("rect",
#            xmin=min(choice_log_data$TimeSwitchTo),
#            xmax=max(choice_log_data$TimeSwitchTo),
#            ymin=get_room_optimal_count(participant_room),
#            ymax=16,
#            fill=poor_color,
#            alpha=0.5) +
#   geom_hline(yintercept=get_room_optimal_count(participant_room),
#              size=2,
#              linetype="dashed",
#              color=optimal_color,
#              alpha=1) +
#   ## DATA (MAIN)
#   geom_line(linetype="solid",
#             size=3) +
#   geom_point(color='black',
#              size=6,
#              stroke=1,
#              shape=21) +
#   ## DATA (PRE/POST) +
#   geom_point(data = ~filter(choice_log_data, IsBefore == TRUE),
#              size=5,
#              shape=3) +
#   geom_point(data = ~filter(choice_log_data, IsAfter == TRUE),
#              size=5,
#              shape=8) +
#   ## DATA (LINE OF BEST FIT)
#   stat_smooth(method="loess",
#               formula=y~x,
#               size=3,
#               se=FALSE,
#               color=alt_color) +
#   ## AESTHETICS
#   #ylim(16,1) +
#   scale_y_reverse(lim=c(16,1),
#                   breaks=c(1, 4, 7, 10, 13, 16)) +
#   #coord_cartesian(
#   # ylim=c(16, 1),
#   # expand=TRUE,
#   # clip="off") +
#   #scale_y_reverse() +
#   labs(title=paste(participant_room, " - Subject ", desired_participant, sep=""),
#        x="Time (Seconds)",
#        y="Facade Rank") +
#   theme_classic() +
#   theme(
#     # Background
#     #panel.background = element_rect(fill = 'green', colour = 'red'),
#     #Legend
#     legend.position = "none",
#     # Title
#     #text=element_text(size=16, family="bold"),
#     #axis.title=element_text(size=16, face = "bold"),
#     plot.title=element_text(size=16, face = "bold", hjust = 0.5),
#     # Y Axis
#     axis.title.y=element_text(size=16, face = "bold"),
#     #axis.title.y=element_blank(),
#     #axis.text.y=element_blank(),
#     axis.text.y=element_text(size=16, face = "bold"),
#     axis.ticks.y=element_blank(),
#     #axis.line.y=element_blank(),
#     # X Axis
#     axis.title.x=element_text(size=16, face = "bold"),
#     #axis.title.x=element_blank(),
#     #axis.text.x=element_blank(),
#     axis.text.x=element_text(size=16, face = "bold"),
#     axis.ticks.x=element_blank()) +
#   #axis.line.x=element_blank()) +
#   ## Color
#   scale_fill_gradient(low=optimal_color, high=poor_color)
# print(choice_line_p)


##########################################################################################################
###### Statistics

library(ggpubr)

print("###### OVERALL RANKS ######")
print("All Subjects")
print(paste(deparse(substitute(subject_data$BeforeRank)), "mean", toString(mean(subject_data$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$BeforeRank)), "median", toString(median(subject_data$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$BeforeRank)), "std", toString(sd(subject_data$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$AfterRank)), "mean", toString(mean(subject_data$AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$AfterRank)), "median", toString(median(subject_data$AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$AfterRank)), "std", toString(sd(subject_data$AfterRank))), sep=" , ")
print("================================================")
print("Conf Subjects")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$BeforeRank)), "mean", toString(mean(subject_data[subject_data$Room=="Conference",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$BeforeRank)), "median", toString(median(subject_data[subject_data$Room=="Conference",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$BeforeRank)), "std", toString(sd(subject_data[subject_data$Room=="Conference",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$AfterRank)), "mean", toString(mean(subject_data[subject_data$Room=="Conference",]$AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$AfterRank)), "median", toString(median(subject_data[subject_data$Room=="Conference",]$AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$AfterRank)), "std", toString(sd(subject_data[subject_data$Room=="Conference",]$AfterRank))), sep=" , ")
print("================================================")
print("Office Subjects")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$BeforeRank)), "mean", toString(mean(subject_data[subject_data$Room=="Office",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$BeforeRank)), "median", toString(median(subject_data[subject_data$Room=="Office",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$BeforeRank)), "std", toString(sd(subject_data[subject_data$Room=="Office",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$AfterRank)), "mean", toString(mean(subject_data[subject_data$Room=="Office",]$AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$AfterRank)), "median", toString(median(subject_data[subject_data$Room=="Office",]$AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$AfterRank)), "std", toString(sd(subject_data[subject_data$Room=="Office",]$AfterRank))), sep=" , ")
print("================================================")
print("Best Subjects")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$BeforeRank)), "mean", toString(mean(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$BeforeRank)), "median", toString(median(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$BeforeRank)), "std", toString(sd(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$AfterRank)), "mean", toString(mean(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$AfterRank)), "median", toString(median(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$AfterRank)), "std", toString(sd(subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]$AfterRank))), sep=" , ")
print("Worst Subjects")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$BeforeRank)), "mean", toString(mean(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$BeforeRank)), "median", toString(median(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$BeforeRank)), "std", toString(sd(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$AfterRank)), "mean", toString(mean(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$AfterRank)), "median", toString(median(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$AfterRank)), "std", toString(sd(subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]$AfterRank))), sep=" , ")

print("###### Daylight RANKS ######")
print("All Subjects")
print(paste(deparse(substitute(subject_data$DL_BeforeRank)), "mean", toString(mean(subject_data$DL_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$DL_BeforeRank)), "median", toString(median(subject_data$DL_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$DL_BeforeRank)), "std", toString(sd(subject_data$DL_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$DL_AfterRank)), "mean", toString(mean(subject_data$DL_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$DL_AfterRank)), "median", toString(median(subject_data$DL_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$DL_AfterRank)), "std", toString(sd(subject_data$DL_AfterRank))), sep=" , ")
print("================================================")
print("Conf Subjects")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$DL_BeforeRank)), "mean", toString(mean(subject_data[subject_data$Room=="Conference",]$DL_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$DL_BeforeRank)), "median", toString(median(subject_data[subject_data$Room=="Conference",]$DL_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$DL_BeforeRank)), "std", toString(sd(subject_data[subject_data$Room=="Conference",]$DL_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$DL_AfterRank)), "mean", toString(mean(subject_data[subject_data$Room=="Conference",]$DL_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$DL_AfterRank)), "median", toString(median(subject_data[subject_data$Room=="Conference",]$DL_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$DL_AfterRank)), "std", toString(sd(subject_data[subject_data$Room=="Conference",]$DL_AfterRank))), sep=" , ")
print("================================================")
print("Office Subjects")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$DL_BeforeRank)), "mean", toString(mean(subject_data[subject_data$Room=="Office",]$DL_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$DL_BeforeRank)), "median", toString(median(subject_data[subject_data$Room=="Office",]$DL_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$DL_BeforeRank)), "std", toString(sd(subject_data[subject_data$Room=="Office",]$DL_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$DL_AfterRank)), "mean", toString(mean(subject_data[subject_data$Room=="Office",]$DL_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$DL_AfterRank)), "median", toString(median(subject_data[subject_data$Room=="Office",]$DL_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$DL_AfterRank)), "std", toString(sd(subject_data[subject_data$Room=="Office",]$DL_AfterRank))), sep=" , ")
print("================================================")

print("###### GLARE RANKS ######")
print("All Subjects")
print(paste(deparse(substitute(subject_data$G_BeforeRank)), "mean", toString(mean(subject_data$G_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$G_BeforeRank)), "median", toString(median(subject_data$G_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$G_BeforeRank)), "std", toString(sd(subject_data$G_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$G_AfterRank)), "mean", toString(mean(subject_data$G_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$G_AfterRank)), "median", toString(median(subject_data$G_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$G_AfterRank)), "std", toString(sd(subject_data$G_AfterRank))), sep=" , ")
print("================================================")
print("Conf Subjects")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$G_BeforeRank)), "mean", toString(mean(subject_data[subject_data$Room=="Conference",]$G_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$G_BeforeRank)), "median", toString(median(subject_data[subject_data$Room=="Conference",]$G_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$G_BeforeRank)), "std", toString(sd(subject_data[subject_data$Room=="Conference",]$G_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$G_AfterRank)), "mean", toString(mean(subject_data[subject_data$Room=="Conference",]$G_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$G_AfterRank)), "median", toString(median(subject_data[subject_data$Room=="Conference",]$G_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$G_AfterRank)), "std", toString(sd(subject_data[subject_data$Room=="Conference",]$G_AfterRank))), sep=" , ")
print("================================================")
print("Office Subjects")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$G_BeforeRank)), "mean", toString(mean(subject_data[subject_data$Room=="Office",]$G_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$G_BeforeRank)), "median", toString(median(subject_data[subject_data$Room=="Office",]$G_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$G_BeforeRank)), "std", toString(sd(subject_data[subject_data$Room=="Office",]$G_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$G_AfterRank)), "mean", toString(mean(subject_data[subject_data$Room=="Office",]$G_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$G_AfterRank)), "median", toString(median(subject_data[subject_data$Room=="Office",]$G_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$G_AfterRank)), "std", toString(sd(subject_data[subject_data$Room=="Office",]$G_AfterRank))), sep=" , ")
print("================================================")

print("###### VIEW FACTOR RANKS ######")
print("All Subjects")
print(paste(deparse(substitute(subject_data$VF_BeforeRank)), "mean", toString(mean(subject_data$VF_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$VF_BeforeRank)), "median", toString(median(subject_data$VF_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$VF_BeforeRank)), "std", toString(sd(subject_data$VF_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$VF_AfterRank)), "mean", toString(mean(subject_data$VF_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$VF_AfterRank)), "median", toString(median(subject_data$VF_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data$VF_AfterRank)), "std", toString(sd(subject_data$VF_AfterRank))), sep=" , ")
print("================================================")
print("Conf Subjects")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$VF_BeforeRank)), "mean", toString(mean(subject_data[subject_data$Room=="Conference",]$VF_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$VF_BeforeRank)), "median", toString(median(subject_data[subject_data$Room=="Conference",]$VF_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$VF_BeforeRank)), "std", toString(sd(subject_data[subject_data$Room=="Conference",]$VF_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$VF_AfterRank)), "mean", toString(mean(subject_data[subject_data$Room=="Conference",]$VF_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$VF_AfterRank)), "median", toString(median(subject_data[subject_data$Room=="Conference",]$VF_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Conference",]$VF_AfterRank)), "std", toString(sd(subject_data[subject_data$Room=="Conference",]$VF_AfterRank))), sep=" , ")
print("================================================")
print("Office Subjects")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$VF_BeforeRank)), "mean", toString(mean(subject_data[subject_data$Room=="Office",]$VF_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$VF_BeforeRank)), "median", toString(median(subject_data[subject_data$Room=="Office",]$VF_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$VF_BeforeRank)), "std", toString(sd(subject_data[subject_data$Room=="Office",]$VF_BeforeRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$VF_AfterRank)), "mean", toString(mean(subject_data[subject_data$Room=="Office",]$VF_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$VF_AfterRank)), "median", toString(median(subject_data[subject_data$Room=="Office",]$VF_AfterRank))), sep=" , ")
print(paste(deparse(substitute(subject_data[subject_data$Room=="Office",]$VF_AfterRank)), "std", toString(sd(subject_data[subject_data$Room=="Office",]$VF_AfterRank))), sep=" , ")
print("================================================")

print("###### CHOICE TIME RANKS ######")

## SPLIT INTO ROOMS
# print("Conference - Best")
# print(paste("Optimal Mean", toString(mean(choice_best_conf_data_summed[choice_best_conf_data_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Optimal Median", toString(median(choice_best_conf_data_summed[choice_best_conf_data_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Optimal STD", toString(sd(choice_best_conf_data_summed[choice_best_conf_data_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Poor Mean", toString(mean(choice_best_conf_data_summed[choice_best_conf_data_summed$Optimal=="Poor",]$Time))), sep=" - ")
# print(paste("Poor Median", toString(median(choice_best_conf_data_summed[choice_best_conf_data_summed$Optimal=="Poor",]$Time))), sep=" - ")
# print(paste("Poor STD", toString(sd(choice_best_conf_data_summed[choice_best_conf_data_summed$Optimal=="Poor",]$Time))), sep=" - ")
#
# print("Conference - Worst")
# print(paste("Optimal Mean", toString(mean(choice_worst_conf_data_summed[choice_worst_conf_data_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Optimal Median", toString(median(choice_worst_conf_data_summed[choice_worst_conf_data_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Optimal STD", toString(sd(choice_worst_conf_data_summed[choice_worst_conf_data_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Poor Mean", toString(mean(choice_worst_conf_data_summed[choice_worst_conf_data_summed$Optimal=="Poor",]$Time))), sep=" - ")
# print(paste("Poor Median", toString(median(choice_worst_conf_data_summed[choice_worst_conf_data_summed$Optimal=="Poor",]$Time))), sep=" - ")
# print(paste("Poor STD", toString(sd(choice_worst_conf_data_summed[choice_worst_conf_data_summed$Optimal=="Poor",]$Time))), sep=" - ")
#
# print("Office - Best")
# print(paste("Optimal Mean", toString(mean(choice_best_office_data_summed[choice_best_office_data_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Optimal Median", toString(median(choice_best_office_data_summed[choice_best_office_data_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Optimal STD", toString(sd(choice_best_office_data_summed[choice_best_office_data_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Poor Mean", toString(mean(choice_best_office_data_summed[choice_best_office_data_summed$Optimal=="Poor",]$Time))), sep=" - ")
# print(paste("Poor Median", toString(median(choice_best_office_data_summed[choice_best_office_data_summed$Optimal=="Poor",]$Time))), sep=" - ")
# print(paste("Poor STD", toString(sd(choice_best_office_data_summed[choice_best_office_data_summed$Optimal=="Poor",]$Time))), sep=" - ")
#
# print("Office - Worst")
# print(paste("Optimal Mean", toString(mean(choice_worst_office_summed[choice_worst_office_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Optimal Median", toString(median(choice_worst_office_summed[choice_worst_office_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Optimal STD", toString(sd(choice_worst_office_summed[choice_worst_office_summed$Optimal=="Optimal",]$Time))), sep=" - ")
# print(paste("Poor Mean", toString(mean(choice_worst_office_summed[choice_worst_office_summed$Optimal=="Poor",]$Time))), sep=" - ")
# print(paste("Poor Median", toString(median(choice_worst_office_summed[choice_worst_office_summed$Optimal=="Poor",]$Time))), sep=" - ")
# print(paste("Poor STD", toString(sd(choice_worst_office_summed[choice_worst_office_summed$Optimal=="Poor",]$Time))), sep=" - ")

## NOT SPLIT INTO ROOMS
print("Best Subject Times")
print(paste("Optimal Mean", toString(mean(choice_bestworst_data[choice_bestworst_data$Optimal=="Optimal" & choice_bestworst_data$Best=="Best Performers",]$Time))), sep=" - ")
print(paste("Optimal Median", toString(median(choice_bestworst_data[choice_bestworst_data$Optimal=="Optimal" & choice_bestworst_data$Best=="Best Performers",]$Time))), sep=" - ")
print(paste("Optimal STD", toString(sd(choice_bestworst_data[choice_bestworst_data$Optimal=="Optimal" & choice_bestworst_data$Best=="Best Performers",]$Time))), sep=" - ")
print(paste("Poor Mean", toString(mean(choice_bestworst_data[choice_bestworst_data$Optimal=="Poor" & choice_bestworst_data$Best=="Best Performers",]$Time))), sep=" - ")
print(paste("Poor Median", toString(median(choice_bestworst_data[choice_bestworst_data$Optimal=="Poor" & choice_bestworst_data$Best=="Best Performers",]$Time))), sep=" - ")
print(paste("Poor STD", toString(sd(choice_bestworst_data[choice_bestworst_data$Optimal=="Poor" & choice_bestworst_data$Best=="Best Performers",]$Time))), sep=" - ")

print("Worst Subject Times")
print(paste("Optimal Mean", toString(mean(choice_bestworst_data[choice_bestworst_data$Optimal=="Optimal" & choice_bestworst_data$Best=="Worst Performers",]$Time))), sep=" - ")
print(paste("Optimal Median", toString(median(choice_bestworst_data[choice_bestworst_data$Optimal=="Optimal" & choice_bestworst_data$Best=="Worst Performers",]$Time))), sep=" - ")
print(paste("Optimal STD", toString(sd(choice_bestworst_data[choice_bestworst_data$Optimal=="Optimal" & choice_bestworst_data$Best=="Worst Performers",]$Time))), sep=" - ")
print(paste("Poor Mean", toString(mean(choice_bestworst_data[choice_bestworst_data$Optimal=="Poor" & choice_bestworst_data$Best=="Worst Performers",]$Time))), sep=" - ")
print(paste("Poor Median", toString(median(choice_bestworst_data[choice_bestworst_data$Optimal=="Poor" & choice_bestworst_data$Best=="Worst Performers",]$Time))), sep=" - ")
print(paste("Poor STD", toString(sd(choice_bestworst_data[choice_bestworst_data$Optimal=="Poor" & choice_bestworst_data$Best=="Worst Performers",]$Time))), sep=" - ")

print("Most Changed Times")
print(paste("Optimal Mean", toString(mean(choice_mostleast_data[choice_mostleast_data$Optimal=="Optimal" & choice_mostleast_data$Change=="Most Changed",]$Time))), sep=" - ")
print(paste("Optimal Median", toString(median(choice_mostleast_data[choice_mostleast_data$Optimal=="Optimal" & choice_mostleast_data$Change=="Most Changed",]$Time))), sep=" - ")
print(paste("Optimal STD", toString(sd(choice_mostleast_data[choice_mostleast_data$Optimal=="Optimal" & choice_mostleast_data$Change=="Most Changed",]$Time))), sep=" - ")
print(paste("Poor Mean", toString(mean(choice_mostleast_data[choice_mostleast_data$Optimal=="Poor" & choice_mostleast_data$Change=="Most Changed",]$Time))), sep=" - ")
print(paste("Poor Median", toString(median(choice_mostleast_data[choice_mostleast_data$Optimal=="Poor" & choice_mostleast_data$Change=="Most Changed",]$Time))), sep=" - ")
print(paste("Poor STD", toString(sd(choice_mostleast_data[choice_mostleast_data$Optimal=="Poor" & choice_mostleast_data$Change=="Most Changed",]$Time))), sep=" - ")

print("Least Changed Times")
print(paste("Optimal Mean", toString(mean(choice_mostleast_data[choice_mostleast_data$Optimal=="Optimal" & choice_mostleast_data$Change=="Least Changed",]$Time))), sep=" - ")
print(paste("Optimal Median", toString(median(choice_mostleast_data[choice_mostleast_data$Optimal=="Optimal" & choice_mostleast_data$Change=="Least Changed",]$Time))), sep=" - ")
print(paste("Optimal STD", toString(sd(choice_mostleast_data[choice_mostleast_data$Optimal=="Optimal" & choice_mostleast_data$Change=="Least Changed",]$Time))), sep=" - ")
print(paste("Poor Mean", toString(mean(choice_mostleast_data[choice_mostleast_data$Optimal=="Poor" & choice_mostleast_data$Change=="Least Changed",]$Time))), sep=" - ")
print(paste("Poor Median", toString(median(choice_mostleast_data[choice_mostleast_data$Optimal=="Poor" & choice_mostleast_data$Change=="Least Changed",]$Time))), sep=" - ")
print(paste("Poor STD", toString(sd(choice_mostleast_data[choice_mostleast_data$Optimal=="Poor" & choice_mostleast_data$Change=="Least Changed",]$Time))), sep=" - ")

print("###### 4.1 ######")

subject_data$DL_Difference <- subject_data$DL_AfterRank - subject_data$DL_BeforeRank
subject_data$VF_Difference <- subject_data$VF_AfterRank - subject_data$VF_BeforeRank
subject_data$G_Difference <- subject_data$G_AfterRank - subject_data$G_BeforeRank

print("Paired T-test for before and after OVERALL rank - all subjects")
print(shapiro.test(subject_data$Difference))
print(t.test(x=subject_data$BeforeRank, y=subject_data$AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("Paired T-test for before and after DAYLIGHTING rank - all subjects")
print(shapiro.test(subject_data$DL_Difference))
print(t.test(x=subject_data$DL_BeforeRank, y=subject_data$DL_AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("Paired T-test for before and after GLARE rank - all subjects")
print(shapiro.test(subject_data$G_Difference))
print(t.test(x=subject_data$G_BeforeRank, y=subject_data$G_AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("Paired T-test for before and after VIEW FACTOR rank - all subjects")
print(shapiro.test(subject_data$VF_Difference))
print(t.test(x=subject_data$VF_BeforeRank, y=subject_data$VF_AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("Paired T-test for before and after OVERALL rank - Conference")
t_test_subjects <- subject_data[subject_data$Room=="Conference",]
print(shapiro.test(t_test_subjects$Difference))
print(t.test(x=t_test_subjects$BeforeRank, y=t_test_subjects$AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("Paired T-test for before and after OVERALL rank - Office")
t_test_subjects <- subject_data[subject_data$Room=="Office",]
print(shapiro.test(t_test_subjects$Difference))
print(t.test(x=t_test_subjects$BeforeRank, y=t_test_subjects$AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("Paired T-test for before and DAYLIGHTING rank - Conference")
t_test_subjects <- subject_data[subject_data$Room=="Conference",]
print(shapiro.test(t_test_subjects$DL_Difference))
print(t.test(x=t_test_subjects$DL_BeforeRank, y=t_test_subjects$DL_AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("Paired T-test for before and DAYLIGHTING rank - Office")
t_test_subjects <- subject_data[subject_data$Room=="Office",]
print(shapiro.test(t_test_subjects$DL_Difference))
print(t.test(x=t_test_subjects$DL_BeforeRank, y=t_test_subjects$DL_AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("Paired T-test for before and GLARE rank - Conference")
t_test_subjects <- subject_data[subject_data$Room=="Conference",]
print(shapiro.test(t_test_subjects$G_Difference))
print(t.test(x=t_test_subjects$G_BeforeRank, y=t_test_subjects$G_AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("Paired T-test for before and GLARE rank - Office")
t_test_subjects <- subject_data[subject_data$Room=="Office",]
print(shapiro.test(t_test_subjects$G_Difference))
print(t.test(x=t_test_subjects$G_BeforeRank, y=t_test_subjects$G_AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("Paired T-test for before and VIEW FACTOR rank - Conference")
t_test_subjects <- subject_data[subject_data$Room=="Conference",]
print(shapiro.test(t_test_subjects$VF_Difference))
print(t.test(x=t_test_subjects$VF_BeforeRank, y=t_test_subjects$VF_AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("Paired T-test for before and VIEW FACTOR rank - Office")
t_test_subjects <- subject_data[subject_data$Room=="Office",]
print(shapiro.test(t_test_subjects$VF_Difference))
print(t.test(x=t_test_subjects$VF_BeforeRank, y=t_test_subjects$VF_AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")



print("###### 4.2 ######")

## SPLIT INTO ROOMS
# print("Welches for TIME - Best - Office")
# optimal <- choice_best_office_data_summed[choice_best_office_data_summed$Optimal=="Optimal",]$Time
# poor <- choice_best_office_data_summed[choice_best_office_data_summed$Optimal=="Poor",]$Time
# print("Shapiro-Wilks for optimal:")
# print(shapiro.test(optimal))
# print("Shapiro-Wilks for poor:")
# print(shapiro.test(poor))
# print(t.test(x=optimal, y=poor, paired = FALSE, alternative = "two.sided", var.equal=FALSE))
# print("================================================")
#
# print("Welches for TIME - Worst - Office")
# optimal <- choice_worst_office_summed[choice_worst_office_summed$Optimal=="Optimal",]$Time
# poor <- choice_worst_office_summed[choice_worst_office_summed$Optimal=="Poor",]$Time
# print("Shapiro-Wilks for optimal:")
# print(shapiro.test(optimal))
# print("Shapiro-Wilks for poor:")
# print(shapiro.test(poor))
# print(t.test(x=optimal, y=poor, paired = FALSE, alternative = "two.sided", var.equal=FALSE))
# print("================================================")
#
# print("Welches for TIME - Best - Conf")
# optimal <- choice_best_conf_data_summed[choice_best_conf_data_summed$Optimal=="Optimal",]$Time
# poor <- choice_best_conf_data_summed[choice_best_conf_data_summed$Optimal=="Poor",]$Time
# print("Shapiro-Wilks for optimal:")
# print(shapiro.test(optimal))
# print("Shapiro-Wilks for poor:")
# print(shapiro.test(poor))
# print(t.test(x=optimal, y=poor, paired = FALSE, alternative = "two.sided", var.equal=FALSE))
# print("================================================")
#
# print("Welches for TIME - Worst - Conf")
# optimal <- choice_worst_conf_data_summed[choice_worst_conf_data_summed$Optimal=="Optimal",]$Time
# poor <- choice_worst_conf_data_summed[choice_worst_conf_data_summed$Optimal=="Poor",]$Time
# print("Shapiro-Wilks for optimal:")
# print(shapiro.test(optimal))
# print("Shapiro-Wilks for poor:")
# print(shapiro.test(poor))
# print(t.test(x=optimal, y=poor, paired = FALSE, alternative = "two.sided", var.equal=FALSE))
# print("================================================")

## NOT SPLIT INTO ROOMS
print("T test for TIME - Best")
# Can run T-test since there are equal number optimal and non-optimal facades. Variance and normality hold
optimal_times <- choice_bestworst_data[choice_bestworst_data$Optimal=="Optimal" & choice_bestworst_data$Best=="Best Performers",]$Time
poor_times <- choice_bestworst_data[choice_bestworst_data$Optimal=="Poor" & choice_bestworst_data$Best=="Best Performers",]$Time
print(shapiro.test(optimal_times - poor_times))
print(var.test(optimal_times, poor_times))
print(t.test(x=optimal_times, y=poor_times, paired = FALSE, alternative = "two.sided", var.equal=TRUE))
print("================================================")

print("T test for TIME - Worst")
# Can run T-test since there are equal number optimal and non-optimal facades. Variance and normality hold
optimal_times <- choice_bestworst_data[choice_bestworst_data$Optimal=="Optimal" & choice_bestworst_data$Best=="Worst Performers",]$Time
poor_times <- choice_bestworst_data[choice_bestworst_data$Optimal=="Poor" & choice_bestworst_data$Best=="Worst Performers",]$Time
print(shapiro.test(optimal_times - poor_times))
print(var.test(optimal_times, poor_times))
print(t.test(x=optimal_times, y=poor_times, paired = FALSE, alternative = "two.sided", var.equal=TRUE))
print("================================================")

print("T test for rank - Best")
t_test_subjects <- subject_bestworst_data[subject_bestworst_data$Best=="Best Performers",]
print(shapiro.test(t_test_subjects$Difference))
print(t.test(x=t_test_subjects$BeforeRank, y=t_test_subjects$AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

print("T test for rank - Worst")
t_test_subjects <- subject_bestworst_data[subject_bestworst_data$Best=="Worst Performers",]
print(shapiro.test(t_test_subjects$Difference))
print(t.test(x=t_test_subjects$BeforeRank, y=t_test_subjects$AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")

##### VVVVVVVVVVVVVVVVVVVVVVV Wilcox tests, probably not needed actually ########################

# print("Paired two sided Wilcox for before and after overall rank - Conference")
# print("Before normality")
# print(shapiro.test(subject_data[subject_data$Room=="Conference",]$BeforeRank))
# print("After normality")
# print(shapiro.test(subject_data[subject_data$Room=="Conference",]$AfterRank))
# #print("The test assumes that differences between paired samples should be distributed symmetrically around the median.") # https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#signed-rank-test-on-paired-samples
# #print(gghistogram(subject_data[subject_data$Room=="Conference",], x = "Difference", y = "..density..", fill = "steelblue",bins = 5, add_density = TRUE))
# print(wilcox.test(x=subject_data[subject_data$Room=="Conference",]$BeforeRank,
#                   y=subject_data[subject_data$Room=="Conference",]$AfterRank,
#                   paired = TRUE,
#                   alternative = "two.sided",
#                   conf.int = TRUE))
# print("================================================")
#
#
# print("Paired two sided Wilcox for before and after overall rank - Office")
# print("Before normality")
# print(shapiro.test(subject_data[subject_data$Room=="Office",]$BeforeRank))
# print("After normality")
# print(shapiro.test(subject_data[subject_data$Room=="Office",]$AfterRank))
# #print("The test assumes that differences between paired samples should be distributed symmetrically around the median.") # https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#signed-rank-test-on-paired-samples
# #print(gghistogram(subject_data[subject_data$Room=="Office",], x = "Difference", y = "..density..", fill = "steelblue",bins = 5, add_density = TRUE))
# print(wilcox.test(x=subject_data[subject_data$Room=="Office",]$BeforeRank,
#                   y=subject_data[subject_data$Room=="Office",]$AfterRank,
#                   paired = TRUE,
#                   alternative = "two.sided",
#                   conf.int = TRUE))
# print("================================================")
#
# subject_data$DL_Difference <- subject_data$DL_AfterRank - subject_data$DL_BeforeRank
# print("Paired two sided Wilcox for before and after Daylight rank - Conference")
# print("Before normality")
# print(shapiro.test(subject_data[subject_data$Room=="Conference",]$DL_BeforeRank))
# print("After normality")
# print(shapiro.test(subject_data[subject_data$Room=="Conference",]$DL_AfterRank))
#
# print("The test assumes that differences between paired samples should be distributed symmetrically around the median.") # https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#signed-rank-test-on-paired-samples
# #print(gghistogram(subject_data[subject_data$Room=="Conference",], x = "DL_Difference", y = "..density..", fill = "steelblue",bins = 5, add_density = TRUE))
# print(wilcox.test(x=subject_data[subject_data$Room=="Conference",]$DL_BeforeRank,
#                   y=subject_data[subject_data$Room=="Conference",]$DL_AfterRank,
#                   paired = TRUE,
#                   alternative = "two.sided",
#                   conf.int = TRUE))
# print("================================================")
#
# subject_data$DL_Difference <- subject_data$DL_AfterRank - subject_data$DL_BeforeRank
# print("Paired two sided Wilcox for before and after Daylight rank - Office")
# print("Before normality")
# print(shapiro.test(subject_data[subject_data$Room=="Office",]$DL_BeforeRank))
# print("After normality")
# print(shapiro.test(subject_data[subject_data$Room=="Office",]$DL_AfterRank))
#
# print("The test assumes that differences between paired samples should be distributed symmetrically around the median.") # https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#signed-rank-test-on-paired-samples
# #print(gghistogram(subject_data[subject_data$Room=="Office",], x = "DL_Difference", y = "..density..", fill = "steelblue",bins = 5, add_density = TRUE))
# print(wilcox.test(x=subject_data[subject_data$Room=="Office",]$DL_BeforeRank,
#                   y=subject_data[subject_data$Room=="Office",]$DL_AfterRank,
#                   paired = TRUE,
#                   alternative = "two.sided",
#                   conf.int = TRUE))
# print("================================================")
#
# subject_data$G_Difference <- subject_data$G_AfterRank - subject_data$G_BeforeRank
# print("Paired two sided Wilcox for before and after Glare rank - Conference")
# print("Before normality")
# print(shapiro.test(subject_data[subject_data$Room=="Conference",]$G_BeforeRank))
# print("After normality")
# print(shapiro.test(subject_data[subject_data$Room=="Conference",]$G_AfterRank))
#
# print("The test assumes that differences between paired samples should be distributed symmetrically around the median.") # https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#signed-rank-test-on-paired-samples
# print(gghistogram(subject_data[subject_data$Room=="Conference",], x = "G_Difference", y = "..density..", fill = "steelblue",bins = 5, add_density = TRUE))
# print(wilcox.test(x=subject_data[subject_data$Room=="Conference",]$G_BeforeRank,
#                   y=subject_data[subject_data$Room=="Conference",]$G_AfterRank,
#                   paired = TRUE,
#                   alternative = "two.sided",
#                   conf.int = TRUE))
# print("================================================")
#
# subject_data$G_Difference <- subject_data$G_AfterRank - subject_data$G_BeforeRank
# print("Paired two sided Wilcox for before and after Glare rank - Office")
# print("Before normality")
# print(shapiro.test(subject_data[subject_data$Room=="Office",]$G_BeforeRank))
# print("After normality")
# print(shapiro.test(subject_data[subject_data$Room=="Office",]$G_AfterRank))
#
# print("The test assumes that differences between paired samples should be distributed symmetrically around the median.") # https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#signed-rank-test-on-paired-samples
# #print(gghistogram(subject_data[subject_data$Room=="Office",], x = "G_Difference", y = "..density..", fill = "steelblue",bins = 5, add_density = TRUE))
# print(wilcox.test(x=subject_data[subject_data$Room=="Office",]$G_BeforeRank,
#                   y=subject_data[subject_data$Room=="Office",]$G_AfterRank,
#                   paired = TRUE,
#                   alternative = "two.sided",
#                   conf.int = TRUE))
# print("================================================")
#
# subject_data$VF_Difference <- subject_data$VF_AfterRank - subject_data$VF_BeforeRank
# print("Paired two sided Wilcox for before and after View Factor rank - Conference")
# print("Before normality")
# print(shapiro.test(subject_data[subject_data$Room=="Conference",]$VF_BeforeRank))
# print("After normality")
# print(shapiro.test(subject_data[subject_data$Room=="Conference",]$VF_AfterRank))
#
# print("The test assumes that differences between paired samples should be distributed symmetrically around the median.") # https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#signed-rank-test-on-paired-samples
# #print(gghistogram(subject_data[subject_data$Room=="Conference",], x = "VF_Difference", y = "..density..", fill = "steelblue",bins = 5, add_density = TRUE))
# print(wilcox.test(x=subject_data[subject_data$Room=="Conference",]$VF_BeforeRank,
#                   y=subject_data[subject_data$Room=="Conference",]$VF_AfterRank,
#                   paired = TRUE,
#                   alternative = "two.sided",
#                   conf.int = TRUE))
# print("================================================")
#
# subject_data$VF_Difference <- subject_data$VF_AfterRank - subject_data$VF_BeforeRank
# print("Paired two sided Wilcox for before and after View Factor rank - Office")
# print("Before normality")
# print(shapiro.test(subject_data[subject_data$Room=="Office",]$VF_BeforeRank))
# print("After normality")
# print(shapiro.test(subject_data[subject_data$Room=="Office",]$VF_AfterRank))
#
# print("The test assumes that differences between paired samples should be distributed symmetrically around the median.") # https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#signed-rank-test-on-paired-samples
# print(gghistogram(subject_data[subject_data$Room=="Office",], x = "VF_Difference", y = "..density..", fill = "steelblue",bins = 5, add_density = TRUE))
# print(wilcox.test(x=subject_data[subject_data$Room=="Office",]$VF_BeforeRank,
#                   y=subject_data[subject_data$Room=="Office",]$VF_AfterRank,
#                   paired = TRUE,
#                   alternative = "two.sided",
#                   conf.int = TRUE))
# print("================================================")

print("###### 4.3 ######")

print("T test for TIME - Most Changed")
# Can run T-test since there are equal number optimal and non-optimal facades. Variance and normality hold
optimal_times <- choice_mostleast_data[choice_mostleast_data$Optimal=="Optimal" & choice_mostleast_data$Change=="Most Changed",]$Time
poor_times <- choice_mostleast_data[choice_mostleast_data$Optimal=="Poor" & choice_mostleast_data$Change=="Most Changed",]$Time
print(shapiro.test(optimal_times - poor_times))
print(var.test(optimal_times, poor_times))
print(t.test(x=optimal_times, y=poor_times, paired = FALSE, alternative = "two.sided", var.equal=TRUE))
print("================================================")

print("T test for TIME - Least Changed")
# Can run T-test since there are equal number optimal and non-optimal facades. Variance and normality hold
optimal_times <- choice_mostleast_data[choice_mostleast_data$Optimal=="Optimal" & choice_mostleast_data$Change=="Least Changed",]$Time
poor_times <- choice_mostleast_data[choice_mostleast_data$Optimal=="Poor" & choice_mostleast_data$Change=="Least Changed",]$Time
print(shapiro.test(optimal_times - poor_times))
print(var.test(optimal_times, poor_times))
print(t.test(x=optimal_times, y=poor_times, paired = FALSE, alternative = "two.sided", var.equal=TRUE))
print("================================================")

# print("T test for rank - Most Changed")
# t_test_subjects <- subject_most_change_data
# print(shapiro.test(t_test_subjects$Difference))
# print(t.test(x=t_test_subjects$BeforeRank, y=t_test_subjects$AfterRank, paired = TRUE, alternative = "two.sided"))
print("================================================")








### LAST MINUTE QUALTRICS STUFF
# Qualtrics data
qualtrics_data <- read.csv("../Subjects/Qualtrics_3_7_22.csv",
                           header=TRUE,
                           sep=",")
qualtrics_data <- qualtrics_data[-c(1, 2), ] # Remove alt-header
qualtrics_data <- select(qualtrics_data, c(72, 73, 74, 75, 76, 77, 78))
names(qualtrics_data)[1] <- "Q2"
names(qualtrics_data)[2] <- "Q3"
names(qualtrics_data)[3] <- "Q4"
names(qualtrics_data)[4] <- "Q5"
names(qualtrics_data)[5] <- "Q6"
names(qualtrics_data)[6] <- "Q7"
names(qualtrics_data)[7] <- "Q8"

qualtrics_data$Q2 <- as.numeric(qualtrics_data$Q2)
qualtrics_data$Q3 <- as.numeric(qualtrics_data$Q3)
qualtrics_data$Q4 <- as.numeric(qualtrics_data$Q4)
qualtrics_data$Q5 <- as.numeric(qualtrics_data$Q5)
qualtrics_data$Q6 <- as.numeric(qualtrics_data$Q6)
qualtrics_data$Q7 <- as.numeric(qualtrics_data$Q7)

print("====== Q2 ========")
print(paste("Mean", toString(mean(qualtrics_data$Q2, na.rm=TRUE))), sep=" - ")
print(paste("STD", toString(sd(qualtrics_data$Q2, na.rm=TRUE))), sep=" - ")

print("====== Q3 ========")
print(paste("Mean", toString(mean(qualtrics_data$Q3, na.rm=TRUE))), sep=" - ")
print(paste("STD", toString(sd(qualtrics_data$Q3, na.rm=TRUE))), sep=" - ")

print("====== Q4 ========")
print(paste("Mean", toString(mean(qualtrics_data$Q4, na.rm=TRUE))), sep=" - ")
print(paste("STD", toString(sd(qualtrics_data$Q4, na.rm=TRUE))), sep=" - ")

print("====== Q5 ========")
print(paste("Mean", toString(mean(qualtrics_data$Q5, na.rm=TRUE))), sep=" - ")
print(paste("STD", toString(sd(qualtrics_data$Q5, na.rm=TRUE))), sep=" - ")

print("====== Q6 ========")
print(paste("Mean", toString(mean(qualtrics_data$Q6, na.rm=TRUE))), sep=" - ")
print(paste("STD", toString(sd(qualtrics_data$Q6, na.rm=TRUE))), sep=" - ")

print("====== Q7 ========")
print(paste("Mean", toString(mean(qualtrics_data$Q7, na.rm=TRUE))), sep=" - ")
print(paste("STD", toString(sd(qualtrics_data$Q7, na.rm=TRUE))), sep=" - ")
