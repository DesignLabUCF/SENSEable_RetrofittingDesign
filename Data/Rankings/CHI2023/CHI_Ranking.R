library(dplyr)
library(KraljicMatrix) # Pareto
library(stringr)
library(ggplot2)
library(reshape2)
library(pastecs) # For summary stats
#library(ggpubr) # For Shapiro-Wilks test


##############################################
### Read in subject data
##############################################
subjects_log <- read.csv("../../Subjects/Log.csv",
                         skip=1,
                         header=TRUE,
                         sep=",")[1:13]
# Drop unneeded columns
subjects_log <- select(subjects_log, Participant.ID, Condition, Room, Facade.Choice, Gender, EndTime)
subjects_log$Facade.Choice <- toupper(subjects_log$Facade.Choice) # Capitalize for easier matching
# Rename final selection column
colnames(subjects_log)[colnames(subjects_log) == 'Facade.Choice'] <- 'After'

##############################################
### Read in Python-generated movement data
##############################################
movement_data <- read.csv("../../Subjects/SubjectMovement.csv",
                          skip=0,
                          header=TRUE,
                          sep=",")
movement_data <- subset(movement_data, select=-c(1))

##############################################
### Read in Qualtrics data
##############################################
qualtrics_data <- read.csv("../../Subjects/Qualtrics.csv",
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
qualtrics_data <- subset(qualtrics_data, select=-c(X1.6, sub))
# Rename columns and values to match subject log
colnames(qualtrics_data)[colnames(qualtrics_data) == 'X1'] <- 'Participant.ID'
colnames(qualtrics_data)[colnames(qualtrics_data) == 'X4'] <- 'Condition'
qualtrics_data$Condition[qualtrics_data$Condition == "Augmented Reality"] <- "AR"
qualtrics_data$Condition[qualtrics_data$Condition == "Desktop PC"] <- "Desktop"
colnames(qualtrics_data)[colnames(qualtrics_data) == 'X5'] <- 'Room'
qualtrics_data$Room[qualtrics_data$Room == "Conference Room"] <- "Conference"

##############################################
### Merge read in data, and clean it up
##############################################
subjects_log <- merge(subjects_log, qualtrics_data, by=c("Participant.ID", "Condition", "Room"))
subjects_log <- merge(subjects_log, movement_data, by=c("Participant.ID"))

##############################################
### Read in facade data
##############################################
facade_rankings <- read.csv("FacadeRankings.csv",
                            skip=1,
                            header=TRUE,
                            sep=",")[1:14]
facade_rankings <- na.omit(facade_rankings)
# Drop the position column, ended up unused
facade_rankings <- subset(facade_rankings, select=-c(Position))
# Factorize/Cast data 
#facade_rankings$Facade <- as.factor(facade_rankings$Facade)
#facade_rankings$Room <- as.factor(facade_rankings$Room)
#facade_rankings$Time <- as.factor(facade_rankings$Time)
facade_rankings$VF_Thru <- as.numeric(facade_rankings$VF_Thru)
facade_rankings$VF_Blocked <- as.numeric(facade_rankings$VF_Blocked)
facade_rankings$VF_Percentage <-as.numeric(facade_rankings$VF_Percentage)
facade_rankings$G_Within <- as.numeric(facade_rankings$G_Within)
facade_rankings$G_Total <- as.numeric(facade_rankings$G_Total)
facade_rankings$G_Percentage <- as.numeric(facade_rankings$G_Percentage)
facade_rankings$DL_Within <- as.numeric(facade_rankings$DL_Within)
facade_rankings$DL_Total <- as.numeric(facade_rankings$DL_Total)
facade_rankings$DL_Percentage <- as.numeric(facade_rankings$DL_Percentage)
# Average the rankings at different times
facade_rankings <- aggregate(cbind(facade_rankings$VF_Percentage, facade_rankings$DL_Percentage, facade_rankings$G_Percentage), by=list(facade_rankings$Facade, facade_rankings$Room), FUN=mean, na.rm=TRUE)
colnames(facade_rankings)[colnames(facade_rankings) == 'Group.1'] <- 'Facade'
colnames(facade_rankings)[colnames(facade_rankings) == 'Group.2'] <- 'Room'
colnames(facade_rankings)[colnames(facade_rankings) == 'V1'] <- 'VF_Percentage'
colnames(facade_rankings)[colnames(facade_rankings) == 'V2'] <- 'DL_Percentage'
colnames(facade_rankings)[colnames(facade_rankings) == 'V3'] <- 'G_Percentage'
# Rename columns and values to match subject log
facade_rankings$Room[facade_rankings$Room == "Conf"] <- "Conference"
facade_rankings <- mutate(facade_rankings, Facade = str_replace(Facade, "1_", "dynamic_"))
facade_rankings <- mutate(facade_rankings, Facade = str_replace(Facade, "louver_10_75", "louver_10_70"))
facade_rankings <- mutate(facade_rankings, Facade = str_replace(Facade, "louver_20_75", "louver_20_70"))
facade_rankings$Facade <- toupper(facade_rankings$Facade)

##############################################
### Pareto
##############################################
pareto_conf <- get_frontier(subset(facade_rankings, Room=="Conference"), VF_Percentage, G_Percentage, quadrant="bottom.right", decreasing = FALSE)
pareto_office <- get_frontier(subset(facade_rankings, Room=="Office"), VF_Percentage, G_Percentage, quadrant="bottom.right", decreasing = FALSE)
# Sort
pareto_conf <- pareto_conf[order(pareto_conf$VF_Percentage, decreasing=TRUE),]
pareto_office <- pareto_office[order(pareto_office$VF_Percentage, decreasing=TRUE),]
# Score the facades by comparing their distance to the pareto line
# https://stackoverflow.com/questions/35194048/using-r-how-to-calculate-the-distance-from-one-point-to-a-line
# dist2d(a2,b2,c2) # distance of point a from line (b,c) in 2D (a2 <- c(0,2)) 
dist2d <- function(point, line_point_a, line_point_b) {
  v1 <- line_point_a - line_point_b
  v2 <- point - line_point_a
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
  return(d)
}
# Calculate distance to Pareto for each facade
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
facade_rankings <- rbind(add_rating_columns(subset(facade_rankings, Room=="Conference"), pareto_conf),
                         add_rating_columns(subset(facade_rankings, Room=="Office"), pareto_office))
# Calculate distance to optimal point
distance_to_optimal_point <- function(x, y, optimal_x, optimal_y)
{
  return(sqrt((optimal_x - x)^2 + (optimal_y - y)^2))
}
facade_rankings$FromOptimal <- distance_to_optimal_point(facade_rankings$VF_Percentage, facade_rankings$G_Percentage, 1.0, 0.0)
# Write to external file
write.csv(facade_rankings,"FacadeOptimalData.csv", row.names=TRUE)

##############################################
### Update subject log with facade ranking data
##############################################
matchForRoom <- function(subjects, rankings, var, isBefore) { # subject log, facade rankings, rankings variable, "Before"/"After"
  # init empty column
  col <- rep(NA, nrow(subjects))
  # iterate and fill column with matching values
  for(row in 1:nrow(subjects)){
    # Get room rankings
    subject_facade <- subjects[row,isBefore]
    subject_room <- subjects[row,"Room"]
    room_rankings <- subset(rankings, Room == subject_room)
    # Find the facade ranking
    for(f in 1:nrow(room_rankings)){
      if(room_rankings[f, "Facade"] == subject_facade){
        col[row] <- room_rankings[f, var]
        break
      }
    }
  }
  return(col)
}
subjects_log$Optimal <- matchForRoom(subjects_log, facade_rankings, "Optimal", "After")
subjects_log$BeforeRank <- matchForRoom(subjects_log, facade_rankings, "Rank", "Before")
subjects_log$AfterRank <- matchForRoom(subjects_log, facade_rankings, "Rank", "After")
subjects_log$VF_BeforeRank <- matchForRoom(subjects_log, facade_rankings, "VF_Rank", "Before")
subjects_log$VF_AfterRank <- matchForRoom(subjects_log, facade_rankings, "VF_Rank", "After")
subjects_log$DL_BeforeRank <- matchForRoom(subjects_log, facade_rankings, "DL_Rank", "Before")
subjects_log$DL_AfterRank <- matchForRoom(subjects_log, facade_rankings, "DL_Rank", "After")
subjects_log$G_BeforeRank <- matchForRoom(subjects_log, facade_rankings, "G_Rank", "Before")
subjects_log$G_AfterRank <- matchForRoom(subjects_log, facade_rankings, "G_Rank", "After")
subjects_log$Difference <- subjects_log$AfterRank - subjects_log$BeforeRank
subjects_log$DL_Difference <- subjects_log$DL_AfterRank - subjects_log$DL_BeforeRank
subjects_log$VF_Difference <- subjects_log$VF_AfterRank - subjects_log$VF_BeforeRank
subjects_log$G_Difference <- subjects_log$G_AfterRank - subjects_log$G_BeforeRank

##############################################
### Classify subjects by movement
##############################################
## Set classification parameters
high_mover_cutoff <- 8 # Number of participants
super_high_mover_cutoff <- 2 # Number of participants
# Raw movement rank of entire subject pool
subjects_log <- subjects_log[order(subjects_log$Movement.Ratio, decreasing=TRUE),]
subjects_log$RawMovementRank <- 1:nrow(subjects_log)
# Reorder by ID
subjects_log <- subjects_log[order(subjects_log$Participant.ID, decreasing=FALSE),]
# Rank subjects by room assigned
conf_subjects <- subjects_log[subjects_log$Room=="Conference", ][order(subjects_log[subjects_log$Room=="Conference", ]$RawMovementRank, decreasing=FALSE),]
conf_subjects$RoomMovementRank <- 1:nrow(conf_subjects)
office_subjects <- subjects_log[subjects_log$Room=="Office", ][order(subjects_log[subjects_log$Room=="Office", ]$RawMovementRank, decreasing=FALSE),]
office_subjects$RoomMovementRank <- 1:nrow(office_subjects)
movement_appended_subjects <- rbind(conf_subjects, office_subjects)
subjects_log <- merge(subjects_log, movement_appended_subjects)
# Clean up environment
rm(conf_subjects)
rm(office_subjects)
rm(movement_appended_subjects)
# Rank subjects by augmentation assigned
ar_subjects <- subjects_log[subjects_log$Condition=="AR", ][order(subjects_log[subjects_log$Condition=="AR", ]$RawMovementRank, decreasing=FALSE),]
ar_subjects$AugmentationMovementRank <- 1:nrow(ar_subjects)

desktop_subjects <- subjects_log[subjects_log$Condition=="Desktop", ][order(subjects_log[subjects_log$Condition=="Desktop", ]$RawMovementRank, decreasing=FALSE),]
desktop_subjects$AugmentationMovementRank <- 1:nrow(desktop_subjects)

movement_appended_subjects <- rbind(ar_subjects, desktop_subjects)
subjects_log <- merge(subjects_log, movement_appended_subjects)
# Clean up environment
rm(ar_subjects)
rm(desktop_subjects)
rm(movement_appended_subjects)
# Rank subjects by overall condition
ar_conf_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference", ][order(subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference", ]$RawMovementRank, decreasing=FALSE),]
ar_conf_subjects$ConditionMovementRank <- 1:nrow(ar_conf_subjects)
ar_conf_subjects$HighMover <- 0
ar_conf_subjects$HighMover[1:high_mover_cutoff] <- 1
ar_conf_subjects$SuperHighMover <- 0
ar_conf_subjects$SuperHighMover[1:super_high_mover_cutoff] <- 1
ar_conf_subjects$LowMover <- 0
ar_conf_subjects$LowMover[nrow(ar_conf_subjects): (nrow(ar_conf_subjects) - (high_mover_cutoff - 1))] <- 1
ar_conf_subjects$SuperLowMover <- 0
ar_conf_subjects$SuperLowMover[nrow(ar_conf_subjects): (nrow(ar_conf_subjects) - (super_high_mover_cutoff - 1))] <- 1
ar_office_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office", ][order(subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office", ]$RawMovementRank, decreasing=FALSE),]
ar_office_subjects$ConditionMovementRank <- 1:nrow(ar_office_subjects)
ar_office_subjects$HighMover <- 0
ar_office_subjects$HighMover[1:high_mover_cutoff] <- 1
ar_office_subjects$SuperHighMover <- 0
ar_office_subjects$SuperHighMover[1:super_high_mover_cutoff] <- 1
ar_office_subjects$LowMover <- 0
ar_office_subjects$LowMover[nrow(ar_office_subjects): (nrow(ar_office_subjects) - (high_mover_cutoff - 1))] <- 1
ar_office_subjects$SuperLowMover <- 0
ar_office_subjects$SuperLowMover[nrow(ar_office_subjects): (nrow(ar_office_subjects) - (super_high_mover_cutoff - 1))] <- 1
desktop_conf_subjects <- subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Conference", ][order(subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Conference", ]$RawMovementRank, decreasing=FALSE),]
desktop_conf_subjects$ConditionMovementRank <- 1:nrow(desktop_conf_subjects)
desktop_conf_subjects$HighMover <- 0
desktop_conf_subjects$HighMover[1:high_mover_cutoff] <- 1
desktop_conf_subjects$SuperHighMover <- 0
desktop_conf_subjects$SuperHighMover[1:super_high_mover_cutoff] <- 1
desktop_conf_subjects$LowMover <- 0
desktop_conf_subjects$LowMover[nrow(desktop_conf_subjects): (nrow(desktop_conf_subjects) - (high_mover_cutoff - 1))] <- 1
desktop_conf_subjects$SuperLowMover <- 0
desktop_conf_subjects$SuperLowMover[nrow(desktop_conf_subjects): (nrow(desktop_conf_subjects) - (super_high_mover_cutoff - 1))] <- 1
desktop_office_subjects <- subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Office", ][order(subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Office", ]$RawMovementRank, decreasing=FALSE),]
desktop_office_subjects$ConditionMovementRank <- 1:nrow(desktop_office_subjects)
desktop_office_subjects$HighMover <- 0
desktop_office_subjects$HighMover[1:high_mover_cutoff] <- 1
desktop_office_subjects$SuperHighMover <- 0
desktop_office_subjects$SuperHighMover[1:super_high_mover_cutoff] <- 1
desktop_office_subjects$LowMover <- 0
desktop_office_subjects$LowMover[nrow(desktop_office_subjects): (nrow(desktop_office_subjects) - (high_mover_cutoff - 1))] <- 1
desktop_office_subjects$SuperLowMover <- 0
desktop_office_subjects$SuperLowMover[nrow(desktop_office_subjects): (nrow(desktop_office_subjects) - (super_high_mover_cutoff - 1))] <- 1
movement_appended_subjects <- rbind(ar_conf_subjects, ar_office_subjects, desktop_conf_subjects, desktop_office_subjects)
subjects_log <- merge(subjects_log, movement_appended_subjects)
# Clean up environment
rm(ar_conf_subjects)
rm(ar_office_subjects)
rm(desktop_conf_subjects)
rm(desktop_office_subjects)
rm(movement_appended_subjects)
# Final classification
classify_movement <- function(subjects) {
  # Init empty col
  col <- rep(NA, nrow(subjects))
  # iterate and fill column 
  for(row in 1:nrow(subjects)){
    class <- ""
    if(subjects$HighMover[row] == 1){
      class <- "High"
    }
    if(subjects$SuperHighMover[row] == 1){
      class <- "Super High"
    }
    if(subjects$LowMover[row] == 1){
      class <- "Low"
    }
    if(subjects$SuperLowMover[row] == 1){
      class <- "Super Low"
    }
    if(class == ""){
      class <- "Medium"
    }
    col[row] <- class 
  }
  return(col)
}
subjects_log$BodyMovementClass <- classify_movement(subjects_log)

##############################################
### Classify subjects by rotation
##############################################
## Set classification parameters
high_head_cutoff <- 8 # Number of participants
super_high_head_cutoff <- 2 # Number of participants
# Raw movement rank of entire subject pool
subjects_log <- subjects_log[order(subjects_log$Head.Ratio, decreasing=TRUE),]
subjects_log$RawRotationRank <- 1:nrow(subjects_log)
# Rank subjects by room assigned
conf_subjects <- subjects_log[subjects_log$Room=="Conference", ][order(subjects_log[subjects_log$Room=="Conference", ]$RawRotationRank, decreasing=FALSE),]
conf_subjects$RoomRotationRank <- 1:nrow(conf_subjects)
office_subjects <- subjects_log[subjects_log$Room=="Office", ][order(subjects_log[subjects_log$Room=="Office", ]$RawRotationRank, decreasing=FALSE),]
office_subjects$RoomRotationRank <- 1:nrow(office_subjects)
movement_appended_subjects <- rbind(conf_subjects, office_subjects)
subjects_log <- merge(subjects_log, movement_appended_subjects)
# Clean up environment
rm(conf_subjects)
rm(office_subjects)
rm(movement_appended_subjects)
# Rank subjects by overall condition
ar_conf_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference", ][order(subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference", ]$RawRotationRank, decreasing=FALSE),]
ar_conf_subjects$ConditionRotationtRank <- 1:nrow(ar_conf_subjects)
ar_conf_subjects$HighRotator <- 0
ar_conf_subjects$HighRotator[1:high_head_cutoff] <- 1
ar_conf_subjects$SuperHighRotator <- 0
ar_conf_subjects$SuperHighRotator[1:super_high_head_cutoff] <- 1
ar_conf_subjects$LowRotator <- 0
ar_conf_subjects$LowRotator[nrow(ar_conf_subjects): (nrow(ar_conf_subjects) - (high_head_cutoff - 1))] <- 1
ar_conf_subjects$SuperLowRotator <- 0
ar_conf_subjects$SuperLowRotator[nrow(ar_conf_subjects): (nrow(ar_conf_subjects) - (super_high_head_cutoff - 1))] <- 1
ar_office_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office", ][order(subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office", ]$RawRotationRank, decreasing=FALSE),]
ar_office_subjects$ConditionRotationtRank <- 1:nrow(ar_office_subjects)
ar_office_subjects$HighRotator <- 0
ar_office_subjects$HighRotator[1:high_head_cutoff] <- 1
ar_office_subjects$SuperHighRotator <- 0
ar_office_subjects$SuperHighRotator[1:super_high_head_cutoff] <- 1
ar_office_subjects$LowRotator <- 0
ar_office_subjects$LowRotator[nrow(ar_office_subjects): (nrow(ar_office_subjects) - (high_head_cutoff - 1))] <- 1
ar_office_subjects$SuperLowRotator <- 0
ar_office_subjects$SuperLowRotator[nrow(ar_office_subjects): (nrow(ar_office_subjects) - (super_high_head_cutoff - 1))] <- 1
desktop_conf_subjects <- subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Conference", ][order(subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Conference", ]$RawRotationRank, decreasing=FALSE),]
desktop_conf_subjects$ConditionRotationtRank <- 1:nrow(desktop_conf_subjects)
desktop_conf_subjects$HighRotator <- 0
desktop_conf_subjects$HighRotator[1:high_head_cutoff] <- 1
desktop_conf_subjects$SuperHighRotator <- 0
desktop_conf_subjects$SuperHighRotator[1:super_high_head_cutoff] <- 1
desktop_conf_subjects$LowRotator <- 0
desktop_conf_subjects$LowRotator[nrow(desktop_conf_subjects): (nrow(desktop_conf_subjects) - (high_head_cutoff - 1))] <- 1
desktop_conf_subjects$SuperLowRotator <- 0
desktop_conf_subjects$SuperLowRotator[nrow(desktop_conf_subjects): (nrow(desktop_conf_subjects) - (super_high_head_cutoff - 1))] <- 1
desktop_office_subjects <- subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Office", ][order(subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Office", ]$RawRotationRank, decreasing=FALSE),]
desktop_office_subjects$ConditionRotationtRank <- 1:nrow(desktop_office_subjects)
desktop_office_subjects$HighRotator <- 0
desktop_office_subjects$HighRotator[1:high_head_cutoff] <- 1
desktop_office_subjects$SuperHighRotator <- 0
desktop_office_subjects$SuperHighRotator[1:super_high_head_cutoff] <- 1
desktop_office_subjects$LowRotator <- 0
desktop_office_subjects$LowRotator[nrow(desktop_office_subjects): (nrow(desktop_office_subjects) - (high_head_cutoff - 1))] <- 1
desktop_office_subjects$SuperLowRotator <- 0
desktop_office_subjects$SuperLowRotator[nrow(desktop_office_subjects): (nrow(desktop_office_subjects) - (super_high_head_cutoff - 1))] <- 1
movement_appended_subjects <- rbind(ar_conf_subjects, ar_office_subjects, desktop_conf_subjects, desktop_office_subjects)
subjects_log <- merge(subjects_log, movement_appended_subjects)
# Clean up environment
rm(ar_conf_subjects)
rm(ar_office_subjects)
rm(desktop_conf_subjects)
rm(desktop_office_subjects)
rm(movement_appended_subjects)
# Final classification
classify_rotation <- function(subjects) {
  # Init empty col
  col <- rep(NA, nrow(subjects))
  # iterate and fill column 
  for(row in 1:nrow(subjects)){
    class <- ""
    if(subjects$HighRotator[row] == 1){
      class <- "High"
    }
    if(subjects$SuperHighRotator[row] == 1){
      class <- "Super High"
    }
    if(subjects$LowRotator[row] == 1){
      class <- "Low"
    }
    if(subjects$SuperLowRotator[row] == 1){
      class <- "Super Low"
    }
    if(class == ""){
      class <- "Medium"
    }
    col[row] <- class 
  }
  return(col)
}
subjects_log$RotationClass <- classify_rotation(subjects_log)

##############################################
### Classify subjects by rotation
##############################################
## Set classification parameters
high_gaze_cutoff <- 8 # Number of participants
super_high_gaze_cutoff <- 2 # Number of participants
# Raw movement rank of entire subject pool
subjects_log <- subjects_log[order(subjects_log$Gaze.Ratio, decreasing=TRUE),]
subjects_log$RawGazeRank <- 1:nrow(subjects_log)
# Rank subjects by room assigned
conf_subjects <- subjects_log[subjects_log$Room=="Conference", ][order(subjects_log[subjects_log$Room=="Conference", ]$RawGazeRank, decreasing=FALSE),]
conf_subjects$RoomGazeRank <- 1:nrow(conf_subjects)
office_subjects <- subjects_log[subjects_log$Room=="Office", ][order(subjects_log[subjects_log$Room=="Office", ]$RawGazeRank, decreasing=FALSE),]
office_subjects$RoomGazeRank <- 1:nrow(office_subjects)
movement_appended_subjects <- rbind(conf_subjects, office_subjects)
subjects_log <- merge(subjects_log, movement_appended_subjects)
# Clean up environment
rm(conf_subjects)
rm(office_subjects)
rm(movement_appended_subjects)
# Rank subjects by overall condition
ar_conf_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference", ][order(subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference", ]$RawGazeRank, decreasing=FALSE),]
ar_conf_subjects$ConditionGazeRank <- 1:nrow(ar_conf_subjects)
ar_conf_subjects$HighGazer <- 0
ar_conf_subjects$HighGazer[1:high_gaze_cutoff] <- 1
ar_conf_subjects$SuperHighGazer <- 0
ar_conf_subjects$SuperHighGazer[1:super_high_gaze_cutoff] <- 1
ar_conf_subjects$LowGazer <- 0
ar_conf_subjects$LowGazer[nrow(ar_conf_subjects): (nrow(ar_conf_subjects) - (high_gaze_cutoff - 1))] <- 1
ar_conf_subjects$SuperLowGazer <- 0
ar_conf_subjects$SuperLowGazer[nrow(ar_conf_subjects): (nrow(ar_conf_subjects) - (super_high_head_cutoff - 1))] <- 1
ar_office_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office", ][order(subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office", ]$RawGazeRank, decreasing=FALSE),]
ar_office_subjects$ConditionGazeRank <- 1:nrow(ar_office_subjects)
ar_office_subjects$HighGazer <- 0
ar_office_subjects$HighGazer[1:high_gaze_cutoff] <- 1
ar_office_subjects$SuperHighGazer <- 0
ar_office_subjects$SuperHighGazer[1:super_high_gaze_cutoff] <- 1
ar_office_subjects$LowGazer <- 0
ar_office_subjects$LowGazer[nrow(ar_office_subjects): (nrow(ar_office_subjects) - (high_gaze_cutoff - 1))] <- 1
ar_office_subjects$SuperLowGazer <- 0
ar_office_subjects$SuperLowGazer[nrow(ar_office_subjects): (nrow(ar_office_subjects) - (super_high_head_cutoff - 1))] <- 1
desktop_conf_subjects <- subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Conference", ][order(subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Conference", ]$RawGazeRank, decreasing=FALSE),]
desktop_conf_subjects$ConditionGazeRank <- 1:nrow(desktop_conf_subjects)
desktop_conf_subjects$HighGazer <- 0
desktop_conf_subjects$HighGazer[1:high_gaze_cutoff] <- 1
desktop_conf_subjects$SuperHighGazer <- 0
desktop_conf_subjects$SuperHighGazer[1:super_high_gaze_cutoff] <- 1
desktop_conf_subjects$LowGazer <- 0
desktop_conf_subjects$LowGazer[nrow(desktop_conf_subjects): (nrow(desktop_conf_subjects) - (high_gaze_cutoff - 1))] <- 1
desktop_conf_subjects$SuperLowGazer <- 0
desktop_conf_subjects$SuperLowGazer[nrow(desktop_conf_subjects): (nrow(desktop_conf_subjects) - (super_high_head_cutoff - 1))] <- 1
desktop_office_subjects <- subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Office", ][order(subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Office", ]$RawGazeRank, decreasing=FALSE),]
desktop_office_subjects$ConditionGazeRank <- 1:nrow(desktop_office_subjects)
desktop_office_subjects$HighGazer <- 0
desktop_office_subjects$HighGazer[1:high_gaze_cutoff] <- 1
desktop_office_subjects$SuperHighGazer <- 0
desktop_office_subjects$SuperHighGazer[1:super_high_gaze_cutoff] <- 1
desktop_office_subjects$LowGazer <- 0
desktop_office_subjects$LowGazer[nrow(desktop_office_subjects): (nrow(desktop_office_subjects) - (high_gaze_cutoff - 1))] <- 1
desktop_office_subjects$SuperLowGazer <- 0
desktop_office_subjects$SuperLowGazer[nrow(desktop_office_subjects): (nrow(desktop_office_subjects) - (super_high_head_cutoff - 1))] <- 1
movement_appended_subjects <- rbind(ar_conf_subjects, ar_office_subjects, desktop_conf_subjects, desktop_office_subjects)
subjects_log <- merge(subjects_log, movement_appended_subjects)
# Clean up environment
rm(ar_conf_subjects)
rm(ar_office_subjects)
rm(desktop_conf_subjects)
rm(desktop_office_subjects)
rm(movement_appended_subjects)
# Final classification
classify_gaze <- function(subjects) {
  # Init empty col
  col <- rep(NA, nrow(subjects))
  # iterate and fill column 
  for(row in 1:nrow(subjects)){
    class <- ""
    if(subjects$HighGazer[row] == 1){
      class <- "High"
    }
    if(subjects$SuperHighGazer[row] == 1){
      class <- "Super High"
    }
    if(subjects$LowGazer[row] == 1){
      class <- "Low"
    }
    if(subjects$SuperLowGazer[row] == 1){
      class <- "Super Low"
    }
    if(class == ""){
      class <- "Medium"
    }
    col[row] <- class 
  }
  return(col)
}
subjects_log$GazeClass <- classify_gaze(subjects_log)

##############################################
### Classify subjects by performance
##############################################
## Set classification parameters
super_high_performers_cutoff <- 2 # Facade ranking
# Sort and classify
subjects_log <- subjects_log[order(subjects_log$AfterRank, decreasing=FALSE),]
# High and low
subjects_log$HighPerformer <- subjects_log$Optimal # If optimal selection, then yes
subjects_log$LowPerformer <- 1 - subjects_log$Optimal # If not optimal selection, then yes
# Super high and super low
subjects_log$SuperHighPerformer <- ifelse(subjects_log$AfterRank <= super_high_performers_cutoff, 1, 0) # Top X subjects
subjects_log$SuperLowPerformer <- ifelse(subjects_log$AfterRank >= ((nrow(facade_rankings) / 2) - (super_high_performers_cutoff - 1)), 1, 0) # Bottom X subjects
# Final classification
classify_performance <- function(subjects) {
  # Init empty col
  col <- rep(NA, nrow(subjects))
  # iterate and fill column 
  for(row in 1:nrow(subjects)){
    class <- ""
    if(subjects$HighPerformer[row] == 1){
      class <- "High"
    }
    if(subjects$SuperHighPerformer[row] == 1){
      class <- "Super High"
    }
    if(subjects$LowPerformer[row] == 1){
      class <- "Low"
    }
    if(subjects$SuperLowPerformer[row] == 1){
      class <- "Super Low"
    }
    if(class == ""){
      class <- "Medium"
    }
    col[row] <- class 
  }
  return(col)
}
subjects_log$PerformanceClass <- classify_performance(subjects_log)

##############################################
### Print statistics
##############################################
print("All Subjects")
print(stat.desc(subjects_log))
print("==============================")

print("Conference Subjects")
print(stat.desc(subjects_log[subjects_log$Room=="Conference",]))
print("==============================")

print("Office Subjects")
print(stat.desc(subjects_log[subjects_log$Room=="Office",]))
print("==============================")

print("AR Subjects")
print(stat.desc(subjects_log[subjects_log$Condition=="AR",]))
print("==============================")

print("Desktop Subjects")
print(stat.desc(subjects_log[subjects_log$Condition=="Desktop",]))
print("==============================")

print("Conference - Desktop Subjects")
print(stat.desc(subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Conference",]))
print("==============================")

print("Conference - AR Subjects")
print(stat.desc(subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference",]))
print("==============================")

print("Office - Desktop Subjects")
print(stat.desc(subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Office",]))
print("==============================")

print("Office - AR Subjects")
print(stat.desc(subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office",]))
print("==============================")

print("High Performers")
print(stat.desc(subjects_log[subjects_log$HighPerformer==1,]))
print("==============================")

print("Low Performers")
print(stat.desc(subjects_log[subjects_log$LowPerformer==1,]))
print("==============================")

##############################################
### T-tests before and after
##############################################

# Params
shapiro_alpha <- 0.05
shapiro_printout <- function(shapiro) {
  print(shapiro)
  p <- shapiro[2]
  if(p <= shapiro_alpha)
  {
    print(paste0("Assumption of normality REJECTED: P-value of ", toString(p), " <= ", toString(shapiro_alpha)))
    print("Therefore, t-test is insufficient for this.")
  }
  else
  {
    print(paste0("Assumption of normality unable to be rejected: P-value of ", toString(p), " > ", toString(shapiro_alpha)))
  }
}
t_alpha <- 0.05
t_printout <- function(t) {
  print(t)
  p <- t[3]
  if(p <= t_alpha)
  {
    print(paste0("NULL hypotheses SUCCESFULLY rejected:  ", toString(p), " <= ", toString(t_alpha)))
  }
  else
  {
    print(paste0("NULL hypotheses UNABLE to be rejected ", toString(p), " > ", toString(t_alpha)))
  }
}


print("****************************")
print("ANOVAs - Overall Rank - Between groups comparisons of final decisions")
print("****************************")

print("Between groups (Room + Augmentation)")
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=AfterRank ~ Condition + Room)
print(summary(anova_res))

# print("Between groups (Room + Augmentation + BodyMovement)")
# stat_subjects <- subjects_log
# anova_res <- aov(data=stat_subjects,
#                  formula=AfterRank ~ Condition + Room + BodyMovementClass)
# print(summary(anova_res))

# Significant
print("Check differences for total body movement between rooms for AR subjects")
stat_subjects <- subjects_log[subjects_log$Condition=="AR",]
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=Movement.Ratio ~ Room + Optimal)
print(summary(anova_res))

# Significant
print("Check differences for total head movement between rooms for AR subjects")
stat_subjects <- subjects_log[subjects_log$Condition=="AR",]
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=Head.Ratio ~ Room + Optimal)
print(summary(anova_res))

print("Check differences for total eye gaze movement between rooms for AR subjects")
stat_subjects <- subjects_log[subjects_log$Condition=="AR",]
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=Gaze.Ratio ~ Room + Optimal)
print(summary(anova_res))

# Significant
print("Check differences for body movement on optimal between rooms for AR subjects")
stat_subjects <- subjects_log[subjects_log$Condition=="AR",]
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=Optimal.Movement.Ratio ~ Room + Optimal)
print(summary(anova_res))

# Significant
print("Check differences for head movement on optimal between rooms for AR subjects")
stat_subjects <- subjects_log[subjects_log$Condition=="AR",]
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=Optimal.Head.Ratio ~ Room + Optimal)
print(summary(anova_res))

print("Check differences for eye gaze on optimal between rooms for AR subjects")
stat_subjects <- subjects_log[subjects_log$Condition=="AR",]
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=Optimal.Gaze.Ratio ~ Room + Optimal)
print(summary(anova_res))

# print("Between groups (Room + Augmentation + Head Movement)")
# stat_subjects <- subjects_log
# anova_res <- aov(data=stat_subjects,
#                  formula=AfterRank ~ Condition + Room + RotationClass)
# print(summary(anova_res))

print("****************************")
print("ANOVA - (All three factors) ~ Condition + Room")
print("****************************")

# Significant
print("Daylighting")
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=DL_AfterRank ~ Condition + Room)
print(summary(anova_res))

# Significant
print("Brightness Discomfort")
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=G_AfterRank ~ Condition + Room)
print(summary(anova_res))

# Significant
print("View Factor")
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=VF_AfterRank ~ Condition + Room)
print(summary(anova_res))

print("****************************")
print("MANOVA - (All three factors) ~ Condition + Room")
print("****************************")

stat_subjects <- subjects_log
manova_res <- manova(data=stat_subjects,
                     formula=cbind(DL_AfterRank, G_AfterRank, VF_AfterRank) ~ Condition + Room)
print(summary.aov(manova_res))

print("****************************")
print("ANOVA - (All three factors), Performance, Body Motion")
print("****************************")

# print("Daylighting")
# stat_subjects <- subjects_log
# anova_res <- aov(data=stat_subjects,
#                  formula=DL_AfterRank ~ Condition + Room + BodyMovementClass)
# print(summary(anova_res))

# print("Brightness Discomfort")
# stat_subjects <- subjects_log
# anova_res <- aov(data=stat_subjects,
#                  formula=G_AfterRank ~ Condition + Room + BodyMovementClass)
# print(summary(anova_res))

# print("View Factor")
# stat_subjects <- subjects_log
# anova_res <- aov(data=stat_subjects,
#                  formula=VF_AfterRank ~ Condition + Room + BodyMovementClass)
# print(summary(anova_res))

print("****************************")
print("ANOVA - (All three factors), Performance, Head Motion")
print("****************************")

# print("Daylighting")
# stat_subjects <- subjects_log
# anova_res <- aov(data=stat_subjects,
#                  formula=DL_AfterRank ~ Condition + Room + RotationClass)
# print(summary(anova_res))

# print("Brightness Discomfort")
# stat_subjects <- subjects_log
# anova_res <- aov(data=stat_subjects,
#                  formula=G_AfterRank ~ Condition + Room + RotationClass)
# print(summary(anova_res))

# print("View Factor")
# stat_subjects <- subjects_log
# anova_res <- aov(data=stat_subjects,
#                  formula=VF_AfterRank ~ Condition + Room + RotationClass)
# print(summary(anova_res))

print("****************************")
print("ANOVA - (All three factors), Performance, EyeGaze")
print("****************************")

# Significant
print("Daylighting - DL_AfterRank ~ Condition + Room + GazeClass")
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=DL_AfterRank ~ Condition + Room + GazeClass)
print(summary(anova_res))

# Significant
print("Brightness Discomfort - G_AfterRank ~ Condition + Room + GazeClass")
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=G_AfterRank ~ Condition + Room + GazeClass)
print(summary(anova_res))

# Significant
print("View Factor - VF_AfterRank ~ Condition + Room + GazeClass")
stat_subjects <- subjects_log
anova_res <- aov(data=stat_subjects,
                 formula=VF_AfterRank ~ Condition + Room + GazeClass)
print(summary(anova_res))

print("****************************")
print("ANOVA - Interaction between condition and movement class")
print("****************************")

# Because our interactions plot indicated that there may be an interaction between condition and bodymovementclass, we are explore this here.

# stat_subjects <- subjects_log
# anova_res <- aov(data=stat_subjects,
#                  formula=AfterRank ~ Room + Condition + (Condition *BodyMovementClass))
# print(summary(anova_res))

print("****************************")
print("ANOVA - High Vs Low IN AR")
print("****************************")

stat_subjects <- subjects_log[subjects_log$Condition=="AR",]
#stat_subjects[stat_subjects$BodyMovementClass=="Super High",]$BodyMovementClass <- "High"
#stat_subjects[stat_subjects$BodyMovementClass=="Super Low",]$BodyMovementClass <- "Low"
anova_res <- aov(data=stat_subjects,
                formula=AfterRank ~ Room + BodyMovementClass)
print(summary(anova_res))

print("****************************")
print("Two Proportion Z-Test - Between groups comparisons of Optimality") # MIGHT ACTUALLY BE CHI SQUARE TEST SINCE ITS A 2X2
print("****************************")
# https://statstutorial.com/two-proportion-z-test-in-r-with-examples/

print("AR vs. Desktop - Combined conditions")
stat_subjects <- subjects_log 
z_res <- prop.test(x = c(nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$Condition=="AR",]),
                         nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$Condition=="Desktop",])),
                   n = c(nrow(stat_subjects[stat_subjects$Condition=="AR",]),
                         nrow(stat_subjects[stat_subjects$Condition=="Desktop",])),
                   alternative = "two.sided")
print(z_res)

print("AR vs. Desktop - Office")
stat_subjects <- subjects_log[subjects_log$Room=="Office",]
z_res <- prop.test(x = c(nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$Condition=="AR",]),
                         nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$Condition=="Desktop",])),
                   n = c(nrow(stat_subjects[stat_subjects$Condition=="AR",]),
                         nrow(stat_subjects[stat_subjects$Condition=="Desktop",])),
                   alternative = "two.sided")
print(z_res)

print("AR vs. Desktop - Conference")
stat_subjects <- subjects_log[subjects_log$Room=="Conference",]
z_res <- prop.test(x = c(nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$Condition=="AR",]),
                         nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$Condition=="Desktop",])),
                   n = c(nrow(stat_subjects[stat_subjects$Condition=="AR",]),
                         nrow(stat_subjects[stat_subjects$Condition=="Desktop",])),
                   alternative = "two.sided")
print(z_res)

print("High Movers in AR Optimality - Combined conditions")
stat_subjects <- subjects_log[subjects_log$Condition=="AR",]
z_res <- prop.test(x = c(nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="High",]) + nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="Super High",]),
                         nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="Low",]) + nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="Super Low",])),
                   n = c(nrow(stat_subjects[stat_subjects$BodyMovementClass=="High",]) + nrow(stat_subjects[stat_subjects$BodyMovementClass=="Super High",]),
                         nrow(stat_subjects[stat_subjects$BodyMovementClass=="Low",]) + nrow(stat_subjects[stat_subjects$BodyMovementClass=="Super Low",])),
                   alternative = "two.sided")
print(z_res)

print("High Movers in AR Optimality - Conference")
stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room == "Conference",]
z_res <- prop.test(x = c(nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="High",]) + nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="Super High",]),
                         nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="Low",]) + nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="Super Low",])),
                   n = c(nrow(stat_subjects[stat_subjects$BodyMovementClass=="High",]) + nrow(stat_subjects[stat_subjects$BodyMovementClass=="Super High",]),
                         nrow(stat_subjects[stat_subjects$BodyMovementClass=="Low",]) + nrow(stat_subjects[stat_subjects$BodyMovementClass=="Super Low",])),
                   alternative = "two.sided")
print(z_res)
print("NOTE: Sample size may be insufficient for this test. - John")

print("High Movers in AR Optimality - Office")
stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room == "Office",]
z_res <- prop.test(x = c(nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="High",]) + nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="Super High",]),
                         nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="Low",]) + nrow(stat_subjects[stat_subjects$Optimal==1 & stat_subjects$BodyMovementClass=="Super Low",])),
                   n = c(nrow(stat_subjects[stat_subjects$BodyMovementClass=="High",]) + nrow(stat_subjects[stat_subjects$BodyMovementClass=="Super High",]),
                         nrow(stat_subjects[stat_subjects$BodyMovementClass=="Low",]) + nrow(stat_subjects[stat_subjects$BodyMovementClass=="Super Low",])),
                   alternative = "two.sided")
print(z_res)
print("NOTE: Sample size may be insufficient for this test. - John")

print("****************************")
print("CHI-squared for optimality")
print("****************************")
# https://www.statology.org/chi-square-test-of-independence-in-r/

print("AR subjects - Conference - BodyMovementClass and Optimal decisions")
stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference",]
chi_table <- table(stat_subjects$Optimal, stat_subjects$BodyMovementClass) 
print(chi_table)
chi_test <- chisq.test(chi_table, correct=FALSE) # Yates correction may unnecessary in small sample sizes
print(chi_test)

print("AR subjects - Office - BodyMovementClass and Optimal decisions")
stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office",]
chi_table <- table(stat_subjects$Optimal, stat_subjects$BodyMovementClass) 
print(chi_table)
chi_test <- chisq.test(chi_table, correct=FALSE) # Yates correction may unnecessary in small sample sizes
print(chi_test)

print("AR subjects - Conference - RotationClass and Optimal decisions")
stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference",]
chi_table <- table(stat_subjects$Optimal, stat_subjects$RotationClass) 
print(chi_table)
chi_test <- chisq.test(chi_table, correct=FALSE) # Yates correction may unnecessary in small sample sizes
print(chi_test)

print("AR subjects - Office - RotationClass and Optimal decisions")
stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office",]
chi_table <- table(stat_subjects$Optimal, stat_subjects$RotationClass) 
print(chi_table)
chi_test <- chisq.test(chi_table, correct=FALSE) # Yates correction may unnecessary in small sample sizes
print(chi_test)

print("AR subjects - Conference - GazeClass and Optimal decisions")
stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference",]
chi_table <- table(stat_subjects$Optimal, stat_subjects$GazeClass) 
print(chi_table)
chi_test <- chisq.test(chi_table, correct=FALSE) # Yates correction may unnecessary in small sample sizes
print(chi_test)

print("AR subjects - Office - GazeClass and Optimal decisions")
stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office",]
chi_table <- table(stat_subjects$Optimal, stat_subjects$GazeClass) 
print(chi_table)
chi_test <- chisq.test(chi_table, correct=FALSE) # Yates correction may unnecessary in small sample sizes
print(chi_test)

print("****************************")
print("T-Tests - Before and Afters Ranks")
print("****************************")

# print("All subjects - Before and After")
# stat_subjects <- subjects_log
# shapiro <- shapiro.test(stat_subjects$Difference)
# shapiro_printout(shapiro)
# #print(t.test(x=stat_subjects$BeforeRank, y=stat_subjects$AfterRank, paired = TRUE, alternative = "two.sided"))
# # Shapiro shows non-normality, so wilcox test is more appropriate
# print(wilcox.test(x=stat_subjects$BeforeRank, y=stat_subjects$AfterRank, paired = TRUE, alternative = "two.sided"))
# print("===============================")
# 
# print("AR Subjects - Before and After")
# stat_subjects <- subjects_log[subjects_log$Condition=="AR",]
# shapiro <- shapiro.test(stat_subjects$Difference)
# shapiro_printout(shapiro)
# t_res <- t.test(x=stat_subjects$BeforeRank, y=stat_subjects$AfterRank, paired = TRUE, alternative = "two.sided")
# t_printout(t_res)
# print("===============================")
# 
# print("AR Subjects (Conference) - Before and After")
# stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference",]
# shapiro <- shapiro.test(stat_subjects$Difference)
# shapiro_printout(shapiro)
# t_res <- t.test(x=stat_subjects$BeforeRank, y=stat_subjects$AfterRank, paired = TRUE, alternative = "two.sided")
# t_printout(t_res)
# print("===============================")
# 
# print("AR Subjects (Office) - Before and After")
# stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office",]
# shapiro <- shapiro.test(stat_subjects$Difference)
# shapiro_printout(shapiro)
# t_res <- t.test(x=stat_subjects$BeforeRank, y=stat_subjects$AfterRank, paired = TRUE, alternative = "two.sided")
# t_printout(t_res)
# print("===============================")
# 
# print("Desktop Subjects - Before and After")
# stat_subjects <- subjects_log[subjects_log$Condition=="Desktop",]
# shapiro <- shapiro.test(stat_subjects$Difference)
# shapiro_printout(shapiro)
# t_res <- t.test(x=stat_subjects$BeforeRank, y=stat_subjects$AfterRank, paired = TRUE, alternative = "two.sided")
# t_printout(t_res)
# print("===============================")
# 
# print("Desktop Subjects (Conference) - Before and After")
# stat_subjects <- subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Conference",]
# shapiro <- shapiro.test(stat_subjects$Difference)
# shapiro_printout(shapiro)
# t_res <- t.test(x=stat_subjects$BeforeRank, y=stat_subjects$AfterRank, paired = TRUE, alternative = "two.sided")
# t_printout(t_res)
# print("===============================")
# 
# print("Desktop Subjects (Office) - Before and After")
# stat_subjects <- subjects_log[subjects_log$Condition=="Desktop" & subjects_log$Room=="Office",]
# shapiro <- shapiro.test(stat_subjects$Difference)
# shapiro_printout(shapiro)
# t_res <- t.test(x=stat_subjects$BeforeRank, y=stat_subjects$AfterRank, paired = TRUE, alternative = "two.sided")
# t_printout(t_res)
# print("===============================")
# 
# print("****************************")
# print("T-Tests - Compare Conditions Ranks")
# print("****************************")
# 
# print("All subjects - After and Afters (Covered by ANOVA already)")
# stat_subjects <- subjects_log
# shapiro <- shapiro.test(stat_subjects$Difference)
# shapiro_printout(shapiro)
# t_res <- t.test(x=stat_subjects[stat_subjects$Condition=="AR",]$AfterRank,
#                 y=stat_subjects[stat_subjects$Condition=="Desktop",]$AfterRank,
#                 paired = FALSE, alternative = "two.sided")
# t_printout(t_res)
# print("===============================")
# 
# print("Conference Subjects - After and Afters (Covered by ANOVA already)")
# stat_subjects <- subjects_log[subjects_log$Room=="Conference",]
# shapiro <- shapiro.test(stat_subjects$Difference)
# shapiro_printout(shapiro)
# t_res <- t.test(x=stat_subjects[stat_subjects$Condition=="AR",]$AfterRank,
#                 y=stat_subjects[stat_subjects$Condition=="Desktop",]$AfterRank,
#                 paired = FALSE, alternative = "two.sided")
# t_printout(t_res)
# print("===============================")
# 
# print("Office Subjects - After and Afters (Covered by ANOVA already)")
# stat_subjects <- subjects_log[subjects_log$Room=="Office",]
# shapiro <- shapiro.test(stat_subjects$Difference)
# shapiro_printout(shapiro)
# t_res <- t.test(x=stat_subjects[stat_subjects$Condition=="AR",]$AfterRank,
#                 y=stat_subjects[stat_subjects$Condition=="Desktop",]$AfterRank,
#                 paired = FALSE, alternative = "two.sided")
# t_printout(t_res)
# print("===============================")

print("****************************")
print("T-Tests - High Movers and Low Movers - After and Afters")
print("****************************")

print("High and Low Movers - After and Afters ")
stat_subjects <- subjects_log[subjects_log$HighMover==1 | subjects_log$LowMover==1,]
shapiro <- shapiro.test(stat_subjects$Difference)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighMover==1,]$AfterRank,
                y=stat_subjects[stat_subjects$LowMover==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("High and Low Movers (Conference) - After and Afters ")
stat_subjects <- subjects_log[(subjects_log$Room=="Conference" & (subjects_log$HighMover==1 | subjects_log$LowMover==1)),]
shapiro <- shapiro.test(stat_subjects$Difference)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighMover==1,]$AfterRank,
                y=stat_subjects[stat_subjects$LowMover==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("High and Low Movers (Conference - AR) - After and Afters ")
stat_subjects <- subjects_log[(subjects_log$Room=="Conference" & subjects_log$Condition=="AR" & (subjects_log$HighMover==1 | subjects_log$LowMover==1)),]
shapiro <- shapiro.test(stat_subjects$Difference)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighMover==1,]$AfterRank,
                y=stat_subjects[stat_subjects$LowMover==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("High and Low Movers (Conference - Desktop) - After and Afters ")
stat_subjects <- subjects_log[(subjects_log$Room=="Conference" & subjects_log$Condition=="Desktop" & (subjects_log$HighMover==1 | subjects_log$LowMover==1)),]
shapiro <- shapiro.test(stat_subjects$Difference)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighMover==1,]$AfterRank,
                y=stat_subjects[stat_subjects$LowMover==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("High and Low Movers (Office) - After and Afters ")
stat_subjects <- subjects_log[(subjects_log$Room=="Office" & (subjects_log$HighMover==1 | subjects_log$LowMover==1)),]
shapiro <- shapiro.test(stat_subjects$Difference)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighMover==1,]$AfterRank,
                y=stat_subjects[stat_subjects$LowMover==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("High and Low Movers (Office - AR) - After and Afters ")
stat_subjects <- subjects_log[(subjects_log$Room=="Office" & subjects_log$Condition=="AR" & (subjects_log$HighMover==1 | subjects_log$LowMover==1)),]
shapiro <- shapiro.test(stat_subjects$Difference)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighMover==1,]$AfterRank,
                y=stat_subjects[stat_subjects$LowMover==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("High and Low Movers (Office - Desktop) - After and Afters ")
stat_subjects <- subjects_log[(subjects_log$Room=="Office" & subjects_log$Condition=="Desktop" & (subjects_log$HighMover==1 | subjects_log$LowMover==1)),]
shapiro <- shapiro.test(stat_subjects$Difference)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighMover==1,]$AfterRank,
                y=stat_subjects[stat_subjects$LowMover==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("****************************")
print("T-Tests - Head Movement - After and Afters")
print("****************************")

print("Head Movement (Conference - AR) - After and Afters ")
stat_subjects <- subjects_log[(subjects_log$Room=="Conference" & subjects_log$Condition=="AR" & (subjects_log$HighRotator==1 | subjects_log$LowRotator==1)),]
shapiro <- shapiro.test(stat_subjects$Difference)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighRotator==1,]$AfterRank,
                y=stat_subjects[stat_subjects$LowRotator==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("Head Movement (Office - AR) - After and Afters ")
stat_subjects <- subjects_log[(subjects_log$Room=="Office" & subjects_log$Condition=="AR" & (subjects_log$HighRotator==1 | subjects_log$LowRotator==1)),]
shapiro <- shapiro.test(stat_subjects$Difference)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighRotator==1,]$AfterRank,
                y=stat_subjects[stat_subjects$LowRotator==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("****************************")
print("T-Tests - Eye Gaze - After and Afters")
print("****************************")

# Significant
print("Eye Gaze (Conference - AR) - After and Afters ")
stat_subjects <- subjects_log[(subjects_log$Room=="Conference" & subjects_log$Condition=="AR" & (subjects_log$HighGazer==1 | subjects_log$LowGazer==1)),]
shapiro <- shapiro.test(stat_subjects$Difference)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighGazer==1,]$AfterRank,
                y=stat_subjects[stat_subjects$LowGazer==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("Eye Gaze (Office - AR) - After and Afters ")
stat_subjects <- subjects_log[(subjects_log$Room=="Office" & subjects_log$Condition=="AR" & (subjects_log$HighGazer==1 | subjects_log$LowGazer==1)),]
shapiro <- shapiro.test(stat_subjects$Difference)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighGazer==1,]$AfterRank,
                y=stat_subjects[stat_subjects$LowGazer==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("****************************")
print("T-Tests - Investigating body movement on optimal facades")
print("****************************")

print("Movement Ratios - Conference - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Conference" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Movement.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Movement.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("Movement Ratios - Office - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Office" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Movement.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Movement.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("Optimal Movement Ratios - Conference - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Conference" & subjects_log$Condition=="AR",]
shapiro <- shapiro.test(stat_subjects[stat_subjects$Optimal==1,]$Optimal.Movement.Ratio - stat_subjects[stat_subjects$Optimal==0,]$Optimal.Movement.Ratio)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Optimal.Movement.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Optimal.Movement.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("Optimal Movement Ratios - Office - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Office" & subjects_log$Condition=="AR",]
shapiro <- shapiro.test(stat_subjects[stat_subjects$Optimal==1,]$Optimal.Movement.Ratio - stat_subjects[stat_subjects$Optimal==0,]$Optimal.Movement.Ratio)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Optimal.Movement.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Optimal.Movement.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("****************************")
print("T-Tests - Investigating head movement on optimal facades")
print("****************************")

print("Head Ratios - Conference - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Conference" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Head.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Head.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("Head Ratios - Office - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Office" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Head.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Head.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

# Significant
print("Optimal Head Ratios - Conference - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Conference" & subjects_log$Condition=="AR",]
shapiro <- shapiro.test(stat_subjects[stat_subjects$Optimal==0,]$Optimal.Head.Ratio - stat_subjects[stat_subjects$Optimal==1,]$Optimal.Head.Ratio)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Optimal.Head.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Optimal.Head.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("Optimal Head Ratios - Office - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Office" & subjects_log$Condition=="AR",]
shapiro <- shapiro.test(stat_subjects[stat_subjects$Optimal==0,]$Optimal.Head.Ratio - stat_subjects[stat_subjects$Optimal==1,]$Optimal.Head.Ratio)
shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Optimal.Head.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Optimal.Head.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("****************************")
print("T-Tests - Investigating eye gaze on optimal facades")
print("****************************")

print("Gaze Ratios - Conference - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Conference" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Gaze.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Gaze.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("Gaze Ratios - Office - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Office" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Gaze.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Gaze.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("Optimal Gaze Ratios - Conference - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Conference" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Optimal.Gaze.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Optimal.Gaze.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("Optimal Gaze Ratios - Office - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Office" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$Optimal==1,]$Optimal.Gaze.Ratio,
                y=stat_subjects[stat_subjects$Optimal==0,]$Optimal.Gaze.Ratio,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("****************************")
print("Explore differences between high movers and high rotators")
print("****************************")

print("ANOVA - Both rooms - AfterRank ~ Room + BodyMovementClass + RotationClass + GazeClass")
stat_subjects <- subjects_log[subjects_log$Condition=="AR",]
anova_res <- aov(data=stat_subjects,
                 formula=AfterRank ~ Room + BodyMovementClass + RotationClass + GazeClass)
print(summary(anova_res))

# Seems significant
print("ANOVA - Conference - AfterRank ~ BodyMovementClass + RotationClass + GazeClass")
stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Conference",]
anova_res <- aov(data=stat_subjects,
                 formula=AfterRank ~ BodyMovementClass + RotationClass + GazeClass)
print(summary(anova_res))

print("ANOVA - Office - AfterRank ~ BodyMovementClass + RotationClass + GazeClass")
stat_subjects <- subjects_log[subjects_log$Condition=="AR" & subjects_log$Room=="Office",]
anova_res <- aov(data=stat_subjects,
                 formula=AfterRank ~ BodyMovementClass + RotationClass + GazeClass)
print(summary(anova_res))

print("T-Test High Movers vs High Rotators in Conference - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Conference" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighMover==1,]$AfterRank,
                y=stat_subjects[stat_subjects$HighRotator==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("T-Test High Movers vs High Rotators in Office - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Office" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighMover==1,]$AfterRank,
                y=stat_subjects[stat_subjects$HighRotator==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("T-Test High Rotators vs High Gazers in Conference - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Conference" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighRotator==1,]$AfterRank,
                y=stat_subjects[stat_subjects$HighGazer==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)

print("T-Test High Rotators vs High Gazers in Office - AR")
stat_subjects <- subjects_log[subjects_log$Room=="Office" & subjects_log$Condition=="AR",]
#shapiro <- shapiro.test(stat_subjects$Difference)
#shapiro_printout(shapiro)
t_res <- t.test(x=stat_subjects[stat_subjects$HighRotator==1,]$AfterRank,
                y=stat_subjects[stat_subjects$HighGazer==1,]$AfterRank,
                paired = FALSE, alternative = "two.sided")
t_printout(t_res)


##############################################
### Interaction Plots
##############################################
# Overall guide: https://conjointly.com/kb/factorial-designs/
# How to do in R: https://rcompanion.org/handbook/G_09.html

# Factorize (just to be safe)
subjects_log$BodyMovementClass = factor(subjects_log$BodyMovementClass, levels=unique(subjects_log$BodyMovementClass))
subjects_log$RotationClass = factor(subjects_log$RotationClass, levels=unique(subjects_log$RotationClass))

plot_subjects <- subjects_log
plot_interaction <- interaction.plot(x.factor = plot_subjects$Condition,
                                     trace.factor = plot_subjects$Room,
                                     response = plot_subjects$AfterRank,
                                     fun = mean,
                                     type="b",
                                     col=c("black","red"),  ### Colors for levels of trace var.
                                     pch=c(19, 17),             ### Symbols for levels of trace var.
                                     fixed=TRUE,                    ### Order by factor order in data
                                     leg.bty = "o")

#### Body Movement ####
plot_subjects <- subjects_log
plot_interaction <- interaction.plot(x.factor = plot_subjects$Condition,
                                     trace.factor = plot_subjects$BodyMovementClass,
                                     response = plot_subjects$AfterRank,
                                     fun = mean,
                                     type="b",
                                     col=c("black","red","green", "blue", "orange"),  ### Colors for levels of trace var.
                                     pch=c(19, 17, 15, 13, 11),             ### Symbols for levels of trace var.
                                     fixed=TRUE,                    ### Order by factor order in data
                                     leg.bty = "o")
print(plot_interaction)

plot_subjects <- subjects_log[subjects_log$Room=="Conference",]
plot_interaction <- interaction.plot(x.factor = plot_subjects$Condition,
                                     trace.factor = plot_subjects$BodyMovementClass,
                                     response = plot_subjects$AfterRank,
                                     fun = mean,
                                     type="b",
                                     col=c("black","red","green", "blue", "orange"),  ### Colors for levels of trace var.
                                     pch=c(19, 17, 15, 13, 11),             ### Symbols for levels of trace var.
                                     fixed=TRUE,                    ### Order by factor order in data
                                     leg.bty = "o")
print(plot_interaction)

#### Head Movement ####
plot_subjects <- subjects_log
plot_interaction <- interaction.plot(x.factor = plot_subjects$Condition,
                                     trace.factor = plot_subjects$RotationClass,
                                     response = plot_subjects$AfterRank,
                                     fun = mean,
                                     type="b",
                                     col=c("black","red","green", "blue", "orange"),  ### Colors for levels of trace var.
                                     pch=c(19, 17, 15, 13, 11),             ### Symbols for levels of trace var.
                                     fixed=TRUE,                    ### Order by factor order in data
                                     leg.bty = "o")
print(plot_interaction)

plot_subjects <- subjects_log[subjects_log$Room=="Conference",]
plot_interaction <- interaction.plot(x.factor = plot_subjects$Condition,
                                     trace.factor = plot_subjects$RotationClass,
                                     response = plot_subjects$AfterRank,
                                     fun = mean,
                                     type="b",
                                     col=c("black","red","green", "blue", "orange"),  ### Colors for levels of trace var.
                                     pch=c(19, 17, 15, 13, 11),             ### Symbols for levels of trace var.
                                     fixed=TRUE,                    ### Order by factor order in data
                                     leg.bty = "o")
print(plot_interaction)


##############################################
### Mean Bar Graph
##############################################
# Group and summarize
plot_subjects <- subjects_log
plot_subjects <- plot_subjects %>%
  group_by(Condition, Room, BodyMovementClass) %>%
  summarise_at(vars(AfterRank), list(MeanRank = mean, STD = sd)) # Should try optimal as well
# Reorder Movement Class
plot_subjects$BodyMovementClass <- factor(plot_subjects$BodyMovementClass, levels=c('Super Low', 'Low', 'Medium', 'High', 'Super High'))

# # Format data
# subject_log_melted <- melt(subjects_log, id=c("Room", "Condition", "BodyMovementClass"))
# subject_log_melted <- subset(subject_log_melted, variable=="AfterRank")
# # Cast
# subject_log_melted$value <- as.numeric(subject_log_melted$value)
# subject_log_melted$BodyMovementClass <- factor(subject_log_melted$BodyMovementClass)

# Plot
plot_mean_rank_bar <- ggplot(plot_subjects, aes(x=BodyMovementClass, y=MeanRank, fill=BodyMovementClass)) +
  geom_bar(stat="identity", alpha=0.7, color="black") +
  geom_errorbar( aes(ymin=MeanRank-STD, ymax=MeanRank+STD), width=0.4, colour="black", alpha=0.9, size=0.5) + 
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  #scale_y_reverse(lim=rev(c(1, 16)),
  #                breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_brewer(palette = "Spectral") + 
  labs(
    title="Mean Bar Graph ",
    x="",
    y="Facade Ranking") +
  #theme_classic() +
  theme_bw() #+
print(plot_mean_rank_bar)


##############################################
### Mean Bar Graph (Simplified)
##############################################
# Format data
subject_log_melted <- melt(subjects_log, id=c("Room", "Condition", "HighMover", "LowMover"))
subject_log_melted <- subset(subject_log_melted, variable=="VF_AfterRank" | variable=="G_AfterRank" | variable=="DL_AfterRank")
# Converge mover colums to a single one and remove all 'medium' movers
subject_log_melted <- subject_log_melted[!(subject_log_melted$HighMover==0 & subject_log_melted$LowMover == 0),] # Remove medium workers
subject_log_melted <- subset(subject_log_melted, select = -c(LowMover)) # Only high and low movers are left, so remove low movers column since it is now implied
# Cast
subject_log_melted$value <- as.numeric(subject_log_melted$value)
subject_log_melted$HighMover <- factor(subject_log_melted$HighMover)
# Group and summarize
plot_subjects <- subjects_log
plot_subjects <- plot_subjects %>%
  group_by(Condition, Room, HighMover) %>%
  summarise_at(vars(AfterRank), list(MeanRank = mean, STD = sd)) # Should try optimal as well
plot_subjects$HighMover <- factor(plot_subjects$HighMover)
# Set params
low_mover_color <- "#E1BE6A"
high_mover_color <- "#40B0A6"
# Plot
plot_mean_rank_bar_simplified <- ggplot(plot_subjects, aes(x=HighMover, y=MeanRank, fill=HighMover)) +
  geom_bar(stat="identity", alpha=0.7, color="black") +
  geom_errorbar( aes(ymin=MeanRank-STD, ymax=MeanRank+STD), width=0.4, colour="black", alpha=0.9, size=0.5) + 
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  #scale_y_reverse(lim=rev(c(1, 16)),
  #                breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  #scale_fill_manual(values=c(low_mover_color,
  #                           high_mover_color)) +
  labs(
    title="Mean Bar Graph (Simplified)",
    x="",
    y="Facade Ranking") +
  #theme_classic() +
  theme_bw() #+
print(plot_mean_rank_bar_simplified)

##############################################
### Pareto Plots
##############################################
# Set params
optimal_color <- "#005AB5"
poor_color <- "#DC3220"
alt_color <- "#FFC107"
optimal_color_scale <- c(poor_color, optimal_color)
# Create plots
plot_pareto_conf <- ggplot(facade_rankings[facade_rankings$Room=="Conference",], aes(x=VF_Percentage, y=G_Percentage)) +
  #annotate("text",x=0.25, y=0.275, label="Less Optimal", size=7, angle=0, fontface = "bold") +
  #annotate("text",x=0.75, y=0.125, label="More Optimal", size=7, angle=0, fontface = "bold") +
  #annotate("text",x=0.56, y=0.18, label="Most Optimal Facade Choices Available", size=6, angle=-30, fontface = "italic") +
  ## DATA (LINE OF BEST FIT)
  stat_smooth(data=pareto_conf,
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
print(plot_pareto_conf)

plot_pareto_office <- ggplot(facade_rankings[facade_rankings$Room=="Office",], aes(x=VF_Percentage, y=G_Percentage)) +
  #annotate("text",x=0.25, y=0.275, label="Less Optimal", size=7, angle=0, fontface = "bold") +
  #annotate("text",x=0.75, y=0.125, label="More Optimal", size=7, angle=0, fontface = "bold") +
  #annotate("text",x=0.56, y=0.18, label="Most Optimal Facade Choices Available", size=6, angle=-30, fontface = "italic") +
  ## DATA (LINE OF BEST FIT)
  stat_smooth(data=pareto_office,
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
  labs(title="Office Room",
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
print(plot_pareto_office)

##############################################
### High Performers Tilemap
##############################################
# Create dataframe for plot (Should probably update to use movement classifications but low priority I guess - 9/7/22)
high_performers <- subjects_log[subjects_log$HighPerformer==1,]
super_high_performers <- subjects_log[subjects_log$SuperHighPerformer==1,]
low_performers <- subjects_log[subjects_log$LowPerformer==1,]
super_low_performers <- subjects_log[subjects_log$SuperLowPerformer==1,]
tilemap_data <- data.frame("Room" = c("Conference", "Conference", "Office", "Office"),
                              "Augmentation" = c("AR", "Desktop", "AR", "Desktop"),
                              #"Count" = c(nrow(high_performers[high_performers$Room=="Conference"&high_performers$Condition=="AR",]), 2, 3, 4))
                              "HighPerformers" = c(
                                nrow(high_performers[high_performers$Room=="Conference"&high_performers$Condition=="AR",]),
                                nrow(high_performers[high_performers$Room=="Conference"&high_performers$Condition=="Desktop",]),
                                nrow(high_performers[high_performers$Room=="Office"&high_performers$Condition=="AR",]),
                                nrow(high_performers[high_performers$Room=="Office"&high_performers$Condition=="Desktop",])),
                              "SuperHighPerformers" = c(
                                nrow(super_high_performers[super_high_performers$Room=="Conference"&super_high_performers$Condition=="AR",]),
                                nrow(super_high_performers[super_high_performers$Room=="Conference"&super_high_performers$Condition=="Desktop",]),
                                nrow(super_high_performers[super_high_performers$Room=="Office"&super_high_performers$Condition=="AR",]),
                                nrow(super_high_performers[super_high_performers$Room=="Office"&super_high_performers$Condition=="Desktop",])),
                              "LowPerformers" = c(
                                nrow(low_performers[low_performers$Room=="Conference"&low_performers$Condition=="AR",]),
                                nrow(low_performers[low_performers$Room=="Conference"&low_performers$Condition=="Desktop",]),
                                nrow(low_performers[low_performers$Room=="Office"&low_performers$Condition=="AR",]),
                                nrow(low_performers[low_performers$Room=="Office"&low_performers$Condition=="Desktop",])),
                              "SuperLowPerformers" = c(
                                nrow(super_low_performers[super_low_performers$Room=="Conference"&super_low_performers$Condition=="AR",]),
                                nrow(super_low_performers[super_low_performers$Room=="Conference"&super_low_performers$Condition=="Desktop",]),
                                nrow(super_low_performers[super_low_performers$Room=="Office"&super_low_performers$Condition=="AR",]),
                                nrow(super_low_performers[super_low_performers$Room=="Office"&super_low_performers$Condition=="Desktop",]))
                              )
# Plot
high_performers_heatmap_plot <- ggplot(tilemap_data, aes(Room, Augmentation)) +
  geom_tile(aes(fill = HighPerformers), color="black") + 
  geom_text(aes(label = paste0("Super High - ", SuperHighPerformers)), nudge_y = .2) + 
  geom_text(aes(label = paste0("High - ", HighPerformers)), nudge_y = .1) +
  geom_text(aes(label = paste0("Low - ", LowPerformers)), nudge_y = -.1) + 
  geom_text(aes(label = paste0("Super Low - ", SuperLowPerformers)), nudge_y = -.2) +
  scale_fill_gradient(low = "white", high = "lightgreen") + 
  labs(title="High performers",
       fill="Participants Who Make Optimal Selections") +
  theme_classic()
print(high_performers_heatmap_plot)
# Clean up enviornment
rm(high_performers)
rm(super_high_performers)
rm(low_performers)
rm(super_low_performers)

##############################################
### Movement Tilemap
##############################################

# TODO

##############################################
### Before/After box plots for overall
##############################################
# Format data
subject_log_melted <- melt(subjects_log, id=c("Room", "Condition"))
subject_log_melted <- subset(subject_log_melted, variable=="BeforeRank" | variable=="AfterRank")
subject_log_melted$value <- as.numeric(subject_log_melted$value)
subject_log_melted$Before <- grepl("Before", subject_log_melted$variable, fixed=TRUE) # Set Before column for coloring
# Set params
before_color <- "#785EF0"
after_color <- "#FE6100"
# Plot
overall_ranks_plot <- ggplot(subject_log_melted, aes(x=variable, y=value, fill=Before)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=0.5,
               position="dodge") +
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  scale_x_discrete(labels=c("BeforeRank" = "Blind\nSelection",
                            "AfterRank" = " Post-Experiment\nSelection")) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(before_color,
                             after_color)) +
  labs(
    title="All ",
    x="",
    y="Facade Overall Rank") +
  #theme_classic() +
  theme_bw() #+
  #coord_flip()
plot(overall_ranks_plot)

##############################################
### After/After box plots for overall
##############################################
# Format data
subject_log_melted <- melt(subjects_log, id=c("Room", "Condition"))
subject_log_melted <- subset(subject_log_melted, variable=="AfterRank")
subject_log_melted$value <- as.numeric(subject_log_melted$value)
#subject_log_melted$Before <- grepl("Before", subject_log_melted$variable, fixed=TRUE) # Set Before column for coloring
# Set params 
ar_color <- "#1AFF1A"
desktop_color <- "#4B0092"
# Plot
overall_after_ranks_plot <- ggplot(subject_log_melted, aes(x=variable, y=value, fill=Condition)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=1,
               position="dodge") +
  geom_hline(yintercept=8, size=0.75) +
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  #scale_x_discrete(labels=c("BeforeRank" = "Blind\nSelection",
  #                          "AfterRank" = " Post-Experiment\nSelection")) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(ar_color,
                             desktop_color)) +
  labs(
    title="All Participants Facade Selection Overall Ranking",
    x="",
    y="Facade Overall Rank (Lower = More Optimal)") +
  #theme_classic() +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size=12, face = "bold", hjust = 0.5),
        axis.title.y=element_text(size=12, face = "bold"),
        axis.title.x=element_text(size=16, face = "bold"),
        #axis.text.x=element_text(size=10),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=14))
#coord_flip()
plot(overall_after_ranks_plot)

##############################################
### Before/After box plots for individual categories
##############################################
# Format data
subject_log_melted <- melt(subjects_log, id=c("Room", "Condition"))
subject_log_melted <- subset(subject_log_melted, variable=="VF_BeforeRank" | variable=="G_BeforeRank" | variable=="DL_BeforeRank" | variable=="VF_AfterRank" | variable=="G_AfterRank" | variable=="DL_AfterRank")
subject_log_melted$value <- as.numeric(subject_log_melted$value)
subject_log_melted$Before <- grepl("Before", subject_log_melted$variable, fixed=TRUE) # Set Before column for coloring
# Set params
vf_color <- "#009E73"
g_color <- "#CC79A7"
dl_color <- "#F0E442"
# Plot
all_ranks_plot <- ggplot(subject_log_melted, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=0.5,
               position="dodge") +
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  scale_x_discrete(labels=c("VF_BeforeRank" = "VF\nB",
                            "VF_AfterRank" = "VF\nA",
                            "G_BeforeRank" = "G\nB",
                            "G_AfterRank" = "G\nA",
                            "DL_BeforeRank" = "DL\nB",
                            "DL_AfterRank" = "DL\nA")) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(vf_color,
                             vf_color,
                             g_color,
                             g_color,
                             dl_color,
                             dl_color)) +
  labs(
    title="All ",
    x="",
    y="Facade Ranking") +
  #theme_classic() +
  theme_bw() #+
#coord_flip()
plot(all_ranks_plot)

##############################################
### After/After box plots for individual categories
##############################################
# Format data
subject_log_melted <- melt(subjects_log, id=c("Room", "Condition"))
subject_log_melted <- subset(subject_log_melted, variable=="VF_AfterRank" | variable=="G_AfterRank" | variable=="DL_AfterRank")
subject_log_melted$value <- as.numeric(subject_log_melted$value)
# Set params
vf_color <- "#009E73"
g_color <- "#CC79A7"
dl_color <- "#F0E442"
# Plot
all_ranks_after_plot <- ggplot(subject_log_melted, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=1,
               position="dodge") +
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  scale_x_discrete(labels=c("G_AfterRank" = "Brightness\nDiscomfort",
                            "VF_AfterRank" = "View\nFactor",
                            "DL_AfterRank" = "Daylighting")) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(vf_color,
                             g_color,
                             dl_color)) +
  labs(
    title="All Participants Facade Selection Variable Ranking",
    x="",
    y="Facade Ranking") +
  #theme_classic() +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size=12, face = "bold", hjust = 0.5),
        axis.title.y=element_text(size=12, face = "bold"),
        axis.title.x=element_text(size=16, face = "bold"),
        axis.text.x=element_text(size=11, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y=element_text(size=14))
#coord_flip()
plot(all_ranks_after_plot)

##############################################
### After/After box plots for individual categories (High vs low movers)
##############################################
# Format data
subject_log_melted <- melt(subjects_log, id=c("Room", "Condition", "HighMover", "LowMover"))
subject_log_melted <- subset(subject_log_melted, variable=="VF_AfterRank" | variable=="G_AfterRank" | variable=="DL_AfterRank")
# Converge mover colums to a single one and remove all 'medium' movers
subject_log_melted <- subject_log_melted[!(subject_log_melted$HighMover==0 & subject_log_melted$LowMover == 0),] # Remove medium workers
subject_log_melted <- subset(subject_log_melted, select = -c(LowMover)) # Only high and low movers are left, so remove low movers column since it is now implied
# Cast
subject_log_melted$value <- as.numeric(subject_log_melted$value)
subject_log_melted$HighMover <- factor(subject_log_melted$HighMover)
# Set params
low_mover_color <- "#E1BE6A"
high_mover_color <- "#40B0A6"
# Plot (High vs low)
all_ranks_movers_box_plot <- ggplot(subject_log_melted, aes(x=variable, y=value, fill=HighMover)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=0.5,
               position="dodge",
               outlier.size=2,
               outlier.color=NULL,
               outlier.fill=NULL,
               outlier.alpha=NULL,
               outlier.shape=21,
               outlier.stroke=0) +
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  scale_x_discrete(labels=c("VF_AfterRank" = "VF",
                            "G_AfterRank" = "BD",
                            "DL_AfterRank" = "DL")) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(low_mover_color,
                             high_mover_color)) +
  labs(
    title="High movers vs low movers for all categories ",
    x="",
    y="Facade Ranking") +
  #theme_classic() +
  theme_bw() #+
#coord_flip()
plot(all_ranks_movers_box_plot)

# Plot Scatter
# all_ranks_movers_scatter_plot <- ggplot(subject_log_melted, aes(x=variable, y=value, fill=HighMover)) + 
#   geom_point(color="black",
#              alpha=1.0,
#              size=1.5,
#              shape=10,
#              position="jitter") +
#   facet_grid(.~Room+Condition,
#              space="fixed",
#              margins = "vs") +
#   scale_x_discrete(labels=c("VF_AfterRank" = "View\nFactor",
#                             "G_AfterRank" = "Brightness\nDiscomfort",
#                             "DL_AfterRank" = "Daylighting")) +
#   scale_y_reverse(lim=rev(c(1, 16)),
#                   breaks=rev(c(1, 4, 7, 10, 13, 16))) +
#   scale_fill_manual(values=c(low_mover_color,
#                              high_mover_color)) +
#   labs(
#     title="All ",
#     x="",
#     y="Facade Ranking") +
#   #theme_classic() +
#   theme_bw() #+
# #coord_flip()
# plot(all_ranks_movers_scatter_plot)

##############################################
### After/After box plots for individual categories (High vs low rotators)
##############################################
# Format data
subject_log_melted <- melt(subjects_log, id=c("Room", "Condition", "HighRotator", "LowRotator"))
subject_log_melted <- subset(subject_log_melted, variable=="VF_AfterRank" | variable=="G_AfterRank" | variable=="DL_AfterRank")
# Converge mover colums to a single one and remove all 'medium' rotators
subject_log_melted <- subject_log_melted[!(subject_log_melted$HighRotator==0 & subject_log_melted$LowRotator == 0),] # Remove medium
subject_log_melted <- subset(subject_log_melted, select = -c(LowRotator)) # Only high and low movers are left, so remove low column since it is now implied
# Cast
subject_log_melted$value <- as.numeric(subject_log_melted$value)
subject_log_melted$HighRotator <- factor(subject_log_melted$HighRotator)
# Set params
low_rotator_color <- "#1A85FF"
high_rotator_color <- "#D41159"
# Plot (High vs low)
all_ranks_rotators_box_plot <- ggplot(subject_log_melted, aes(x=variable, y=value, fill=HighRotator)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=0.5,
               position="dodge",
               outlier.size=2,
               outlier.color=NULL,
               outlier.fill=NULL,
               outlier.alpha=NULL,
               outlier.shape=21,
               outlier.stroke=0) +
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  scale_x_discrete(labels=c("VF_AfterRank" = "VF",
                            "G_AfterRank" = "BD",
                            "DL_AfterRank" = "DL")) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(low_rotator_color,
                             high_rotator_color)) +
  labs(
    title="High rotators vs low rotators for all categories ",
    x="",
    y="Facade Ranking") +
  #theme_classic() +
  theme_bw() #+
#coord_flip()
plot(all_ranks_rotators_box_plot)

##############################################
### After/After box plots for individual categories (High vs low gazers)
##############################################
# Format data
subject_log_melted <- melt(subjects_log[subjects_log$Condition=="AR",], id=c("Room", "Condition", "HighGazer", "LowGazer"))
subject_log_melted <- subset(subject_log_melted, variable=="VF_AfterRank" | variable=="G_AfterRank" | variable=="DL_AfterRank")
# Converge mover colums to a single one and remove all 'medium' gazers
subject_log_melted <- subject_log_melted[!(subject_log_melted$HighGazer==0 & subject_log_melted$LowGazer == 0),] # Remove medium
subject_log_melted <- subset(subject_log_melted, select = -c(LowGazer)) # Only high and low movers are left, so remove low column since it is now implied
# Cast
subject_log_melted$value <- as.numeric(subject_log_melted$value)
subject_log_melted$HighGazer <- factor(subject_log_melted$HighGazer)
# Set params
low_gazer_color <- "#E66100"
high_gazer_color <- "#5D3A9B"
# Plot (High vs low)
all_ranks_gazers_box_plot <- ggplot(subject_log_melted, aes(x=variable, y=value, fill=HighGazer)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=0.5,
               position="dodge",
               outlier.size=2,
               outlier.color=NULL,
               outlier.fill=NULL,
               outlier.alpha=NULL,
               outlier.shape=21,
               outlier.stroke=0) +
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  scale_x_discrete(labels=c("VF_AfterRank" = "VF",
                            "G_AfterRank" = "BD",
                            "DL_AfterRank" = "DL")) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(low_gazer_color,
                             high_gazer_color)) +
  labs(
    title="High gazers vs low gazers for all categories ",
    x="",
    y="Facade Ranking") +
  #theme_classic() +
  theme_bw() #+
#coord_flip()
plot(all_ranks_gazers_box_plot)

##############################################
### After/After box plots for individual categories (All Classifications)
##############################################
# Reorder Movement Class
subjects_log$BodyMovementClass <- factor(subjects_log$BodyMovementClass, levels=c('Super Low', 'Low', 'Medium', 'High', 'Super High'))
# Format data
subject_log_melted <- melt(subjects_log, id=c("Room", "Condition", "BodyMovementClass"))
subject_log_melted <- subset(subject_log_melted, variable=="VF_AfterRank" | variable=="G_AfterRank" | variable=="DL_AfterRank")
# Cast
subject_log_melted$value <- as.numeric(subject_log_melted$value)
subject_log_melted$BodyMovementClass <- factor(subject_log_melted$BodyMovementClass)
# Plot (All classifications)
all_ranks_movers_classification_box_plot <- ggplot(subject_log_melted, aes(x=variable, y=value, fill=BodyMovementClass)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=0.5,
               position="dodge",
               outlier.size=2,
               outlier.color=NULL,
               outlier.fill=NULL,
               outlier.alpha=NULL,
               outlier.shape=21,
               outlier.stroke=0) +
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  scale_x_discrete(labels=c("VF_AfterRank" = "VF",
                            "G_AfterRank" = "BD",
                            "DL_AfterRank" = "DL")) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_brewer(palette = "Spectral") + 
  labs(
    title="High movers vs low movers for all categories ",
    x="",
    y="Facade Ranking") +
  #theme_classic() +
  theme_bw() #+
#coord_flip()
plot(all_ranks_movers_classification_box_plot)

##############################################
### High/Low movers for overall
##############################################
## Format data
# Isolate to desired variables
subject_log_melted <- melt(subjects_log, id=c("Room", "Condition", "HighMover", "LowMover"))
subject_log_melted <- subset(subject_log_melted, variable=="AfterRank")
# Converge mover colums to a single one and remove all 'medium' movers
subject_log_melted <- subject_log_melted[!(subject_log_melted$HighMover==0 & subject_log_melted$LowMover == 0),] # Remove medium workers
subject_log_melted <- subset(subject_log_melted, select = -c(LowMover)) # Only high and low movers are left, so remove low movers column since it is now implied
# Cast
subject_log_melted$value <- as.numeric(subject_log_melted$value)
subject_log_melted$HighMover <- as.factor(subject_log_melted$HighMover)
# Set params
low_mover_color <- "#E1BE6A"
high_mover_color <- "#40B0A6"
# Plot
movers_performance_plot <- ggplot(subject_log_melted, aes(x=HighMover, y=value, fill=HighMover)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=1,
               position="dodge") +
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  scale_x_discrete(labels=c("0" = "Low\nMovers+",
                            "1" = " High\nMovers+")) +
  #scale_y_continuous(lim=c(1, 16),
  #                   breaks=c(1, 4, 7, 10, 13, 16)) +
  scale_y_reverse(lim=rev(c(1, 16)),
                     breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(low_mover_color,
                             high_mover_color)) +
  labs(
    title="High Movers vs Low Movers Facade Selection Overall Ranking",
    x="",
    y="Facade Overall Rank") +
  #theme_classic() +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size=12, face = "bold", hjust = 0.5),
        axis.title.y=element_text(size=12, face = "bold"),
        axis.title.x=element_text(size=16, face = "bold"),
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=14))

#coord_flip()
plot(movers_performance_plot)

##############################################
### High/Low rotators for overall
##############################################
## Format data
# Isolate to desired variables
subject_log_melted <- melt(subjects_log, id=c("Room", "Condition", "HighRotator", "LowRotator"))
subject_log_melted <- subset(subject_log_melted, variable=="AfterRank")
# Converge mover colums to a single one and remove all 'medium' movers
subject_log_melted <- subject_log_melted[!(subject_log_melted$HighRotator==0 & subject_log_melted$LowRotator == 0),] # Remove medium subjects
subject_log_melted <- subset(subject_log_melted, select = -c(LowRotator)) # Only high and low rotators are left, so remove low rotators column since it is now implied
# Cast
subject_log_melted$value <- as.numeric(subject_log_melted$value)
subject_log_melted$HighRotator <- as.factor(subject_log_melted$HighRotator)
# Set params
low_rotator_color <- "#1A85FF"
high_rotator_color <- "#D41159"
# Plot
rotators_performance_plot <- ggplot(subject_log_melted, aes(x=HighRotator, y=value, fill=HighRotator)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=1,
               position="dodge") +
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  scale_x_discrete(labels=c("0" = "Low\nRotator+",
                            "1" = " High\nRotator+")) +
  #scale_y_continuous(lim=c(1, 16),
  #                   breaks=c(1, 4, 7, 10, 13, 16)) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(low_rotator_color,
                             high_rotator_color)) +
  labs(
    title="High Rotators vs Low Rotators Facade Selection Overall Ranking",
    x="",
    y="Facade Overall Rank") +
  #theme_classic() +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size=12, face = "bold", hjust = 0.5),
        axis.title.y=element_text(size=12, face = "bold"),
        axis.title.x=element_text(size=16, face = "bold"),
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=14))
#coord_flip()
plot(rotators_performance_plot)

##############################################
### High/Low gazers for overall
##############################################
## Format data
# Isolate to desired variables
subject_log_melted <- melt(subjects_log[subjects_log$Condition=="AR",], id=c("Room", "HighGazer", "LowGazer"))
subject_log_melted <- subset(subject_log_melted, variable=="AfterRank")
# Converge mover colums to a single one and remove all 'medium' movers
subject_log_melted <- subject_log_melted[!(subject_log_melted$HighGazer==0 & subject_log_melted$LowGazer == 0),] # Remove medium subjects
subject_log_melted <- subset(subject_log_melted, select = -c(LowGazer)) # Only high and low rotators are left, so remove low rotators column since it is now implied
# Cast
subject_log_melted$value <- as.numeric(subject_log_melted$value)
subject_log_melted$HighGazer <- as.factor(subject_log_melted$HighGazer)
# Set params
low_gazer_color <- "#E66100"
high_gazer_color <- "#5D3A9B"
# Plot
gazers_performance_plot <- ggplot(subject_log_melted, aes(x=HighGazer, y=value, fill=HighGazer)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=1,
               position="dodge") +
  facet_grid(.~Room,
             space="fixed",
             margins = "vs") +
  scale_x_discrete(labels=c("0" = "Low\nGazer+",
                            "1" = " High\nGazer+")) +
  #scale_y_continuous(lim=c(1, 16),
  #                   breaks=c(1, 4, 7, 10, 13, 16)) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(low_gazer_color,
                             high_gazer_color)) +
  labs(
    title="High Gazers vs Low Gazers Facade Selection Overall Ranking",
    x="",
    y="Facade Overall Rank") +
  #theme_classic() +
  theme_bw() + 
  theme(legend.position = "none",
        plot.title = element_text(size=12, face = "bold", hjust = 0.5),
        axis.title.y=element_text(size=12, face = "bold"),
        axis.title.x=element_text(size=16, face = "bold"),
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=14))
#coord_flip()
plot(gazers_performance_plot)

##############################################
### All Movement Classifications for overall
##############################################
# Reorder Movement Class
subjects_log$BodyMovementClass <- factor(subjects_log$BodyMovementClass, levels=c('Super Low', 'Low', 'Medium', 'High', 'Super High'))
## Format data
# Isolate to desired variables
subject_log_melted <- melt(subjects_log, id=c("Room", "Condition", "BodyMovementClass"))
subject_log_melted <- subset(subject_log_melted, variable=="AfterRank")
# Cast
subject_log_melted$value <- as.numeric(subject_log_melted$value)
subject_log_melted$BodyMovementClass <- factor(subject_log_melted$BodyMovementClass)
# Plot
movers_class_performance_plot <- ggplot(subject_log_melted, aes(x=BodyMovementClass, y=value, fill=BodyMovementClass)) + 
  geom_boxplot(color="black",
               #fill="white",
               alpha=0.75,
               #width=0.20,
               width=0.3,
               lwd=0.5,
               position="dodge") +
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  #scale_x_discrete(labels=c("0" = "Low\nMovers",
  #                          "1" = " High\nMovers")) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_brewer(palette = "Spectral") + 
  labs(
    title="All Movers final selection ranking",
    x="",
    y="Facade Overall Rank") +
  #theme_classic() +
  theme_bw() #+
#coord_flip()
plot(movers_class_performance_plot)

##############################################
### Explore body movement on good vs bad for optimal subjects
##############################################
# Group and summarize
plot_subjects <- subjects_log
plot_subjects$Optimal <- as.factor(plot_subjects$Optimal)
plot_subjects <- plot_subjects %>%
  group_by(Condition, Room, Optimal) %>%
  summarise_at(vars(Optimal.Movement.Ratio), list(MeanMovementOnOptimal = mean, STD = sd)) # Should try optimal as well
plot_subjects$Optimal <- factor(plot_subjects$Optimal)
# Set params
optimal_color <- "#005AB5"
poor_color <- "#DC3220"
# Plot
plot_optimal_head_movement_bar <- ggplot(plot_subjects, aes(x=Optimal, y=MeanMovementOnOptimal, fill=Optimal)) +
  geom_bar(stat="identity", alpha=0.7, size=1.0, color="black") +
  geom_errorbar( aes(ymin=MeanMovementOnOptimal-STD, ymax=MeanMovementOnOptimal+STD), width=0.5, colour="black", alpha=0.9, size=0.5) + 
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  #scale_y_reverse(lim=rev(c(1, 16)),
  #                breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(poor_color,
                             optimal_color)) +
  scale_x_discrete(labels=c("0" = "Non-optimal",
                            "1" = " Optimal\nParticipants")) +
  labs(
    title="Body Movement on Optimal Facades for High/Low Performers ",
    x="",
    y="Optimal Movement Ratio (Body Movement On Optimal Facades)") +
  #theme_classic() +
  theme_bw() + 
  theme(legend.position = "none",
        plot.title = element_text(size=12, face = "bold", hjust = 0.5),
        axis.title.y=element_text(size=12, face = "bold"),
        axis.title.x=element_text(size=16, face = "bold"),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=14))
print(plot_optimal_head_movement_bar)

##############################################
### Explore head movement on good vs bad for optimal subjects
##############################################
# Group and summarize
plot_subjects <- subjects_log
plot_subjects$Optimal <- as.factor(plot_subjects$Optimal)
plot_subjects <- plot_subjects %>%
  group_by(Condition, Room, Optimal) %>%
  summarise_at(vars(Optimal.Head.Ratio), list(MeanHeadMovementOnOptimal = mean, STD = sd)) # Should try optimal as well
plot_subjects$Optimal <- factor(plot_subjects$Optimal)
# Set params
optimal_color <- "#005AB5"
poor_color <- "#DC3220"
# Plot
plot_optimal_body_movement_bar <- ggplot(plot_subjects, aes(x=Optimal, y=MeanHeadMovementOnOptimal, fill=Optimal)) +
  geom_bar(stat="identity", alpha=0.7, size=1.0, color="black") +
  geom_errorbar( aes(ymin=MeanHeadMovementOnOptimal-STD, ymax=MeanHeadMovementOnOptimal+STD), width=0.5, colour="black", alpha=0.9, size=0.5) + 
  facet_grid(.~Room+Condition,
             space="fixed",
             margins = "vs") +
  #scale_y_reverse(lim=rev(c(1, 16)),
  #                breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(poor_color,
                             optimal_color)) +
  scale_x_discrete(labels=c("0" = "Non-optimal",
                            "1" = " Optimal\nParticipants")) +
  labs(
    title="Head Movement on Optimal Facades for High/Low Performers ",
    x="",
    y="Optimal Rotation Ratio (Head Movement On Optimal Facades)") +
  #theme_classic() +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size=12, face = "bold", hjust = 0.5),
        axis.title.y=element_text(size=12, face = "bold"),
        axis.title.x=element_text(size=16, face = "bold"),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=14))
print(plot_optimal_body_movement_bar)

##############################################
### Explore eye gaze on good vs bad for optimal subjects
##############################################
# Group and summarize
plot_subjects <- subjects_log[subjects_log$Condition=="AR",]
plot_subjects$Optimal <- as.factor(plot_subjects$Optimal)
plot_subjects <- plot_subjects %>%
  group_by(Condition, Room, Optimal) %>%
  summarise_at(vars(Optimal.Gaze.Ratio), list(MeanEyeGazeOnOptimal = mean, STD = sd)) # Should try optimal as well
plot_subjects$Optimal <- factor(plot_subjects$Optimal)
# Set params
optimal_color <- "#005AB5"
poor_color <- "#DC3220"
# Plot
plot_optimal_eye_gaze_bar <- ggplot(plot_subjects, aes(x=Optimal, y=MeanEyeGazeOnOptimal, fill=Optimal)) +
  geom_bar(stat="identity", alpha=0.7, size=1.0, color="black") +
  geom_errorbar(aes(ymin=MeanEyeGazeOnOptimal-STD, ymax=MeanEyeGazeOnOptimal+STD), width=0.5, colour="black", alpha=0.9, size=0.5) + 
  facet_grid(.~Room,
             space="fixed",
             margins = "vs") +
  #scale_y_reverse(lim=rev(c(1, 16)),
  #                breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(poor_color,
                             optimal_color)) +
  scale_x_discrete(labels=c("0" = "Non-optimal\nParticipants",
                            "1" = " Optimal\nParticipants")) +
  labs(
    title="Eye Gaze on Optimal Facades for High/Low Performers ",
    x="",
    y="Optimal Gaze Ratio (Eye Movement On Optimal Facades)") +
  #theme_classic() +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size=12, face = "bold", hjust = 0.5),
        axis.title.y=element_text(size=12, face = "bold"),
        axis.title.x=element_text(size=16, face = "bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14))
print(plot_optimal_eye_gaze_bar)



# 
# theme(
#   # Grid
#   panel.grid.major=element_line(size=1, color="gray"),
#   panel.grid.minor=element_line(size=0.5, color="gray"),
#   # Background
#   #panel.background = element_rect(fill = 'green', colour = 'red'),
#   #Legend
#   legend.position = "none",
#   # Title
#   #text=element_text(size=16, family="bold"),
#   #axis.title=element_text(size=16, face = "bold"),
#   plot.title=element_text(size=16, face = "bold", hjust = 0.5),
#   # Y Axis
#   axis.title.y=element_text(size=16, face = "bold"),
#   #axis.title.y=element_blank(),
#   #axis.text.y=element_blank(),
#   axis.text.y=element_text(size=16, face = "bold"),
#   axis.ticks.y=element_blank(),
#   #axis.line.y=element_blank(),
#   # X Axis
#   axis.title.x=element_text(size=16, face = "bold"),
#   #axis.title.x=element_blank(),
#   #axis.text.x=element_blank(),
#   axis.text.x=element_text(size=16, face = "bold"),
#   axis.ticks.x=element_blank()) +
#   