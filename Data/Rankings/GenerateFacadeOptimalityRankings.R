library(dplyr)
library(KraljicMatrix) # Pareto
library(stringr)

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
write.csv(facade_rankings,"FacadeOptimalData.csv", row.names=FALSE)