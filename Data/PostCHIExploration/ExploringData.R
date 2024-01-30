library(MASS) # For polr (Ordinal logistic regression)

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
#library(ggcorrplot)
#library(GGally)
library(RColorBrewer)
library(pastecs) # For summary stats
library(effsize)
library(see) # For half-violin plot
library(rcompanion) # For Cramer's V (Chi-square effect size) / R effect size
library(car) # Levene's Test


subject_performance_filename <- "../Subjects/SubjectPerformance.csv"

output_folder <- "Stats"
usability_filename <- "usability.txt"
movement_filename <- "movement.txt"
selection_filename <- "selections.txt"
time_filename <- "times.txt"
generic_plots_folder <- "Plots"
interaction_plots_folder <- "Plots/Interactions"

optimal_color <- "#1A85FF"
poor_color <- "#D41159"
#poor_color <- "#FF4FCE"
compliment_color <- "black"
#compliment_color <- "#BAC311"
    
conference_color <- "#E1BE6A"
office_color <- "#40B0A6"
  
ar_color <- "#1AFF1A"
desktop_color <- "#4B0092"
  
low_movement_color <- "#E66100"
high_movement_color <- "#5D3A9B"
  
rank_color <- "#648FFF"
dayighting_color <- "#785EF0"
glare_color <- "#DC267F"
viewfactor_color <- "#FE6100"

# Tol_muted (https://thenode.biologists.com/data-visualization-with-flying-colors/research/)
# sus_color <- "#88ccee"
# physical_demand_color <- "#117733"
# mental_demand_color <- "#332288"
# temporal_demand_color <- "#999933"
# performance_color <- "#cc6677"
# effort_demand_color <- "#882255"
sus_color <- "#2AAAF5"
physical_demand_color <- "#6DE038"
mental_demand_color <- "gray80"
temporal_demand_color <- "gray65"
performance_color <- "gray50"
effort_demand_color <- "gray35"

plot_outline_color <- "black"
plot_outline_size <- 1.5
plot_grid_color <- "gray85"
plot_grid_size_major <- 0.55
plot_grid_size_minor <- plot_grid_size_major
#plot_grid_size_minor <- 0.35

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Generic Functions #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
plot_and_savepdf <- function(plot, filename_no_extension, pdf_width=8, pdf_height=6, png_width=1200, png_height=800) {
  #print(paste0("Saving graph: ", filename_no_extension))
  pdf(file = paste0(generic_plots_folder, "/", filename_no_extension, ".pdf"),
      width = pdf_width,
      height = pdf_height)
  print(plot)
  dev.off()
  
  png(file = paste0(generic_plots_folder, "/", filename_no_extension, ".png"),
      width = png_width,
      height = png_height,
      units = "px")
  print(plot)
  dev.off()
}

summarize_stats <- function(groups) { # Pass in a c("Variable1", "Variable2, ~~~)
  stats_summary <-  subjects_data %>%
    group_by_at(groups) %>%
    summarise(n = n(),
              optimal.n = sum(Optimal==TRUE),
              poor.n = sum(Optimal==FALSE),
              rank.mean = mean(Rank),
              rank.mdn = median(Rank),
              rank.sd = sd(Rank),
              movement.mean = mean(Movement.Ratio),
              movement.mdn = median(Movement.Ratio),
              movement.sd = sd(Movement.Ratio),
              rotation.mean = mean(Head.Ratio),
              rotation.mdn = median(Head.Ratio),
              rotation.sd = sd(Head.Ratio),
              gaze.mean = mean(Gaze.Ratio),
              gaze.mdn = median(Gaze.Ratio),
              gaze.sd = sd(Gaze.Ratio),
              construction_exp.mean = mean(Construction.Familiarity),
              construction_exp.mdn = median(Construction.Familiarity),
              construction_exp.sd = sd(Construction.Familiarity),
              vr_exp.mean = mean(Prev.VR.Experience),
              vr_exp.mdn = median(Prev.VR.Experience),
              vr_exp.sd = sd(Prev.VR.Experience),
              ar_exp.mean = mean(Prev.AR.Experience),
              ar_exp.mdn = median(Prev.AR.Experience),
              ar_exp.sd = sd(Prev.AR.Experience),
              modeling_exp.mean = mean(Prev.3D.Modeling.Experience),
              modeling_exp.mdn = median(Prev.3D.Modeling.Experience),
              modeling_exp.sd = sd(Prev.3D.Modeling.Experience),
              computer_exp.mean = mean(Prev.Computer.Experience),
              computer_exp.mdn = median(Prev.Computer.Experience),
              computer_exp.sd = sd(Prev.Computer.Experience),
              videogame_exp.mean = mean(Video.Games),
              videogame_exp.mdn = median(Video.Games),
              videogame_exp.sd = sd(Video.Games),
              sus.mean = mean(SUS_Score),
              sus.mdn = median(SUS_Score),
              sus.sd = sd(SUS_Score),
              mental_demand.mean = mean(Mental_Demand_Score),
              mental_demand.mdn = median(Mental_Demand_Score),
              mental_demand.sd = sd(Mental_Demand_Score),
              physical_demand.mean = mean(Physical_Demand_Score),
              physical_demand.mdn = median(Physical_Demand_Score),
              physical_demand.sd = sd(Physical_Demand_Score),
              temporal_demand.mean = mean(Temporal_Demand_Score),
              temporal_demand.mdn = median(Temporal_Demand_Score),
              temporal_demand.sd = sd(Temporal_Demand_Score),
              performance_demand.mean = mean(Performance_Demand_Score),
              performance_demand.mdn = median(Performance_Demand_Score),
              performance_demand.sd = sd(Performance_Demand_Score),
              effort_demand.mean = mean(Effort_Demand_Score),
              effort_demand.mdn = median(Effort_Demand_Score),
              effort_demand.sd = sd(Effort_Demand_Score))
  return(stats_summary)
}

print_basic_stats <- function(room, condition, metric){
  print(paste0("===== Basic Stats", " ~ Room: ", room, " ~ Condition: ", condition, " ~ Metric: ", metric, " ====="))
  subjects <- subset(subjects_data, Condition==condition & Room==room)
  print(stat.desc(subjects[[metric]]))
}

print_basic_split_stats <- function(room, condition, metric){
  print(paste0("===== Basic Stats (High Performers)", " ~ Room: ", room, " ~ Condition: ", condition, " ~ Metric: ", metric, " ====="))
  subjects <- subset(subjects_data, Condition==condition & Room==room & Optimal==TRUE)
  print(stat.desc(subjects[[metric]]))
  print(paste0("===== Basic Stats (Low Performers)", " ~ Room: ", room, " ~ Condition: ", condition, " ~ Metric: ", metric, " ====="))
  subjects <- subset(subjects_data, Condition==condition & Room==room & Optimal==FALSE)
  print(stat.desc(subjects[[metric]]))
}

print_sample_metric <- function(sample, metric) {
  print(stat.desc(sample[[metric]]))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Read in Data #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Subjects Log ##
subjects_data <- read.csv("../Subjects/Log.csv",
                         skip=1,
                         header=TRUE,
                         sep=",")[1:13]
## Qualtrics Log ##
qualtrics_data <- read.csv("../Subjects/Qualtrics.csv",
                           header=TRUE,
                           sep=",")
colnames(qualtrics_data)[colnames(qualtrics_data) == "X1"] <- "Participant.ID" # Sync column name with subjects log
qualtrics_data <- qualtrics_data[-c(1, 2), ] # Remove alt-header
qualtrics_data <- qualtrics_data[-c(1:17, 19:22)] # Drop useless columns
## Movement Data ##
movement_data <- read.csv("../Subjects/SubjectMovement.csv",
                          skip=0,
                          header=TRUE,
                          sep=",")
movement_data <- subset(movement_data, select=-c(1))
movement_data$Optimal.Time.Ratio <- movement_data$Total.Time.Optimal / movement_data$Total.Time
movement_data$Poor.Time.Ratio <- movement_data$Total.Time.Poor / movement_data$Total.Time
## Merge the subject-related dataframes
subjects_data <- merge(subjects_data, qualtrics_data, by="Participant.ID")
rm(qualtrics_data)
subjects_data <- merge(subjects_data, movement_data, by="Participant.ID")
rm(movement_data)
## Remove unused subjects
subjects_data <- subjects_data[subjects_data$Room=="Office" | subjects_data$Room=="Conference",]
## Rename columns
colnames(subjects_data)[colnames(subjects_data) == "X5.1"] <- "Prev.XR.Experience" # (1-10)
colnames(subjects_data)[colnames(subjects_data) == "X1.4"] <- "Prev.VR.Experience" # (1-7)
colnames(subjects_data)[colnames(subjects_data) == "X2.4"] <- "Prev.AR.Experience" # (1-7)
colnames(subjects_data)[colnames(subjects_data) == "X4.3"] <- "Prev.3D.Modeling.Experience" # (1-7)
colnames(subjects_data)[colnames(subjects_data) == "X5.2"] <- "Prev.Computer.Experience" # (1-7)
colnames(subjects_data)[colnames(subjects_data) == "X1.5"] <- "Participated.In.Construction" # (Yes/No)
colnames(subjects_data)[colnames(subjects_data) == "X2.5"] <- "Construction.Familiarity" # (1-7)
colnames(subjects_data)[colnames(subjects_data) == "X3.4"] <- "Video.Games" # (1-7)
colnames(subjects_data)[colnames(subjects_data) == "X5.3"] <- "Participated.In.Renovation" # (Yes/No)
## Factor and format data
subjects_data$Condition <- as.factor(subjects_data$Condition)
subjects_data$Room <- as.factor(subjects_data$Room)
subjects_data$Prev.XR.Experience <- as.numeric(subjects_data$Prev.XR.Experience)
subjects_data$Prev.VR.Experience <- as.numeric(subjects_data$Prev.VR.Experience)
subjects_data$Prev.AR.Experience <- as.numeric(subjects_data$Prev.AR.Experience)
subjects_data$Prev.3D.Modeling.Experience <- as.numeric(subjects_data$Prev.3D.Modeling.Experience)
subjects_data$Prev.Computer.Experience <- as.numeric(subjects_data$Prev.Computer.Experience)
subjects_data$Participated.In.Construction <- ifelse(subjects_data$Participated.In.Construction=="Yes", TRUE, FALSE)
subjects_data$Construction.Familiarity <- as.numeric(subjects_data$Construction.Familiarity)
subjects_data$Video.Games <- as.numeric(subjects_data$Video.Games)
subjects_data$Participated.In.Renovation <- ifelse(subjects_data$Participated.In.Renovation=="Yes", TRUE, FALSE)
## Reformat Pre-choice data
colnames(subjects_data)[colnames(subjects_data) == "X1.6"] <- "Pre.Type"
colnames(subjects_data)[colnames(subjects_data) == "X2a"] <- "Pre.Type.Config.1"
colnames(subjects_data)[colnames(subjects_data) == "X2b"] <- "Pre.Type.Config.2"
colnames(subjects_data)[colnames(subjects_data) == "X2c"] <- "Pre.Type.Config.3"
colnames(subjects_data)[colnames(subjects_data) == "X2d"] <- "Pre.Type.Config.4"
create_pre_choice_col <- function() {
  col <- list()
  for(row in 1:nrow(subjects_data)) {
    type <- subjects_data$Pre.Type[row]
    type <- gsub(' ','', type) # Strip white-spaces
    if(type == "Dynamic"){
      config <- subjects_data$Pre.Type.Config.4[row]
      if(grepl("30", config, fixed = TRUE)){
        col[[row]] <-"DYNAMIC_30"
      } else if(grepl("60", config, fixed = TRUE)) {
        col[[row]] <-"DYNAMIC_60"
      } else if(grepl("90", config, fixed = TRUE)) {
        col[[row]] <-"DYNAMIC_90"
      } else {
        col[[row]] <-"DYNAMIC_0"
      }
    } else if(type == "Louvers") {
      config <- subjects_data$Pre.Type.Config.1[row]
      if(grepl("10", config, fixed = TRUE) & grepl("3.15", config, fixed = TRUE)){
        col[[row]] <-"LOUVER_10_30"
      } else if(grepl("10", config, fixed = TRUE) & grepl("7.32", config, fixed = TRUE)) {
        col[[row]] <-"LOUVER_10_70"
      } else if(grepl("20", config, fixed = TRUE) & grepl("1.58", config, fixed = TRUE)) {
        col[[row]] <-"LOUVER_20_30"
      } else {
        col[[row]] <-"LOUVER_20_70"
      }
    } else if(type == "Fritting") {
      config <- subjects_data$Pre.Type.Config.3[row]
      if(grepl("15", config, fixed = TRUE) & grepl("1.22", config, fixed = TRUE)){
        col[[row]] <-"FRITT_15_5"
      } else if(grepl("15", config, fixed = TRUE) & grepl("2.44", config, fixed = TRUE)) {
        col[[row]] <-"FRITT_15_10"
      } else if(grepl("30", config, fixed = TRUE) & grepl("1.22", config, fixed = TRUE)) {
        col[[row]] <-"FRITT_30_5"
      } else {
        col[[row]] <-"FRITT_30_10"
      }
    } else {
      config <- subjects_data$Pre.Type.Config.2[row]
      if(grepl("10", config, fixed = TRUE) & grepl("3.15", config, fixed = TRUE)){
        col[[row]] <-"FIN_10_30"
      } else if(grepl("10", config, fixed = TRUE) & grepl("7.32", config, fixed = TRUE)) {
        col[[row]] <-"FIN_10_70"
      } else if(grepl("20", config, fixed = TRUE) & grepl("1.58", config, fixed = TRUE)) {
        col[[row]] <-"FIN_20_30"
      } else {
        col[[row]] <-"FIN_20_70"
      }
    }
  }
  return(unlist(col))
}
subjects_data$Pre.Choice <- create_pre_choice_col()

## Facades Data ##
facades_data <- read.csv("../Rankings/FacadeOptimalData.csv",
                         header=TRUE,
                         sep=",")
## Match facade info selection info to subject logs
colnames(subjects_data)[colnames(subjects_data) == "Facade.Choice"] <- "Final.Choice" 
subjects_data$Final.Choice <- toupper(subjects_data$Final.Choice)
getFacadeVariableColumn <- function(facade_df_column, when.facade) {
  print(paste0("Matching facade df column values for variable: ", facade_df_column))
  var_column <- list()
  
  for(row in 1:nrow(subjects_data)){
    #facade <- subjects_data$Final.Choice[row]
    facade <- subjects_data[row, when.facade]
    room <- subjects_data$Room[row]
    var_val <- subset(facades_data, Facade==facade & Room==room)[,facade_df_column]
    #print(var_val)
    var_column[[row]] <- var_val
  }
  #print(var_column)
  return(unlist(var_column))
}
# Final choices
subjects_data$VF_Percentage <- getFacadeVariableColumn("VF_Percentage", "Final.Choice")
subjects_data$DL_Percentage <- getFacadeVariableColumn("DL_Percentage", "Final.Choice")
subjects_data$G_Percentage <- getFacadeVariableColumn("G_Percentage", "Final.Choice")
subjects_data$Distance <- getFacadeVariableColumn("Distance", "Final.Choice")
subjects_data$Optimal <- getFacadeVariableColumn("Optimal", "Final.Choice")
subjects_data$Rank <- getFacadeVariableColumn("Rank", "Final.Choice")
subjects_data$DL_Rank <- getFacadeVariableColumn("DL_Rank", "Final.Choice")
subjects_data$VF_Rank <- getFacadeVariableColumn("VF_Rank", "Final.Choice")
subjects_data$G_Rank <- getFacadeVariableColumn("G_Rank", "Final.Choice")
subjects_data$FromOptimal <- getFacadeVariableColumn("FromOptimal", "Final.Choice")
# Pre choices
subjects_data$VF_Percentage_Pre <- getFacadeVariableColumn("VF_Percentage", "Pre.Choice")
subjects_data$DL_Percentage_Pre <- getFacadeVariableColumn("DL_Percentage", "Pre.Choice")
subjects_data$G_Percentage_Pre <- getFacadeVariableColumn("G_Percentage", "Pre.Choice")
subjects_data$Distance_Pre <- getFacadeVariableColumn("Distance", "Pre.Choice")
subjects_data$Optimal_Pre <- getFacadeVariableColumn("Optimal", "Pre.Choice")
subjects_data$Rank_Pre <- getFacadeVariableColumn("Rank", "Pre.Choice")
subjects_data$DL_Rank_Pre <- getFacadeVariableColumn("DL_Rank", "Pre.Choice")
subjects_data$VF_Rank_Pre <- getFacadeVariableColumn("VF_Rank", "Pre.Choice")
subjects_data$G_Rank_Pre <- getFacadeVariableColumn("G_Rank", "Pre.Choice")
subjects_data$FromOptimal_Pre <- getFacadeVariableColumn("FromOptimal", "Pre.Choice")
# Re-factor the optimal column
subjects_data$Optimal <- ifelse(subjects_data$Optimal==1, TRUE, FALSE)
subjects_data$Optimal_Pre <- ifelse(subjects_data$Optimal_Pre==1, TRUE, FALSE)
# Columns covering difference between pre and post
subjects_data$Improved <- subjects_data$Rank < subjects_data$Rank_Pre
subjects_data$Became_Optimal <- !subjects_data$Optimal_Pre & subjects_data$Optimal
subjects_data$Rank_Diff <- subjects_data$Rank - subjects_data$Rank_Pre
subjects_data$VF_Rank_Diff <- subjects_data$VF_Rank - subjects_data$VF_Rank_Pre
subjects_data$DL_Rank_Diff <- subjects_data$DL_Rank - subjects_data$DL_Rank_Pre
subjects_data$G_Rank_Diff <- subjects_data$G_Rank - subjects_data$G_Rank_Pre

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Create Movement Metrics From Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Experimented with clustering, but seems not appropriate...
order_by_movement_metric <- function(metric){
  ordered <- subjects_data[order(subjects_data[[metric]], decreasing=TRUE),]
  sort_thru <- function(room, condition){
    ids <- list()
    results <- list()
    ordered_subset <- subset(ordered, Room == room & Condition == condition)
    for(i in 1:nrow(ordered_subset)){
      ids[[i]] <- ordered_subset$Participant.ID[i]
      if(i <= nrow(ordered_subset) / 2){
        results[[i]] <- "High"
      }
      else{
        results[[i]] <- "Low"
      }
    }
    return(data.frame(Participant.ID = unlist(ids),
                      Res = unlist(results)))
  }
  merged <- rbind(sort_thru("Office", "AR"),
                  sort_thru("Conference", "AR"),
                  sort_thru("Office", "Desktop"),
                  sort_thru("Conference", "Desktop"))
  merged <- merged[order(merged$Participant.ID),]
  return(merged$Res)
}

# Total movement for each
subjects_data <- subjects_data[order(subjects_data$Participant.ID),]
subjects_data$Movement.Classification <- order_by_movement_metric("Movement.Ratio")
subjects_data$Rotation.Classification <- order_by_movement_metric("Head.Ratio")
subjects_data$Gaze.Classification <- order_by_movement_metric("Gaze.Ratio")
# Optimal movement for each
subjects_data <- subjects_data[order(subjects_data$Participant.ID),]
subjects_data$Optimal.Movement.Classification <- order_by_movement_metric("Optimal.Movement.Ratio")
subjects_data$Optimal.Rotation.Classification <- order_by_movement_metric("Optimal.Head.Ratio")
subjects_data$Optimal.Gaze.Classification <- order_by_movement_metric("Optimal.Gaze.Ratio")
# Poor movement for each
subjects_data <- subjects_data[order(subjects_data$Participant.ID),]
subjects_data$Poor.Movement.Classification <- order_by_movement_metric("Poor.Movement.Ratio")
subjects_data$Poor.Rotation.Classification <- order_by_movement_metric("Poor.Head.Ratio")
subjects_data$Poor.Gaze.Classification <- order_by_movement_metric("Poor.Gaze.Ratio")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Read in Usability Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Score the SUS (1 (Strongly disagree) to 5 (Strongly agree))
sus_cols <- c("X1.8", "X2.7", "X3.8", "X4.6", "X5.5", "X6.6", "X7.2", "X8.2", "X9.1", "X10")
sus_data <- subjects_data[,sus_cols]
colnames(sus_data) <- 1:10 # Update column names to match answer key
sus_data[1:10] <- sapply(sus_data[1:10],as.numeric) # Cast to numeric for scoring
sus_data$Participant.ID <- subjects_data$Participant.ID # Re-add participant ID
# FORMULA: = ((B2-1)+(5-C2)+(D2-1)+(5-E2)+(F2-1)+(5-G2)+(H2-1)+(5-I2)+(J2-1)+(5-K2))*2.5 <---- (https://www.measuringux.com/sus/index.htm)
sus_data$SUS_Score <- (
  (sus_data$'1' - 1) +
    (5 - sus_data$'2') +
    (sus_data$'3' - 1) +
    (5 - sus_data$'4') +
    (sus_data$'5' - 1) +
    (5 - sus_data$'6') +
    (sus_data$'7' - 1) +
    (5 - sus_data$'8') +
    (sus_data$'9' - 1) +
    (5 - sus_data$'10')) * 2.5
# Add SUS back into overall subject log
subjects_data <- merge(x = subjects_data, y = sus_data[ , c("Participant.ID", "SUS_Score")], by = "Participant.ID", all.x=TRUE)
# Clean env
rm(sus_data)

## Score the TLX
tlx_cols <- c("X1_1", "X2_1", "X3_1", "X4_1", "X5_1")
tlx_data <- subjects_data[,tlx_cols]
tlx_data <- mutate_all(tlx_data, function(x) as.numeric(as.character(x)))
tlx_data$Participant.ID <- subjects_data$Participant.ID # Re-add participant ID
# Rename columns
colnames(tlx_data)[1] <- "Mental_Demand_Raw"
colnames(tlx_data)[2] <- "Physical_Demand_Raw"
colnames(tlx_data)[3] <- "Temporal_Demand_Raw"
colnames(tlx_data)[4] <- "Performance_Demand_Raw"
colnames(tlx_data)[5] <- "Effort_Demand_Raw"
# Score them
tlx_data$Mental_Demand_Score <- (tlx_data$Mental_Demand_Raw - 1) * 5
tlx_data$Physical_Demand_Score <- (tlx_data$Physical_Demand_Raw - 1) * 5
tlx_data$Temporal_Demand_Score <- (tlx_data$Temporal_Demand_Raw - 1) * 5
tlx_data$Performance_Demand_Score <- (tlx_data$Performance_Demand_Raw - 1) * 5
tlx_data$Effort_Demand_Score <- (tlx_data$Effort_Demand_Raw - 1) * 5
# Frustration missing...
# Normalize necessary scales to [1 = bad] and [100 = good]
tlx_data$Mental_Demand_Score <- abs(tlx_data$Mental_Demand_Score - 100)
tlx_data$Physical_Demand_Score <- abs(tlx_data$Physical_Demand_Score - 100)
tlx_data$Temporal_Demand_Score <- abs(tlx_data$Temporal_Demand_Score - 100)
tlx_data$Effort_Demand_Score <- abs(tlx_data$Effort_Demand_Score - 100)
# Add TLX back into overall subject log
subjects_data <- merge(x = subjects_data, y = tlx_data[ , c("Participant.ID", "Mental_Demand_Score", "Physical_Demand_Score", "Temporal_Demand_Score", "Performance_Demand_Score", "Effort_Demand_Score")], by = "Participant.ID", all.x=TRUE)
# Clean env
rm(tlx_data)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Generate Subjects Optimality csv (for external 3D plotting) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

output_data <- subjects_data %>%
  select(Participant.ID,
         Condition,
         Room,
         Final.Choice,
         Optimal,
         Rank,
         G_Rank,
         DL_Rank,
         VF_Rank,
         Movement.Classification,
         Rotation.Classification,
         Gaze.Classification)
write.csv(output_data, subject_performance_filename, row.names=FALSE)
rm(output_data)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### General Stats ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

stats_summary <- summarize_stats(c("Room", "Condition"))
# stats_summary <-  subjects_data %>%
#   group_by(Room, Condition) %>%
#   summarise(n = n(),
#             n.optimal = sum(Optimal==TRUE),
#             n.poor = sum(Optimal==FALSE),
#             rank_mean = mean(Rank),
#             rank_mdn = median(Rank),
#             rank_sd = sd(Rank),
#             movement_mean = mean(Movement.Ratio),
#             movement_mdn = median(Movement.Ratio),
#             movement_sd = sd(Movement.Ratio),
#             rotation_mean = mean(Head.Ratio),
#             rotation_mdn = median(Head.Ratio),
#             rotation_sd = sd(Head.Ratio),
#             gaze_mean = mean(Gaze.Ratio),
#             gaze_mdn = median(Gaze.Ratio),
#             gaze_sd = sd(Gaze.Ratio),
#             sus_mean = mean(SUS_Score),
#             sus_mdn = median(SUS_Score),
#             sus_sd = sd(SUS_Score))           

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Interaction Plots/Stats ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Plot interaction effects to determine what will be needed in Manova
interaction_plot <- function(name_prefix, subjects, colname_x, colname_trace, colname_response) {
  ## File data
  plot_filename <- paste(name_prefix, colname_x, colname_trace, colname_response, sep="_")
  plot_path <- paste0(interaction_plots_folder, "/",plot_filename,".png")
  png(filename=plot_path) # Only save it to file
  ## Create plot
  x <- subjects[[colname_x]]
  trace <- subjects[[colname_trace]]
  response <- subjects[[colname_response]]
  interaction.plot(x.factor = x,
                   trace.factor = trace,
                   response = response,
                   fun = mean,
                   col = c("#005AB5", "#DC3220"),
                   lty = 1,
                   lwd = 3,
                   xlab=colname_x,
                   ylab=colname_response,
                   trace.label=colname_trace,
  )
  ## Finish saving
  #dev.copy(png, filename=plot_path); # Still see in console
  dev.off()
}
interaction_plot("All", subjects_data, "Condition", "Room", "Optimal")
interaction_plot("AR", subjects_data[subjects_data$Condition=="AR",], "Movement.Classification", "Room", "Optimal")
interaction_plot("AR", subjects_data[subjects_data$Condition=="AR",], "Rotation.Classification", "Room", "Optimal")
interaction_plot("AR", subjects_data[subjects_data$Condition=="AR",], "Gaze.Classification", "Room", "Optimal")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Usability Statistics ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Correlations
usability_correlation_test <- function(room, metric){
  print(paste0("===== Correlation test ~ Room: ", room, " ~ Metric: ", metric, " ====="))
  ar_subjects <- subset(subjects_data, Condition=="AR" & Room==room)
  desktop_subjects <- subset(subjects_data, Condition=="Desktop" & Room==room)
  cor <- cor.test(ar_subjects[[metric]],
                  desktop_subjects[[metric]],
                  method="spearman")
  print(cor)
}
usability_linear_regression <- function(metric){
  print(paste0("===== Linear regression ~ Metric: ", metric, " ====="))
  model <- lm(metric ~ Room + Condition, data = subjects_data)
  print(summary(model))
}

sink(paste(output_folder, usability_filename, sep="/"), append=FALSE) # Start writing file

### Depracted for now. Failed normalicy tests
# print("===== Usability Manova for ~ Room + Condition =====")
# usability_manova <- manova(cbind(SUS_Score, Mental_Demand_Score, Physical_Demand_Score, Temporal_Demand_Score, Performance_Demand_Score, Effort_Demand_Score) ~ Room + Condition, data = subjects_data)
# print(summary(usability_manova))
# print(summary.aov(usability_manova))
# print(shapiro.test(residuals(usability_manova)))
# 
# print("===== Usability Manova for ~ Room + Condition + Optimal =====")
# usability_manova <- manova(cbind(SUS_Score, Mental_Demand_Score, Physical_Demand_Score, Temporal_Demand_Score, Performance_Demand_Score, Effort_Demand_Score) ~ Room + Condition + Optimal, data = subjects_data)
# print(summary(usability_manova))
# print(summary.aov(usability_manova))
# print(shapiro.test(residuals(usability_manova)))

### Going to try to incorporate the interactions now
interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "SUS_Score")
interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "Mental_Demand_Score")
interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "Physical_Demand_Score")
interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "Temporal_Demand_Score")
interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "Performance_Demand_Score")
interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "Effort_Demand_Score")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "SUS_Score")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "Mental_Demand_Score")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "Physical_Demand_Score")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "Temporal_Demand_Score")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "Performance_Demand_Score")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "Effort_Demand_Score")
print("===== Usability Manova (Office) for ~ Condition + Optimal =====")
usability_manova <- manova(cbind(SUS_Score,
                                 Mental_Demand_Score,
                                 Physical_Demand_Score,
                                 Temporal_Demand_Score,
                                 Performance_Demand_Score,
                                 Effort_Demand_Score)
                           ~ Condition + Optimal + Condition:Optimal,
                           data = subset(subjects_data, Room=="Office"))
print(summary(usability_manova))
print(summary.aov(usability_manova))
print(shapiro.test(residuals(usability_manova)))
print("===== Usability Manova (Conference) for ~ Condition + Optimal =====")
usability_manova <- manova(cbind(SUS_Score,
                                 Mental_Demand_Score,
                                 Physical_Demand_Score,
                                 Temporal_Demand_Score,
                                 Performance_Demand_Score,
                                 Effort_Demand_Score)
                           ~ Condition + Optimal,
                           data = subset(subjects_data, Room=="Conference"))
print(summary(usability_manova))
print(summary.aov(usability_manova))
print(shapiro.test(residuals(usability_manova)))

print("===== Usability Ordinal Logistic Regression =====")
olr <- polr(factor(SUS_Score) ~ factor(Condition) * factor(Movement.Classification),
            data=subset(subjects_data, Room=="Office"),
            Hess=TRUE)
print(olr)
print(summary(olr))
# ctable <- coef(summary(olr)) # https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))

# Doesnt seem to be something you can do VVVVV
# print("===== Usability Kruskall-Wallis =====")
# usability_kruskal <- kruskal.test(SUS_Score ~ factor(Condition) + factor(Room),
#             data=subjects_data)
# print(usability_kruskal)
# print(summary(usability_kruskal))

print("===== Usability Kruskall-Wallis  =====")
metrics <- c("SUS_Score", "Mental_Demand_Score", "Physical_Demand_Score", "Temporal_Demand_Score", "Effort_Demand_Score", "Performance_Demand_Score")
res_office <- lapply(metrics,
                     function(var){
                       print(var)
                       var_name <- var
                       sub <- subset(subjects_data, Room=="Office")
                       kt <- kruskal.test(sub[[var]] ~ factor(sub$Condition))
                       print(kt)
                     })
res_conf <- lapply(metrics,
                   function(var){
                     print(var)
                     var_name <- var
                     sub <- subset(subjects_data, Room=="Conference")
                     kt <- kruskal.test(sub[[var]] ~ factor(sub$Condition))
                     print(kt)
                   })
rm(metrics)
rm(res_conf)
rm(res_office)

usability_wilcoxin_room <- function(room, metric){
  print(paste0("===== Wilcoxin"," ~ Room: ", room, " ~ Metric: ", metric, " ====="))
  subjects <- select(subset(subjects_data, Room==room), c("Condition", metric))
  results <- wilcox.test(subjects[[metric]] ~ subjects$Condition,
                         paired = FALSE,
                         alternative = "two.sided",
                         conf.level = 0.95,
                         conf.int = TRUE,
                         exact = FALSE)
  print(results)
  print(cohen.d(subjects[[metric]], subjects$Condition))
  print(wilcoxonR(x = subjects[[metric]], g = subjects$Condition))
}

usability_wilcoxin_room("Office", "SUS_Score")
usability_wilcoxin_room("Office", "Mental_Demand_Score")
usability_wilcoxin_room("Office", "Physical_Demand_Score")
usability_wilcoxin_room("Office", "Temporal_Demand_Score")
usability_wilcoxin_room("Office", "Performance_Demand_Score")
usability_wilcoxin_room("Office", "Effort_Demand_Score")
usability_wilcoxin_room("Conference", "SUS_Score")
usability_wilcoxin_room("Conference", "Mental_Demand_Score")
usability_wilcoxin_room("Conference", "Physical_Demand_Score")
usability_wilcoxin_room("Conference", "Temporal_Demand_Score")
usability_wilcoxin_room("Conference", "Performance_Demand_Score")
usability_wilcoxin_room("Conference", "Effort_Demand_Score")

usability_wilcoxin_movement <- function(condition, room, movement_metric, usability_metric) {
    print(paste0("===== Wilcoxin (Movement) ~ Room: ", room, " ~ Condition: ", condition, " ~ Movement_Metric: ", movement_metric, " ~ Usability_Metric: ", usability_metric, " ====="))
    # wilcoxin_subjects <- subset(subjects_data, Condition==condition & Room==room)
    # high_subjects <- wilcoxin_subjects[wilcoxin_subjects[[movement_metric]]=="High",]
    # low_subjects <- wilcoxin_subjects[wilcoxin_subjects[[movement_metric]]=="Low",]
    # results <- wilcox.test(high_subjects[[usability_metric]],
    #                        low_subjects[[usability_metric]],
    #                        paired = FALSE,
    #                        alternative = "two.sided",
    #                        conf.level = 0.95,
    #                        conf.int = TRUE,
    #                        exact = FALSE)
    # print(results)
    # print(cohen.d(high_subjects[[usability_metric]], low_subjects[[usability_metric]]))

    subjects = select(subset(subjects_data, Condition==condition & Room==room), c(movement_metric, usability_metric))
    results <- wilcox.test(subjects[[usability_metric]] ~ subjects[[movement_metric]],
                           paired = FALSE,
                           alternative = "two.sided",
                           conf.level = 0.95,
                           conf.int = TRUE,
                           exact = FALSE)
    print(results)
    print(cohen.d(subjects[[usability_metric]], subjects[[movement_metric]]))
    print(wilcoxonR(x = subjects[[usability_metric]], g = subjects[[movement_metric]]))
}

usability_wilcoxin_movement("AR", "Office", "Movement.Classification", "SUS_Score")
usability_wilcoxin_movement("AR", "Office", "Movement.Classification", "Physical_Demand_Score")
usability_wilcoxin_movement("AR", "Conference", "Movement.Classification", "SUS_Score")
usability_wilcoxin_movement("AR", "Conference", "Movement.Classification", "Physical_Demand_Score")
usability_wilcoxin_movement("AR", "Office", "Rotation.Classification", "SUS_Score")
usability_wilcoxin_movement("AR", "Office", "Rotation.Classification", "Physical_Demand_Score")
usability_wilcoxin_movement("AR", "Conference", "Rotation.Classification", "SUS_Score")
usability_wilcoxin_movement("AR", "Conference", "Rotation.Classification", "Physical_Demand_Score")
usability_wilcoxin_movement("AR", "Office", "Gaze.Classification", "SUS_Score")
usability_wilcoxin_movement("AR", "Office", "Gaze.Classification", "Physical_Demand_Score") # Hmmmmm
usability_wilcoxin_movement("AR", "Conference", "Gaze.Classification", "SUS_Score")
usability_wilcoxin_movement("AR", "Conference", "Gaze.Classification", "Mental_Demand_Score")

print("==== Usability - High Eye movement AR vs Desktop ====")
x <- select(subset(subjects_data, Condition=="Desktop" & Room=="Office"), c("Condition", "Physical_Demand_Score"))
y <- select(subset(subjects_data, Condition=="AR" & Room=="Office" & Gaze.Classification=="High"), c("Condition", "Physical_Demand_Score"))
comb <- rbind(x, y)
results <- wilcox.test(comb$Physical_Demand_Score ~ comb$Condition,
                       paired = FALSE,
                       alternative = "two.sided",
                       conf.level = 0.95,
                       conf.int = TRUE,
                       exact = FALSE)
print(results)
print(cohen.d(comb$Physical_Demand_Score, comb$Condition))
print(wilcoxonR(x = comb$Physical_Demand_Score, g = comb$Condition))
rm(x, y, comb, results)

# Difference between optimal and non-optimal
# print("==================================================================================================================")
# print("==================================================================================================================")
# print("==================================================================================================================")

usability_wilcoxin_individual <- function(condition, room, metric){
  print(paste0("===== Wilcoxin ~ Room: ", room, " ~ Condition: ", condition, " ~ Metric: ", metric, " ====="))
  wilcoxin_subjects <- subset(subjects_data, Condition==condition & Room==room)
  optimal_subjects <- subset(wilcoxin_subjects, Optimal==TRUE)
  poor_subjects <- subset(wilcoxin_subjects, Optimal==FALSE)
  results <- wilcox.test(optimal_subjects[[metric]],
                         poor_subjects[[metric]],
                         paired = FALSE,
                         alternative = "two.sided",
                         conf.level = 0.95,
                         conf.int = TRUE,
                         exact = FALSE)
  print(results)
  print(cohen.d(optimal_subjects[[metric]], poor_subjects[[metric]]))
}

# usability_wilcoxin_individual("AR", "Office", "SUS_Score")
# usability_wilcoxin_individual("AR", "Office", "Mental_Demand_Score")
# usability_wilcoxin_individual("AR", "Office", "Physical_Demand_Score")
# usability_wilcoxin_individual("AR", "Office", "Temporal_Demand_Score")
# usability_wilcoxin_individual("AR", "Office", "Performance_Demand_Score")
# usability_wilcoxin_individual("AR", "Office", "Effort_Demand_Score")
# usability_wilcoxin_individual("AR", "Conference", "SUS_Score")
# usability_wilcoxin_individual("AR", "Conference", "Mental_Demand_Score")
# usability_wilcoxin_individual("AR", "Conference", "Physical_Demand_Score")
# usability_wilcoxin_individual("AR", "Conference", "Temporal_Demand_Score")
# usability_wilcoxin_individual("AR", "Conference", "Performance_Demand_Score")
# usability_wilcoxin_individual("AR", "Conference", "Effort_Demand_Score")
# usability_wilcoxin_individual("Desktop", "Office", "SUS_Score")
# usability_wilcoxin_individual("Desktop", "Office", "Mental_Demand_Score")
# usability_wilcoxin_individual("Desktop", "Office", "Physical_Demand_Score")
# usability_wilcoxin_individual("Desktop", "Office", "Temporal_Demand_Score")
# usability_wilcoxin_individual("Desktop", "Office", "Performance_Demand_Score")
# usability_wilcoxin_individual("Desktop", "Office", "Effort_Demand_Score")
# usability_wilcoxin_individual("Desktop", "Conference", "SUS_Score")
# usability_wilcoxin_individual("Desktop", "Conference", "Mental_Demand_Score")
# usability_wilcoxin_individual("Desktop", "Conference", "Physical_Demand_Score")
# usability_wilcoxin_individual("Desktop", "Conference", "Temporal_Demand_Score")
# usability_wilcoxin_individual("Desktop", "Conference", "Performance_Demand_Score")
# usability_wilcoxin_individual("Desktop", "Conference", "Effort_Demand_Score")
# print("==================================================================================================================")
# print("==================================================================================================================")
# print("==================================================================================================================")

## Relationship between Usability and previous experience
lm_usability_prevexp <- function(condition, room){
  print(paste0("===== Multivariate Mutliple Regression - Usability ~ Previous Experience in ", condition, "-", room, " ====="))
  
  usability_experience_lm <- lm(cbind(SUS_Score,
                                      Mental_Demand_Score,
                                      Physical_Demand_Score,
                                      Temporal_Demand_Score,
                                      Performance_Demand_Score,
                                      Effort_Demand_Score)
                                ~ Prev.XR.Experience + Prev.VR.Experience + Prev.AR.Experience + Prev.3D.Modeling.Experience + Prev.Computer.Experience + Construction.Familiarity,
                                data = subset(subjects_data, Condition==condition & Room==room))
  print(summary(usability_experience_lm))
  print(shapiro.test(residuals(usability_experience_lm)))
}
# Have to run multiple since Room and Condition are only two factors
lm_usability_prevexp("AR", "Office")
lm_usability_prevexp("AR", "Conference")
lm_usability_prevexp("Desktop", "Office")
lm_usability_prevexp("Desktop", "Conference")

# Stuff I'm going to use on this
print(paste0("===== ", "Spearman Correlation - XR and Usability" , " ====="))
#subjects <- subset(subjects_data, Condition=="AR" & Room=="Conference")
subjects <- subset(subjects_data, Condition=="AR")
sus_cor <- cor.test(subjects$SUS_Score,
                    subjects$Prev.AR.Experience,
                    alternative="two.sided",
                    method="spearman",
                    conf.level=0.95,
                    exact=FALSE)
print(sus_cor)
rm(subjects, sus_cor)
#hist() # Quick Distribution plot

sink() # Stop writing file

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Movement Statistics ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Cramer's V (effect size): https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5426219/#:~:text=Effect%20size&text=There%20are%20three%20different%20measures,but%20not%20for%20bigger%20tables.
movement_chi_sqr <- function(room, condition, classification_metric){
  print(paste0("===== Chi-Square (Uncorrected) ~ Room: ", room, " ~ Condition: ", condition, " ~ Metric: ", classification_metric, " ====="))
  subjects_subset <- subset(subjects_data, Room == room & Condition == condition)
  t <- table(subjects_subset[[classification_metric]], unlist(subjects_subset$Optimal))
  print(t)
  effect_size <- cramerV(t)
  results <- chisq.test(subjects_subset[[classification_metric]], unlist(subjects_subset$Optimal), correct=FALSE)
  print(results)
  return(c(room, condition, classification_metric, results$statistic, results$parameter, results$p.value, effect_size, NA))
}

sink(paste(output_folder, movement_filename, sep="/"), append=FALSE) # Start writing file

correction_table <- data.frame(matrix(ncol=8, nrow=0)) 
colnames(correction_table) <- c("Room", "Condition", "Metric", "Statistic", "df", "P.Original", "Cramers.V", "P.Corrected")

# Total Movement classification and performance
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Office", "AR", "Movement.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Conference", "AR", "Movement.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Office", "AR", "Rotation.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Conference", "AR", "Rotation.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Office", "AR", "Gaze.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Conference", "AR", "Gaze.Classification")
# Optimal Movement classification and performance
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Office", "AR", "Optimal.Movement.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Conference", "AR", "Optimal.Movement.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Office", "AR", "Optimal.Rotation.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Conference", "AR", "Optimal.Rotation.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Office", "AR", "Optimal.Gaze.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Conference", "AR", "Optimal.Gaze.Classification")
# Poor Movement classification and performance
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Office", "AR", "Poor.Movement.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Conference", "AR", "Poor.Movement.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Office", "AR", "Poor.Rotation.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Conference", "AR", "Poor.Rotation.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Office", "AR", "Poor.Gaze.Classification")
correction_table[nrow(correction_table) + 1,] <- movement_chi_sqr("Conference", "AR", "Poor.Gaze.Classification")
# Apply Correction to Chi-square p-values
correction_table$P.Corrected <- p.adjust(correction_table$P.Original, method="hommel")
print(correction_table)

## Another method, Ordinal Logist Regression (https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/)
# "Integer-ize" data
# subjects_ordinal <- subjects_data
# subjects_ordinal$Optimal <- as.factor(subjects_ordinal$Optimal)
# subjects_ordinal$Movement.Classification <- revalue(subjects_ordinal$Movement.Classification, c("Low"=0, "High"=1))
# subjects_ordinal$Rotation.Classification <- revalue(subjects_ordinal$Rotation.Classification, c("Low"=0, "High"=1))
# subjects_ordinal$Gaze.Classification <- revalue(subjects_ordinal$Gaze.Classification, c("Low"=0, "High"=1))
# subjects_ordinal$Condition <- revalue(subjects_ordinal$Condition, c("Paper"=0, "AR"=1))
# subjects_ordinal$Room <- revalue(subjects_ordinal$Room, c("Conference"=0, "Office"=1))
# Run formula
# polr(formula = Optimal ~ Movement.Classification + Rotation.Classification + Gaze.Classification + Condition + Room,
#      data = subjects_ordinal,
#      Hess = TRUE)
# Didn't end up working because Optimal needs THREE factors, not just two.....

# Determine what interactions effects are necessary 
interaction_plot("ARHeadMovement", subset(subjects_data, Condition=="AR"), "Room", "Optimal", "Movement.Ratio")
interaction_plot("ARHeadMovement", subset(subjects_data, Condition=="AR"), "Room", "Optimal", "Poor.Movement.Ratio")
interaction_plot("ARHeadMovement", subset(subjects_data, Condition=="AR"), "Room", "Optimal", "Optimal.Movement.Ratio")
interaction_plot("ARHeadRotation", subset(subjects_data, Condition=="AR"), "Room", "Optimal", "Head.Ratio")
interaction_plot("ARHeadRotation", subset(subjects_data, Condition=="AR"), "Room", "Optimal", "Optimal.Head.Ratio")
interaction_plot("ARHeadRotation", subset(subjects_data, Condition=="AR"), "Room", "Optimal", "Poor.Head.Ratio")
interaction_plot("ARGaze", subset(subjects_data, Condition=="AR"), "Room", "Optimal", "Gaze.Ratio")
interaction_plot("ARGaze", subset(subjects_data, Condition=="AR"), "Room", "Optimal", "Optimal.Gaze.Ratio")
interaction_plot("ARGaze", subset(subjects_data, Condition=="AR"), "Room", "Optimal", "Poor.Gaze.Ratio")

print("==== AR Head Movement Ratios Manova ~ Room + Optimal ====")
ar_movement_manova <- manova(cbind(Movement.Ratio,
                                   Optimal.Movement.Ratio,
                                   Poor.Movement.Ratio)
                             ~ Room + Optimal,
                             data = subset(subjects_data, Condition=="AR"))
print(summary(ar_movement_manova))
print(summary.aov(ar_movement_manova))
print(shapiro.test(residuals(ar_movement_manova)))
print("Failed Assumption of normailty check so IGNORE")

print("==== AR Head Rotation Ratios Manova ~ Room + Optimal + Room:Optimal ====")
ar_movement_manova <- manova(cbind(Head.Ratio,
                                   Optimal.Head.Ratio,
                                   Poor.Head.Ratio)
                             ~ Room + Optimal + Room:Optimal,
                             data = subset(subjects_data, Condition=="AR"))
print(summary(ar_movement_manova))
print(summary.aov(ar_movement_manova))
print(shapiro.test(residuals(ar_movement_manova)))
print("Failed Assumption of normailty check so IGNORE")

print("==== AR Eye Gaze Ratios Manova ~ Room + Optimal + Room:Optimal ====")
ar_movement_manova <- manova(cbind(Gaze.Ratio,
                                   Optimal.Gaze.Ratio,
                                   Poor.Gaze.Ratio)
                             ~ Room + Optimal + Room:Optimal,
                             data = subset(subjects_data, Condition=="AR"))
print(summary(ar_movement_manova))
print(summary.aov(ar_movement_manova))
print(shapiro.test(residuals(ar_movement_manova)))
print("Failed Assumption of normailty check so IGNORE")

print("==== Final Rank (All) Anova ~  ====")
ar_movement_manova <- aov(Rank ~ Room * (
                            Movement.Classification +
                            Optimal.Movement.Classification +
                            Poor.Movement.Classification +
                            Rotation.Classification +
                            Optimal.Rotation.Classification +
                            Poor.Rotation.Classification +
                            Gaze.Classification +
                            Optimal.Gaze.Classification +
                            Poor.Gaze.Classification),
                             data = subset(subjects_data, Condition=="AR"))
print(summary(ar_movement_manova))
print(shapiro.test(residuals(ar_movement_manova)))

print("==== Final Rank (Conference) Anova ~  ====")
# ar_movement_manova <- aov(Rank ~ 
#                             Movement.Classification +
#                             Optimal.Movement.Classification +
#                             Poor.Movement.Classification +
#                             Rotation.Classification +
#                             Optimal.Rotation.Classification +
#                             Poor.Rotation.Classification +
#                             Gaze.Classification +
#                             Optimal.Gaze.Classification +
#                             Poor.Gaze.Classification,
#                           data = subset(subjects_data, Condition=="AR" & Room=="Conference"))
# print(summary(ar_movement_manova))
# print(shapiro.test(residuals(ar_movement_manova)))
lm_subjects <- subset(subjects_data, Condition=="AR" & Room=="Conference")
ar_movement_lm <- lm(lm_subjects$Rank ~ 
                       ifelse(lm_subjects$Movement.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Optimal.Movement.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Poor.Movement.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Rotation.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Optimal.Rotation.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Poor.Rotation.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Gaze.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Optimal.Gaze.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Poor.Gaze.Classification == "High", 1, 0))
print(summary(ar_movement_lm))
print(shapiro.test(residuals(ar_movement_lm)))
#print_sample_metric(subset(subjects_data, Condition=="AR" & Room=="Conference" & Optimal.Rotation.Classification == "High"), "Rank")

print("==== Final Rank (Office) Anova ~  ====")
lm_subjects <- subset(subjects_data, Condition=="AR" & Room=="Office")
ar_movement_lm <- lm(lm_subjects$Rank ~ 
                       ifelse(lm_subjects$Movement.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Optimal.Movement.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Poor.Movement.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Rotation.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Optimal.Rotation.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Poor.Rotation.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Gaze.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Optimal.Gaze.Classification == "High", 1, 0) +
                       ifelse(lm_subjects$Poor.Gaze.Classification == "High", 1, 0))
print(summary(ar_movement_lm))
print(shapiro.test(residuals(ar_movement_lm)))

usability_wilcoxin_room <- function(room, metric){
  print(paste0("===== Wilcoxin"," ~ Room: ", room, " ~ Metric: ", metric, " ====="))
  subjects <- select(subset(subjects_data, Room==room), c("Condition", metric))
  results <- wilcox.test(subjects[[metric]] ~ subjects$Condition,
                         paired = FALSE,
                         alternative = "two.sided",
                         conf.level = 0.95,
                         conf.int = TRUE,
                         exact = FALSE)
  print(results)
  print(cohen.d(subjects[[metric]], subjects$Condition))
  print(wilcoxonR(x = subjects[[metric]], g = subjects$Condition))
}

usability_wilcoxin_condition <- function(condition, metric){
  print(paste0("===== Wilcoxin"," ~ Condition: ", condition, " ~ Metric: ", metric, " ====="))
  subjects <- select(subset(subjects_data, Condition==condition), c("Room", metric))
  results <- wilcox.test(subjects[[metric]] ~ subjects$Room,
                         paired = FALSE,
                         alternative = "two.sided",
                         conf.level = 0.95,
                         conf.int = TRUE,
                         exact = FALSE)
  print(results)
  print(cohen.d(subjects[[metric]], subjects$Room))
  print(wilcoxonR(x = subjects[[metric]], g = subjects$Room))
}

usability_wilcoxin_condition("AR", "Movement.Ratio")
usability_wilcoxin_condition("AR", "Head.Ratio")
usability_wilcoxin_condition("AR", "Gaze.Ratio")

usability_wilcoxin_movement("AR", "Conference", "Gaze.Classification", "DL_Rank")
usability_wilcoxin_movement("AR", "Conference", "Gaze.Classification", "G_Rank")
usability_wilcoxin_movement("AR", "Conference", "Gaze.Classification", "VF_Rank")

usability_wilcoxin_movement("AR", "Office", "Gaze.Classification", "Movement.Ratio")
usability_wilcoxin_movement("AR", "Office", "Gaze.Classification", "Head.Ratio")
usability_wilcoxin_movement("AR", "Conference", "Gaze.Classification", "Movement.Ratio")
usability_wilcoxin_movement("AR", "Conference", "Gaze.Classification", "Head.Ratio")

sink() # Stop writing file

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Selection Statistics ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

movement_experience_wilcoxin <- function(condition, room, x, y){
  print(paste0("===== Wilcoxin ~ Room: ", room, " ~ Condition: ", condition, " ~ X: ", x, " ~ Y: ", y, " ====="))
  results <- wilcox.test(subjects_data[subjects_data$Condition==condition,][[y]]
                         ~ subjects_data[subjects_data$Condition==condition,][[x]],
                         paired = FALSE,
                         alternative = "two.sided",
                         conf.level = 0.95,
                         exact = FALSE)
  print(results)
}

wilcoxin_single <- function(condition, room, metric){
  print(paste0("===== Wilcoxin ~ Room: ", room, " ~ Condition: ", condition, " ~ Metric: ", metric, " ====="))
  subjects_in_room <- subset(subjects_data, Room==room & Condition==condition)
  #print(shapiro.test(subjects_in_room))
  results <- wilcox.test(subjects_in_room[[metric]],
                         paired = FALSE,
                         alternative = "two.sided",
                         conf.int = TRUE,
                         mu = 0,
                         conf.level = 0.95,
                         exact = FALSE)
  print(results)
  print(cohen.d(subjects_in_room[[metric]], NA))
  #print(wilcoxonR(x = subjects[[metric]], g = subjects$Condition))
}

t_test_single <- function(condition, room, metric){
  print(paste0("===== T-test ~ Room: ", room, " ~ Condition: ", condition, " ~ Metric: ", metric, " ====="))
  subjects_in_room <- subset(subjects_data, Room==room & Condition==condition)
  print(shapiro.test(subjects_in_room[[metric]]))
  results <- t.test(subjects_in_room[[metric]],
                         paired = FALSE,
                         alternative = "two.sided",
                         conf.int = TRUE,
                         mu = 0,
                         conf.level = 0.95,
                         exact = FALSE)
  print(results)  
}

sink(paste(output_folder, selection_filename, sep="/"), append=FALSE) # Start writing file

# Correlations with optimal selections and previous experience
movement_experience_wilcoxin("AR", NA, "Optimal", "Prev.XR.Experience")
movement_experience_wilcoxin("AR", NA, "Optimal", "Prev.VR.Experience")
movement_experience_wilcoxin("AR", NA, "Optimal", "Prev.AR.Experience")
movement_experience_wilcoxin("AR", NA, "Optimal", "Prev.3D.Modeling.Experience")
movement_experience_wilcoxin("AR", NA, "Optimal", "Prev.Computer.Experience")
movement_experience_wilcoxin("AR", NA, "Optimal", "Construction.Familiarity")
movement_experience_wilcoxin("Desktop", NA, "Optimal", "Prev.3D.Modeling.Experience")
movement_experience_wilcoxin("Desktop", NA, "Optimal", "Prev.Computer.Experience")
movement_experience_wilcoxin("Desktop", NA, "Optimal", "Construction.Familiarity")

## NO INTERACTIONS ##
print("==== Desktop Subjects (No interactions) - 4 Ranking Categories ~ Room + Head Movement Classification + Head Rotation Classification  ====")
movement_anova <- manova(cbind(Rank,
                               DL_Rank,
                               G_Rank,
                               VF_Rank)
                         ~ Room + Movement.Classification + Rotation.Classification,
                         data = subset(subjects_data, Condition=="Desktop"))
print(summary(movement_anova))
print(summary.aov(movement_anova))
print(shapiro.test(residuals(movement_anova)))

print("==== AR Subjects (No interactions) - 4 Ranking Categories ~ Room + Head Movement Classification + Head Rotation Classification + Gaze Classification  ====")
movement_anova <- manova(cbind(Rank,
                               DL_Rank,
                               G_Rank,
                               VF_Rank)
                         ~ Room + Movement.Classification + Rotation.Classification + Gaze.Classification,
                         data = subset(subjects_data, Condition=="AR"))
print(summary(movement_anova))
print(summary.aov(movement_anova))
print(shapiro.test(residuals(movement_anova)))

## WITH INTERACTIONS ##
print("==== Desktop Subjects (With interactions) - 4 Ranking Categories ~ Room + Head Movement Classification + Head Rotation Classification  ====")
movement_anova <- manova(cbind(Rank,
                               DL_Rank,
                               G_Rank,
                               VF_Rank)
                         ~ Room + Movement.Classification + Rotation.Classification + Room:Rotation.Classification,
                         data = subset(subjects_data, Condition=="Desktop"))
print(summary(movement_anova))
print(summary.aov(movement_anova))
print(shapiro.test(residuals(movement_anova)))

print("==== AR Subjects (With interactions) - 4 Ranking Categories ~ Room + Head Movement Classification + Head Rotation Classification + Gaze Classification  ====")
movement_anova <- manova(cbind(Rank,
                               DL_Rank,
                               G_Rank,
                               VF_Rank)
                         ~ Room + Movement.Classification + Rotation.Classification + Gaze.Classification + Room:Movement.Classification + Room:Gaze.Classification,
                         data = subset(subjects_data, Condition=="AR"))
print(summary(movement_anova))
print(summary.aov(movement_anova))
print(shapiro.test(residuals(movement_anova)))

interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "Prev.XR.Experience")
interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "Prev.VR.Experience")
interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "Prev.AR.Experience")
interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "Prev.3D.Modeling.Experience")
interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "Prev.Computer.Experience")
interaction_plot("Office", subset(subjects_data, Room=="Office"), "Condition", "Optimal", "Construction.Familiarity")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "Prev.XR.Experience")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "Prev.VR.Experience")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "Prev.AR.Experience")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "Prev.3D.Modeling.Experience")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "Prev.Computer.Experience")
interaction_plot("Conference", subset(subjects_data, Room=="Conference"), "Condition", "Optimal", "Construction.Familiarity")
print("===== Prev. Experience Manova (Office) for ~ Condition + Experience Metrics =====")
experience_manova <- manova(cbind(Prev.XR.Experience,
                                  Prev.VR.Experience,
                                  Prev.AR.Experience,
                                  Prev.3D.Modeling.Experience,
                                  Prev.Computer.Experience,
                                  Construction.Familiarity)
                            ~ Condition + Optimal + Condition:Optimal,
                            data = subset(subjects_data, Room=="Office"))
print(summary(experience_manova))
print(summary.aov(experience_manova))
print(shapiro.test(residuals(experience_manova)))
print("===== Prev. Experience Manova (Conference) for ~ Condition + Experience Metrics =====")
experience_manova <- manova(cbind(Prev.XR.Experience,
                                  Prev.VR.Experience,
                                  Prev.AR.Experience,
                                  Prev.3D.Modeling.Experience,
                                  Prev.Computer.Experience,
                                  Construction.Familiarity)
                            ~ Condition + Optimal + Condition:Optimal,
                            data = subset(subjects_data, Room=="Conference"))
print(summary(experience_manova))
print(summary.aov(experience_manova))
print(shapiro.test(residuals(experience_manova)))

usability_wilcoxin_individual("AR", "Office", "Construction.Familiarity")
usability_wilcoxin_individual("AR", "Conference", "Construction.Familiarity")
usability_wilcoxin_individual("Desktop", "Office", "Construction.Familiarity")
usability_wilcoxin_individual("Desktop", "Conference", "Construction.Familiarity")

print("===== Difference in Pre/Post (All subjects) ~ Condition + Room + Movement Classification + Rotation Classification =====")
manova <- manova(cbind(Rank_Diff,
                       VF_Rank_Diff,
                       DL_Rank_Diff,
                       G_Rank_Diff)
                 ~ Room + Condition + Movement.Classification + Rotation.Classification,
                 data = subjects_data)
print(summary(manova))
print(summary.aov(manova))
print(shapiro.test(residuals(manova)))
print("Failed Assumption of normailty check so IGNORE")

print("===== Difference in Pre/Post (AR Subjects) ~ Room + Movement Classification + Rotation Classification + Gaze Classification =====")
manova <- manova(cbind(Rank_Diff,
                       VF_Rank_Diff,
                       DL_Rank_Diff,
                       G_Rank_Diff)
                 ~ Room + Movement.Classification + Rotation.Classification + Gaze.Classification,
                 data = subset(subjects_data, Condition=="AR"))
print(summary(manova))
print(summary.aov(manova))
print(shapiro.test(residuals(manova)))

print("===== Wilcoxins for final selection rankings =====")
print(shapiro.test(subset(subjects_data, Condition=="AR" & Room=="Office")$Rank))
print(shapiro.test(subset(subjects_data, Condition=="Desktop" & Room=="Office")$Rank))
usability_wilcoxin_room("Office", "Rank")
print_basic_stats("Office", "AR", "Rank")
print_basic_stats("Office", "Desktop", "Rank")
print(shapiro.test(subset(subjects_data, Condition=="AR" & Room=="Conference")$Rank))
print(shapiro.test(subset(subjects_data, Condition=="Desktop" & Room=="Conference")$Rank))
usability_wilcoxin_room("Conference", "Rank")
print_basic_stats("Conference", "AR", "Rank")
print_basic_stats("Conference", "Desktop", "Rank")
#t_test_single("AR", "Office", "Rank_Diff")
wilcoxin_single("AR", "Office", "Rank_Diff")
print_basic_stats("Office", "AR", "Rank_Diff")
wilcoxin_single("Desktop", "Office", "Rank_Diff")
print_basic_stats("Office", "Desktop", "Rank_Diff")
wilcoxin_single("AR", "Conference", "Rank_Diff")
print_basic_stats("Conference", "AR", "Rank_Diff")
wilcoxin_single("Desktop", "Conference", "Rank_Diff")
print_basic_stats("Conference", "Desktop", "Rank_Diff")

print("===== Difference in Pre/Post ANOVA ~ Condition * Room=====")
difference_anova <- aov(Rank_Diff ~ Condition * Room,
                        data = subjects_data)
print(summary(difference_anova))
print(summary.aov(difference_anova))
print(shapiro.test(residuals(difference_anova)))
print("===== Difference in Pre/Post Variables MANOVA ~ Condition * Room=====")
difference_anova <- manova(cbind(VF_Rank_Diff, G_Rank_Diff, DL_Rank_Diff) ~ Condition * Room,
                        data = subjects_data)
print(summary(difference_anova))
print(summary.aov(difference_anova))
print(shapiro.test(residuals(difference_anova)))

print("===== Binary Logistical Regression (All) : Optimal ~ Room * Movement Ratios =====")
blr <- glm(factor(Optimal) ~ # https://digitaschools.com/binary-logistic-regression-in-r/
             Room * (Movement.Ratio +
                       Optimal.Movement.Ratio +
                       Poor.Movement.Ratio +
                       Head.Ratio +
                       Optimal.Head.Ratio +
                       Poor.Head.Ratio +
                       Gaze.Ratio +
                       Optimal.Gaze.Ratio +
                       Poor.Gaze.Ratio),
           family = binomial,
           data = subset(subjects_data, Condition=="AR"))
print(summary(blr))

print("===== Binary Logistical Regression (All) : Optimal ~ Room * Movement Classifications =====")
blr <- glm(factor(Optimal) ~ # https://digitaschools.com/binary-logistic-regression-in-r/
             Room * (Movement.Classification +
                       Optimal.Movement.Classification +
                       Poor.Movement.Classification +
                       Rotation.Classification +
                       Optimal.Rotation.Classification +
                       Poor.Rotation.Classification +
                       Gaze.Classification +
                       Optimal.Gaze.Classification +
                       Poor.Gaze.Classification),
           family = binomial,
           data = subset(subjects_data, Condition=="AR"))
print(summary(blr))

print("===== Binary Logistical Regression (Office) : Optimal ~ Movement Ratios =====")
blr <- glm(factor(Optimal) ~ # https://digitaschools.com/binary-logistic-regression-in-r/
             Movement.Ratio +
             Optimal.Movement.Ratio +
             Poor.Movement.Ratio +
             Head.Ratio +
             Optimal.Head.Ratio +
             Poor.Head.Ratio +
             Gaze.Ratio +
             Optimal.Gaze.Ratio +
             Poor.Gaze.Ratio,
           family = binomial,
           data = subset(subjects_data, Condition=="AR" & Room=="Office"))
print(summary(blr))

print("===== Binary Logistical Regression (Office) : Optimal ~ Movement Categories =====")
blr <- glm(factor(Optimal) ~ # https://digitaschools.com/binary-logistic-regression-in-r/
             factor(Movement.Classification) +
             factor(Optimal.Movement.Classification) +
             factor(Poor.Movement.Classification) +
             factor(Rotation.Classification) +
             factor(Optimal.Rotation.Classification) +
             factor(Poor.Rotation.Classification) +
             factor(Gaze.Classification) +
             factor(Optimal.Gaze.Classification) +
             factor(Poor.Gaze.Classification),
           family = binomial,
           data = subset(subjects_data, Condition=="AR" & Room=="Office"))
print(summary(blr))

print("===== Binary Logistical Regression (Conference) : Optimal ~ Movement Ratios =====")
blr <- glm(factor(Optimal) ~ # https://digitaschools.com/binary-logistic-regression-in-r/
             Movement.Ratio +
             Optimal.Movement.Ratio +
             Poor.Movement.Ratio +
             Head.Ratio +
             Optimal.Head.Ratio +
             Poor.Head.Ratio +
             Gaze.Ratio +
             Optimal.Gaze.Ratio +
             Poor.Gaze.Ratio,
           family = binomial,
           data = subset(subjects_data, Condition=="AR" & Room=="Conference"))
print(summary(blr))

print("===== Binary Logistical Regression 2 (Conference) : Optimal ~ Movement Ratios =====")
blr <- glm(factor(Optimal) ~ # https://digitaschools.com/binary-logistic-regression-in-r/
             Optimal.Head.Ratio +
             Gaze.Ratio +
             Optimal.Gaze.Ratio,
           family = binomial,
           data = subset(subjects_data, Condition=="AR" & Room=="Conference"))
print(summary(blr))

print("===== Binary Logistical Regression (Conference) : Optimal ~ Movement Categories =====")
blr <- glm(factor(Optimal) ~ # https://digitaschools.com/binary-logistic-regression-in-r/
             factor(Movement.Classification) +
             factor(Optimal.Movement.Classification) +
             factor(Poor.Movement.Classification) +
             factor(Rotation.Classification) +
             factor(Optimal.Rotation.Classification) +
             factor(Poor.Rotation.Classification) +
             factor(Gaze.Classification) +
             factor(Optimal.Gaze.Classification) +
             factor(Poor.Gaze.Classification),
           family = binomial,
           data = subset(subjects_data, Condition=="AR" & Room=="Conference"))
print(summary(blr))

print("===== Binary Logistical Regression 2 (Conference) : Optimal ~ Movement Categories =====")
blr <- glm(factor(Optimal) ~ # https://digitaschools.com/binary-logistic-regression-in-r/
             factor(Optimal.Rotation.Classification) +
             factor(Gaze.Classification) +
             factor(Optimal.Gaze.Classification),
           family = binomial,
           data = subset(subjects_data, Condition=="AR" & Room=="Conference"))
print(summary(blr))

sink() # Stop writing file

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Time Statistics ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

t_test_individual <- function(condition, room, metric){
  print(paste0("===== t-test ~ Room: ", room, " ~ Condition: ", condition, " ~ Metric: ", metric, " ====="))
  test_subjects <- subset(subjects_data, Condition==condition & Room==room)
  optimal_subjects <- subset(test_subjects, Optimal==TRUE)
  poor_subjects <- subset(test_subjects, Optimal==FALSE)
  results <- t.test(optimal_subjects[[metric]],
                    poor_subjects[[metric]],
                    paired = FALSE,
                    alternative = "two.sided",
                    var.equal = FALSE,
                    conf.int = TRUE,
                    conf.level = 0.95,
                    exact = FALSE)
  print(results)
  print(cohen.d(optimal_subjects[[metric]], poor_subjects[[metric]]))
}

sink(paste(output_folder, time_filename, sep="/"), append=FALSE) # Start writing file

# AR - Office
usability_wilcoxin_individual("AR", "Office", "Optimal.Time.Ratio")
#t_test_individual("AR", "Office", "Optimal.Time.Ratio")
print_basic_split_stats("Office", "AR", "Optimal.Time.Ratio")
print(shapiro.test(subset(subjects_data, Condition=="AR" & Room=="Office" & Optimal==TRUE)$Optimal.Time.Ratio))
print(shapiro.test(subset(subjects_data, Condition=="AR" & Room=="Office" & Optimal==FALSE)$Optimal.Time.Ratio))
# AR - Conference
usability_wilcoxin_individual("AR", "Conference", "Optimal.Time.Ratio")
#t_test_individual("AR", "Conference", "Optimal.Time.Ratio")
print_basic_split_stats("Conference", "AR", "Optimal.Time.Ratio")
print(shapiro.test(subset(subjects_data, Condition=="AR" & Room=="Conference" & Optimal==TRUE)$Optimal.Time.Ratio))
print(shapiro.test(subset(subjects_data, Condition=="AR" & Room=="Conference" & Optimal==FALSE)$Optimal.Time.Ratio))
# Desktop - Office
usability_wilcoxin_individual("Desktop", "Office", "Optimal.Time.Ratio")
#t_test_individual("Desktop", "Office", "Optimal.Time.Ratio")
print_basic_split_stats("Office", "Desktop", "Optimal.Time.Ratio")
print(shapiro.test(subset(subjects_data, Condition=="Desktop" & Room=="Office" & Optimal==TRUE)$Optimal.Time.Ratio))
print(shapiro.test(subset(subjects_data, Condition=="Desktop" & Room=="Office" & Optimal==FALSE)$Optimal.Time.Ratio))
# Desktop - Conference
usability_wilcoxin_individual("Desktop", "Conference", "Optimal.Time.Ratio")
#t_test_individual("Desktop", "Conference", "Optimal.Time.Ratio")
print_basic_split_stats("Conference", "Desktop", "Optimal.Time.Ratio")
print(shapiro.test(subset(subjects_data, Condition=="Desktop" & Room=="Conference" & Optimal==TRUE)$Optimal.Time.Ratio))
print(shapiro.test(subset(subjects_data, Condition=="Desktop" & Room=="Conference" & Optimal==FALSE)$Optimal.Time.Ratio))

sink() # Stop writing file

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Construction Experience Statistics ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

construction_experience_data <- summarize_stats(c("Room", "Condition", "Participated.In.Construction"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plots - Correlation ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

correlation_plot <- function(condition, room){ # Failed experiment (for now...)
  subjects_cor <- subjects_data %>%
    subset(Condition==condition & Room==room) %>%
    select("Prev.XR.Experience",
           "Prev.VR.Experience",
           "Prev.AR.Experience",
           "Prev.3D.Modeling.Experience",
           "Prev.Computer.Experience",
           "Construction.Familiarity",
           #"Optimal",
           #"Movement.Classification",
           #"Rotation.Classification",
           #"Gaze.Classification",
           "SUS_Score",
           "Mental_Demand_Score",
           "Physical_Demand_Score",
           "Temporal_Demand_Score",
           "Performance_Demand_Score",
           "Effort_Demand_Score")
  
  #subjects_cor <- ggcorr(subjects_cor, method ="pearson")
  #subjects_cor <- cor_pmat(subjects_cor)
  #ggcorrplot(subjects_cor, method = "circle") 
}

# TODO

# massive_anova <- manova(cbind(SUS_Score,
#                               Mental_Demand_Score,
#                               Physical_Demand_Score,
#                               Temporal_Demand_Score,
#                               Performance_Demand_Score,
#                               Effort_Demand_Score)
#                         ~ Room + Condition ,
#                         data = subjects_data)


#correlation_plot("AR", "Office")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Basic Post-quesionnaires ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
usability_melted <- melt(select(subjects_data, c("Condition", "Room", "SUS_Score", "Mental_Demand_Score", "Physical_Demand_Score", "Temporal_Demand_Score", "Performance_Demand_Score", "Effort_Demand_Score")), 
                         id = c("Condition", "Room"),
                         variable.name = "Metric",
                         value.name = "Score")

plot_usability <- ggplot(usability_melted, aes(x=Metric, y=Score, fill=Condition)) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  # geom_jitter(color="black",
  #             size=0.4,
  #             alpha=0.9) +
  scale_fill_manual(values=c(ar_color, desktop_color)) +
  #scale_x_discrete(limits = rev(levels(usability_melted$Metric))) +
  facet_grid(~Room,
             space="fixed",
             margins = "vs") +
  labs(
    title="Post-Questionnaire Metrics",
    x="Test",
    y="Normalized Score") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_usability, "BasicUsability")
rm(usability_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Basic Post-quesionnaires (optimal seperated) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
usability_melted <- melt(select(subjects_data, c("Condition", "Room", "Optimal", "SUS_Score", "Mental_Demand_Score", "Physical_Demand_Score", "Temporal_Demand_Score", "Performance_Demand_Score", "Effort_Demand_Score")), 
                         id = c("Condition", "Room", "Optimal"),
                         variable.name = "Metric",
                         value.name = "Score")

plot_usability <- ggplot(usability_melted, aes(x=Metric, y=Score, fill=Optimal)) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  # geom_jitter(color="black",
  #             size=0.4,
  #             alpha=0.9) +
  scale_fill_manual(values=c(poor_color, optimal_color)) +
  #scale_x_discrete(limits = rev(levels(usability_melted$Metric))) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  labs(
    title="Post-Questionnaire Metrics",
    x="Test",
    y="Normalized Score") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_usability, "BasicUsability2")
rm(usability_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Movement Ratios ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
movement_melted <- melt(select(subset(subjects_data, Condition=="AR"), 
                               c("Participant.ID",
                                 "Room",
                                 "Optimal",
                                 "Movement.Ratio",
                                 "Head.Ratio", 
                                 "Gaze.Ratio")), 
                        id = c("Participant.ID", "Room", "Optimal"),
                        variable.name = "Metric",
                        value.name = "Score")

## Movement data
plot_movement <- ggplot(movement_melted, aes(x=Metric, y=Score, fill=Room)) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  # geom_violin(color="black",
  #             draw_quantiles=TRUE,
  #             trim=TRUE,
  #             alpha=0.7,
  #             #linewidth=0.8,
  #             linewidth=0.6,
  #             adjust=8.0,
  #             position=position_dodge(width=0.7)) +
  # stat_summary(color="black",
  #              #aes(color=Condition),
  #              show.legend = FALSE,
  #              fun = "mean",
  #              geom = "point", 
  #              #width = 0.5,
  #              position=position_dodge(width=0.7)) +#,
  scale_fill_manual(values=c(Office=office_color,
                             Conference=conference_color)) +
  # facet_grid(Room~.,
  #            space="fixed",
  #            margins = "vs") +
  labs(
    title="Movement (AR)",
    x="Movement Metric",
    y="Movement Ratio") +
  #theme_classic() +
  scale_y_continuous(trans="log10",
                     limits=c(1e-01, 1e+02)) + 
  coord_flip() +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.text = element_text(size=12),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks=element_line(size=1.0),
    # Legend
    legend.position = "bottom",
    #legend.position = "right",
    legend.background = element_rect(color="black", size=0.5, linetype="dashed"),
    legend.title = element_text(size=14, face="plain", hjust=0.5),
    legend.text = element_text(size=12),
    legend.direction = "horizontal",
    #legend.direction = "vertical",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  )
plot_and_savepdf(plot_movement, "Movement")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Body Movement ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
movement_melted <- melt(select(subjects_data, c("Condition",
                                                "Room",
                                                "Optimal",
                                                "Movement.Ratio",
                                                "Optimal.Movement.Ratio", 
                                                "Poor.Movement.Ratio", 
                                                "Head.Ratio", 
                                                "Optimal.Head.Ratio", 
                                                "Poor.Head.Ratio", 
                                                "Gaze.Ratio", 
                                                "Optimal.Gaze.Ratio", 
                                                "Poor.Gaze.Ratio", 
                                                "SUS_Score")), 
                        id = c("Condition", "Room", "Optimal", "SUS_Score"),
                        variable.name = "Metric",
                        value.name = "Score")

## Movement data
plot_movement <- ggplot(subset(movement_melted, grepl("Movement", Metric)), aes(x=Metric, y=Score, fill=Optimal)) +
  geom_boxplot(alpha=0.7,
               #width=0.6,
               size=0.5,
               color="black") +
  geom_jitter(color="black",
              size=0.4,
              alpha=0.9) +
  scale_fill_manual(values=c(poor_color, optimal_color)) +
  #scale_x_discrete(limits = rev(levels(usability_melted$Metric))) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  labs(
    title="Movement and Performance - Movement",
    x="Movement Metric",
    y="Movement Ratio") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_movement, "BodyMovement_Performance")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Head Movement ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Head data
plot_movement <- ggplot(subset(movement_melted, grepl("Head", Metric)), aes(x=Metric, y=Score, fill=Optimal)) +
  geom_boxplot(alpha=0.7,
               #width=0.6,
               size=0.5,
               color="black") +
  geom_jitter(color="black",
              size=0.4,
              alpha=0.9) +
  scale_fill_manual(values=c(poor_color, optimal_color)) +
  #scale_x_discrete(limits = rev(levels(usability_melted$Metric))) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  labs(
    title="Movement and Performance - Head",
    x="Movement Metric",
    y="Movement Ratio") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_movement, "HeadRotation_Performance")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Gaze ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## Gaze data
plot_movement <- ggplot(subset(movement_melted, grepl("Gaze", Metric)), aes(x=Metric, y=Score, fill=Optimal)) +
  geom_boxplot(alpha=0.7,
               #width=0.6,
               size=0.5,
               color="black") +
  geom_jitter(color="black",
              size=0.4,
              alpha=0.9) +
  scale_fill_manual(values=c(poor_color, optimal_color)) +
  #scale_x_discrete(limits = rev(levels(usability_melted$Metric))) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  labs(
    title="Movement and Performance - Gaze",
    x="Movement Metric",
    y="Movement Ratio") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_movement, "EyeGaze_Performance")

rm(movement_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Rankings (All) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
        
# Re-work data
ranks_melted <- melt(select(subjects_data, c("Condition", "Room", "DL_Rank", "VF_Rank", "G_Rank")), 
                     id = c("Condition", "Room"),
                     variable.name = "Metric",
                     value.name = "Rank")
ranks_melted$Metric <- factor(ranks_melted$Metric,
                              levels=c("DL_Rank", "G_Rank", "VF_Rank"))

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Metric)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_x_discrete(limits = rev(c("DL_Rank", "G_Rank", "VF_Rank")),
                   labels = c(G_Rank="Brightness\nDiscomfort",
                              VF_Rank="View\nFactor",
                              DL_Rank="Daylighting")) +
  scale_fill_manual(values=c(DL_Rank=dayighting_color,
                             VF_Rank=viewfactor_color,
                             G_Rank=glare_color),
                    labels = c(G_Rank="Brightness\nDiscomfort",
                               VF_Rank="View\nFactor",
                               DL_Rank="Daylighting")) +
  coord_flip() +
  labs(
    title="Final Selection Rankings - Individual Design Variables",
    x="Metric",
    y="Rank") +
  theme_bw() +
  theme(legend.position="none") +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.title.y = element_blank(),
    axis.text = element_text(size=12),
    axis.text.y = element_blank(),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks = element_line(size=1.0),
    axis.ticks.y = element_blank(),
    # Legend
    #legend.position = "bottom",
    legend.position = "right",
    legend.background = element_rect(color="black", size=0.5, linetype="dashed"),
    legend.title = element_text(size=14, face="plain", hjust=0.5),
    legend.text = element_text(size=12),
    legend.spacing.y = unit(0.5, "lines"),
    #legend.direction = "horizontal",
    legend.direction = "vertical",
    legend.key.size = unit(1.5, "lines"),
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  ) + 
  guides(fill = guide_legend(byrow = TRUE)) # For legend spacing
plot_and_savepdf(plot_ranks, "Rankings-All")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Rankings (Overall) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
        
# Re-work data
ranks_melted <- melt(select(subjects_data, c("Condition", "Room", "Rank")), 
                     id = c("Condition", "Room"),
                     variable.name = "Metric",
                     value.name = "Rank")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Metric)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_point(position="jitter",
             alpha=0.25,
             size=1.5,
             color="black",
             shape=16) +
  geom_boxplot(alpha=0.7,
               width=0.4,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(rank_color)) +
  coord_flip() +
  labs(
    title="Final Selection Rankings - Overall Ranking",
    x="Metric",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.title.y = element_blank(),
    axis.text = element_text(size=12),
    axis.text.y = element_blank(),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks = element_line(size=1.0),
    axis.ticks.y = element_blank(),
    # Legend
    legend.position = "none",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  )
plot_and_savepdf(plot_ranks, "Rankings-Overall")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Rankings (All - Optimal/Nonoptimal) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Condition", "Room", "Optimal", "DL_Rank", "VF_Rank", "G_Rank")), 
                     id = c("Condition", "Room", "Optimal"),
                     variable.name = "Metric",
                     value.name = "Rank")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Optimal)) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(poor_color, optimal_color)) +
  labs(
    title="Final Selection Rankings",
    x="Metric",
    y="Rank") +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_ranks, "Rankings-All-Optimal")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Rankings (Overall - Optimal/Nonoptimal) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Condition", "Room", "Optimal", "Rank")), 
                     id = c("Condition", "Room", "Optimal"),
                     variable.name = "Metric",
                     value.name = "Rank")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Optimal)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.4,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_fill_manual(values=c(poor_color, optimal_color)) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  labs(
    title="Final Selection Rankings",
    x="Metric",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_ranks, "Rankings-Overall-Optimal")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Rankings (All - High/Low Head Movers) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Condition", "Room", "Movement.Classification", "DL_Rank", "VF_Rank", "G_Rank")), 
                     id = c("Condition", "Room", "Movement.Classification"),
                     variable.name = "Metric",
                     value.name = "Rank")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Movement.Classification)) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(
    title="Final Selection Rankings - Head Movement",
    x="Metric",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_ranks, "Rankings-All-HeadMovement")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Rankings (Overall - High/Low Head Movers) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Condition", "Room", "Movement.Classification", "Rank")), 
                     id = c("Condition", "Room", "Movement.Classification"),
                     variable.name = "Metric",
                     value.name = "Rank")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Movement.Classification)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(
    title="Final Selection Rankings - Head Movement",
    x="Metric",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_ranks, "Rankings-Overall-HeadMovement")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Plot - Rankings (All - High/Low Head Rotation) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Condition", "Room", "Rotation.Classification", "DL_Rank", "VF_Rank", "G_Rank")), 
                     id = c("Condition", "Room", "Rotation.Classification"),
                     variable.name = "Metric",
                     value.name = "Rank")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Rotation.Classification)) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(
    title="Final Selection Rankings - Head Rotation",
    x="Metric",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_ranks, "Rankings-All-HeadRotation")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Plot - Rankings (Overall - High/Low Head Rotation) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Condition", "Room", "Rotation.Classification", "Rank")), 
                     id = c("Condition", "Room", "Rotation.Classification"),
                     variable.name = "Metric",
                     value.name = "Rank")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Rotation.Classification)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(
    title="Final Selection Rankings - Head Rotation",
    x="Metric",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_ranks, "Rankings-Overall-HeadRotation")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Plot - Rankings (All - High/Low Gaze) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Room", "Condition", "Gaze.Classification", "DL_Rank", "VF_Rank", "G_Rank")), 
                     id = c("Room", "Condition", "Gaze.Classification"),
                     variable.name = "Metric",
                     value.name = "Rank")

plot_ranks <- ggplot(subset(ranks_melted, Room=="Conference" & Condition=="AR"), aes(x=Metric, y=Rank, fill=Gaze.Classification)) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(.~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_x_discrete(limits = rev(c("DL_Rank", "G_Rank", "VF_Rank")),
                   labels = c(G_Rank="Brightness\nDiscomfort",
                              VF_Rank="View\nFactor",
                              DL_Rank="Daylighting")) +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(fill="Participant Eye Movement Classification",
       caption="Rank: Lower = More optimal") +
  coord_flip() +
  labs(
    title="Final Selection Rankings - Eye Gaze",
    #x="Metric",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.title.y = element_blank(),
    axis.text = element_text(size=12),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks=element_line(size=1.0),
    # Legend
    legend.position = "bottom",
    #legend.position = "right",
    legend.background = element_rect(color="black", size=0.5, linetype="dashed"),
    legend.title = element_text(size=14, face="plain", hjust=0.5),
    legend.text = element_text(size=12),
    legend.direction = "horizontal",
    #legend.direction = "vertical",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
  )
plot_and_savepdf(plot_ranks, "Rankings-All-EyeGaze")
rm(ranks_melted) # GERE

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Rankings (Head Movement Overall 2) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subset(subjects_data, Condition=="AR"),
                            c("Room", "Movement.Classification", "Optimal.Movement.Classification", "Poor.Movement.Classification", "Rank")), 
                     id = c("Room", "Rank"),
                     variable.name = "Metric",
                     value.name = "Value")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Value)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(.~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(
    title="Final Selection Rankings - Head Movement",
    x="Metric",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_ranks, "Rankings-Overall-HeadMovement2")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Rankings (Head Rotation Overall 2) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subset(subjects_data, Condition=="AR"),
                            c("Room", "Rotation.Classification", "Optimal.Rotation.Classification", "Poor.Rotation.Classification", "Rank")), 
                     id = c("Room", "Rank"),
                     variable.name = "Metric",
                     value.name = "Value")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Value)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(.~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(
    title="Final Selection Rankings - Head Rotation",
    x="Metric",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_ranks, "Rankings-Overall-HeadRotation2")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Rankings (Eye Gaze Overall 2) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subset(subjects_data, Condition=="AR"),
                            c("Room", "Gaze.Classification", "Optimal.Gaze.Classification", "Poor.Gaze.Classification", "Rank")), 
                     id = c("Room", "Rank"),
                     variable.name = "Metric",
                     value.name = "Value")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Value)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(.~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(
    title="Final Selection Rankings - Eye Gaze",
    x="Metric",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_ranks, "Rankings-Overall-EyeGaze2")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Rankings - All Movement Types (Office) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subset(subjects_data, Condition=="AR" & Room=="Office"),
                            c("Room",
                              "Rank",
                              "Movement.Classification",
                              "Optimal.Movement.Classification",
                              "Poor.Movement.Classification",
                              "Rotation.Classification",
                              "Optimal.Rotation.Classification",
                              "Poor.Rotation.Classification",
                              "Gaze.Classification",
                              "Optimal.Gaze.Classification",
                              "Poor.Gaze.Classification")), 
                     id = c("Room", "Rank"),
                     variable.name = "Metric",
                     value.name = "Value")
ranks_melted$Type <- NA
ranks_melted[grepl("Movement", ranks_melted$Metric, fixed = TRUE),]$Type <- "Head Movement"
ranks_melted[grepl("Rotation", ranks_melted$Metric, fixed = TRUE),]$Type <- "Head Rotation"
ranks_melted[grepl("Gaze", ranks_melted$Metric, fixed = TRUE),]$Type <- "Eye Gaze"
ranks_melted$Facade <- "All"
ranks_melted[grepl("Poor", ranks_melted$Metric, fixed = TRUE),]$Facade <- "Poor"
ranks_melted[grepl("Optimal", ranks_melted$Metric, fixed = TRUE),]$Facade <- "Optimal"

plot_ranks <- ggplot(ranks_melted, aes(x=Facade, y=Rank, fill=Value)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(.~Type,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_x_discrete(labels=c(Poor="When\nPoor\nFacades\nVisible",
                            Optimal="When\nOptimal\nFacades\nVisible",
                            All="When\nAll\nFacades\nVisible")) +
  scale_fill_manual(name="Movement Quantity",
                    values=c(High=high_movement_color,
                             Low=low_movement_color),
                    labels=c(High="Utilized More Movement",
                             Low="Utilized Less Movement")) +
  coord_flip() +
  labs(
    title="Final Selection Rankings - Office",
    x="Classification of Facade",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.text = element_text(size=12),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks=element_line(size=1.0),
    # Legend
    legend.position = "bottom",
    #legend.position = "right",
    legend.background = element_rect(color="black", size=0.5, linetype="dashed"),
    legend.title = element_text(size=14, face="plain", hjust=0.5),
    legend.text = element_text(size=12),
    legend.direction = "horizontal",
    #legend.direction = "vertical",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    #strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  )
plot_and_savepdf(plot_ranks, "Rankings-Overall-Office")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Rankings - All Movement Types (Conference) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subset(subjects_data, Condition=="AR" & Room=="Conference"),
                            c("Room",
                              "Rank",
                              "Movement.Classification",
                              "Optimal.Movement.Classification",
                              "Poor.Movement.Classification",
                              "Rotation.Classification",
                              "Optimal.Rotation.Classification",
                              "Poor.Rotation.Classification",
                              "Gaze.Classification",
                              "Optimal.Gaze.Classification",
                              "Poor.Gaze.Classification")), 
                     id = c("Room", "Rank"),
                     variable.name = "Metric",
                     value.name = "Value")
ranks_melted$Type <- NA
ranks_melted[grepl("Movement", ranks_melted$Metric, fixed = TRUE),]$Type <- "Head Movement"
ranks_melted[grepl("Rotation", ranks_melted$Metric, fixed = TRUE),]$Type <- "Head Rotation"
ranks_melted[grepl("Gaze", ranks_melted$Metric, fixed = TRUE),]$Type <- "Eye Gaze"
ranks_melted$Facade <- "All"
ranks_melted[grepl("Poor", ranks_melted$Metric, fixed = TRUE),]$Facade <- "Poor"
ranks_melted[grepl("Optimal", ranks_melted$Metric, fixed = TRUE),]$Facade <- "Optimal"

plot_ranks <- ggplot(ranks_melted, aes(x=Facade, y=Rank, fill=Value)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(.~Type,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_x_discrete(labels=c(Poor="When\nPoor\nFacades\nVisible",
                            Optimal="When\nOptimal\nFacades\nVisible",
                            All="When\nAll\nFacades\nVisible")) +
  scale_fill_manual(name="Movement Quantity",
                     values=c(High=high_movement_color,
                              Low=low_movement_color),
                     labels=c(High="Utilized More Movement",
                              Low="Utilized Less Movement")) +
  coord_flip() +
  labs(
    title="Final Selection Rankings - Conference",
    x="Classification of Facade",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.text = element_text(size=12),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks=element_line(size=1.0),
    # Legend
    legend.position = "bottom",
    #legend.position = "right",
    legend.background = element_rect(color="black", size=0.5, linetype="dashed"),
    legend.title = element_text(size=14, face="plain", hjust=0.5),
    legend.text = element_text(size=12),
    legend.direction = "horizontal",
    #legend.direction = "vertical",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    #strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  )
plot_and_savepdf(plot_ranks, "Rankings-Overall-Conference")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Difference Pre and Post (All) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Room", "Condition", "Optimal", "VF_Rank_Diff", "DL_Rank_Diff", "G_Rank_Diff")), 
                     id = c("Room", "Condition", "Optimal"),
                     variable.name = "Metric")
ranks_melted$Metric <- factor(ranks_melted$Metric,
                              levels=c("DL_Rank_Diff", "G_Rank_Diff", "VF_Rank_Diff"))

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=value, fill=Metric)) +
  geom_hline(yintercept=0,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(-15, 15)),
                  breaks=rev(c(-15, -10, -5, 0, 5, 10, 15)),
                  labels=c("15", "10\nRegression", "5", "0", "5", "10\nImprovement", "15")) +
  scale_x_discrete(limits = rev(c("DL_Rank_Diff", "G_Rank_Diff", "VF_Rank_Diff")),
                   labels = c(G_Rank_Diff="Brightness\nDiscomfort",
                              VF_Rank_Diff="View\nFactor",
                              DL_Rank_Diff="Daylighting")) +
  scale_fill_manual(values=c(DL_Rank_Diff=dayighting_color,
                             VF_Rank_Diff=viewfactor_color,
                             G_Rank_Diff=glare_color),
                    labels = c(G_Rank_Diff="Brightness\nDiscomfort",
                               VF_Rank_Diff="View\nFactor",
                               DL_Rank_Diff="Daylighting")) +
  labs(
    title="A Priori Vs. Final Selection - Individual Design Variables",
    x="Metric",
    y="Value") +
  coord_flip() +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.title.y = element_blank(),
    axis.text = element_text(size=12),
    axis.text.y = element_blank(),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks = element_line(size=1.0),
    axis.ticks.y = element_blank(),
    # Legend
    #legend.position = "bottom",
    legend.position = "right",
    legend.background = element_rect(color="black", size=0.5, linetype="dashed"),
    legend.title = element_text(size=14, face="plain", hjust=0.5),
    legend.text = element_text(size=12),
    legend.spacing.y = unit(0.5, "lines"),
    #legend.direction = "horizontal",
    legend.direction = "vertical",
    legend.key.size = unit(1.5, "lines"),
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  ) + 
  guides(fill = guide_legend(byrow = TRUE)) # For legend spacing
plot_and_savepdf(plot_ranks, "Difference-All")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Difference Pre and Post (Overall) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Room", "Condition", "Rank_Diff")), 
                     id = c("Room", "Condition"),
                     variable.name = "Metric")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=value, fill=Metric)) +
  geom_hline(yintercept=0,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_point(position="jitter",
             alpha=0.25,
             size=1.5,
             color="black",
             shape=16) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(-15, 15)),
                  breaks=rev(c(-15, -10, -5, 0, 5, 10, 15)),
                  labels=c("15", "10\nRegression", "5", "0", "5", "10\nImprovement", "15")) +
  scale_fill_manual(values=c(rank_color)) +
  labs(
    title="A Priori Vs. Final Selection - Overall Ranking",
    x="Metric",
    y="Value") +
  coord_flip() +
  theme_bw() +
  theme(legend.position="none") +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.title.y = element_blank(),
    axis.text = element_text(size=12),
    axis.text.y = element_blank(),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks = element_line(size=1.0),
    axis.ticks.y = element_blank(),
    # Legend
    legend.position = "none",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  )
plot_and_savepdf(plot_ranks, "Difference-Overall")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Pre and Post Ranks (All) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Room", "Condition", "Rank", "Rank_Pre")), 
                     id = c("Room", "Condition"),
                     variable.name = "Metric")
plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=value, fill=Metric)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(optimal_color, poor_color)) +
  labs(
    title="Pre Vs. Post Difference",
    x="Metric",
    y="Rank") +
  theme_bw() +
  theme(legend.position="none") + 
  coord_flip()
plot_and_savepdf(plot_ranks, "Difference2-Overall")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Difference in High Vs. Low Head Movers ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Room", "Condition", "Movement.Classification", "Rank_Diff")), 
                     id = c("Room", "Condition", "Movement.Classification"),
                     variable.name = "Metric")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=value, fill=Movement.Classification)) +
  geom_hline(yintercept=0,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse() +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(
    title="Pre Vs. Post Difference",
    x="Metric",
    y="Value") +
  theme_bw() +
  #theme(legend.position="none") +
  coord_flip()
plot_and_savepdf(plot_ranks, "Difference-BodyMovement")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Difference in High Vs. Low Head Rotation ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Room", "Condition", "Rotation.Classification", "Rank_Diff")), 
                     id = c("Room", "Condition", "Rotation.Classification"),
                     variable.name = "Metric")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=value, fill=Rotation.Classification)) +
  geom_hline(yintercept=0,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse() +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(
    title="Pre Vs. Post Difference",
    x="Metric",
    y="Value") +
  theme_bw() +
  #theme(legend.position="none") +
  coord_flip()
plot_and_savepdf(plot_ranks, "Difference-HeadRotation")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Difference in High Vs. Low Head Eye Gazers ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Room", "Condition", "Gaze.Classification", "Rank_Diff")), 
                     id = c("Room", "Condition", "Gaze.Classification"),
                     variable.name = "Metric")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=value, fill=Gaze.Classification)) +
  geom_hline(yintercept=0,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse() +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(
    title="Pre Vs. Post Difference",
    x="Metric",
    y="Value") +
  theme_bw() +
  #theme(legend.position="none") +
  coord_flip()
plot_and_savepdf(plot_ranks, "Difference-EyeGaze")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Plot - Rankings (Overall - High/Low Gaze) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Room", "Gaze.Classification", "Rank")), 
                     id = c("Room", "Gaze.Classification"),
                     variable.name = "Metric",
                     value.name = "Rank")

plot_ranks <- ggplot(ranks_melted, aes(x=Metric, y=Rank, fill=Gaze.Classification)) +
  geom_hline(yintercept=8,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(.~Room,
             space="fixed",
             margins = "vs") +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  labs(
    title="Final Selection Rankings - Eye Gaze",
    x="Metric",
    y="Rank") +
  #theme_classic() +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_ranks, "Rankings-Overall-EyeGaze")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Movement Classifications and Optimal Selections (Heatmap) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Format our data
count_optimal <- function(condition, optimal, movement_metric){
  count_dataframe <- subset(subjects_data, Condition=="AR") %>%
    group_by_at(c("Room", movement_metric)) %>%
    summarise(Count = sum(Optimal==optimal))
  count_dataframe$Metric <- movement_metric
  count_dataframe$Optimal <- optimal
  colnames(count_dataframe)[colnames(count_dataframe) == movement_metric] ="Classification"
  return(count_dataframe)
}
# Create empty dataset to fill
tilemap_data <- data.frame(matrix(ncol=6, nrow=0)) 
colnames(tilemap_data) <- c("Room", "Classification", "Count",  "Metric", "Optimal")
# Fill it with tilemap-friendly data structure counting optimal and poor subjects for each
tilemap_data <- rbind(tilemap_data, count_optimal("AR", TRUE, "Movement.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", FALSE, "Movement.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", TRUE, "Rotation.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", FALSE, "Rotation.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", TRUE, "Gaze.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", FALSE, "Gaze.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", TRUE, "Optimal.Movement.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", FALSE, "Optimal.Movement.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", TRUE, "Optimal.Rotation.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", FALSE, "Optimal.Rotation.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", TRUE, "Optimal.Gaze.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", FALSE, "Optimal.Gaze.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", TRUE, "Poor.Movement.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", FALSE, "Poor.Movement.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", TRUE, "Poor.Rotation.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", FALSE, "Poor.Rotation.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", TRUE, "Poor.Gaze.Classification"))
tilemap_data <- rbind(tilemap_data, count_optimal("AR", FALSE, "Poor.Gaze.Classification"))
# Reorder and rename metric factors for aesthetics in the plot
tilemap_data$Metric <- as.factor(tilemap_data$Metric)
levels(tilemap_data$Metric) <- c("G", "HM", "G-Opt", "HM-Opt", "HR-Opt", "G-Poor", "HM-Poor", "HR-Poor", "HR") # Rename
 # Reorder

plot_movement_tilemap <- ggplot(tilemap_data, aes(x=Optimal, y=Classification, fill=Count)) +
  geom_tile(color="Black",
            lwd=1.0) +
  geom_text(aes(label=Count),
            color="red",
            size=3) +
  facet_grid(factor(Metric, levels=c("HM-Opt", "HM", "HM-Poor", "HR-Opt", "HR", "HR-Poor", "G-Opt", "G", "G-Poor"))
             ~ Room,
             space="fixed",
             margins = "vs") +
  scale_x_discrete(limits=c("FALSE", "TRUE"), labels=c("Poor Participants", "Optimal Participants")) + 
  scale_y_discrete(limits=c("Low", "High"), labels=c("Low Movement", "High Movement")) + 
  scale_fill_distiller(palette = "Greys",
                       direction=1) +
  theme_bw() 
plot_and_savepdf(plot_movement_tilemap, "MovementClassifications-Optimal")
rm(tilemap_data)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Movement Classifications and Optimal Selections (Bar Plot)  ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Create dataset
movement_classification_group <- function(metric){
  g <- subset(subjects_data, Condition=="AR") %>%
    group_by_at(c("Room", metric)) %>%
    summarise(n = n(),
              n.optimal = sum(Optimal==TRUE),
              n.poor = sum(Optimal==FALSE),
              diff = n.optimal - n.poor)
  colnames(g)[2] <- "MovementLevel" # Rename metric column
  g$MovementType <- metric
  return(g)
}
movement_summary <- movement_classification_group("Movement.Classification")
movement_summary <- rbind(movement_summary, movement_classification_group("Rotation.Classification"))
movement_summary <- rbind(movement_summary, movement_classification_group("Gaze.Classification"))
movement_summary <- rbind(movement_summary, movement_classification_group("Optimal.Movement.Classification"))
movement_summary <- rbind(movement_summary, movement_classification_group("Optimal.Rotation.Classification"))
movement_summary <- rbind(movement_summary, movement_classification_group("Optimal.Gaze.Classification"))
movement_summary <- rbind(movement_summary, movement_classification_group("Poor.Movement.Classification"))
movement_summary <- rbind(movement_summary, movement_classification_group("Poor.Rotation.Classification"))
movement_summary <- rbind(movement_summary, movement_classification_group("Poor.Gaze.Classification"))
# Color dataset
movement_summary$Body <- NA
movement_summary[grepl("Movement", movement_summary$MovementType, fixed = TRUE),]$Body <- "HeadMovement"
movement_summary[grepl("Rotation", movement_summary$MovementType, fixed = TRUE),]$Body <- "HeadRotation"
movement_summary[grepl("Gaze", movement_summary$MovementType, fixed = TRUE),]$Body <- "EyeGaze"
movement_summary$Positive <- movement_summary$diff >= 0
# Group and re-order
movement_summary$MovementType <- factor(movement_summary$MovementType,
                                        levels=c("Movement.Classification",
                                                 "Optimal.Movement.Classification",
                                                 "Poor.Movement.Classification",
                                                 "Rotation.Classification",
                                                 "Optimal.Rotation.Classification",
                                                 "Poor.Rotation.Classification",
                                                 "Gaze.Classification",
                                                 "Optimal.Gaze.Classification",
                                                 "Poor.Gaze.Classification"),
                                        labels=c("HM",
                                                 "HM on Opt",
                                                 "HM on Poor",
                                                 "HR",
                                                 "HR on Opt",
                                                 "HR on Poor",
                                                 "EG",
                                                 "EG on Opt",
                                                 "EG on Poor"))

geom_diff <- function(room, movementType) {
  color <- compliment_color
  fill <- color
  data <- subset(movement_summary, Room==room & MovementType==movementType)
  base <- geom_rect(data=data,
                    xmin=min(data$diff),
                    xmax=max(data$diff),
                    ymin=1.48,
                    ymax=1.52,
                    color=color, fill=fill, alpha=1.0)
  left <- geom_rect(data=data,
                    xmin=min(data$diff),
                    xmax=min(data$diff + 0.1),
                    ymin=1.3,
                    ymax=1.7,
                    color=color, fill=fill, alpha=1.0)
  right <- geom_rect(data=data,
                     xmin=max(data$diff),
                     xmax=max(data$diff - 0.1),
                     ymin=1.3,
                     ymax=1.7,
                     color=color, fill=fill, alpha=1.0)
  return(c(base, left, right))
}

plot_movement_bar <- ggplot(movement_summary, aes(x=diff, y=MovementLevel)) +
  geom_bar(aes(x=n.optimal),
           stat="identity",
           alpha=0.2,
           size=0.75,
           width=0.75,
           #color="blue",
           color="lightblue",
           fill="lightblue") +
  geom_bar(aes(x=-n.poor),
           stat="identity",
           alpha=0.2,
           size=0.75,
           width=0.75,
           #color="red",
           color="pink",
           fill="pink") +
  geom_bar(aes(fill=Positive),
           stat="identity",
           alpha=1.0,
           width=0.75) +
           #fill="gray50") +
  geom_vline(xintercept=0,
             linewidth=1.0,
             color=plot_outline_color) +
  geom_diff("Conference", "HM") + 
  geom_diff("Conference", "HM on Opt") + 
  geom_diff("Conference", "HM on Poor") + 
  geom_diff("Conference", "HR") + 
  geom_diff("Conference", "HR on Opt") + 
  geom_diff("Conference", "HR on Poor") + 
  geom_diff("Conference", "EG") + 
  geom_diff("Conference", "EG on Opt") + 
  geom_diff("Conference", "EG on Poor") +
  geom_diff("Office", "HM") + 
  geom_diff("Office", "HM on Opt") + 
  geom_diff("Office", "HM on Poor") + 
  geom_diff("Office", "HR") + 
  geom_diff("Office", "HR on Opt") + 
  geom_diff("Office", "HR on Poor") + 
  geom_diff("Office", "EG") + 
  geom_diff("Office", "EG on Opt") + 
  geom_diff("Office", "EG on Poor") +
  facet_grid(factor(MovementType, )
             ~ Room,
             space="fixed",
             margins = "vs") +
  scale_fill_manual(values = c("red", "blue")) +
  scale_x_continuous(limits=c(-12, 12),
                     breaks=c(-10, -5, 0, 5, 10),
                     labels=c("10\nFavored by Low Performers", "5", "0", "5", "10\nFavored by High Performers")) +
  labs(title="Movement and Performance",
       x="# of Participants",
       y="Amount of Movement") +
  theme_bw() +
  theme(# Text
        text = element_text(family="serif"), # Times New Roman
        plot.title = element_text(size=18, face="bold", hjust=0.5), 
        axis.title = element_text(size=18, face="italic", hjust=0.5),
        axis.text = element_text(size=12),
        # Axis Ticks
        #axis.ticks = element_blank(),
        axis.ticks=element_line(size=1.0),
        # Legend
        legend.position="none",
        # legend.background = element_rect(color="black", size=0.5, linetype="dashed"),
        # legend.position = c(0.23, 0.91),
        # legend.title = element_text(size=14, face="italic", hjust=0.5),
        # legend.text = element_text(size=12),
        # legend.direction = "horizontal",
        # Borders
        panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
        panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
        panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
        # Facet
        strip.background=element_rect(colour=plot_outline_color,
                                      fill=NA,
                                      linewidth=plot_outline_size),
        strip.text.x = element_text(size=12),
        strip.text.y = element_text(size=8),
        strip.placement = "inside",
        #panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines")
        )
plot_and_savepdf(plot_movement_bar, "MovementClassifications-Optimal2", 8, 10)
rm(movement_summary)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Movement Ratio Vs. Usability ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ratios_melted <- melt(select(subjects_data, c("Room",
                                              "Condition",
                                              "Movement.Ratio",
                                              "SUS_Score",
                                              "Mental_Demand_Score",
                                              "Physical_Demand_Score",
                                              "Temporal_Demand_Score",
                                              "Performance_Demand_Score",
                                              "Effort_Demand_Score")), 
                     id = c("Room", "Condition", "Movement.Ratio"),
                     variable.name = "Metric",
                     value.name = "Score")

plot_ratios_usability <- ggplot(ratios_melted, aes(x=Movement.Ratio, y=Score, fill=Metric, color=Metric)) +
  geom_point(aes(shape=Metric),
             size=1,
             alpha=0.5) +
  geom_smooth(method=lm,
              alpha=0.75,
              se=FALSE) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  labs(
    title="Usability Vs. Movement - Body",
    x="Ratio",
    y="Usability Score") +
  theme_bw() 
plot_and_savepdf(plot_ratios_usability, "Usability_Movement-BodyMovement")
rm(ratios_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Head Rotation Ratio Vs. Usability ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ratios_melted <- melt(select(subjects_data, c("Room",
                                              "Condition",
                                              "Head.Ratio",
                                              "SUS_Score",
                                              "Mental_Demand_Score",
                                              "Physical_Demand_Score",
                                              "Temporal_Demand_Score",
                                              "Performance_Demand_Score",
                                              "Effort_Demand_Score")), 
                      id = c("Room", "Condition", "Head.Ratio"),
                      variable.name = "Metric",
                      value.name = "Score")

plot_ratios_usability <- ggplot(ratios_melted, aes(x=Head.Ratio, y=Score, fill=Metric, color=Metric)) +
  geom_point(aes(shape=Metric),
             size=1,
             alpha=0.5) +
  geom_smooth(method=lm,
              alpha=0.75,
              se=FALSE) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  labs(
    title="Usability Vs. Movement - Head Rotation",
    x="Ratio",
    y="Usability Score") +
  theme_bw() 
plot_and_savepdf(plot_ratios_usability, "Usability_Movement-HeadRotation")
rm(ratios_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Gaze Ratio Vs. Usability ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ratios_melted <- melt(select(subjects_data, c("Room",
                                              "Gaze.Ratio",
                                              "SUS_Score",
                                              "Mental_Demand_Score",
                                              "Physical_Demand_Score",
                                              "Temporal_Demand_Score",
                                              "Performance_Demand_Score",
                                              "Effort_Demand_Score")), 
                      id = c("Room", "Gaze.Ratio"),
                      variable.name = "Metric",
                      value.name = "Score")

plot_ratios_usability <- ggplot(ratios_melted, aes(x=Gaze.Ratio, y=Score, fill=Metric, color=Metric)) +
  geom_point(aes(shape=Metric),
             size=1,
             alpha=0.5) +
  geom_smooth(method=lm,
              alpha=0.75,
              se=FALSE) +
  facet_grid(.~Room,
             space="fixed",
             margins = "vs") +
  labs(
    title="Usability Vs. Movement - Eye Gaze",
    x="Ratio",
    y="Usability Score") +
  theme_bw() 
plot_and_savepdf(plot_ratios_usability, "Usability_Movement-EyeGaze")
rm(ratios_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Prev. Experience Vs. Usability ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ratios_melted <- melt(select(subjects_data, c("Room",
                                              "Condition",
                                              "Participant.ID",
                                              "SUS_Score",
                                              "Mental_Demand_Score",
                                              "Physical_Demand_Score",
                                              "Temporal_Demand_Score",
                                              "Performance_Demand_Score",
                                              "Effort_Demand_Score")), 
                      id = c("Room", "Condition", "Participant.ID"),
                      variable.name = "Metric",
                      value.name = "Metric.Score")
exp_melted <- melt(select(subjects_data, c("Room",
                                           "Condition",
                                           "Participant.ID",
                                           #"Prev.XR.Experience",
                                           "Prev.VR.Experience",
                                           "Prev.AR.Experience",
                                           "Prev.3D.Modeling.Experience",
                                           "Prev.Computer.Experience",
                                           "Construction.Familiarity")), 
                   id = c("Room", "Condition", "Participant.ID"),
                   variable.name = "Experience.Type",
                   value.name = "Experience.Type.Score")
merged <- merge(ratios_melted, exp_melted, by=c("Room", "Condition", "Participant.ID"))

plot_experience_usability <- ggplot(merged, aes(x=Experience.Type.Score, y=Metric.Score, fill=Metric, color=Metric)) +
  # geom_point(aes(shape=Metric),
  #            size=1,
  #            alpha=0.5) +
  geom_smooth(aes(linetype=Experience.Type),
              method=lm,
              alpha=0.75,
              se=FALSE) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  #xlim(1,7) + 
  #ylim(0, 100) +
  coord_cartesian(xlim=c(1, 7),
                  ylim=c(0, 100)) +
  labs(
    title="Usability Vs. Previous Experience",
    x="Experience Score",
    y="Usability Score") +
  theme_bw()
plot_and_savepdf(plot_experience_usability, "Usability_Experience")
# Make alternative versions
plot_and_savepdf(plot_experience_usability %+% subset(merged, Metric=="SUS_Score"), "Usability_Experience_SUS")
plot_and_savepdf(plot_experience_usability %+% subset(merged, Metric=="Mental_Demand_Score"), "Usability_Experience_Mental")
plot_and_savepdf(plot_experience_usability %+% subset(merged, Metric=="Physical_Demand_Score"), "Usability_Experience_Physical")
plot_and_savepdf(plot_experience_usability %+% subset(merged, Metric=="Temporal_Demand_Score"), "Usability_Experience_Temporal")
plot_and_savepdf(plot_experience_usability %+% subset(merged, Metric=="Effort_Demand_Score"), "Usability_Experience_Effort")
plot_and_savepdf(plot_experience_usability %+% subset(merged, Metric=="Performance_Demand_Score"), "Usability_Experience_Perf")
# Clean env
rm(ratios_melted)
rm(exp_melted)
rm(merged)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Usability Vs. Ranks ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Room",
                                             "Condition",
                                             "Rank",
                                             "SUS_Score",
                                             "Mental_Demand_Score",
                                             "Physical_Demand_Score",
                                             "Temporal_Demand_Score",
                                             "Performance_Demand_Score",
                                             "Effort_Demand_Score")), 
                     id = c("Room", "Condition", "Rank"),
                     variable.name = "Metric",
                     value.name = "Score")

plot_ranks_usability <- ggplot(ranks_melted, aes(x=Score, y=Rank, fill=Metric, color=Metric)) +
  geom_point(aes(shape=Metric),
             size=1,
             alpha=0.75) +
  geom_hline(yintercept=8,
             linetype="dashed",
             linewidth=2,
             color="black") +
  geom_smooth(method=loess,
              alpha=0.75,
              se=FALSE) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  labs(
    title="Selection Rank Vs Usability",
    x="Usability Score",
    y="Rank") +
  theme_bw() 
plot_and_savepdf(plot_ranks_usability, "Usability_Ranks")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Usability Vs. Ranks 2 ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Room",
                                             "Condition",
                                             "Rank",
                                             "SUS_Score",
                                             "Mental_Demand_Score",
                                             "Physical_Demand_Score",
                                             "Temporal_Demand_Score",
                                             "Performance_Demand_Score",
                                             "Effort_Demand_Score")), 
                     id = c("Room", "Condition", "Rank"),
                     variable.name = "Metric",
                     value.name = "Score")
ranks_melted$Metric <- factor(ranks_melted$Metric,
                              levels=c("Mental_Demand_Score", "Temporal_Demand_Score", "Performance_Demand_Score", "Effort_Demand_Score", "Physical_Demand_Score", "SUS_Score"))

plot_ranks_usability <- ggplot(ranks_melted, aes(x=Rank, y=Score, color=Metric)) +
  geom_point(aes(shape=Metric),
             size=0.75,
             alpha=0.25,
             show.legend=FALSE) +
  geom_vline(xintercept=8,
             linetype="dashed",
             linewidth=1,
             color="black") +
  geom_smooth(method=loess,
              alpha=0.75,
              se=FALSE) +
  scale_x_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  scale_y_continuous(limits=c(0, 100),
                     breaks=c(0, 25, 50, 75, 100)) +
                     #labels=c("10\nFavored by Low Performers", "5", "0", "5", "10\nFavored by High Performers"))
  scale_color_manual(name="Metric",
                     values=c(SUS_Score=sus_color,
                              Mental_Demand_Score=mental_demand_color,
                              Physical_Demand_Score=physical_demand_color,
                              Temporal_Demand_Score=temporal_demand_color,
                              Performance_Demand_Score=performance_color,
                              Effort_Demand_Score=effort_demand_color),
                     labels=c(SUS_Score="System Usability Scale (SUS)",
                              Mental_Demand_Score="TLX - Mental Demand",
                              Physical_Demand_Score="TLX - Physical Demand",
                              Temporal_Demand_Score="TLX - Temporal Demand",
                              Performance_Demand_Score="TLX - Performance",
                              Effort_Demand_Score="TLX - Effort Demand")) +
                     # values=c(sus_color,
                     #          mental_demand_color,
                     #          physical_demand_color,
                     #          temporal_demand_color,
                     #          performance_color,
                     #          effort_demand_color),
                     # labels=c("System Usability Scale (SUS)",
                     #          "TLX - Mental Demand",
                     #          "TLX - Physical Demand",
                     #          "TLX - Temporal Demand",
                     #          "TLX - Performance",
                     #          "TLX - Effort Demand")) +
  labs(caption="Rank: Lower = More optimal   \nScore: Higher = More acceptable") +
  facet_grid(Room~Condition,
             space="fixed",
             margins = "vs") +
  labs(
    title="Usability Vs. Final Selection Ranks",
    x="Rank",
    y="Usability Score") +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.text = element_text(size=12),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks=element_line(size=1.0),
    # Legend
    legend.position = "bottom",
    #legend.position = "right",
    legend.background = element_rect(color="black", size=0.5, linetype="dashed"),
    legend.title = element_text(size=14, face="plain", hjust=0.5),
    legend.text = element_text(size=12),
    legend.direction = "horizontal",
    #legend.direction = "vertical",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  )
plot_and_savepdf(plot_ranks_usability, "Usability_Ranks2")
rm(ranks_melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Usability ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
melted <- melt(select(subjects_data,c("Participant.ID",
                                      "Room",
                                      "Condition",
                                      "Movement.Classification",
                                      "Rotation.Classification",
                                      "Gaze.Classification",
                                      #"Gaze.Classification")),
                                      "SUS_Score",
                                      "Mental_Demand_Score",
                                      "Physical_Demand_Score",
                                      "Temporal_Demand_Score",
                                      "Performance_Demand_Score",
                                      "Effort_Demand_Score")), 
               id = c("Participant.ID", "Room", "Condition", "Movement.Classification", "Rotation.Classification", "Gaze.Classification"),
               #id = c("Participant.ID", "Condition"),
               variable.name = "Metric",
               value.name = "Score")

plot_usability <- ggplot(melted, aes(x=Metric, y=Score, fill=Condition)) +
  # geom_violinhalf(color="black",
  #             draw_quantiles=TRUE,
  #             trim=TRUE,
  #             alpha=0.7,
  #             linewidth=0.5,
  #             adjust=6.0) +
  geom_violin(color="black",
              draw_quantiles=TRUE,
              trim=TRUE,
              alpha=0.7,
              #linewidth=0.8,
              linewidth=0.6,
              adjust=4.0,
              position=position_dodge(width=0.7)) +
  stat_summary(color="black",
               #aes(color=Condition),
               show.legend = FALSE,
               fun = "mean",
               geom = "point", 
               #width = 0.5,
               position=position_dodge(width=0.7)) +#,
               #colour = "red") +
  # geom_boxplot(alpha=0.7,
  #              width=0.6,
  #              size=1,
  #              color="black",
  #              outlier.shape=NA) +
  facet_grid(.~Room,
             space="fixed",
             margins = "vs") +
  scale_fill_manual(values=c(ar_color, desktop_color)) +
  #scale_y_continuous(lim=c(0,100),
  #                   breaks=c(0,20,40,60,80,100)) +
  coord_flip() +
  labs(
   title="Usability Metrics",
   x="Metric",
   y="Score") +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.text = element_text(size=12),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks=element_line(size=1.0),
    # Legend
    legend.position = "bottom",
    #legend.position = "right",
    legend.background = element_rect(color="black", size=0.5, linetype="dashed"),
    legend.title = element_text(size=14, face="plain", hjust=0.5),
    legend.text = element_text(size=12),
    legend.direction = "horizontal",
    #legend.direction = "vertical",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  )
plot_and_savepdf(plot_usability, "Usability")
rm(melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Usability Vs. Movement (AR) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
melted <- melt(select(subjects_data, c("Participant.ID",
                                       "Room",
                                       "Condition",
                                       "Movement.Classification",
                                       "Rotation.Classification",
                                       "Gaze.Classification",
                                       #"Gaze.Classification")),
                                       "SUS_Score",
                                       "Mental_Demand_Score",
                                       "Physical_Demand_Score",
                                       "Temporal_Demand_Score",
                                       "Performance_Demand_Score",
                                       "Effort_Demand_Score")), 
               id = c("Participant.ID", "Room", "Condition", "Movement.Classification", "Rotation.Classification", "Gaze.Classification"),
               #id = c("Participant.ID", "Condition"),
               variable.name = "Metric",
               value.name = "Score")
# Custom formatting
head_movement <- melted
head_movement$Type <- "Head.Classification"
head_movement$Quantity <- head_movement$Movement.Classification
head_movement <- select(head_movement, c("Participant.ID", "Room", "Condition", "Metric", "Score", "Type", "Quantity"))
head_rotation <- melted
head_rotation$Type <- "Head.Rotation"
head_rotation$Quantity <- head_rotation$Rotation.Classification
head_rotation <- select(head_rotation, c("Participant.ID", "Room", "Condition", "Metric", "Score", "Type", "Quantity"))
eye_gaze <- melted
eye_gaze$Type <- "Eye.Gaze"
eye_gaze$Quantity <- eye_gaze$Gaze.Classification
eye_gaze <- select(eye_gaze, c("Participant.ID", "Condition", "Room", "Metric", "Score", "Type", "Quantity"))
rm(melted) # Get rid of old one
melted <- rbind(head_movement, head_rotation, eye_gaze) # Create new one
#melted$Show <- !(melted$Condition=="Desktop" & melted$Type=="Eye.Gaze") # Create filter because desktop has no valid eye gaze data
#filter(melted, Show==TRUE)

plot_usability <- ggplot(filter(melted, Condition=="AR"), aes(x=Metric, y=Score, fill=Quantity)) +
  # geom_boxplot(alpha=0.7,
  #              width=0.8,
  #              size=0.6,
  #              color="black",
  #              outlier.shape=NA) +
  geom_violin(color="black",
              draw_quantiles=TRUE,
              trim=TRUE,
              alpha=0.7,
              linewidth=0.6,
              adjust=1.0,
              position=position_dodge(width=0.75)) +
  stat_summary(color="black",
               #aes(color=Condition),
               show.legend = FALSE,
               fun = "mean",
               geom = "point", 
               size = 0.75,
               position=position_dodge(width=0.75)) +#,
  facet_grid(Room~Type,
             space="fixed",
             margins = "vs") +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  scale_y_continuous(lim=c(0,100),
                     breaks=c(0,20,40,60,80,100)) +
  coord_flip() +
  labs(
    title="Usability Metrics",
    x="Metric",
    y="Score",
    caption="Score: Higher = More acceptable") +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.text = element_text(size=10),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks=element_line(size=1.0),
    # Legend
    legend.position = "bottom",
    #legend.position = "right",
    legend.background = element_rect(color="black", size=0.5, linetype="dashed"),
    legend.title = element_text(size=14, face="plain", hjust=0.5),
    legend.text = element_text(size=12),
    legend.direction = "horizontal",
    #legend.direction = "vertical",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  )
plot_and_savepdf(plot_usability, "Usability-Movement")
rm(melted)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Usability Vs. Movement (AR - Optimal) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
melted <- melt(select(subjects_data, c("Participant.ID",
                                       "Room",
                                       "Condition",
                                       "Optimal.Movement.Classification",
                                       "Optimal.Rotation.Classification",
                                       "Optimal.Gaze.Classification",
                                       #"Gaze.Classification")),
                                       "SUS_Score",
                                       "Mental_Demand_Score",
                                       "Physical_Demand_Score",
                                       "Temporal_Demand_Score",
                                       "Performance_Demand_Score",
                                       "Effort_Demand_Score")), 
               id = c("Participant.ID", "Room", "Condition",
                      "Optimal.Movement.Classification",
                      "Optimal.Rotation.Classification",
                      "Optimal.Gaze.Classification"),
               #id = c("Participant.ID", "Condition"),
               variable.name = "Metric",
               value.name = "Score")
# Custom formatting
head_movement <- melted
head_movement$Type <- "Head.Classification"
head_movement$Quantity <- head_movement$Optimal.Movement.Classification
head_movement <- select(head_movement, c("Participant.ID", "Room", "Condition", "Metric", "Score", "Type", "Quantity"))
head_rotation <- melted
head_rotation$Type <- "Head.Rotation"
head_rotation$Quantity <- head_rotation$Optimal.Rotation.Classification
head_rotation <- select(head_rotation, c("Participant.ID", "Room", "Condition", "Metric", "Score", "Type", "Quantity"))
eye_gaze <- melted
eye_gaze$Type <- "Eye.Gaze"
eye_gaze$Quantity <- eye_gaze$Optimal.Gaze.Classification
eye_gaze <- select(eye_gaze, c("Participant.ID", "Condition", "Room", "Metric", "Score", "Type", "Quantity"))
rm(melted) # Get rid of old one
melted <- rbind(head_movement, head_rotation, eye_gaze) # Create new one
#melted$Show <- !(melted$Condition=="Desktop" & melted$Type=="Eye.Gaze") # Create filter because desktop has no valid eye gaze data
#filter(melted, Show==TRUE)

plot_usability <- ggplot(filter(melted, Condition=="AR"), aes(x=Metric, y=Score, fill=Quantity)) +
  # geom_boxplot(alpha=0.7,
  #              width=0.8,
  #              size=0.6,
  #              color="black",
  #              outlier.shape=NA) +
  geom_violin(color="black",
              draw_quantiles=TRUE,
              trim=TRUE,
              alpha=0.7,
              linewidth=0.6,
              adjust=1.0,
              position=position_dodge(width=0.75)) +
  facet_grid(Room~Type,
             space="fixed",
             margins = "vs") +
  scale_fill_manual(values=c(high_movement_color, low_movement_color)) +
  scale_y_continuous(lim=c(0,100),
                     breaks=c(0,20,40,60,80,100)) +
  coord_flip() +
  labs(
    title="Usability Metrics",
    x="Metric",
    y="Score",
    caption="Score: Higher = More acceptable") +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.text = element_text(size=10),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks=element_line(size=1.0),
    # Legend
    legend.position = "bottom",
    #legend.position = "right",
    legend.background = element_rect(color="black", size=0.5, linetype="dashed"),
    legend.title = element_text(size=14, face="plain", hjust=0.5),
    legend.text = element_text(size=12),
    legend.direction = "horizontal",
    #legend.direction = "vertical",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines"),
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  )
plot_and_savepdf(plot_usability, "Usability-Movement-Optimal")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Prev. Experience Vs. Ranks ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Re-work data
ranks_melted <- melt(select(subjects_data, c("Room",
                                             "Condition",
                                             "Rank",
                                             "Prev.XR.Experience",
                                             "Prev.VR.Experience",
                                             "Prev.AR.Experience",
                                             "Prev.3D.Modeling.Experience",
                                             "Prev.Computer.Experience",
                                             "Construction.Familiarity")), 
                     id = c("Room", "Condition", "Rank"),
                     variable.name = "Metric",
                     value.name = "Score")

plot_ranks_experience <- ggplot(ranks_melted, aes(x=Score, y=Rank, fill=Metric, color=Metric)) +
  geom_point(aes(shape=Metric),
             size=1,
             alpha=0.75) +
  geom_hline(yintercept=8,
             linetype="dashed",
             linewidth=2,
             color="black") +
  geom_smooth(method="glm",
              alpha=0.75,
              se=FALSE) +
  scale_y_reverse(lim=rev(c(1, 16)),
                  breaks=rev(c(1, 4, 7, 10, 13, 16))) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  labs(
    title="Selection Rank Vs Prev. Experience",
    x="Experience Score",
    y="Rank") +
  theme_bw() 
plot_and_savepdf(plot_ranks_experience, "Ranks_Experience")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Optimal Time Ratios ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

plot_time_ratio <- ggplot(subjects_data, aes(x=as.factor(Optimal), y=Optimal.Time.Ratio, fill=as.factor(Optimal))) +
  geom_hline(yintercept=0.5,
             alpha=0.5,
             linetype="dashed",
             color="black",
             linewidth=1) +
  geom_point(color="black",
             size=0.5,
             alpha=0.25,
             position=position_dodge2(width=0.25)) +
  # geom_boxplot(alpha=0.7,
  #              width=0.6,
  #              size=1,
  #              color="black",
  #              outlier.shape=NA) +
  geom_violin(color="black",
              draw_quantiles=TRUE,
              trim=FALSE,
              alpha=0.7,
              linewidth=0.6,
              adjust=0.75) + #,
              #position=position_dodge(width=0.75)) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  #ylim(0, 1.0) +
  scale_y_continuous(limits=c(0, 1.0),
                     breaks=c(0, 0.25, 0.5, 0.75, 1.0),
                     labels=c("0%", "25%", "50%", "75%", "100%")) +
  # scale_x_discrete(labels=c(FALSE="Non-optimal",
  #                           TRUE="Optimal")) +
  # scale_fill_manual(values=c(FALSE=poor_color,
  #                            TRUE=optimal_color)) +
  scale_x_discrete(labels=c("Low\nPerformers", "High\nPerformers")) +
  scale_fill_manual(values=c(poor_color, optimal_color)) +
  labs(
    #title="Time Ratio on Optimal Facades",
    #y="Percentage of Time Spent on Optimal Facades") +
    title="Percentage of Time Spent on Optimal Facades") +
  coord_flip() +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5),
    #axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size=12, hjust=0.5),
    # Axis Ticks
    axis.ticks = element_line(size=1.0),
    # Legend
    legend.position = "None",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
    # Facet
    strip.background=element_rect(colour=plot_outline_color,
                                  fill=NA,
                                  linewidth=plot_outline_size),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    strip.placement = "inside",
    panel.spacing.x = unit(0.5, "lines")
    #panel.spacing.x = unit(0, "lines"),
    #panel.spacing.y = unit(0, "lines")
  )
plot_and_savepdf(plot_time_ratio, "TimeRatio-Optimal")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Optimal Time Ratios (Head Movement) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

plot_time_ratio <- ggplot(subjects_data, aes(x=Movement.Classification, y=Optimal.Time.Ratio, fill=Movement.Classification)) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  ylim(0, 1.0) +
  scale_fill_manual(values=c(poor_color, optimal_color)) +
  labs(
    title="Time Ratio on Optimal Facades (Head Movement)",
    x="Metric",
    y="Time Ratio") +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()
plot_and_savepdf(plot_time_ratio, "TimeRatio-HeadMovement")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Optimal Time Ratios (Head Rotation) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

plot_time_ratio <- ggplot(subjects_data, aes(x=Rotation.Classification, y=Optimal.Time.Ratio, fill=Rotation.Classification)) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  ylim(0, 1.0) +
  scale_fill_manual(values=c(poor_color, optimal_color)) +
  labs(
    title="Time Ratio on Optimal Facades (Head Rotation)",
    x="Metric",
    y="Time Ratio") +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()
plot_and_savepdf(plot_time_ratio, "TimeRatio-HeadRotation")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Optimal Time Ratios (Eye Gaze) ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

plot_time_ratio <- ggplot(subjects_data, aes(x=Gaze.Classification, y=Optimal.Time.Ratio, fill=Gaze.Classification)) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  ylim(0, 1.0) +
  scale_fill_manual(values=c(poor_color, optimal_color)) +
  labs(
    title="Time Ratio on Optimal Facades (Eye Gaze)",
    x="Metric",
    y="Time Ratio") +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()
plot_and_savepdf(plot_time_ratio, "TimeRatio-EyeGaze")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Poor Time Ratios ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

plot_time_ratio <- ggplot(subjects_data, aes(x=Optimal, y=Poor.Time.Ratio, fill=Optimal)) +
  geom_boxplot(alpha=0.7,
               width=0.6,
               size=1,
               color="black",
               outlier.shape=NA) +
  facet_grid(Condition~Room,
             space="fixed",
             margins = "vs") +
  ylim(0, 1.0) +
  scale_fill_manual(values=c(poor_color, optimal_color)) +
  labs(
    title="Time Ratio on Poor Facades",
    x="Metric",
    y="Time Ratio") +
  theme_bw() +
  coord_flip()
plot_and_savepdf(plot_time_ratio, "TimeRatio-Poor")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Pareto - Conf ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Set params
pareto_data <- subset(facades_data, Room=="Conference")
pareto_data$Optimal <- ifelse(pareto_data$Optimal==1, "Optimal", "Poor")
# Create plots
plot_pareto_conf <- ggplot(pareto_data, aes(x=VF_Percentage, y=G_Percentage)) +
  #annotate("text",x=0.25, y=0.275, label="Less Optimal", size=7, angle=0, fontface = "bold") +
  #annotate("text",x=0.75, y=0.125, label="More Optimal", size=7, angle=0, fontface = "bold") +
  #annotate("text",x=0.56, y=0.18, label="Most Optimal Facade Choices Available", size=6, angle=-30, fontface = "italic") +
  ## DATA (LINE OF BEST FIT)
  stat_smooth(data=subset(pareto_data, Optimal=="Optimal"),
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
  # scale_shape_manual(values = c(22, 24)) +
  scale_shape_manual(values=c(Optimal=24,
                              Poor=22)) +
  ## Color
  scale_fill_manual(values=c(Optimal=optimal_color,
                             Poor=poor_color)) +
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
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.text = element_text(size=12),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks=element_line(size=1.0),
    # Legend
    legend.position = "none",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
  )
plot_and_savepdf(plot_pareto_conf, "Pareto-Conf")
#print(plot_pareto_conf)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Pareto - Office ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Set params
pareto_data <- subset(facades_data, Room=="Office")
pareto_data$Optimal <- ifelse(pareto_data$Optimal==1, "Optimal", "Poor")
# Create plots
plot_pareto_office <- ggplot(pareto_data, aes(x=VF_Percentage, y=G_Percentage)) +
  #annotate("text",x=0.25, y=0.275, label="Less Optimal", size=7, angle=0, fontface = "bold") +
  #annotate("text",x=0.75, y=0.125, label="More Optimal", size=7, angle=0, fontface = "bold") +
  #annotate("text",x=0.56, y=0.18, label="Most Optimal Facade Choices Available", size=6, angle=-30, fontface = "italic") +
  ## DATA (LINE OF BEST FIT)
  stat_smooth(data=subset(pareto_data, Optimal=="Optimal"),
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
  # scale_shape_manual(values = c(22, 24)) +
  scale_shape_manual(values=c(Optimal=24,
                              Poor=22)) +
  ## Color
  scale_fill_manual(values=c(Optimal=optimal_color,
                             Poor=poor_color)) +
  # geom_text(data = ~filter(pareto_data_conf, Optimal == 1),
  #           aes(label=Facade), 
  #           size=5, 
  #           hjust=0, 
  #           nudge_x=0.0, 
  #           nudge_y=0.02) +
  labs(title="Office",
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
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.text = element_text(size=12),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks=element_line(size=1.0),
    # Legend
    legend.position = "none",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
  )
plot_and_savepdf(plot_pareto_office, "Pareto-Office")
#print(plot_pareto_office)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Plot - Histogram ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
ar_hist <- ggplot(subset(subjects_data, Condition=="AR"), aes(x=Prev.AR.Experience)) +
  geom_histogram(binwidth=1,
                 size=1,
                 color="Black",
                 fill="gray75") +
  geom_vline(aes(xintercept = mean(Prev.AR.Experience)),
             color="black",
             linetype="dashed",
             size=1.5) +
  scale_x_binned(limits = c(1, 8),
                 breaks = c(1, 2, 3, 4, 5, 6, 7, 8),
                 labels = c("1\nLess", "2", "3", "4", "5", "6", "7\nMore", "8")) +
  labs(title="Previous AR Experience - AR Participants",
       x="Prev. AR Experience",
       y="Count") +
  theme_bw() +
  theme(# Text
    text = element_text(family="serif"), # Times New Roman
    plot.title = element_text(size=18, face="bold", hjust=0.5), 
    axis.title = element_text(size=18, face="plain", hjust=0.5),
    axis.text = element_text(size=12),
    # Axis Ticks
    #axis.ticks = element_blank(),
    axis.ticks=element_line(size=1.0),
    # Legend
    legend.position = "none",
    # Borders
    panel.border = element_rect(color=plot_outline_color, fill=NA, size=plot_outline_size),
    panel.grid.major = element_line(color=plot_grid_color, size=plot_grid_size_major),
    panel.grid.minor = element_line(color=plot_grid_color, size=plot_grid_size_minor),
  )
plot_and_savepdf(ar_hist, "PrevARExperience")
#print(ar_hist)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### Rhino-generated data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

surface_variance <- function(high_filname, low_filename, surface_name, test_type) {
  high <- read.csv(high_filname,
                   header=TRUE,
                   sep=",")
  high <- subset(high, Surface==surface_name)
  # high$normalized <- high$l / max(high$e)
  print(shapiro.test(high$l))
  high$HighLow <- "High"
  
  low <- read.csv(low_filename,
                   header=TRUE,
                   sep=",")
  low <- subset(low, Surface==surface_name)
  # low$normalized <- low$l / max(low$e)
  print(shapiro.test(low$l))
  low$HighLow <- "Low"
  
  var_data <- select(rbind(high, low), c("l", "HighLow"))
  
  # results <- var.test(var_data$l ~ var_data$HighLow,
  #                     paired = FALSE,
  #                     #alternative = "two.sided",
  #                     #alternative = "less",
  #                     #alternative = "greater",
  #                     alternative = test_type,
  #                     conf.level = 0.95,
  #                     conf.int = TRUE,
  #                     exact = FALSE)
  results <- leveneTest(var_data$l ~ var_data$HighLow)
  print(results)
  #print(cohen.d(eg_high_office$normalized, eg_low_office$normalized))
}

surface_variance("HeatmapData/Viewing_Office_AR_3_Head_HighGazer.csv",
                 "HeatmapData/Viewing_Office_AR_3_Head_LowGazer.csv",
                 "Window1",
                 "less")
surface_variance("HeatmapData/Viewing_Conference_AR_3_Eye_High.csv",
                 "HeatmapData/Viewing_Conference_AR_3_Eye_Low.csv",
                 "Floor",
                 "two.sided")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### END ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

sink()
print("DONE :)")