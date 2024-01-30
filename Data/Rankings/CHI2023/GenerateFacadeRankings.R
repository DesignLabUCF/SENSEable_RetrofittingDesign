library(dplyr)

conf_Pos1 <- read.csv("CalcDaylightOutput/out_conf_Pos1.csv", header=TRUE, sep=",")
conf_Pos2 <- read.csv("CalcDaylightOutput/out_conf_Pos2.csv", header=TRUE, sep=",")
office_Pos1 <- read.csv("CalcDaylightOutput/out_office_Pos1.csv", header=TRUE, sep=",")
viewFactors <- read.csv("CalcDaylightOutput/out_ViewFactor.csv", header=TRUE, sep=",")

# View Factors
viewFactors$Percentage <- (viewFactors$Thru / viewFactors$Total) * 100.0