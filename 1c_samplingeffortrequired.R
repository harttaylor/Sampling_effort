# attach packages
library(tidyverse)
library(unmarked)
library(lubridate)
library(dplyr)

# Set working directory 
setwd("~/Documents/Chapter 2")

# load occupancy/wildtrax data 
my_data <- read.csv("Data/Raw/SpeciesRawDownload-10/BU_Hart-BTNW-BU-2022_basic_summary.csv")
head(my_data)

# Filter data for only BTNW detections
btnw_data <- subset(my_data, species_code == "BTNW")

# For each site, calculate the detection probability
site_list <- unique(my_data$location)
detection_probs <- numeric(length(site_list))

for (i in 1:length(site_list)) {
  site <- site_list[i]
  
  total_visits <- nrow(subset(my_data, location == site))
  btnw_visits <- nrow(subset(btnw_data, location == site))
  
  detection_probs[i] <- btnw_visits / total_visits
}

# Calculate the average detection probability across all sites
average_detection_prob <- mean(detection_probs)
print(average_detection_prob)

p <- average_detection_prob
d <- 0.05 # we want to be 95% sure that if its there we detected it

required_visits <- function(p, d) {
  return(ceiling(log(d) / log(1 - p)))
}

visits <- required_visits(p, d)
print(visits)







