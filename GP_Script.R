#set working directory to folder where csv file is downloaded

# Load Libraries
library(readr)
suppressPackageStartupMessages(library(dplyr))
library(reshape2)
suppressPackageStartupMessages(library(ggplot2))

# df <- read_csv("Police_Incidents.csv", col_types = cols(`Hate Crime` = col_character(), `Victim Business Phone` = col_character()))
# df <- filter(df, `Year of Incident` == 2018)
# write_csv(df, "Police_Incidents_2018.csv")

