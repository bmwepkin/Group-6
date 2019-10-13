#set working directory to folder where csv file is downloaded

# Load Libraries
library(readr)
suppressPackageStartupMessages(library(dplyr))
library(reshape2)
suppressPackageStartupMessages(library(ggplot2))

# Uncomment these lines to filter by Year 2018 and write it into a csv, if not done earlier
# df <- read_csv("Police_Incidents.csv", col_types = cols(`Hate Crime` = col_character(), `Victim Business Phone` = col_character()))
# df <- filter(df, `Year of Incident` == 2018)
# write_csv(df, "Police_Incidents_2018.csv")

# Load the filtered version
my_cols <- cols(`Hate Crime` = col_character(), 
                `Victim Business Phone` = col_character())
df <- read_csv("Police_Incidents_2018.csv", col_types = my_cols)


