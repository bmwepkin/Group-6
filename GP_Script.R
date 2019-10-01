#set working directory to folder where csv file is downloaded
#install readr package
library(readr)
df <- read_csv("Police_Incidents.csv", col_types = cols(`Hate Crime` = col_character(), `Victim Business Phone` = col_character()))

