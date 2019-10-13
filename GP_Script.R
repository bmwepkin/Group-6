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


# Separate Lat, Long from Location1 Column
library(stringr)
mat <- str_match(df$Location1, "\\((.*?), (.*?)\\)")
df$lat <- as.double(mat[,2])
df$long <- as.double(mat[,3])

# Plot Zipcode, Lat and Long
df$region <- factor(substr(df$`Zip Code`, 1, 2))
p <- qplot(long, lat, na.rm = TRUE, data = df1, geom = "dotplot", color = region) +
  geom_jitter(aes(alpha = I(0.2), color = "red")) +
  scale_y_continuous(name = "Latitude", limits = c(32.5, 33.1)) +
  scale_x_continuous(name = "Longitude", limits = c(-97.1, -96.45))