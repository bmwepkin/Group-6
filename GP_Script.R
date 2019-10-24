#set working directory to folder where csv file is downloaded

###############################################################################
# Load Libraries
###############################################################################

library(readr)
suppressPackageStartupMessages(library(dplyr))
library(reshape2)
suppressPackageStartupMessages(library(ggplot2))
library(stringr)

###############################################################################

read_PoliceIncidents_ByYear <- function(year, columns, nas) {
  mycols <- columns
  
  df <- NULL
  datafilename <- sprintf("Police_Incidents_%s.csv", year)
  if (file.exists(datafilename)) {
    print (sprintf("Reading...%s", datafilename)) 
    df <- read_csv(datafilename, col_types = mycols, na = nas)
  } else {
    print("Reading...Police_Incidents.csv")
    df <- read_csv("Police_Incidents.csv", col_types = mycols, na = nas)
    df <- filter(df, `Year of Incident` == year)
    print (sprintf("Writing...%s", datafilename)) 
    write_csv(df, datafilename)
  }
  
  df
}

splitLatLongFromLocation <- function() {
  # Separate Lat, Long from Location1 Column
  
  mat <- str_match(df$Location1, "\\((.*?), (.*?)\\)")
  df$lat <- as.double(mat[,2])
  df$long <- as.double(mat[,3])
  
  df
}

renameColumns <- function(df) {
  names(df) <- gsub(" ", "_", names(df))
  names(df) <- gsub("Day1", "Day", names(df))
  names(df) <- gsub("Date1", "Date", names(df))
  names(df)[names(df) == "Date_incident_created"] <- "Date_Incident_Created"

  df
}

getColumnswithLessNAs <- function(df, threshold) {
  nas <- list()
  for(i in names(df)) {
    nas[[i]] <- sum(is.na(df[i]))
  }
  
  nadf <- data.frame(names(nas), unlist(nas))
  names(nadf) <- c("Name", "NACount")
  nadf$Name <- as.character(nadf$Name)  # Convert to character from factor so as to get the filtered list
  nadf <- filter(nadf, NACount < threshold)
  
  nadf$Name
}

convertDataTypes <- function(df) {
  df$NIBRS_Crime_Category <- factor(df$NIBRS_Crime_Category)
  df$Offense_Status <- factor(df$Offense_Status)
  df$Person_Involvement_Type <- factor(df$Person_Involvement_Type)
  df$Council_District <- factor(df$Council_District)
  df$Division <- factor(df$Division)
  
  df$Victim_Gender <- factor(df$Victim_Gender)
  df$Victim_Race <- factor(df$Victim_Race)
  df$Victim_Type <- factor(df$Victim_Type)
  
  df <- mutate(df, Victim_Type = recode(Victim_Type, "Financial Institutio" = "Financial Institution",
                                        "Law Enforcement Offi" = "Law Enforcement Officer",
                                        "Religious Organizati" = "Religious Organization"))
  
  df$Day_of_the_Week <- factor(df$Day_of_the_Week)
  levels(df$Day_of_the_Week) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  df$Call_Dispatch_Date_Time <- as.POSIXct(df$Call_Dispatch_Date_Time, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
  df$Call_Cleared_Date_Time <- as.POSIXct(df$Call_Cleared_Date_Time, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
  df$Call_Date_Time <- as.POSIXct(df$Call_Date_Time, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
  df$Date_Incident_Created <- as.POSIXct(df$Date_Incident_Created, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
  df$Date_of_Report <- as.POSIXct(df$Date_of_Report, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")  
  df$Date_of_Occurrence<- as.Date(df$Date_of_Occurrence, format="%m/%d/%Y")
  
  df$Reporting_Area <- as.integer(df$Reporting_Area)
  df$Victim_Age <- as.integer(df$Victim_Age)
  
  df
}

saveplot <- function(plot = NULL, name = "graphics", width = 6, height = 4) {
  ggsave(filename = paste(name, ".png", sep = ""), plot = plot, width = width, height = height, dpi = 600)
}

analyzeAndPlot_ResponseTime <- function(df) {
  df <- mutate(df, Response_Time = as.numeric(difftime(df$Call_Dispatch_Date_Time, df$Call_Date_Time, units = "mins")))
  df <- mutate(df, Month = factor(months(df$Date_of_Occurrence, abbreviate = FALSE), levels = month.name))
  df <- mutate(df, Day_of_Month <- as.POSIXlt(df$Date_of_Occurrence)$mday)
  
  df <- filter(df, Response_Time > 0, Response_Time < 5000)
  
  # qplot(Day_of_Month, data = df, geom = "bar")
  # qplot(Day_of_Week, data = df, geom = "bar")
  # qplot(Month, data =df, geom = "bar")
  
  plotResponseTime(df, "Month", "Reponse_By_Month")
  plotResponseTime(df, "Day_of_the_Week", "Reponse_By_DayofWeek")
  plotResponseTime(df, "Day_of_Month", "Reponse_By_DayofMonth")
}

plotResponseTime <- function(df, xcol, name) {
  df <- ungroup(df)
  df <- group_by(df, !!xcol)
  df <- summarize(df, num_incidents = n(), med_response = median(Response_Time))
  p <- ggplot(df, aes(x = !!xcol, y = num_incidents, fill = med_response)) + geom_col(position = "dodge")
  saveplot(p, name)
}

preparePoliceIncidents <- function(year) {
  # Skip the below columns as they have some invalid data
  my_cols <- cols(`Hate Crime` = col_skip(), `Victim Business Phone` = col_skip())
  
  # Define the NAs
  my_na <- c("", "0", "UNK", "Unknown", "G", "J", "None", "N", "NA")
  
  df <- read_PoliceIncidents_ByYear(year, my_cols, my_na)
  
  columns <- c(getColumnswithLessNAs(df, 10000), names(df)[grepl("Victim*", names(df))])

  df <- select (df, columns)
  df <- renameColumns(df)
  df <- convertDataTypes(df)
  
  df
}

###############################################################################

df <- preparePoliceIncidents(2018)
analyzeAndPlot_ResponseTime(df)
