# set working directory as appropriate
# use install.packages(c("readr", "plyr", "dplyr", "reshape2", "ggplot2", "stringr", "ggrepel", "devtools", "ggmap", "compstatr")) 
# to install the necessary packages

####Load Libraries####
library(readr)
library(plyr)
library(reshape2)
library(stringr)
library(ggrepel)
library(devtools)
library(ggmap)
library(compstatr)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

####Utility Functions####
getMyPalette <- function() {
  "YlOrRd"
}

saveplot <- function(plot = NULL, name = "graphics", type = ".png", width = 6, height = 4) {
  ggsave(filename = paste(name, type, sep = ""), plot = plot, width = width, height = height, dpi = 600)
}

my_theme <- function() {
  theme(text = element_text(size = 12), panel.background = element_rect(fill = "azure3", colour = "darkslategray"),
        panel.grid.major.x = element_line(colour = "gainsboro"), panel.grid.major.y = element_line(colour = "gainsboro"),
        panel.grid.minor.y = element_line(colour = "gainsboro"), panel.grid.minor.x = element_line(colour = "gainsboro"))
}

splitLatLongFromLocation <- function(dataframe = Police_Incidents) {
  message("Extracting Latitude and Longitude from Location1 Column")
  
  df <- dataframe
  mat <- str_match(df$Location1, "\\((.*?), (.*?)\\)")
  df$lat <- as.double(mat[,2])
  df$long <- as.double(mat[,3])
  
  df
}

setupGoogleMaps <- functions(api_key=NA) {
  stop(is.na(api_key))

  require(devtools)
  devtools::install_github("dkahle/ggmap", ref = "tidyup")
  register_google(key=api_key)
}

####Data Functions####

renameColumns <- function(dataframe = Police_Incidents) {
  message("Renaming Columns for easy access.")
  
  df <- dataframe
  names(df) <- gsub(" ", "_", names(df))
  names(df) <- gsub("Day1", "Day", names(df))
  names(df) <- gsub("Date1", "Date", names(df))
  names(df) <- gsub("Year1", "Year", names(df))
  names(df) <- gsub("Month1", "Month", names(df))
  names(df) <- gsub("Time1", "Time", names(df))  
  df <- rename(df, Date_Incident_Created = Date_incident_created)

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

convertDataTypes <- function(dataframe = Police_Incidents) {
  message("Coercing Columns...")
  df <- dataframe
  
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

factorHourofDay <- function(dataframe = Police_Incidents) {
  message("Slicing Time of the Day")
  df <- dataframe
  
  df <- mutate(df, Hour = cut(Hour_of_the_Day, breaks = c(0, 4, 8, 12, 16, 20, 24), include.lowest = TRUE, right = TRUE, labels = c("F", "A", "B", "C", "D", "E")))
  df <- mutate(df, Hour = factor(Hour, levels = sort(levels(df$Hour))))
  df <- mutate(df, Hour = recode(Hour, "A" = "Early Morning (4AM - 8AM)", 
                                 "B" = "Morning (8AM - 12PM)", 
                                 "C" = "Afternoon (12PM - 4PM)", 
                                 "D" = "Evening (4PM - 8PM)", 
                                 "E" = "Night (8PM - 12AM)",
                                 "F" = "Mid Night (12AM - 4AM)"))  
}

factorVictimRaces <- function(dataframe = Police_Incidents) {
  message("Factoring Race of victim.")
  df <- dataframe
  
  df <- mutate(df, Victim_Race = recode(Victim_Race, "White" = "White", "Black" = "Black", "Hispanic or Latino" = "Hispanic/Latino", .default = "Other"))
  
  # Group the Race and Summarize the incidents and reorder the factor based on it
  df1 <- ungroup(df) %>%
         group_by(Victim_Race) %>%
         summarize(num_incidents = n())
  df1 <- mutate(df1, Victim_Race = reorder(Victim_Race, -num_incidents))
  
  # Sort the levels based on the calculation above
  victim_races = levels(df1$Victim_Race)
  df <- mutate(df, Victim_Race = factor(Victim_Race, levels = victim_races))
  
  message("Factored Victim Race")
  print(levels(df$Victim_Race))
  
  df
}

factorVictimAge <- function(dataframe = Police_Incidents) {
  message("Factoring Age groups of victim.")
  df <- dataframe
  
  df <- mutate(df, Age_Group = cut(Victim_Age, breaks = c(0, 19, 35, 55, 99), labels = c("Teenager", "Young Adult", "Middle Age", "Elderly")))  
  df
}

violentCrimes <- c("Assault", "Homicide", "Robbery")

factorViolentCrimes <- function(dataframe = Police_Incidents) {
  message("Factoring violent/nonviolent crime indicator.")  
  df <- dataframe

  df <- mutate(df, NIBRS_Crime_Category = recode(NIBRS_Crime_Category, "ASSAULT OFFENSES" = violentCrimes[1], "HOMICIDE OFFENSES" = violentCrimes[2], "ROBBERY" = violentCrimes[3], .default = "Other"))
  
  crime_levels = levels(df$NIBRS_Crime_Category)
  df <- mutate(df, NIBRS_Crime_Category = factor(NIBRS_Crime_Category, levels = c(sort(crime_levels[crime_levels != "Other"]), "Other")))
  
  mat1 <- df$NIBRS_Crime_Category %in% violentCrimes
  df$v_nv <- "Nonviolent"
  df$v_nv[mat1] <- "Violent"
  df$v_nv <- factor(df$v_nv)

  print(levels(df$NIBRS_Crime_Category))
  
  df
}

sub_vc_latlon <- function(dataframe = violent_crime){
  message("Creating subset of violent_crime that has latitude and longitude values")
  violent_crime <- dataframe
  violent_crime_latlon <- violent_crime[is.na(violent_crime$lat) == FALSE, ]
  violent_crime_latlon <- violent_crime[is.na(violent_crime$long) == FALSE, ]
  
  violent_crime_latlon
}

sub_crime_categories <- function(dataframe = Police_Incidents){
  message("Creating a table with categories of crime, number of incidents, and percentages.")
  df <- dataframe
  df_crime_category <- sort(table(df$NIBRS_Crime_Category), decreasing = TRUE)
  df_crime_category <- data.frame(df_crime_category[df_crime_category > 100])
  colnames(df_crime_category) <- c("Category", "Frequency")
  df_crime_category$Percentage <- df_crime_category$Frequency / sum(df_crime_category$Frequency)
  
  df_crime_category
}

sub_assault <- function(dataframe = violent_crime){
  message("Creating subset of violent crime with only assaults.")
  violent_crime <- dataframe
  mylist <- split(violent_crime, violent_crime$NIBRS_Crime_Category)
  assault <- as.data.frame(mylist$`ASSAULT OFFENSES`)
  assault
}

sub_homicide <- function(dataframe = violent_crime){
  message("Creating subset of violent crime with only homicides.")
  violent_crime <- dataframe
  mylist <- split(violent_crime, violent_crime$NIBRS_Crime_Category)
  murder <- as.data.frame(mylist$`HOMICIDE OFFENSES`)
  murder
}

sub_robbery <- function(dataframe = violent_crime){
  message("Creating subset of violent crime with only robberies.")
  violent_crime <- dataframe
  mylist <- split(violent_crime, violent_crime$NIBRS_Crime_Category)
  robbery <- as.data.frame(mylist$`ROBBERY`)
  robbery
}

getViolentCrimes <- function(df) {
  violent_crime <- filter(df, NIBRS_Crime_Category %in% violentCrimes)
}

read_PoliceIncidents_byYear_fromUrl <- function(url, year = ""){
  my_na <- c("", "0", "UNK", "G", "J", "None", "N", "Unknown", "NA")
  my_cols <- cols(`Hate Crime` = col_character(), `Victim Business Phone` = col_character(),
                  `Hate Crime Description` = col_character(), `Apartment Number` = col_character(),
                  `Victim Apartment` = col_character(), `Victim Zip Code` = col_character())
  
  df <- NULL
  datafilename <- "Police_Incidents.csv"

  message ("Downloading Police Incidents file. This is a large file, the download may take several minutes...")
  download.file(url, datafilename)
  message ("Download complete. Reading Police Incidents file...")
  
  df <- read_csv(datafilename, col_types = my_cols, na = my_na)
  if(year != ""){
    message(sprintf("Filtering to %s...", year))
    df <- filter(df, `Year of Incident` == year)
  } else {
    message("Importing all years.")
  }
  
  message("Completed loading data.")
  df
}

read_PoliceIncidents_byYear <- function(year) {
  my_na <- c("", "0", "UNK", "G", "J", "None", "N", "Unknown", "NA")
  my_cols <- cols(`Hate Crime` = col_character(), `Victim Business Phone` = col_character(),
                  `Hate Crime Description` = col_character(), `Apartment Number` = col_character(),
                  `Victim Apartment` = col_character(), `Victim Zip Code` = col_character())
  
  df <- NULL
  datafilename <- sprintf("Police_Incidents_%s.csv", year)
  if (file.exists(datafilename)) {
    message (sprintf("Reading Police Incidents file...%s", datafilename))
    df <- read_csv(datafilename, col_types = my_cols, na = my_na)
  } else {
    message("Reading...Police_Incidents.csv")
    df <- read_csv("Police_Incidents.csv", col_types = my_cols, na = my_na)
    df <- filter(df, `Year of Incident` == year)
    message(sprintf("Writing...%s", datafilename)) 
    write_csv(df, datafilename)
  }
  
  message("Completed loading data.")
  
  df
}

create_response_time <- function(dataframe = Police_Incidents) {
  message("Creating response time values. Response time is the number of minutes from 911 call to police dispatch.") 
  df <- dataframe
  
  df <- mutate(df, Response_Time = as.numeric(difftime(df$Call_Dispatch_Date_Time, df$Call_Date_Time, units = "mins")))
  df <- mutate(df, Month = factor(months(df$Date_of_Occurrence, abbreviate = TRUE), levels = month.abb))
  df <- mutate(df, Day_of_Month = as.POSIXlt(df$Date_of_Occurrence)$mday)
  df <- mutate(df, Hour_of_the_Day = as.numeric(format(as.POSIXct(df$Time_of_Occurrence,format="%H:%M:%S"),"%H")))
  
  df <- filter(df, Response_Time > 0, Response_Time < 5000)
  
  df
}

processPoliceIncidents <- function(dataframe = Police_Incidents) {
  message("Processing data...")
  
  df <- dataframe
  
  columns <- c(getColumnswithLessNAs(df, 10000), "Victim Gender", "Victim Age", "Victim Ethnicity", "Victim Race")
  
  df <- select (df, columns)
  df <- df[complete.cases(df), ]
  
  message(sprintf("Cleaned up the data and the final number of rows are : %s", nrow(df)))

  df <- df %>%  
        splitLatLongFromLocation() %>% 
        renameColumns() %>%
        convertDataTypes() %>%
        create_response_time() %>%
        factorHourofDay() %>% 
        factorViolentCrimes() %>% 
        factorVictimRaces() %>% 
        factorVictimAge()

  message("Analysis Complete.")
  
  df
}

####Plot Functions#### 

top_10_offenses <- function(pname = "bp", file = "graphics", fwidth = 8) {
  message("Creating a plot of the top 10 offenses of 2018.") 
  message("Using alternative color scheme as standard (YlOrRd) does not have enough colors.")
  mat2 <- Crime_Categories$Category != "ALL OTHER OFFENSES" & Crime_Categories$Category != "MISCELLANEOUS"
  df_crime_category_top10 <- Crime_Categories[mat2, ]
  df_crime_category_top10 <- df_crime_category_top10[1:10, ]
  updated_levels <- c("Theft", "Dest. of Property", "Assault", "Motor Vehicle Theft", "Burglary",
                      "Drunkenness", "Robbery", "Drug Violations", "Traffic Violations", "DUI Offenses")
  levels(df_crime_category_top10$Category) <- c(levels(df_crime_category_top10$Category), updated_levels)
  df_crime_category_top10$Category[1:10] <- c(updated_levels)
  pname <- ggplot(df_crime_category_top10, aes(x=Category, y=Frequency, fill=Category)) + 
    geom_bar(stat="identity") + 
    ggtitle("Top 10 Offenses in 2018") +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    scale_y_continuous(name = "Number of Incidents") +
    theme(axis.text.x=element_blank()) +
    theme(axis.title.x = element_blank())
  saveplot(plot = pname, name = file, width = fwidth)
} 

total_crime_plot <- function(dataframe = Police_Incidents, pname = "tc", file = "graphics", fwidth = 8){
  message("Creating plot of total police incidents by month in 2018.")
  df <- dataframe
  pname <- qplot(Month, data = df, geom = "bar", fill = v_nv) +
    ggtitle("Total Police Incidents in 2018 by Month") +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.title = element_blank()) +
    theme(axis.title.x = element_blank()) +
    scale_fill_brewer(palette = "YlOrRd")
  saveplot(plot = pname, name = file, width = fwidth)
}

tc_by_div_plot <- function(dataframe = Police_Incidents, pname = "tc by div", file = "graphics", fwidth = 8){
  message("Creating plot of total police incidents by division in 2018.")
  df <- dataframe
  mat3 <- !is.na(df$Division)
  df_sub <- df[mat3, ]
  pname <- qplot(Division, data = df_sub, geom = "bar", fill = v_nv) +
    ggtitle("Total Police Incidents in 2018 by Division") +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.title = element_blank()) +
    theme(axis.title.x = element_blank()) +
    scale_fill_brewer(palette = "YlOrRd") +
    theme(axis.text.x = element_text(angle = -45))
  saveplot(plot = pname, name = file, width = fwidth)
}

violent_crime_plot <- function(dataframe = violent_crime, pname = "vc", file = "graphics", fwidth = 8){
  message("Creating plot of violent crime by month in 2018")
  df <- dataframe
  pname <- qplot(Month, data = df, geom = "bar", fill = NIBRS_Crime_Category) +
    ggtitle("Violent Crime in 2018 by Month") +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.title = element_blank()) +
    theme(axis.title.x = element_blank()) +
    scale_fill_brewer(palette = "YlOrRd") 
  saveplot(plot = pname, name = file, width = fwidth)
}

vc_by_div_plot <- function(dataframe = violent_crime, pname = "vc by div", file = "graphics", fwidth = 8){
  message("Creating plot of violent crime by division in 2018")
  df <- dataframe
  pname <- qplot(Division, data = df, geom = "bar", fill = NIBRS_Crime_Category) +
    ggtitle("Violent Crime in 2018 by Division") +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.title = element_blank()) +
    theme(axis.title.x = element_blank()) +
    scale_fill_brewer(palette = "YlOrRd") +
    theme(axis.text.x = element_text(angle = -45))
  saveplot(plot = pname, name = file, width = fwidth)
}

plotResponseTime_InDifferentAspects <- function(df, year, suffix = "") {
  plotResponseTime(df, "Month",  paste("Reponse_By_Month", suffix, year, sep = "_"))
  plotResponseTime(df, "Day_of_the_Week", paste("Reponse_By_DayofWeek", suffix, year, sep = "_"))
  plotResponseTime(df, "Day_of_Month", paste("Reponse_By_DayofMonth", suffix, year, sep = "_"))
  plotResponseTime(df, "Hour", paste("Reponse_By_HouroftheDay", suffix, year, sep = "_"), flip = TRUE)
  
  message("Completed plotting and saving Charts.")
}

plotResponseTime_InDifferentAspectsByCrime <- function(df, year, suffix = "") {
  plotResponseTimeByCrime(df, "Month",  paste("Reponse_By_Month", suffix, year, sep = "_"))
  plotResponseTimeByCrime(df, "Day_of_the_Week", paste("Reponse_By_DayofWeek", suffix, year, sep = "_"))
  plotResponseTimeByCrime(df, "Day_of_Month", paste("Reponse_By_DayofMonth", suffix, year, sep = "_"))
  plotResponseTimeByCrime(df, "Hour", paste("Reponse_By_HouroftheDay", suffix, year, sep = "_"), flip = TRUE)
  
  message("Completed plotting and saving Charts.")
}

plotResponseTime <- function(df, xcol, name, flip = FALSE) {
  df <- ungroup(df) %>%
        group_by_(xcol) %>%
        summarize(num_incidents = n(), med_response = median(Response_Time)) %>%
        mutate(response = cut(med_response, breaks = quantile(med_response), include.lowest = TRUE, labels = c("Fastest", "Fast", "Slow", "Slowest")))
  
  p  <- ggplot(df, aes(x = df[[xcol]], y = num_incidents, fill = response)) +
        geom_col(position = "dodge") +
        scale_fill_brewer(palette = getMyPalette(), name = "Response Time", direction = -1, type = "qual") +
        xlab(gsub("_", " ", xcol)) + ylab("No. of Incidents") +
        ggtitle("How responsive the Dallas Police Dept is?") 
  

  if (flip == TRUE) {
    p <- p + coord_flip()
  }

  saveplot(p, name)
  message(sprintf("Saving %s", name))
  
  p
}

plotResponseTimeByCrime <- function(df, xcol, name, flip = FALSE) {
  df <- ungroup(df) %>%
    group_by_(xcol, "NIBRS_Crime_Category") %>%
    summarize(num_incidents = n(), med_response = median(Response_Time)) %>%
    mutate(response = cut(med_response, breaks = quantile(med_response), include.lowest = TRUE, labels = c("Fastest", "Fast", "Slow", "Slowest")))
  
  p  <- ggplot(df, aes(x = df[[xcol]], y = num_incidents, fill = response)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = getMyPalette(), name = "Response Time", direction = -1, type = "qual") +
    xlab(gsub("_", " ", xcol)) + ylab("No. of Incidents") +
    theme(text = element_text(size = 12)) +
    theme(panel.background = element_rect(fill = "azure3", colour = "slategray2" )) +
    ggtitle("How responsive the Dallas Police Dept is?")
  
  if (flip == TRUE) {
    p <- p + coord_flip()
  }
  
  saveplot(p, name)
  message(sprintf("Saving %s", name))
  
  df
}

plotViolentCrimesByAgeGroup <- function(df, name) {
  message("Analyzing Age Group...")

  df <- filter(df, Victim_Age > 0, Victim_Age < 100)
  df <- mutate(df, Age_Group = cut(Victim_Age, breaks = c(0, 19, 35, 55, 99), labels = c("Teenager", "Young Adult", "Middle Age", "Elderly"))) 
  df <- factorViolentCrimes(df)
                              
  df <- ungroup(df) %>%
        group_by(Age_Group, NIBRS_Crime_Category) %>%
        summarize(num_incidents = n())

  p  <- ggplot(df, aes(x = Age_Group, y = num_incidents, fill = NIBRS_Crime_Category)) +
        geom_col(position = position_dodge2()) +
        scale_fill_brewer(palette = "YlOrRd", name = "Type", direction = -1, type = "qual") +
        ggtitle("Violent Crimes by Age Group") +
        my_theme() + theme(axis.title = element_blank())

  p <- p + coord_flip()
  
  saveplot(p, name)
  message(sprintf("Saving %s", name))

  message("Analysis Complete.")
  
  p
}

plotAgeGroupByRace <- function(df, name) {
  df <- factorVictimRaces(df)
  
  df <- filter(df, Victim_Age > 0, Victim_Age < 100)
  df <- mutate(df, Age_Group = cut(Victim_Age, breaks = c(0, 19, 35, 55, 99), labels = c("Teenager", "Young Adult", "Middle Age", "Elderly")))
  
  df <- ungroup(df) %>%
        group_by(Victim_Race, Age_Group) %>%
        summarize(num_incidents = n())
  
  colors <- c("Other"="darkgray", "Black"="black", "Hispanic/Latino"="brown", "White"="white")
  p  <- ggplot(df, aes(x = Age_Group, y = num_incidents, fill = Victim_Race)) +
        geom_col(position = position_dodge2()) +
        scale_fill_manual(values = colors, name = "Race") +
        xlab(gsub("_", " ", "Age_Group")) + ylab("No. of Incidents") +
        ggtitle("Race By AgeGroup") +
        my_theme()
  
  p <- p + coord_flip()
  
  saveplot(p, name)
  message(sprintf("Saving %s", name))
  
  message("Analysis Complete.")
  
  p
}

plotViolentCrimesByAgeGroupAndGender <- function(df, name) {
  df <- filter(df, Victim_Age > 0, Victim_Age < 100)
  

  df <- ungroup(df) %>%
        group_by(Age_Group, Victim_Gender, NIBRS_Crime_Category) %>%
        summarize(num_incidents = n())

  p  <- ggplot(df, aes(x = NIBRS_Crime_Category, y = num_incidents, fill = Victim_Gender)) +
        geom_col(position = position_dodge2()) +
        facet_wrap(. ~ Age_Group) +
        scale_fill_brewer(palette = "YlOrRd", name = "Type", direction = 1, type = "qual") +
        xlab(gsub("_", " ", "Age_Group")) + ylab("No. of Incidents") +
        ggtitle("Violent Crimes By Sex/Gender") +
        my_theme()
  
  p <- p + coord_flip()
  
  saveplot(p, name)
  message(sprintf("Saving %s", name))
  
  message("Analysis Complete.")
  
  p
}


plotByVictimRacesAsLollipop <- function(df, name, flip = FALSE) {
  df <- ungroup(df) %>%
        group_by(Victim_Race, NIBRS_Crime_Category) %>%
        summarize(num_incidents = n())
  
  colors <- c("Other"="darkgray", "Black"="black", "Hispanic/Latino"="brown", "White"="white")

  p  <- ggplot(df, aes(x = Victim_Race, y = num_incidents)) +
        geom_linerange(aes(x = Victim_Race, ymin=0, ymax=num_incidents, color = Victim_Race), size=1.5, position = position_dodge2(width = 0.85)) +
        geom_point(position = position_dodge2(width = 0.85), aes(fill= NIBRS_Crime_Category), size=5, shape=21, stroke=1) +
        scale_color_manual(values = colors, name = "Race") +
        scale_fill_brewer(palette = "YlOrRd", name = "Type", direction = -1, type = "qual") +
        xlab("Race of Victim") + ylab("No. of Incidents") +
        ggtitle("Violent Crimes by Race") +
        my_theme()

  if (flip == TRUE) {
    p <- p + coord_flip()
  }
  
  saveplot(p, name)
  message(sprintf("Saving %s", name))
}

plotCrimeByTimeofDay <- function(df, name, flip = FALSE) {
  # Reverse the Levels to start from Early Morning when flipped
  if (flip == TRUE) {
    df <- mutate(df, Hour = factor(Hour, rev(levels(Hour))))
  }
  
  df <- factorViolentCrimes(df)
  
  df <- ungroup(df) %>%
        group_by(Hour, NIBRS_Crime_Category) %>%
        summarize(num_incidents = n())

  p  <- ggplot(df, aes(x = Hour, y = num_incidents, fill = NIBRS_Crime_Category)) +
        geom_col(position = "dodge") +
        scale_fill_brewer(palette = getMyPalette(), name = "Type", direction = -1, type = "qual") +
        xlab("Time of Day") + 
        ylab("No. of Incidents") +
        ggtitle("Violent Crimes by Time of Day") +
        my_theme()
  
  if (flip == TRUE) {
    p <- p + coord_flip()
  }
  
  saveplot(p, name)
  message(sprintf("Saving %s", name))
  
  p
}

plotCrimeByDayoftheWeek <- function(df, name, flip = FALSE) {
  df <- ungroup(df) %>%
    group_by(Day_of_the_Week, NIBRS_Crime_Category) %>%
    summarize(num_incidents = n())
  
  p  <- ggplot(df, aes(x = Day_of_the_Week, y = num_incidents, fill = NIBRS_Crime_Category)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = getMyPalette(), name = "Type", direction = -1, type = "qual") +
    xlab("Month") + 
    ylab("No. of Incidents") +
    ggtitle("Violent Crimes by Days of the Week") +
    my_theme()
  
  if (flip == TRUE) {
    p <- p + coord_flip()
  }
  
  saveplot(p, name)
  message(sprintf("Saving %s", name))
  
  p
}

####Main####
main() <- function() {
  setupGoogleMaps("<API Key goes here>")
  
  # import and transform the data frame
  year <- 2018
  data_url <- "https://www.dallasopendata.com/api/views/qv6i-rri7/rows.csv"
  
  # Police_Incidents <- read_PoliceIncidents_byYear_fromUrl(url = data_url, year = year)
  Police_Incidents <- read_PoliceIncidents_byYear(year = year)
  
  Police_Incidents <- processPoliceIncidents(Police_Incidents)
  
  # subset the data frame
  
  violent_crime <- getViolentCrimes(Police_Incidents)
  violent_crime_latlon <- sub_vc_latlon(dataframe = violent_crime)
  Crime_Categories <- sub_crime_categories()
  assault <- sub_assault(dataframe = violent_crime)
  murder <- sub_homicide(dataframe = violent_crime)
  robbery <- sub_robbery(dataframe = violent_crime)
  
  # generate plots
  top_10_offenses(pname = "Top Offenses", file = "Top_Offenses", fwidth = 8)
  total_crime_plot(dataframe = Police_Incidents, pname = "Total Crime", file = "Total_Crime")
  tc_by_div_plot(pname = "Total Crime by Division", file = "Ttl_Crime_Division")
  violent_crime_plot(pname = "Violent Crime", file = "Violent_Crime")
  vc_by_div_plot(pname = "Violent Crime by Division", file = "Violent_Crime_Division")
  
  plotResponseTime_InDifferentAspects(Police_Incidents, year)
  plotResponseTime_InDifferentAspects(getViolentCrimes(Police_Incidents), year, "Violent")
  plotResponseTime_InDifferentAspectsByCrime(Police_Incidents, year, "Crime")
  plotViolentCrimesByAgeGroup(getViolentCrimes(Police_Incidents), paste("CrimesByAgeGroup", year, sep = "_"))
  plotByVictimRacesAsLollipop(getViolentCrimes(Police_Incidents), paste("CrimesByRace", year, sep = "_"), FALSE)
  plotAgeGroupByRace(Police_Incidents, paste("RaceByAgeGroup", year, sep = "_"))
  plotViolentCrimesByAgeGroupAndGender(getViolentCrimes(Police_Incidents), paste("CrimesByGender", year, sep = "_"))
  
  plotCrimeByTimeofDay(getViolentCrimes(Police_Incidents), paste("ViolentCrimesByTimeofDay", year, sep = "_"), TRUE)
  plotCrimeByDayoftheWeek(getViolentCrimes(Police_Incidents), paste("ViolentCrimesByDayoftheWeek", year, sep = "_"), FALSE)
}

main()

####TODO####

vc_division <- qplot(Division, data = violent_crime, geom = "bar", fill = NIBRS_Crime_Category) +
  ggtitle("Violent Crime in 2018 by Division") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.title.x = element_blank()) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text.x = element_text(angle = -45))

rt <- df
rt <- filter(rt, Response_Time > 0)
rt <- filter(rt, Response_Time < 5000)
rt <- group_by(rt, v_nv)
rt <- summarize(rt, num_incidents = n(), med_response = median(Response_Time))
responsetime_type <- ggplot(rt, aes(x = v_nv, y = med_response, fill = num_incidents)) + geom_col(position = "dodge")

rt <- violent_crime
rt <- filter(rt, Response_Time > 0)
rt <- filter(rt, Response_Time < 5000)
rt <- group_by(rt, Division)
rt <- summarize(rt, num_incidents = n(), med_response = median(Response_Time))
responsetime_division <- ggplot(rt, aes(x = Division, y = med_response, fill = num_incidents)) + 
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = -45))

responsetime_division2 <- qplot(x = num_incidents, y = med_response, data = rt, geom = "point", color = Division, size = num_incidents)

bp <- ggplot(df_crime_category, aes(x=Category, y=Frequency, fill=Category)) + geom_bar(stat="identity") + theme(axis.text.x=element_blank())
pie1 <-ggplot(df_crime_category, aes(x="", y=Percentage, fill=Category))+ geom_bar(stat="identity") + coord_polar("y")

violent_crime <- group_by(violent_crime, NIBRS_Crime_Category)
summ <- summarise(violent_crime, num_crimes=n())
p <- qplot(violent_crime$NIBRS_Crime_Category, xlab = "Crime", main="Violent Crimes in Dallas")
p <- p + scale_y_continuous("Number of Crimes")

# crime by time of day
d1 <- qplot(violent_crime$`Time_of_Occurrence`, xlab="Time of day", main= "Crimes by time of day")
d1 <- d1 + scale_y_continuous("Number of crimes")
# asssaults by time of day
a1 <- qplot(assault$`Time_of_Occurrence`, xlab="Time of day", main= "Assaults by time of day")
a1 <- a1 + scale_y_continuous("Number of assaults")
# murders by time of day
m1 <- qplot(murder$`Time_of_Occurrence`, xlab="Time of day", main= "Murders by time of day")
m1 <- m1 + scale_y_continuous("Number of murders")
# robberies by time of day
r1 <- qplot(robbery$`Time_of_Occurrence`, xlab="Time of day", main= "Robberies by time of day")
r1 <- r1 + scale_y_continuous("Number of robberies")

d2 <- qplot(violent_crime$Day_of_the_Week, xlab= "Day of week", main="Violent crimes by day of week")+scale_y_continuous("Number of violent crimes")
# assault by day of week
a2 <- qplot(assault$Day_of_the_Week, xlab= "Day of week", main="Assaults by day of week")+scale_y_continuous("Number of assaults")
# murder by day of week
m2 <- qplot(murder$Day_of_the_Week, xlab= "Day of week", main="Murders by day of week")+scale_y_continuous("Number of murders")
# robbery by day of week
r2 <- qplot(robbery$Day_of_the_Week, xlab= "Day of week", main="Robberies by day of week")+scale_y_continuous("Number of robberies")
# violent crime by month
d3 <- qplot(violent_crime$Month, xlab= "Month", main= "Violent crimes by month") + scale_y_continuous("Number of violent crimes")
# assaults by month
a3 <- qplot(assault$Month, xlab= "Month", main= "Assaults by month") + scale_y_continuous("Number of assaults")
# murders by month
m3 <- qplot(murder$Month, xlab= "Month", main= "Murders by month") + scale_y_continuous("Number of murders")
# robberies by month
r3 <- qplot(robbery$Month, xlab= "Month", main= "Robberies by month") + scale_y_continuous("Number of robberies")
# violent crimes by day of week
temp <- ddply(violent_crime, .(NIBRS_Crime_Category, Day_of_the_Week), summarise, count=length(Date_of_Occurrence))
d4 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Day_of_the_Week, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Violent crime", expand = c(0,0)) + scale_y_discrete("Day of Week", expand = c(0,-2)) + scale_fill_gradient("Number of violent crimes", low="white", high = "steelblue")+theme_bw()+ggtitle("Violent crimes by day of week")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA)) 
# assaults by day of week
temp <- ddply(assault, .(NIBRS_Crime_Category, Day_of_the_Week), summarise, count=length(Date_of_Occurrence))
a4 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Day_of_the_Week, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Assaults", expand = c(0,0)) + scale_y_discrete("Day of Week", expand = c(0,-2)) + scale_fill_gradient("Number of assaults", low="white", high = "steelblue")+theme_bw()+ggtitle("Assaults by day of week")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))              
# murders by day of week
temp <- ddply(murder, .(NIBRS_Crime_Category, Day_of_the_Week), summarise, count=length(Date_of_Occurrence))
m4 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Day_of_the_Week, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Murders", expand = c(0,0)) + scale_y_discrete("Day of Week", expand = c(0,-2)) + scale_fill_gradient("Number of murders", low="white", high = "steelblue")+theme_bw()+ggtitle("Murders by day of week")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))              
# robberies by day of week
temp <- ddply(robbery, .(NIBRS_Crime_Category, Day_of_the_Week), summarise, count=length(Date_of_Occurrence))
r4 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Day_of_the_Week, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Robberies", expand = c(0,0)) + scale_y_discrete("Day of Week", expand = c(0,-2)) + scale_fill_gradient("Number of robberies", low="white", high = "steelblue")+theme_bw()+ggtitle("Robberies by day of week")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
# violent crimes by month
temp <- ddply(violent_crime, .(NIBRS_Crime_Category, Month), summarise, count=length(Date_of_Occurrence))
d5 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Month, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Violent crime", expand = c(0,0)) + scale_y_discrete("Month", expand = c(0,-2)) + scale_fill_gradient("Number of violent crimes", low="white", high = "steelblue")+theme_bw()+ggtitle("Violent crimes by month")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
# assaults by month
temp <- ddply(assault, .(NIBRS_Crime_Category, Month), summarise, count=length(Date_of_Occurrence))
a5 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Month, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Assaults", expand = c(0,0)) + scale_y_discrete("Month", expand = c(0,-2)) + scale_fill_gradient("Number of assaults", low="white", high = "steelblue")+theme_bw()+ggtitle("Assaults by month")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
# murders by month
temp <- ddply(murder, .(NIBRS_Crime_Category, Month), summarise, count=length(Date_of_Occurrence))
m5 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Month, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Murders", expand = c(0,0)) + scale_y_discrete("Month", expand = c(0,-2)) + scale_fill_gradient("Number of murders", low="white", high = "steelblue")+theme_bw()+ggtitle("Murders by month")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
# robberies by month
temp <- ddply(robbery, .(NIBRS_Crime_Category, Month), summarise, count=length(Date_of_Occurrence))
r5 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Month, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Robberies", expand = c(0,0)) + scale_y_discrete("Month", expand = c(0,-2)) + scale_fill_gradient("Number of robberies", low="white", high = "steelblue")+theme_bw()+ggtitle("Robberies by month")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
# violent crime over time - had to change the code a little to get this to work, likely not usable - commented out
# temp <- ddply(violent_crime, .(NIBRS_Crime_Category, Date_of_Occurrence), summarise, count=length(Date_of_Occurrence))
# temp <- arrange(temp, Date_of_Occurrence)
# plot <- ggplot(temp, aes(x=Date_of_Occurrence, y=count)) + geom_line(color = "#F2CA27", size = 0.1) + geom_smooth(color = "#1A1A1A") + scale_x_date(breaks = waiver(), date_breaks = "1 month") + labs(x = "Date of Violent Crime", y = "Number of Violent Crimes", title = "Daily Violent Crimes in Dallas in 2018")
# same with assaults, murders, and robberies over time

# age of victims of violent crime - should likely remove outliers
age <- ddply(violent_crime, .(NIBRS_Crime_Category, Victim_Age), summarise, count=length(Victim_Age))
d8 <- age %>% ggplot (aes(x=Victim_Age, y=count)) + geom_line(group=1) + geom_point(size=0.5) + labs(title = "Age Most Likely to Become A Violent Crime Victim", x= "Victim Age", y="Violent Crimes")
# age of victims of assaults - should likely remove outliers
age <- ddply(assault, .(NIBRS_Crime_Category, Victim_Age), summarise, count=length(Victim_Age))
a8 <- age %>% ggplot (aes(x=Victim_Age, y=count)) + geom_line(group=1) + geom_point(size=0.5) + labs(title = "Age Most Likely to Become An Assault Victim", x= "Victim Age", y="Assaults")
# age of murder victims
age <- ddply(murder, .(NIBRS_Crime_Category, Victim_Age), summarise, count=length(Victim_Age))
m8 <- age %>% ggplot (aes(x=Victim_Age, y=count)) + geom_line(group=1) + geom_point(size=0.5) + labs(title = "Age Most Likely to Become A Murder Victim", x= "Victim Age", y="Murder")
#age of robbery victims
age <- ddply(robbery, .(NIBRS_Crime_Category, Victim_Age), summarise, count=length(Victim_Age))
r8 <- age %>% ggplot (aes(x=Victim_Age, y=count)) + geom_line(group=1) + geom_point(size=0.5) + labs(title = "Age Most Likely to Become A Robbery Victim", x= "Victim Age", y="Robberies")

violent_crime$age_group <- cut(violent_crime$Victim_Age, breaks = c(0, 19, 35, 55, 99), labels = c("Teenager", "Young Adult", "Middle Age", "Elderly"))
age.group <- ddply(violent_crime, .(age_group, NIBRS_Crime_Category), summarise, count=length(age_group))
d9 <- age.group %>% ggplot(aes(reorder(x = NIBRS_Crime_Category, count), y = count)) + geom_col(fill="red") + geom_text(aes(label=count), color="black", hjust=-0.1, size=3) + coord_flip() + facet_wrap(~age_group) + labs(x="Total", y="Crime Description")


# plots using google maps

Dallas <- ggmap(get_map("Dallas, Texas"))
dallas_map <- get_map("Dallas", maptype = "satellite")
ggmap(dallas_map)
# doing an overall map of crime on Dallas is outside the computing power of my laptop,
# commented out the code below
# overall_crime_map <- ggmap(dallas_map, base_layer = ggplot(data= df, aes(x=long, y=lat)))
# overall_crime_map <- d + geom_point(color="red", size=3, alpha=0.5)

Dallas <- ggmap(get_map("Dallas, Texas"))
dallas_map <- get_map("Dallas", maptype = "terrain")
ggmap(dallas_map)
d <- ggmap(dallas_map, base_layer = ggplot(data= violent_crime, aes(x=long, y=lat)))
d <- d + geom_point(color="red", size=3, alpha=0.5)

d_assault <- ggmap(dallas_map, base_layer = ggplot(data= assault, aes(x=long, y=lat)))
d_assault <- d_assault + geom_point(color="red", size=3, alpha=0.5)
d_assault <- d_assault + theme_void() + labs(title = "Location of reported assaults", subtitle = "Dallas 2018", caption = "source: City of Dallas")
d_assault

d_murder <- ggmap(dallas_map, base_layer = ggplot(data= murder, aes(x=long, y=lat)))
d_murder <- d_murder + geom_point(color="red", size=3, alpha=0.5)
d_murder <- d_murder + theme_void() + labs(title = "Location of reported murders", subtitle = "Dallas 2018", caption = "source: City of Dallas")
d_murder

d_robbery <- ggmap(dallas_map, base_layer = ggplot(data= murder, aes(x=long, y=lat)))
d_robbery <- d_robbery + geom_point(color="red", size=3, alpha=0.5)
d_robbery <- d_robbery + theme_void() + labs(title = "Location of reported robberies", subtitle = "Dallas 2018", caption = "source: City of Dallas")
d_robbery

map_dallas_sat <- get_map('Dallas', zoom = 12, maptype = 'satellite')
ggmap(map_dallas_sat)
d7 <- ggmap(map_dallas_sat) + stat_density2d(data = violent_crime, aes(x = long, y = lat, fill = ..density..), geom = 'tile', contour = F, alpha = .5)
d7 <- d7 + scale_fill_viridis_c(option = "inferno")
d7 <- d7 + labs(title=str_c("Violent Crime in Dallas"),fill = str_c('Number of', '\ncrime incidents'))
d7 <- d7 + theme(text=element_text(color = "#444444"), plot.title=element_text(size=22, face='bold'), axis.text=element_blank(),axis.title = element_blank(), axis.ticks=element_blank())
d7 <- d7 + guides(fill = guide_legend(override.aes= list(alpha = 1)))

a7 <- ggmap(map_dallas_sat) + stat_density2d(data = assault, aes(x = long, y = lat, fill = ..density..), geom = 'tile', contour = F, alpha = .5) +  scale_fill_viridis_c(option = "inferno") + labs(title=str_c("Downtown Dallas has most assaults"),fill = str_c('Number of', '\nassaults')) + theme(text=element_text(color = "#444444"), plot.title=element_text(size=22, face='bold'), axis.text=element_blank(),axis.title = element_blank(), axis.ticks=element_blank())+ guides(fill = guide_legend(override.aes= list(alpha = 1)))

dallas_terrain <- ggmap(get_googlemap("Dallas, Tx"),zoom = 11, scale = 2,maptype ='terrain',color = 'color')
d10 <- dallas_terrain + geom_point(aes(x = long, y = lat,  colour = NIBRS_Crime_Category), data = violent_crime, size = 0.5) + 
  theme(legend.position="bottom")
d11 <- dallas_terrain + stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha =..level..),size = 0.2, bins = 30, data = violent_crime, geom = "polygon") + geom_density2d(data = violent_crime, aes(x = long, y = lat), size = 0.3) + facet_wrap(~NIBRS_Crime_Category, nrow=2)
d12 <- dallas_terrain +
  stat_bin2d(
    aes(x = long, y = lat, colour = NIBRS_Crime_Category, fill = NIBRS_Crime_Category),
    size = .5, bins = 30, alpha = 1/2,
    data = violent_crime
  )

d11_updated <- dallas_terrain + 
  stat_density2d(aes(x = long, y = lat, alpha =..level..),
                 size = 0.2, bins = 30, data = violent_crime, geom = "polygon") +
  geom_density2d(data = violent_crime, aes(x = long, y = lat), size = 0.3) +
  facet_wrap(~NIBRS_Crime_Category, nrow=2) +
  scale_fill_viridis_c(option = "inferno")

# this one didn't work, I didn't copy over the same ones for the specific types of violent crime
violent_crime_dallas <- cs_projectXY(violent_crime_latlon, varX = lat, varY = long)
d13 <- mapview(violent_crime_dallas)

DallasMap <- qmap(location = "dallas", zoom=14, color="bw", extent="device", legend="topleft")
bubble_chart <- DallasMap + geom_point(aes(x = long, y = lat, colour = NIBRS_Crime_Category, size = NIBRS_Crime_Category), data = violent_crime) + bubble_chart + scale_colour_discrete("Offense", labels = c("Assault","Murder","Robbery"))+ scale_size_discrete("Offense", labels = c("Assault","Murder","Robbery"))+  guides(size = guide_legend(override.aes = list(size = 6))) +theme(legend.key.size = grid::unit(1.8,"lines"), legend.title = element_text(size = 16, face = "bold"),legend.text = element_text(size = 14))+ bubble_chart + labs(colour = "Offense", size = "Offense")
bubble_chart_assault <- DallasMap + geom_point(aes(x = long, y = lat, colour = NIBRS_Crime_Category, size = NIBRS_Crime_Category), data = assault)+ + scale_colour_discrete("Offense", labels = c("Assault"))+ scale_size_discrete("Offense", labels = c("Assault"))
bubble_chart_robbery<- DallasMap + geom_point(aes(x = long, y = lat, colour = NIBRS_Crime_Category, size = NIBRS_Crime_Category), data = robbery)+ scale_colour_discrete("Offense", labels = c("Robbery"))+ scale_size_discrete("Offense", labels = c("Robbery"))
# tonerlitemap
d13 <- qmplot(long, lat, data = violent_crime, maptype = "toner-lite", color = NIBRS_Crime_Category, size = NIBRS_Crime_Category, legend = "topleft")
a13 <- qmplot(long, lat, data = assault, maptype = "toner-lite", color = NIBRS_Crime_Category, size = NIBRS_Crime_Category, legend = "topleft")
m13 <- qmplot(long, lat, data = murder, maptype = "toner-lite", color = NIBRS_Crime_Category, size = NIBRS_Crime_Category, legend = "topleft")
r13 <- qmplot(long, lat, data = robbery, maptype = "toner-lite", color = NIBRS_Crime_Category, size = NIBRS_Crime_Category, legend = "topleft")
#tonerlite2 map
d14 <- qmplot(long, lat, data = violent_crime, maptype = "toner-lite", color = NIBRS_Crime_Category, size = NIBRS_Crime_Category, legend = "topleft") + scale_colour_discrete("Offense", labels = c("Assaults","Murders","Robberies")) + scale_size_discrete("Offense", labels = c("Assaults","Murders","Robberies"),range = c(1.75,6))+ guides(size = guide_legend(override.aes = list(size = 6))) + theme(legend.key.size = grid::unit(1.8,"lines"), legend.title = element_text(size = 16, face = "bold"),legend.text = element_text(size = 14)) + labs(colour = "Offense", size = "Offense") 
#contourmap
d15 <- DallasMap + stat_density2d(aes(x = long, y = lat, colour = NIBRS_Crime_Category), size = 3, bins = 2, alpha = 3/4, data = violent_crime) + scale_colour_discrete("Offense", labels = c("Assault","Murders","Robberies"))+ theme(legend.text = element_text(size = 15, vjust = .5),legend.title = element_text(size = 15,face="bold"),legend.key.size = grid::unit(1.8,"lines"))
#densitymap 
dallas1 <- get_map("dallas", zoom = 14)
DallasMap1 <- ggmap(dallas1, extent = "device", legend = "topleft")
d17 <- DallasMap1 + stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 2, bins = 4, data = violent_crime, geom = "polygon")+ scale_fill_gradient("Violent\nCrime\nDensity") +scale_alpha(range = c(.4, .75), guide = FALSE) + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))
a17 <- DallasMap1 + stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 2, bins = 4, data = assault, geom = "polygon")+ scale_fill_gradient("Assault\nDensity") +scale_alpha(range = c(.4, .75), guide = FALSE) + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))
r17 <- DallasMap1 + stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 2, bins = 4, data = robbery, geom = "polygon")+ scale_fill_gradient("Robbery\nDensity") +scale_alpha(range = c(.4, .75), guide = FALSE) + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))
#countourmapbymonth
dallas2 <- get_map(location = "dallas", zoom = 14, color = "bw")
ggmap(dallas2)
DallasMap2 <- ggmap(dallas2, base_layer = ggplot(aes(x = long, y = lat), data = violent_crime))
d18 <- DallasMap2 + stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = ..level..), bins = I(5), geom = "polygon", data = violent_crime)+scale_fill_gradient2("Violent\nCrime\nDensity", low = "white", mid = "orange", high = "red", midpoint = 500)+ labs(x = "Longitude", y = "Latitude") + facet_wrap(~ Month) + scale_alpha(range = c(.2, .55), guide = FALSE) + ggtitle("Violent Crime Contour Map of Dallas by Month") + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))

