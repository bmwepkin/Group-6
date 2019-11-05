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
library(compstatr)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

library(ggmap)
#' D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1), 144-161. URL
#' http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
#' 
#' A BibTeX entry for LaTeX users is
#' 
#' @Article{,
#'   author = {David Kahle and Hadley Wickham},
#'   title = {ggmap: Spatial Visualization with ggplot2},
#'   journal = {The R Journal},
#'   year = {2013},
#'   volume = {5},
#'   number = {1},
#'   pages = {144--161},
#'   url = {https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf},
#' }

####Utility Functions####
getMyPalette1 <- function() {
  "YlOrRd"
}

getMyPalette2 <- function() {
  "BrBG"
}

getViolentCrimeList <- function() {
  violentCrimes <- c("Assault", "Homicide", "Robbery")
}

saveplot <- function(plot = NULL, name = "graphics", type = ".png", width = 6, height = 4) {
  name_with_extn <- paste(name, type, sep = "")
  
  ggsave(filename = name_with_extn, plot = plot, width = width, height = height, dpi = 600)
  message(sprintf("Saving %s", name_with_extn))
}

my_theme <- function(title=FALSE, axis=FALSE, legendtitle=TRUE, legend=TRUE) {
  t <- theme(text = element_text(size = 12), 
        panel.background = element_rect(fill = "azure3", colour = "darkslategray"),
        panel.grid.major.x = element_line(colour = "gainsboro"), 
        panel.grid.major.y = element_line(colour = "gainsboro"),
        panel.grid.minor.y = element_line(colour = "gainsboro"), 
        panel.grid.minor.x = element_line(colour = "gainsboro"),
        )
  
  if (title == FALSE) t <- t + theme(axis.title = element_blank())
  if (axis == FALSE) {
    t <- t + theme(axis.text = element_blank())
    t <- t + theme(axis.ticks = element_blank())
  }
  
  if (legend == FALSE) t <- t + theme(legend.position = "none")
  if (legendtitle == FALSE) t <- t + theme(legend.title = element_blank())
  
  t
}

splitLatLongFromLocation <- function(df) {
  message("Extracting Latitude and Longitude from Location1 Column")
  
  mat <- str_match(df$Location1, "\\((.*?), (.*?)\\)")
  df$lat <- as.double(mat[,2])
  df$long <- as.double(mat[,3])
  
  df
}

setupGoogleMaps <- function(api_key=NA) {
  message("Please provide an API Key for Google Maps to proceed.")
  # stop(is.na(api_key) == TRUE)
  
  require(devtools)
  devtools::install_github("dkahle/ggmap", ref = "tidyup")
  register_google(key=api_key)
}

####Data Functions####

filterRequiredColumns <- function(df) {
  columns <- c("Division",
               "Date1 of Occurrence",
               "Year1 of Occurrence",
               "Month1 of Occurence",
               "Day1 of the Week",
               "Time1 of Occurrence",
               "Date of Report",
               "Date incident created",
               "Council District",
               "Call Received Date Time",
               "Call Date Time",
               "Call Cleared Date Time",
               "Call Dispatch Date Time",
               "Victim Type",
               "NIBRS Crime Category",
               "NIBRS Code",
               "Zip Code",
               "Location1",
               "Victim Gender",
               "Victim Age",
               "Victim Ethnicity",
               "Victim Race",
               "Victim City")
  
    df <- select(df, columns)
}

renameColumns <- function(df) {
  message("Renaming Columns for easy access.")

  names(df) <- gsub(" ", "_", names(df))
  names(df) <- gsub("Day1", "Day", names(df))
  names(df) <- gsub("Date1", "Date", names(df))
  names(df) <- gsub("Year1", "Year", names(df))
  names(df) <- gsub("Month1", "Month", names(df))
  names(df) <- gsub("Time1", "Time", names(df))  
  df <- rename(df, Date_Incident_Created = Date_incident_created)
  
  df
}

convertDataTypes <- function(df) {
  message("Coercing Columns...")
  
  df$NIBRS_Crime_Category <- factor(df$NIBRS_Crime_Category)
  df$Council_District <- factor(df$Council_District)
  df$Division <- factor(df$Division)
  
  df$Victim_Gender <- factor(df$Victim_Gender)
  df$Victim_Race <- factor(df$Victim_Race)
  df$Victim_Type <- factor(df$Victim_Type)
  
  df <- mutate(df, Division = recode(Division, "CENTRAL" = "Central", "NORTH CENTRAL" = "North Central", 
                                     "NORTHEAST" = "Northeast", "NORTHWEST" = "Northwest", 
                                     "SOUTH CENTRAL" = "South Central", "SOUTHEAST" = "Southeast", 
                                     "SOUTHWEST" = "Southwest"))
               
  df <- mutate(df, Resident = factor(ifelse(Victim_City == 'DALLAS', "Resident", "Non-Resident")))
  
  df$Call_Dispatch_Date_Time <- as.POSIXct(df$Call_Dispatch_Date_Time, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
  df$Call_Cleared_Date_Time <- as.POSIXct(df$Call_Cleared_Date_Time, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
  df$Call_Date_Time <- as.POSIXct(df$Call_Date_Time, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
  df$Date_Incident_Created <- as.POSIXct(df$Date_Incident_Created, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
  df$Date_of_Occurrence<- as.Date(df$Date_of_Occurrence, format="%m/%d/%Y")
  
  df$Victim_Age <- as.integer(df$Victim_Age)
  
  df
}

factorHourofDay <- function(df) {
  message("Factoring Day and Hour.")

  df <- mutate(df, Day_of_the_Week = factor(Day_of_the_Week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
  df <- mutate(df, Hour = cut(Hour_of_the_Day, breaks = c(0, 4, 8, 12, 16, 20, 24), include.lowest = TRUE, right = TRUE, labels = c("F", "A", "B", "C", "D", "E")))
  df <- mutate(df, Hour = factor(Hour, levels = sort(levels(df$Hour))))
  df <- mutate(df, Hour = recode(Hour, "A" = "Early Morning (4AM - 8AM)", 
                                 "B" = "Morning (8AM - 12PM)", 
                                 "C" = "Afternoon (12PM - 4PM)", 
                                 "D" = "Evening (4PM - 8PM)", 
                                 "E" = "Night (8PM - 12AM)",
                                 "F" = "Mid Night (12AM - 4AM)"))  
}

factorVictimRace <- function(df) {
  message("Factoring Race of victim.")
  
  df <- mutate(df, Victim_Race = recode(Victim_Race, "White" = "White", "Black" = "Black", "Hispanic or Latino" = "Hispanic/Latino", .default = "Other"))
  
  # Group the Race and Summarize the incidents and reorder the factor based on it
  df1 <- ungroup(df) %>%
    group_by(Victim_Race) %>%
    summarize(num_incidents = n())
  df1 <- mutate(df1, Victim_Race = reorder(Victim_Race, -num_incidents))
  
  # Sort the levels based on the calculation above
  victim_races = levels(df1$Victim_Race)
  df <- mutate(df, Victim_Race = factor(Victim_Race, levels = victim_races))
  
  df
}

factorVictimAge <- function(df) {
  message("Factoring Age groups of Victim.")

  df <- mutate(df, Age_Group = cut(Victim_Age, breaks = c(0, 19, 35, 55, 99), 
                                   labels = c("Teenager", "Young Adult", "Middle Age", "Elderly")))  
  df
}

factorViolentCrimes <- function(df) {
  message("Factoring Crime Categories and indicating Violent/Non-Violent Crime type.")  

  df <- mutate(df, NIBRS_Crime_Category = recode(NIBRS_Crime_Category,  
  "MISCELLANEOUS" = "Miscellaneous",
  "LARCENY/ THEFT OFFENSES" = "Theft",
  "ASSAULT OFFENSES" = "Assault",
  "BURGLARY/ BREAKING & ENTERING" = "Burglary",
  "ALL OTHER OFFENSES" = "Other",
  "MOTOR VEHICLE THEFT" = "Motor Vehicle Theft",
  "ROBBERY" = "Robbery",
  "DESTRUCTION/ DAMAGE/ VANDALISM OF PROPERTY" = "Vandalism",
  "TRESPASS OF REAL PROPERTY" = "Trespass",
  "TRAFFIC VIOLATION - HAZARDOUS" = "Traffic Violations",
  "COUNTERFEITING / FORGERY" = "Forgery",
  "FAMILY OFFENSES, NONVIOLENT" = "Family Offenses",
  "DISORDERLY CONDUCT" = "Disorderly Conduct",
  "FRAUD OFFENSES" = "Fraud Offenses",
  "DRIVING UNDER THE INFLUENCE" = "DUI Offenses",
  "EMBEZZELMENT" = "Embezzelment",
  "TRAFFIC VIOLATION - NON HAZARDOUS" = "Traffic Violations",
  "DRUNKENNESS" = "Drunkenness",
  "HOMICIDE OFFENSES" = "Homicide",
  "ANIMAL OFFENSES" = "Animal Offenses",
  "ARSON" = "Arson",
  "WEAPON LAW VIOLATIONS" = "Weapon Law Violations",
  "KIDNAPPING/ ABDUCTION" = "Abduction",
  "DRUG/ NARCOTIC VIOLATIONS" = "Drug Violations",
  "LIQUOR LAW VIOLATIONS" = "Liquor Law Violations",
  "BRIBERY" = "Bribery",
  "PORNOGRAPHY/ OBSCENE MATERIAL" = "Pornography"
  ))
  
  df <- mutate(df, Violent_Crime_Category = NIBRS_Crime_Category)
  
  violent_crimes_levels = getViolentCrimeList()
  df <- mutate(df, Violent_Crime_Category = recode(Violent_Crime_Category, "Assault" = violent_crimes_levels[1], 
                                                   "Homicide" = violent_crimes_levels[2], 
                                                   "Robbery" = violent_crimes_levels[3] , .default = "Other"))
  
  crime_levels = levels(df$Violent_Crime_Category)
  df <- mutate(df, Violent_Crime_Category = factor(Violent_Crime_Category, 
                                                 levels = c(sort(crime_levels[crime_levels != "Other"]), "Other")))
  
  df <- mutate(df, Broad_Crime_Type = factor(ifelse(Violent_Crime_Category %in% violent_crimes_levels, "Violent", "Non-Violent")))

  df
}

read_PoliceIncidents <- function(filename, year = NA, url) {
  df <- NULL
  
  datafilename = filename
  
  filtered_file_exists <- FALSE
  
  if (!is.na(year)) {
    # First check whether the filename exists by the year passed (ie., Police_Incidents_2018.csv)
    filename_by_year <- sprintf("%s_%s.csv", filename, year)
    if (file.exists(filename_by_year)) {
      datafilename = filename_by_year
      filtered_file_exists <- TRUE
    }
  }
  
  if (grepl(".csv", datafilename) == FALSE) {
    datafilename <- sprintf("%s.csv", datafilename)
  }

  # Check if the file to be read exists
  if (!file.exists(datafilename)) {
      # If the data is not available in the working directory, download it from the url
      message ("Downloading Police Incidents file. This is a large file, the download may take several minutes...")
      download.file(url, datafilename)
      message ("Download complete. Reading Police Incidents file...")      
  }
  
  # Once it reaches this point, the data is either available already in the working directory or downloaded now
  my_na <- c("", "0", "UNK", "G", "J", "None", "N", "Unknown", "NA")
  my_cols <- cols(`Hate Crime` = col_character(), `Victim Business Phone` = col_character(),
                  `Hate Crime Description` = col_character(), `Apartment Number` = col_character(),
                  `Victim Apartment` = col_character(), `Victim Zip Code` = col_character())
  
  message (sprintf("Reading Police Incidents file...%s", datafilename))
  df <- read_csv(datafilename, col_types = my_cols, na = my_na)
  
  if (!is.na(year)) {
    if (!filtered_file_exists) {
      message(sprintf("Filtering to %s...", year))
      df <- filter(df, `Year of Incident` == year)
      
      filename_by_year <- sprintf("%s_%s.csv", filename, year)
      message(sprintf("Writing...%s", filename_by_year)) 
      write_csv(df, filename_by_year)
    }
  } else {
    message("Importing all years.")
  } 
  
  df
}


create_response_time <- function(df) {
  message("Creating response time values. Response time is the number of minutes from 911 call to police dispatch.") 

  df <- mutate(df, Response_Time = as.numeric(difftime(df$Call_Dispatch_Date_Time, df$Call_Date_Time, units = "mins")))
  df <- mutate(df, Month = factor(months(df$Date_of_Occurrence, abbreviate = TRUE), levels = month.abb))
  df <- mutate(df, Day_of_Month = as.POSIXlt(df$Date_of_Occurrence)$mday)
  df <- mutate(df, Hour_of_the_Day = as.numeric(format(as.POSIXct(df$Time_of_Occurrence,format="%H:%M:%S"),"%H")))
  
  df <- filter(df, Response_Time > 0, Response_Time < 5000)
  
  df
}

processPoliceIncidents <- function(df, removeNAs = TRUE) {
  message("Processing data...")

  df <- filterRequiredColumns(df)
  
  # columns <- c(getColumnswithLessNAs(df, 10000), "Victim Gender", "Victim Age", "Victim Ethnicity", "Victim Race")
  # df <- select (df, columns)
  
  # Filter all the rows that has NA
  if (removeNAs == TRUE) {
    df <- df[complete.cases(df), ]
  }
  
  message(sprintf("Cleaned up the data and the final number of rows are : %s", nrow(df)))

  df <- df %>%  
    splitLatLongFromLocation() %>% 
    renameColumns() %>%
    convertDataTypes() %>%
    create_response_time() %>%
    factorHourofDay() %>% 
    factorViolentCrimes() %>% 
    factorVictimRace() %>% 
    factorVictimAge()

  message("Processing Complete.")
  
  df
}

####Plot Functions#### 

plotTopOffenses <- function(df, filename, n = 10, fwidth = 8) {
  message("Creating a plot of the top 10 offenses.") 

  violent_crimes_levels = getViolentCrimeList()
  
  Crime_Categories <- data.frame(table(df$NIBRS_Crime_Category))
  colnames(Crime_Categories) <- c("Category", "Frequency")

  Crime_Categories <- Crime_Categories %>%
                      filter(Frequency > 100) %>% 
                      mutate(Percentage = scales::percent(Frequency/sum(Frequency))) %>%
                      mutate(Broad_Crime_Type = factor(ifelse(Category %in% violent_crimes_levels, "Violent", "Non-Violent"))) %>% 
                      filter(!(Category %in% c("Other", "Miscellaneous"))) %>%
                      top_n(10, Frequency) %>%
                      arrange(desc(Frequency))

  p <- ggplot(Crime_Categories, aes(x=reorder(Category, -Frequency), y=Frequency, fill=Broad_Crime_Type)) + 
    geom_bar(stat="identity") + 
    scale_fill_brewer(palette = getMyPalette1()) +
    my_theme(axis = TRUE, legendtitle = FALSE) +
    theme(axis.text.x=element_text(angle = -45))
  
  saveplot(p, filename, width = fwidth)
  
  p
} 

plotTotalCrimes <- function(df, filename, fwidth = 8) {
  message("Creating plot of total police incidents by month.")

  p <- qplot(Month, data = df, geom = "bar", fill = Broad_Crime_Type) +
      scale_fill_brewer(palette = getMyPalette1()) +
      my_theme(axis = TRUE, legend = FALSE)

  saveplot(p, filename, width = fwidth)
  
  p
}

plotTotalCrimesByDivision <- function(df, filename, fwidth = 8){
  message("Creating plot of total police incidents by division.")
  
  p <- qplot(Division, data = df, geom = "bar", fill = Broad_Crime_Type) +
    scale_fill_brewer(palette = getMyPalette1()) +
    theme(axis.text.x = element_text(angle = -45)) +
    theme(legend.title = element_blank()) +
    my_theme(axis = TRUE)
  
  saveplot(p, filename, width = fwidth)
  
  p
}

plotViolentCrimesByMonth <- function(df, filename, fwidth = 8){
  message("Creating plot of violent crime by month")

  p <- qplot(Month, data = df, geom = "bar", fill = Violent_Crime_Category) +
      scale_fill_brewer(name = "Type", palette = getMyPalette1(), direction = -1) +
      my_theme(axis = TRUE)
  
  saveplot(p, filename, width = fwidth)
  
  p
}


plotViolentCrimesByDivision <- function(df, filename, fwidth = 8){
  message("Creating plot of violent crimes by division.")
  
  p <- qplot(Division, data = df, geom = "bar", fill = Violent_Crime_Category) +
    scale_fill_brewer(palette = getMyPalette1(), direction = -1) +
    theme(axis.text.x = element_text(angle = -45)) +
    theme(legend.title = element_blank()) +
    my_theme(axis = TRUE)
  
  saveplot(p, filename, width = fwidth)
  
  p
}

plotResponseTime_InDifferentAspects <- function(df, year, suffix = "") {
  plotResponseTime(df, "Month", paste("Reponse_By_Month", suffix, year, sep = "_"))
  plotResponseTime(df, "Day_of_the_Week", paste("Reponse_By_DayofWeek", suffix, year, sep = "_"))
  plotResponseTime(df, "Day_of_Month", paste("Reponse_By_DayofMonth", suffix, year, sep = "_"))
  plotResponseTime(df, "Hour", paste("Reponse_By_HouroftheDay", suffix, year, sep = "_"), flip = TRUE)
  
  message("Completed plotting and saving Charts.")
}

plotResponseTime <- function(df, xcol, filename, flip = FALSE) {
  # Reverse the Levels to start from Early Morning when flipped
  # if (flip == TRUE) {
  #   df[xcol] <- factor(xcol, rev(levels(df[[xcol]])))
  # }

  df <- ungroup(df) %>%
    group_by_(xcol) %>%
    summarize(num_incidents = n(), med_response = median(Response_Time)) %>%
    mutate(response = cut(med_response, breaks = quantile(med_response), include.lowest = TRUE, labels = c("Fastest", "Fast", "Slow", "Slowest")))
  
  p  <- ggplot(df, aes(x = df[[xcol]], y = num_incidents, fill = response)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = getMyPalette1(), name = "Response Time", direction = -1, type = "qual") +
    xlab(gsub("_", " ", xcol)) + ylab("No. of Incidents") +
    my_theme(axis = TRUE)
  
  if (flip == TRUE) {
    p <- p + coord_flip()
  }
  
  saveplot(p, filename)
  
  p
}

plotResponseTimeByDivision <- function(df, filename, flip = FALSE) {
  totalIncidents <- nrow(df)
  
  df <- ungroup(df) %>%
    group_by(Division) %>%
    summarize(num_incidents = n(), med_response = median(Response_Time), share = scales::percent(n()/totalIncidents)) %>%
    mutate(response = cut(med_response, breaks = quantile(med_response), include.lowest = TRUE, 
                          labels = c("Fastest", "Fast", "Slow", "Slowest")))
  
  p  <- ggplot(df, aes(x = reorder(Division, num_incidents), y = num_incidents, fill = response)) +
    geom_col(position = "dodge") +
    geom_text(nudge_y = -350, aes(label = share)) +
    scale_fill_brewer(palette = getMyPalette1(), name = "Response Time", direction = -1) +
    my_theme(axis = TRUE) +
    theme(axis.text.x = element_text(angle = -45))
  
  if (flip == TRUE) {
    p <- p + coord_flip()
  }

  saveplot(p, filename)

  df
}

plotHourlyResponseTimeByDivision <- function(df, filename, flip = FALSE) {
  # Reverse the Levels to start from Early Morning when flipped
  if (flip == TRUE) {
    df <- mutate(df, Hour = factor(Hour, rev(levels(Hour))))
    levels(df$Hour) <- sub("?.\\(.*\\)", "", levels(df$Hour))
  }
  
  df <- ungroup(df) %>%
    group_by(Division, Hour) %>%
    summarize(num_incidents = n(), med_response = median(Response_Time)) %>%
    mutate(response = cut(med_response, breaks = quantile(med_response), include.lowest = TRUE, 
                          labels = c("Fastest", "Fast", "Slow", "Slowest")))
  
  p  <- ggplot(df, aes(x = Hour, y = num_incidents, fill = response)) +
    geom_col(position = position_dodge2()) +
    facet_wrap(. ~ Division) +
    scale_fill_brewer(palette = getMyPalette1(), name = "Response Time", direction = -1) +
    my_theme(axis = TRUE) +
    theme(axis.text.x = element_blank()) +
    theme(axis.ticks.x = element_blank())

  if (flip == TRUE) {
    p <- p + coord_flip()
  }
  
  saveplot(p, filename)
  
  df
}

plotViolentCrimesByAgeGroup <- function(df, filename, flip = FALSE) {
  df <- filter(df, Victim_Age > 0, Victim_Age < 100)
  
  df <- ungroup(df) %>%
    group_by(Age_Group, Violent_Crime_Category) %>%
    summarize(num_incidents = n())
  
  p  <- ggplot(df, aes(x = Age_Group, y = num_incidents, fill = Violent_Crime_Category)) +
    geom_col(position = position_dodge2()) +
    scale_fill_brewer(palette = "YlOrRd", name = "Type", direction = -1, type = "qual") +
    my_theme()
  
  p <- p + coord_flip()
  
  saveplot(p, filename)

  p
}

plotAgeGroupByRace <- function(df, name, flip = FALSE) {
  df <- filter(df, Victim_Age > 0, Victim_Age < 100)
  
  df <- ungroup(df) %>%
    group_by(Victim_Race, Age_Group) %>%
    summarize(num_incidents = n())
  
  p  <- ggplot(df, aes(x = Age_Group, y = num_incidents, fill = Victim_Race)) +
    geom_col(position = position_dodge2()) +
    scale_fill_brewer(palette = getMyPalette2(), name = "Race") +
    xlab(gsub("_", " ", "Age_Group")) + ylab("No. of Incidents") +
    my_theme()
  
  if (flip == TRUE) {
    p <- p + coord_flip()
  }
  
  saveplot(p, name)

  p
}

plotViolentCrimesByAgeGroupAndGender <- function(df, name, flip = FALSE) {
  df <- filter(df, Victim_Age > 0, Victim_Age < 100)
  
  df <- ungroup(df) %>%
    group_by(Age_Group, Victim_Gender, Violent_Crime_Category) %>%
    summarize(num_incidents = n())
  
  p  <- ggplot(df, aes(x = Violent_Crime_Category, y = num_incidents, fill = Victim_Gender)) +
    geom_col(position = position_dodge2()) +
    facet_wrap(. ~ Age_Group) +
    scale_fill_brewer(palette = getMyPalette1(), name = "Gender", direction = 1) +
    my_theme()
  
  if (flip == TRUE) {
    p <- p + coord_flip()
  }
  
  saveplot(p, name)

  p
}


plotByVictimRacesAsLollipop <- function(df, name, flip = FALSE) {
  df <- ungroup(df) %>%
    group_by(Victim_Race, Violent_Crime_Category) %>%
    summarize(num_incidents = n())
  
  p  <- ggplot(df, aes(x = Victim_Race, y = num_incidents)) +
    geom_linerange(aes(x = Victim_Race, ymin=0, ymax=num_incidents, color = Victim_Race), size=1.5, position = position_dodge2(width = 0.85)) +
    geom_point(position = position_dodge2(width = 0.85), aes(fill= Violent_Crime_Category), size=5, shape=21, stroke=1) +
    scale_color_brewer(palette = getMyPalette2(), name = "Race") +
    scale_fill_brewer(palette = getMyPalette1(), name = "Type", direction = -1) +
    xlab("Race of Victim") + ylab("No. of Incidents") +
    my_theme()
  
  if (flip == TRUE) {
    p <- p + coord_flip()
  }
  
  saveplot(p, name)
  
  p
}

plotCrimeByTimeofDay <- function(df, name, flip = FALSE) {
  # Reverse the Levels to start from Early Morning when flipped
  if (flip == TRUE) {
    df <- mutate(df, Hour = factor(Hour, rev(levels(Hour))))
  }
  
  df <- ungroup(df) %>%
    group_by(Hour, Violent_Crime_Category) %>%
    summarize(num_incidents = n())
  
  p  <- ggplot(df, aes(x = Hour, y = num_incidents, fill = Violent_Crime_Category)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = getMyPalette1(), name = "Type", direction = -1, type = "qual") +
    my_theme()
  
  if (flip == TRUE) {
    p <- p + coord_flip()
  }
  
  saveplot(p, name)

  p
}

plotCrimeByDayoftheWeek <- function(df, name, flip = FALSE) {
  df <- ungroup(df) %>%
    group_by(Day_of_the_Week, Violent_Crime_Category) %>%
    summarize(num_incidents = n())
  
  p  <- ggplot(df, aes(x = Day_of_the_Week, y = num_incidents, fill = Violent_Crime_Category)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = getMyPalette1(), name = "Type", direction = -1, type = "qual") +
    my_theme(axis = TRUE)
  
  if (flip == TRUE) {
    p <- p + coord_flip()
  }
  
  saveplot(p, name)

  p
}

plotResidentsByDivisions <- function(df, filename) {
    p <- qplot(Division, data = df, geom = "bar", fill = Resident) +
        scale_fill_brewer(palette = getMyPalette1()) +
        my_theme(axis = TRUE, legendtitle = FALSE) +
        theme(axis.text.x = element_text(angle = -45)) +
        theme(axis.ticks.x = element_blank())

    saveplot(p, filename)
    
    p
}

plotCrimeHeatMap <- function(df, filename) {
  map_dallas_sat <- get_map('Dallas', zoom = 12, maptype = 'satellite')
  p <- ggmap(map_dallas_sat) + stat_density2d(data = df, aes(x = long, y = lat, fill = ..density..), geom = 'tile', contour = F, alpha = .5) +
      scale_fill_viridis_c(option = "inferno") +
      labs(fill = str_c('No. of', '\nIncidents')) +
      my_theme() +
      guides(fill = guide_legend(override.aes= list(alpha = 1)))
  
  saveplot(p, filename)
  
  p
}

plotCrimeConcentrationMap <- function(df, filename) {
  dallasmap <- get_map("dallas", zoom = 10, scale = 2,maptype ='terrain',color = 'color')

  p <- ggmap(dallasmap) + 
    stat_density2d(data = df, aes(x = long, y = lat, fill = ..level.., alpha =..level..), size = 0.2, bins = I(30), geom = "polygon") +
    geom_density2d(data = df, aes(x = long, y = lat), size = 0.3, color = I("red")) +
    scale_fill_gradient2("Violent\nCrime\nDensity", low = "orange", mid = "orange4", high = "orangered4", midpoint = 500) +
    facet_wrap(~Violent_Crime_Category) +
    guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
    my_theme(legend = FALSE)
  
  saveplot(p, filename)
  
  p  
}

plotCrimeDensityMap <- function(df, filename) {
  dallasmap <- get_map("dallas", zoom = 14)
  
  p <- ggmap(dallasmap, extent = "device", legend = "topleft") +
      stat_density2d(data = df, aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 2, bins = I(5), geom = "polygon") +
      scale_fill_gradient2("Violent\nCrime\nDensity", low = "white", mid = "orange", high = "red", midpoint = 500) +
      scale_alpha(range = c(.4, .75), guide = FALSE) + 
      guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
      my_theme()

  saveplot(p, filename)
  
  p
}

plotCrimeDensityByMonthMap <- function(df, filename) {
  dallasmap <- get_map(location = "dallas", zoom = 14, color = "bw")
  
  p <- ggmap(dallasmap, base_layer = ggplot(aes(x = long, y = lat), data = df)) +
    stat_density2d(data = df, aes(x = long, y = lat, fill = ..level.., alpha = ..level..), bins = I(5), geom = "polygon") +
    scale_fill_gradient2("Violent\nCrime\nDensity", low = "white", mid = "orange", high = "red", midpoint = 500) +
    labs(x = "Longitude", y = "Latitude") +
    facet_wrap(~ Month) + 
    scale_alpha(range = c(.2, .55), guide = FALSE) +
    guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
    my_theme()
    
  saveplot(p, filename)
  
  p
}


####Main####
main <- function() {
  # setupGoogleMaps("<API Key goes here>")
  
  # setupGoogleMaps("AIzaSyAuGTvZR1abbOcMQxutQt5127L8AXExQOo")
  
  year <- 2018
  data_url <- "https://www.dallasopendata.com/api/views/qv6i-rri7/rows.csv"

  Police_Incidents <- read_PoliceIncidents("Police_Incidents", year, data_url)  # Provide filename with extn

  Police_Incidents <- processPoliceIncidents(Police_Incidents)
  
  Violent_Crimes <- filter(Police_Incidents, Violent_Crime_Category %in% getViolentCrimeList())
  
  # Generate Various Plots covered in the slides
  plotTotalCrimes(Police_Incidents, "TotalCrimes")
  plotTopOffenses(Police_Incidents, "Top_Offenses", fwidth = 8)
  plotViolentCrimesByMonth(Violent_Crimes, "ViolentCrimesByMonth")
  plotTotalCrimesByDivision(Police_Incidents, "TotalCrimesByDivision")
  plotViolentCrimesByDivision(Violent_Crimes, "ViolentCrimesByDivision")
  
  plotCrimeConcentrationMap(Violent_Crimes, "ViolentCrimeConcentration")
  plotCrimeDensityMap(Violent_Crimes, "ViolentCrimeDensityMap")
  plotCrimeHeatMap(Violent_Crimes, "ViolentCrimeHeatMap")
  plotCrimeDensityByMonthMap(Violent_Crimes, "ViolentCrimeDensityMap_ByMonth")
  
  plotCrimeByDayoftheWeek(Violent_Crimes, paste("ViolentCrimesByDayoftheWeek", year, sep = "_"), FALSE)
  plotCrimeByTimeofDay(Violent_Crimes, paste("ViolentCrimesByTimeofDay", year, sep = "_"), TRUE)
  plotResidentsByDivisions(Police_Incidents, "ResidentsByDivision")

  plotResponseTime_InDifferentAspects(Police_Incidents, year)
  plotResponseTime_InDifferentAspects(Violent_Crimes, year, "Violent")
  plotResponseTimeByDivision(Police_Incidents, "ResponseByDivision")
  plotHourlyResponseTimeByDivision(Police_Incidents, "HourlyResponseByDivision", TRUE)

  plotByVictimRacesAsLollipop(Violent_Crimes, paste("CrimesByRace", year, sep = "_"), FALSE)
  plotViolentCrimesByAgeGroup(Violent_Crimes, paste("CrimesByAgeGroup", year, sep = "_"))
  plotViolentCrimesByAgeGroupAndGender(Violent_Crimes, paste("CrimesByGender", year, sep = "_"))
  plotAgeGroupByRace(Police_Incidents, paste("RaceByAgeGroup", year, sep = "_"))
}

main()
