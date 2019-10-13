setwd("~/Desktop/R Fall/Dallas Project")

library(readr)
#install dplyr
library(dplyr)

#defing na and assign to my_na
my_na <- c("", "0", "UNK", "G", "J", "None", "N", "Unknown", "NA")
#read in raw data file; (check file name prior to read_csv)
df <- read_csv("Police_Incidents (1).csv", na=my_na)
#filter for 2018
df <- filter(df, `Year of Incident` == 2018)
#define columns for us in project
my_cols <- c("Type of Incident", "Type  Location", "Incident Address", "Reporting Area", "Division", "Council District", "Target Area Action Grids", "Date1 of Occurrence", "Day1 of the Week", "Time1 of Occurrence", "Date of Report", "Date incident created", "Call Date Time", "Call Cleared Date Time", "Call Dispatch Date Time", "Person Involvement Type", "Victim Type", "Victim Name", "Victim Race", "Victim Gender", "Victim Age", "Offense Status", "Hate Crime", "Weapon Used", "Gang Related Offense", "Drug Related Istevencident", "NIBRS Crime Category", "Update Date", "X Coordinate", "Y Cordinate", "Zip Code")
df <- df[, my_cols]
library(readxl)
data_description <- read_excel("Dallas_Data_Definitions.xlsx", sheet = 1)
#Rename columns, removing space
names(df)[names(df) == "Y Cordinate"] <- "Y_Coordinate" 
names(df)[names(df) == "Zip Code"] <- "Zip_Code" 
names(df)[names(df) == "X Coordinate"] <- "X_Coordinate" 
names(df)[names(df) == "Update Date"] <- "Update_Date" 
names(df)[names(df) == "NIBRS Crime Category"] <- "NIBRS_Crime_Category" 
names(df)[names(df) == "Drug Related Istevencident"] <- "Drug_Related_Incident" 
names(df)[names(df) == "Gang Related Offense"] <- "Gang_Related_Offense" 
names(df)[names(df) == "Weapon Used"] <- "Weapon_Used" 
names(df)[names(df) == "Hate Crime"] <- "Hate_Crime" 
names(df)[names(df) == "Offense Status"] <- "Offense_Status" 
names(df)[names(df) == "Victim Age"] <- "Victim_Age" 
names(df)[names(df) == "Victim Gender"] <- "Victim_Gender"
names(df)[names(df) == "Target Area Action Grids"] <- "Target_Area_Action_Grids" 
names(df)[names(df) == "Council District"] <- "Council_District" 
names(df)[names(df) == "Reporting Area"] <- "Reporting_Area" 
names(df)[names(df) == "Incident Address"] <- "Incident_Address" 
names(df)[names(df) == "Type  Location"] <- "Type_Location" 
names(df)[names(df) == "Type of Incident"] <- "Type_of_Incident" 
names(df)[names(df) == "Person Involvement Type"] <- "Person_Involvement_Type" 
names(df)[names(df) == "Call Dispatch Date Time"] <- "Call_Dispatch_Date_Time" 
names(df)[names(df) == "Call Cleared Date Time"] <- "Call_Cleared_Date_Time" 
names(df)[names(df) == "Call Date Time"] <- "Call_Date_Time"
names(df)[names(df) == "Victim Race"] <- "Victim_Race"
names(df)[names(df) == "Victim Type"] <- "Victim_Type"
names(df)[names(df) == "Date incident created"] <- "Date_Incident_Created"
names(df)[names(df) == "Date of Report"] <- "Date_of_Report"
names(df)[names(df) == "Day1 of Occurrence"] <- "Day_of_Occurrence"
names(df)[names(df) == "Date1 of Occurrence"] <- "Date_of_Occurrence"
names(df)[names(df) == "Day1 of the Week"] <- "Day_of_the_Week"
df$NIBRS_Crime_Category <- factor(df$NIBRS_Crime_Category)
df$Drug_Related_Incident <- factor(df$Drug_Related_Incident)
df$Gang_Related_Offense <- factor(df$Gang_Related_Offense)
df$Weapon_Used <- factor(df$Weapon_Used)
df$Hate_Crime <- factor(df$Hate_Crime)
df$Offense_Status <- factor(df$Offense_Status)
df$Victim_Age <- as.integer(df$Victim_Age)
df$Victim_Gender <- factor(df$Victim_Gender)
df$Victim_Race <- factor(df$Victim_Race)
df$Victim_Type <- factor(df$Victim_Type)
levels(df$Victim_Type) [levels(df$Victim_Type) == "Financial Institutio"] <- "Financial Institution"
levels(df$Victim_Type) [levels(df$Victim_Type) == "Law Enforcement Offi"] <- "Law Enforcement Officer"
levels(df$Victim_Type) [levels(df$Victim_Type) == "Religious Organizati"] <- "Religious Organization"
df$Person_Involvement_Type <- factor(df$Person_Involvement_Type)
df$Call_Dispatch_Date_Time <- as.POSIXct(df$Call_Dispatch_Date_Time, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
df$Call_Cleared_Date_Time <- as.POSIXct(df$Call_Cleared_Date_Time, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
df$Call_Date_Time <- as.POSIXct(df$Call_Date_Time, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
df$Date_Incident_Created <- as.POSIXct(df$Date_Incident_Created, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
df$Date_of_Report <- as.POSIXct(df$Date_of_Report, format="%m/%d/%Y %H:%M:%S", tz="America/Chicago")
df$Day_of_the_Week <- factor(df$Day_of_the_Week)
levels(df$Day_of_the_Week) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
df$Date_of_Occurrence<- as.Date(df$Date_of_Occurrence, format="%m/%d/%Y")
df$Council_District <- factor(df$Council_District)
df$Division <- factor(df$Division)
df$Reporting_Area <- as.integer(df$Reporting_Area)
df$Type_Location <- factor(df$Type_Location)
df <- subset(df, Victim_Age >0 & Victim_Age <100)
df <- subset(df, Zip_Code >=75001 & Zip_Code <=76217)
df <- arrange(df, Call_Dispatch_Date_Time)
df <- subset(df, Council_District !="9")
#Start Filter out some observations outsied of 2018
df <- df[(7:66385), ]
df <- arrange(df, desc(Call_Cleared_Date_Time))
df <- df[(17:66369), ]
df <- arrange(df, Date_of_Report)
df <- df[(10:66353), ]
df <- df[(1:66344), ]
df <- arrange(df, Date_of_Occurrence)
df <- df[(747:66344), ]
df <- df[(1:65596), ]
#End Filter out some observations outsied of 2018
df$Zip_Code <- factor(df$Zip_Code)
df <- arrange(df, desc(Update_Date))
df <- df[(11878:65596), ]
df$Hate_Crime <- NULL

#Plans
#Formating the date and time for response time - Jon
#Handling of NAs - Viswa
#When does crime occur most, months days of the week, etc - Jon
#Intital plotting incedents by zip code/neighborhood/council districts(12-14) - Brandon
  #Gender or race as potential other feateures to plot - Jay
#Another option to filter on another year and view trends
#Violent crime maps, type of crime groupings of crime category NIBRS Crime category - Nimisha
