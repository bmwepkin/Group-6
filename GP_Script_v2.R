# set working directory as appropriate
# use install.packages(c("readr", "plyr", "dplyr", "reshape2", "ggplot2", "stringr", "ggrepel", "devtools", "ggmap", "compstatr")) 
# to install the necessary packages

# load the packages
library(readr)
library(plyr)
suppressPackageStartupMessages(library(dplyr))
library(reshape2)
suppressPackageStartupMessages(library(ggplot2))
library(stringr)
library(ggrepel)
library(devtools)
library(ggmap)
library(compstatr)


# set up google maps
require(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
# register_google(key="") add your own key

# create the data frame
url <- "https://www.dallasopendata.com/api/views/qv6i-rri7/rows.csv"
download.file(url, "initial_dataset.csv")
my_na <- c("", "0", "UNK", "G", "J", "None", "N", "Unknown", "NA")
df <- read_csv("initial_dataset.csv", na=my_na, col_types = cols(
  `Hate Crime` = col_character(), `Victim Business Phone` = col_character(),
  `Hate Crime Description` = col_character(), `Apartment Number` = col_character(),
  `Victim Apartment` = col_character(), `Victim Zip Code` = col_character()))

# create functions to be used

### IMPORTANT - functions don't actually change the dataframe (df) as that is
### a global object. For several of the functions created, we want the changes
### to occur at the global level, so using a function wouldn't be appropriate.
### I've updated the code so that these changes are performed globally.

saveplot <- function(plot = NULL, name = "graphics", type = ".png", width = 6, height = 4) {
  ggsave(filename = paste(name, type, sep = ""), plot = plot, width = width, height = height, dpi = 600)
}

plotResponseTime <- function(df, xcol, name) {
  df <- ungroup(df)
  df <- group_by_(df, xcol) 
  df <- summarize(df, num_incidents = n(), med_response = median(Response_Time))
  df <- mutate(df, response = cut(med_response, breaks = quantile(med_response), 
                                  include.lowest = TRUE, labels = c("Fastest", "Fast", "Slow", "Slowest")))
  p  <- ggplot(df, aes(x = df[[xcol]], y = num_incidents, fill = response)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = "YlOrRd", name = "Response Time", direction = -1, type = "qual") +
    xlab(gsub("_", " ", xcol)) + ylab("Total Incidents") +
    theme(text = element_text(size = 12)) +
    theme(panel.background = element_rect(fill = "azure3", colour = "slategray2" )) +
    ggtitle("How responsive the Dallas Police Dept is?")
  
  saveplot(p, name)
  message(sprintf("Saving %s", name))
  
  p
}

analyzeAndPlot_ResponseTime <- function(df, year) {
  message("Analyzing Response time...")
  
  df <- mutate(df, Response_Time = as.numeric(difftime(df$Call_Dispatch_Date_Time, df$Call_Date_Time, units = "mins")))
  df <- mutate(df, Month = factor(months(df$Date_of_Occurrence, abbreviate = TRUE), levels = month.abb))
  df <- mutate(df, Day_of_Month = as.POSIXlt(df$Date_of_Occurrence)$mday)
  
  df <- filter(df, Response_Time > 0, Response_Time < 5000)
  
  plotResponseTime(df, "Month",  paste("Reponse_By_Month", year, sep = "_"))
  plotResponseTime(df, "Day_of_the_Week", paste("Reponse_By_DayofWeek", year, sep = "_"))
  plotResponseTime(df, "Day_of_Month", paste("Reponse_By_DayofMonth", year, sep = "_"))
  
  message("Completed plotting and saving Charts.")
}

# transform the data frame

# rename columns
names(df) <- gsub(" ", "_", names(df))
names(df)[names(df) == "Date_incident_created"] <- "Date_Incident_Created"
names(df) <- gsub("Day1", "Day", names(df))
names(df) <- gsub("Date1", "Date", names(df))
names(df) <- gsub("Year1", "Year", names(df))
names(df) <- gsub("Month1", "Month", names(df))
names(df) <- gsub("Time1", "Time", names(df))
             
# filter to 2018
df <- filter(df, Year_of_Incident == 2018)


# split latitude and longitude from location
mat <- str_match(df$Location1, "\\((.*?), (.*?)\\)")
df$lat <- as.double(mat[,2])
df$long <- as.double(mat[,3])

# update column types
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

# add columns for response time
df <- mutate(df, Response_Time = as.numeric(difftime(df$Call_Dispatch_Date_Time, df$Call_Date_Time, units = "mins")))
df <- mutate(df, Month = factor(months(df$Date_of_Occurrence, abbreviate = TRUE), levels = month.abb))
df <- mutate(df, Day_of_Month = as.POSIXlt(df$Date_of_Occurrence)$mday)
df <- mutate(df, Hour = format(as.POSIXct(df$Time_of_Occurrence,format="%H:%M:%S"),"%H"))

# create victim age groups
df$age_group <- cut(df$Victim_Age, breaks = c(0, 19, 35, 55, 99), 
                    labels = c("Teenager", "Young Adult", "Middle Age", "Elderly"))

# Add Column as classifier of violent/nonviolent crime

mat1 <- df$NIBRS_Crime_Category == "ASSAULT OFFENSES" | df$NIBRS_Crime_Category == "HOMICIDE OFFENSES"| df$NIBRS_Crime_Category == "ROBBERY"
df$v_nv <- "Nonviolent"
df$v_nv[mat1] <- "Violent"

# create additional subsets of df
violent_crime <- subset(
  df, NIBRS_Crime_Category=="ASSAULT OFFENSES" | NIBRS_Crime_Category=="HOMICIDE OFFENSES"| NIBRS_Crime_Category =="ROBBERY")
df_crime_category <- sort(table(df$NIBRS_Crime_Category), decreasing = TRUE)
df_crime_category <- data.frame(df_crime_category[df_crime_category > 100])
colnames(df_crime_category) <- c("Category", "Frequency")
df_crime_category$Percentage <- df_crime_category$Frequency / sum(df_crime_category$Frequency)
violent_crime_latlon <- violent_crime[is.na(violent_crime$lat) == FALSE, ]
violent_crime_latlon <- violent_crime[is.na(violent_crime$long) == FALSE, ]
mylist <- split(violent_crime, violent_crime$NIBRS_Crime_Category)
assault <- as.data.frame(mylist$`ASSAULT OFFENSES`)
murder <- as.data.frame(mylist$`HOMICIDE OFFENSES`)
robbery <- as.data.frame(mylist$ROBBERY)

# create plots
### NOTE - several of Nimisha's plots used a function called "ddply" which is not available in the version of 
### r I am using. I omitted the code for those plots below.

color_table <- tibble(
  v_nv = c("Nonviolent", "Violent"),
  color = c("blue4", "red")
)

total_crime <- qplot(Month, data = df, geom = "bar", fill = v_nv) +
  ggtitle("Total Police Incidents in 2018 by Month") +
  theme(plot.title = element_text(size = 28, face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(values = color_table$color)

color_table_2 <- tibble(
  NIBRS_Crime_Category = c("ASSAULT OFFENSES", "HOMICIDE OFFENSES", "ROBBERY"),
  color = c("yellow", "red", "orange")
)

vc <- qplot(Month, data = violent_crime, geom = "bar", fill = NIBRS_Crime_Category) +
  ggtitle("Violent Crime in 2018 by Month") +
  theme(plot.title = element_text(size = 28, face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(values = color_table_2$color)

vc_division <- qplot(Division, data = violent_crime, geom = "bar", fill = NIBRS_Crime_Category) +
  ggtitle("Violent Crime in 2018 by Division") +
  theme(plot.title = element_text(size = 28, face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(values = color_table_2$color) +
  theme(axis.text.x = element_text(angle = -45))

rt <- df
rt <- filter(rt, Response_Time > 0)
rt <- filter(rt, Response_Time < 5000)
rt <- group_by(rt, v_nv)
rt <- summarize(rt, num_incidents = n(), med_response = median(Response_Time))
responsetime_type <- ggplot(rt, aes(x = v_nv, y = med_response, fill = num_incidents)) + geom_col(position = "dodge")

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

mylist <- split(violent_crime, violent_crime$NIBRS_Crime_Category)
assault <- as.data.frame(mylist$`ASSAULT OFFENSES`)
d_assault <- ggmap(dallas_map, base_layer = ggplot(data= assault, aes(x=long, y=lat)))
d_assault <- d_assault + geom_point(color="red", size=3, alpha=0.5)
d_assault <- d_assault + theme_void() + labs(title = "Location of reported assaults", subtitle = "Dallas 2018", caption = "source: City of Dallas")
d_assault

murder <- as.data.frame(mylist$`HOMICIDE OFFENSES`)
d_murder <- ggmap(dallas_map, base_layer = ggplot(data= murder, aes(x=long, y=lat)))
d_murder <- d_murder + geom_point(color="red", size=3, alpha=0.5)
d_murder <- d_murder + theme_void() + labs(title = "Location of reported murders", subtitle = "Dallas 2018", caption = "source: City of Dallas")
d_murder

robbery <- as.data.frame(mylist$ROBBERY)
d_robbery <- ggmap(dallas_map, base_layer = ggplot(data= murder, aes(x=long, y=lat)))
d_robbery <- d_robbery + geom_point(color="red", size=3, alpha=0.5)
d_robbery <- d_robbery + theme_void() + labs(title = "Location of reported robberies", subtitle = "Dallas 2018", caption = "source: City of Dallas")
d_robbery

map_dallas_sat <- get_map('Dallas', zoom = 12, maptype = 'satellite')
ggmap(map_dallas_sat)
d7 <- ggmap(map_dallas_sat) + stat_density2d(data = violent_crime, aes(x = long, y = lat, fill = ..density..), geom = 'tile', contour = F, alpha = .5)
d7 <- d7 + scale_fill_viridis_c(option = "inferno")
d7 <- d7 + labs(title=str_c("Downtown Dallas has most crime"),fill = str_c('Number of', '\ncrime incidents'))
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

# this one didn't work, I didn't copy over the same ones for the specific types of violent crime
violent_crime_dallas <- cs_projectXY(violent_crime_latlon, varX = lat, varY = long)
d13 <- mapview(violent_crime_dallas)

library(ggmap)
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

#for brandon - code for slide 17
temp <- ddply(violent_crime, .(NIBRS_Crime_Category, `Time1 of Occurrence`), summarise, count=length(`Time1 of Occurrence`))
g <- ggplot(temp, aes(fill= NIBRS_Crime_Category, y=count, x=`Time1 of Occurrence`)) + geom_bar(position="stack", stat="identity")
    
#for brandon- code for slide 16
 temp <- ddply(violent_crime, .(NIBRS_Crime_Category, Day_of_the_Week), summarise, count=length(Date_of_Occurrence))
f <- ggplot(temp, aes(fill= NIBRS_Crime_Category, y=count, x=Day_of_the_Week)) + geom_bar(position="stack", stat="identity")
f <- f + scale_fill_brewer(name="OFFENSE")
f <- f + ggtitle("Violent Crime by Day of Week")

