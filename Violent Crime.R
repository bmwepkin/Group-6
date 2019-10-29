violent_crime <- subset(df, NIBRS_Crime_Category=="ASSAULT OFFENSES" | NIBRS_Crime_Category=="HOMICIDE OFFENSES"| NIBRS_Crime_Category =="ROBBERY")
violent_crime$Gang_Related_Offense <- NULL
violent_crime$Drug_Related_Incident <- NULL
violent_crime$Reporting_Area <- NULL
violent_crime$Target_Area_Action_Grids <- NULL
violent_crime$Type_Location <- NULL
violent_crime$Date_of_Report <- NULL
violent_crime$Date_Incident_Created <- NULL
violent_crime$Call_Date_Time <- NULL
violent_crime$Call_Cleared_Date_Time <- NULL
violent_crime$Call_Dispatch_Date_Time <- NULL
violent_crime$Offense_Status <- NULL
violent_crime$Weapon_Used <- NULL
violent_crime <- violent_crime[(complete.cases(violent_crime)), ]
require(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
#dallas map
library(ggmap)
register_google(key="AIzaSyAuGTvZR1abbOcMQxutQt5127L8AXExQOo")
Dallas <- ggmap(get_map("Dallas, Texas"))
dallas_map <- get_map("Dallas", maptype = "roadmap")
ggmap(dallas_map)
d <- ggmap(dallas_map, base_layer = ggplot(data= violent_crime, aes(x=long, y=lat)))
d <- d + geom_point(color="red", size=3, alpha=0.5)

#crime graphs
violent_crime <- group_by(violent_crime, NIBRS_Crime_Category)
summ <- summarise(violent_crime, num_crimes=n())
p <- qplot(violent_crime$NIBRS_Crime_Category, xlab = "Crime", main="Violent Crimes in Dallas")
p <- p + scale_y_continuous("Number of Crimes")

#Separate them into robbery, assault & homicide
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

#crime by time of day
d1 <- qplot(violent_crime$`Time1 of Occurrence`, xlab="Time of day", main= "Crimes by time of day")
d1 <- d1 + scale_y_continuous("Number of crimes")
#asssaults by time of day
a1 <- qplot(assault$`Time1 of Occurrence`, xlab="Time of day", main= "Assaults by time of day")
a1 <- a1 + scale_y_continuous("Number of assaults")
#murders by time of day
m1 <- qplot(murder$`Time1 of Occurrence`, xlab="Time of day", main= "Murders by time of day")
m1 <- m1 + scale_y_continuous("Number of murders")
#robberies by time of day
r1 <- qplot(robbery$`Time1 of Occurrence`, xlab="Time of day", main= "Robberies by time of day")
r1 <- r1 + scale_y_continuous("Number of robberies")

#crime by day of week
d2 <- qplot(violent_crime$Day_of_the_Week, xlab= "Day of week", main="Violent crimes by day of week")+scale_y_continuous("Number of violent crimes")
#assault by day of week
a2 <- qplot(assault$Day_of_the_Week, xlab= "Day of week", main="Assaults by day of week")+scale_y_continuous("Number of assaults")
#murder by day of week
m2 <- qplot(murder$Day_of_the_Week, xlab= "Day of week", main="Murders by day of week")+scale_y_continuous("Number of murders")
#robbery by day of week
r2 <- qplot(robbery$Day_of_the_Week, xlab= "Day of week", main="Robberies by day of week")+scale_y_continuous("Number of robberies")

#crime by months
violent_crime$month <- months(violent_crime$Date_of_Occurrence, abbreviate = TRUE)
violent_crime$month <- factor(violent_crime$month)
violent_crime$month <- factor(violent_crime$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
d3 <- qplot(violent_crime$month, xlab= "Month", main= "Violent crimes by month") + scale_y_continuous("Number of violent crimes")
#assaults by months
assault$month <- months(assault$Date_of_Occurrence, abbreviate = TRUE)
assault$month <- factor(assault$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
a3 <- qplot(assault$month, xlab= "Month", main= "Assaults by month") + scale_y_continuous("Number of assaults")
#murder by months
murder$month <- months(murder$Date_of_Occurrence, abbreviate = TRUE)
murder$month <- factor(murder$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
m3 <- qplot(murder$month, xlab= "Month", main= "Murders by month") + scale_y_continuous("Number of murders")
#robberies by months
robbery$month <- months(robbery$Date_of_Occurrence, abbreviate = TRUE)
robbery$month <- factor(robbery$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
r3 <- qplot(robbery$month, xlab= "Month", main= "Robberies by month") + scale_y_continuous("Number of robberies")
#heatmaps of violent crimes
library(plyr)
temp <- ddply(violent_crime, .(NIBRS_Crime_Category, Day_of_the_Week), summarise, count=length(Date_of_Occurrence))
d4 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Day_of_the_Week, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Violent crime", expand = c(0,0)) + scale_y_discrete("Day of Week", expand = c(0,-2)) + scale_fill_gradient("Number of violent crimes", low="white", high = "steelblue")+theme_bw()+ggtitle("Violent crimes by day of week")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA)) 
#heatmaps of assaults
temp <- ddply(assault, .(NIBRS_Crime_Category, Day_of_the_Week), summarise, count=length(Date_of_Occurrence))
a4 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Day_of_the_Week, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Assaults", expand = c(0,0)) + scale_y_discrete("Day of Week", expand = c(0,-2)) + scale_fill_gradient("Number of assaults", low="white", high = "steelblue")+theme_bw()+ggtitle("Assaults by day of week")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))              
#heatmaps of murders
temp <- ddply(murder, .(NIBRS_Crime_Category, Day_of_the_Week), summarise, count=length(Date_of_Occurrence))
m4 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Day_of_the_Week, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Murders", expand = c(0,0)) + scale_y_discrete("Day of Week", expand = c(0,-2)) + scale_fill_gradient("Number of murders", low="white", high = "steelblue")+theme_bw()+ggtitle("Murders by day of week")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))              
#heatmaps of robberies by Day of Week                        
temp <- ddply(robbery, .(NIBRS_Crime_Category, Day_of_the_Week), summarise, count=length(Date_of_Occurrence))
r4 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=Day_of_the_Week, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Robberies", expand = c(0,0)) + scale_y_discrete("Day of Week", expand = c(0,-2)) + scale_fill_gradient("Number of robberies", low="white", high = "steelblue")+theme_bw()+ggtitle("Robberies by day of week")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
#heatmaps of violent crimes by month
temp <- ddply(violent_crime, .(NIBRS_Crime_Category, month), summarise, count=length(Date_of_Occurrence))
d5 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=month, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Violent crime", expand = c(0,0)) + scale_y_discrete("Month", expand = c(0,-2)) + scale_fill_gradient("Number of violent crimes", low="white", high = "steelblue")+theme_bw()+ggtitle("Violent crimes by month")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
#heatmaps of assaults by month
temp <- ddply(assault, .(NIBRS_Crime_Category, month), summarise, count=length(Date_of_Occurrence))
a5 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=month, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Assaults", expand = c(0,0)) + scale_y_discrete("Month", expand = c(0,-2)) + scale_fill_gradient("Number of assaults", low="white", high = "steelblue")+theme_bw()+ggtitle("Assaults by month")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
#heatmaps of murders by month
temp <- ddply(murder, .(NIBRS_Crime_Category, month), summarise, count=length(Date_of_Occurrence))
m5 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=month, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Murders", expand = c(0,0)) + scale_y_discrete("Month", expand = c(0,-2)) + scale_fill_gradient("Number of murders", low="white", high = "steelblue")+theme_bw()+ggtitle("Murders by month")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
#heatmaps of robberies by month
temp <- ddply(robbery, .(NIBRS_Crime_Category, month), summarise, count=length(Date_of_Occurrence))
r5 <- ggplot(temp, aes(x=NIBRS_Crime_Category, y=month, fill=count)) + geom_tile(aes(fill=count)) + scale_x_discrete("Robberies", expand = c(0,0)) + scale_y_discrete("Month", expand = c(0,-2)) + scale_fill_gradient("Number of robberies", low="white", high = "steelblue")+theme_bw()+ggtitle("Robberies by month")+theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
#violent crime over time
temp <- ddply(violent_crime, .(NIBRS_Crime_Category, Date_of_Occurrence), summarise, count=length(Date_of_Occurrence))
temp <- arrange(temp, Date_of_Occurrence)
plot <- ggplot(temp, aes(x=Date_of_Occurrence, y=count)) + geom_line(color = "#F2CA27", size = 0.1) + geom_smooth(color = "#1A1A1A") + scale_x_date(breaks = date_breaks("1 month"), labels = date_format(c("Jan", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) + labs(x = "Date of Violent Crime", y = "Number of Violent Crimes", title = "Daily Violent Crimes in Dallas in 2018")
#assaults over time
temp <- ddply(assault, .(NIBRS_Crime_Category, Date_of_Occurrence), summarise, count=length(Date_of_Occurrence))
temp <- arrange(temp, Date_of_Occurrence)
a6 <- ggplot(temp, aes(x=Date_of_Occurrence, y=count)) + geom_line(color = "#F2CA27", size = 0.1) + geom_smooth(color = "#1A1A1A") + scale_x_date(breaks = date_breaks("1 month"), labels = date_format(c("Jan", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) + labs(x = "Date of Assault", y = "Number of Assaults", title = "Daily Assaults in Dallas in 2018")
#murders over time
temp <- ddply(murder, .(NIBRS_Crime_Category, Date_of_Occurrence), summarise, count=length(Date_of_Occurrence))
temp <- arrange(temp, Date_of_Occurrence)
m6 <- ggplot(temp, aes(x=Date_of_Occurrence, y=count)) + geom_line(color = "#F2CA27", size = 0.1) + geom_smooth(color = "#1A1A1A") + scale_x_date(breaks = date_breaks("1 month"), labels = date_format(c("Jan", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) + labs(x = "Date of Murder", y = "Number of Murders", title = "Daily Murders in Dallas in 2018")
#robberies over time
temp <- ddply(robbery, .(NIBRS_Crime_Category, Date_of_Occurrence), summarise, count=length(Date_of_Occurrence))
temp <- arrange(temp, Date_of_Occurrence)
r6 <- ggplot(temp, aes(x=Date_of_Occurrence, y=count)) + geom_line(color = "#F2CA27", size = 0.1) + geom_smooth(color = "#1A1A1A") + scale_x_date(breaks = date_breaks("1 month"), labels = date_format(c("Jan", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) + labs(x = "Date of Robbery", y = "Number of Robberies", title = "Daily Robberies in Dallas in 2018")
#separate lat and long
library(stringr)
mat <- str_match(violent_crime$Location1, "\\((.*?), (.*?)\\)")
violent_crime$lat <- as.double(mat[,2])
violent_crime$long <- as.double(mat[,3])
#violent crime heat map over satellite
map_dallas_sat <- get_map('Dallas', zoom = 12, maptype = 'satellite')
ggmap(map_dallas_sat)
d7 <- ggmap(map_dallas_sat) + stat_density2d(data = violent_crime, aes(x = long, y = lat, fill = ..density..), geom = 'tile', contour = F, alpha = .5)
d7 <- d7 + scale_fill_viridis_c(option = "inferno")
d7 <- d7 + labs(title=str_c("Downtown Dallas has most crime"),fill = str_c('Number of', '\ncrime incidents'))
d7 <- d7 + theme(text=element_text(color = "#444444"), plot.title=element_text(size=22, face='bold'), axis.text=element_blank(),axis.title = element_blank(), axis.ticks=element_blank())
d7 <- d7 + guides(fill = guide_legend(override.aes= list(alpha = 1)))
#assault heat map over satellite
a7 <- ggmap(map_dallas_sat) + stat_density2d(data = assault, aes(x = long, y = lat, fill = ..density..), geom = 'tile', contour = F, alpha = .5) +  scale_fill_viridis_c(option = "inferno") + labs(title=str_c("Downtown Dallas has most assaults"),fill = str_c('Number of', '\nassaults')) + theme(text=element_text(color = "#444444"), plot.title=element_text(size=22, face='bold'), axis.text=element_blank(),axis.title = element_blank(), axis.ticks=element_blank())+ guides(fill = guide_legend(override.aes= list(alpha = 1)))
#age graph
age <- ddply(violent_crime, .(NIBRS_Crime_Category, Victim_Age), summarise, count=length(Victim_Age))
d8 <- age %>% ggplot (aes(x=Victim_Age, y=count)) + geom_line(group=1) + geom_point(size=0.5) + labs(title = "Age Most Likely to Become A Violent Crime Victim", x= "Victim Age", y="Violent Crimes")
# age assaults
age <- ddply(assault, .(NIBRS_Crime_Category, Victim_Age), summarise, count=length(Victim_Age))
a8 <- age %>% ggplot (aes(x=Victim_Age, y=count)) + geom_line(group=1) + geom_point(size=0.5) + labs(title = "Age Most Likely to Become An Assault Victim", x= "Victim Age", y="Assaults")
#age murder
age <- ddply(murder, .(NIBRS_Crime_Category, Victim_Age), summarise, count=length(Victim_Age))
m8 <- age %>% ggplot (aes(x=Victim_Age, y=count)) + geom_line(group=1) + geom_point(size=0.5) + labs(title = "Age Most Likely to Become A Murder Victim", x= "Victim Age", y="Murder")
#age robberies
age <- ddply(robbery, .(NIBRS_Crime_Category, Victim_Age), summarise, count=length(Victim_Age))
r8 <- age %>% ggplot (aes(x=Victim_Age, y=count)) + geom_line(group=1) + geom_point(size=0.5) + labs(title = "Age Most Likely to Become A Robbery Victim", x= "Victim Age", y="Robberies")
#agegroup violent crime
violent_crime$age_group <- cut(violent_crime$Victim_Age, breaks = c(0, 19, 35, 55, 99), labels = c("Teenager", "Young Adult", "Middle Age", "Elderly"))
age.group <- ddply(violent_crime, .(age_group, NIBRS_Crime_Category), summarise, count=length(age_group))
d9<- age.group %>% ggplot(aes(reorder(x = NIBRS_Crime_Category, count), y = count)) + geom_col(fill="red") + geom_text(aes(label=count), color="black", hjust=-0.1, size=3) + coord_flip() + facet_wrap(~age_group) + labs(x="Total", y="Crime Description")

#overall violent crime map
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

violent_crime_dallas <- cs_projectXY(violent_crime, varX = X_Coordinate, varY = Y_Coordinate)
d13 <- mapview(murder_dallas)

#mapview violent crime
assault_dallas <- cs_projectXY(assault, varX = X_Coordinate, varY = Y_Coordinate)
a10 <- mapview(assault_dallas)
murder_dallas <- cs_projectXY(murder, varX = X_Coordinate, varY = Y_Coordinate)
m10 <- mapview(murder_dallas)
robbery_dallas <- cs_projectXY(robbery, varX = X_Coordinate, varY = Y_Coordinate)
r10<- mapview(robbery_dallas)

#bubblechart overall violent crimes
library(ggmap)
DallasMap <- qmap(location = "dallas", zoom=14, color="bw", extent="device", legend="topleft")
bubble_chart <- DallasMap + geom_point(aes(x = long, y = lat, colour = NIBRS_Crime_Category, size = NIBRS_Crime_Category), data = violent_crime)+ bubble_chart + scale_colour_discrete("Offense", labels = c("Assault","Murder","Robbery"))+ scale_size_discrete("Offense", labels = c("Assault","Murder","Robbery"))+  guides(size = guide_legend(override.aes = list(size = 6)) +theme(legend.key.size = grid::unit(1.8,"lines"), legend.title = element_text(size = 16, face = "bold"),legend.text = element_text(size = 14))+ bubble_chart + labs(colour = "Offense", size = "Offense")
bubble_chart_assault <- DallasMap + geom_point(aes(x = long, y = lat, colour = NIBRS_Crime_Category, size = NIBRS_Crime_Category), data = assault)+ + scale_colour_discrete("Offense", labels = c("Assault"))+ scale_size_discrete("Offense", labels = c("Assault"))
bubble_chart_robbery<- DallasMap + geom_point(aes(x = long, y = lat, colour = NIBRS_Crime_Category, size = NIBRS_Crime_Category), data = robbery)+ scale_colour_discrete("Offense", labels = c("Robbery"))+ scale_size_discrete("Offense", labels = c("Robbery"))
#tonerlitemap
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
d18 <- DallasMap2 + stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = ..level..), bins = I(5), geom = "polygon", data = violent_crime)+scale_fill_gradient2("Violent\nCrime\nDensity", low = "white", mid = "orange", high = "red", midpoint = 500)+ labs(x = "Longitude", y = "Latitude") + facet_wrap(~ month) + scale_alpha(range = c(.2, .55), guide = FALSE) + ggtitle("Violent Crime Contour Map of Dallas by Month") + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))

#byrace
(library plyr)
race.group <- ddply(violent_crime, .(Victim_Race, NIBRS_Crime_Category), summarise, count=length(Victim_Race))
o2<- race.group %>% ggplot(aes(reorder(x = NIBRS_Crime_Category, count), y = count)) + geom_col(fill="red") + geom_text(aes(label=count), color="black", hjust=-0.1, size=3) + coord_flip() + facet_wrap(~Victim_Race) + labs(x="Total", y="Crime Description")

