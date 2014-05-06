rm(list=ls())
library(plotrix)
library(ggmap)
library(rworldmap)
library(RgoogleMaps)
library(mapproj)
library(sqldf)
library(doBy)
library(fpc)
library(cluster) 
getwd()
delim =","
dec = "."
setwd("C:\\Users\\MangeshVilas\\Documents\\Spring2014\\DIC\\Project1")
csvdata_NewYork  = read.csv(file ="NY.csv", header = TRUE,sep = delim, dec = dec, stringsAsFactors=FALSE )
csvdata_NewYork$LATITUDE = as.numeric(csvdata_NewYork$LATITUDE)
csvdata_NewYork$LONGITUDE = as.numeric(csvdata_NewYork$LONGITUDE)
csvdata_NewYork$PRCP[csvdata_NewYork$PRCP ==-9999] = 0
csvdata_NewYork$PRCP[csvdata_NewYork$PRCP ==9999] = 0

head(csvdata_NewYork)
# Precipitation and Snow 

StationList_Precipitation_Snow  = sqldf("Select distinct(STATION_NAME) as Station_Name, 
                                   sum(PRCP) as TotalPrecipitationCount,sum(SNOW) as TotalSnowCount,
                                    LATITUDE as Latitude, LONGITUDE as Longitude 
                                   from csvdata_NewYork where LATITUDE <> 'unknown' and LONGITUDE <> 'unknown'
                                   group by STATION_NAME, LATITUDE, LONGITUDE")
# floor(StationList_Precipitation$LONGITUDE)
# General Graph of Precipitation : GGPlot 
ggplot(StationList_Precipitation_Snow, aes(x = Station_Name, y = TotalPrecipitationCount)) + geom_histogram(stat ="identity")
# barplot(StationList_Precipitation)
# abline(h=mean(StationList_Precipitation$TotalPrecipitationCount))
# General Graph of Precipitation : GGBoxPlot 
# ggplot(StationList_Precipitation, aes(x = Station_Name, y = TotalPrecipitationCount)) + geom_boxplot()

# Pie Chart for Precipitation and Snow 
# Precipitation 
attach(StationList_Precipitation_Snow)
StationList_Precipitation_Snow$Precipitation_Type_in_10000mm[TotalPrecipitationCount < 25000] <- "Mild(<2.5)"
StationList_Precipitation_Snow$Precipitation_Type_in_10000mm[TotalPrecipitationCount >= 25000 & TotalPrecipitationCount < 75000] <- "Average(2.5 to 7.5) "
StationList_Precipitation_Snow$Precipitation_Type_in_10000mm[TotalPrecipitationCount >= 75000] <- "High(>7.5)"
detach(StationList_Precipitation_Snow)

# Snow 
attach(StationList_Precipitation_Snow)
StationList_Precipitation_Snow$Snow_Type_in_1000mm[TotalSnowCount < 7500] <- "Mild(<7.5)"
StationList_Precipitation_Snow$Snow_Type_in_1000mm[TotalSnowCount >= 7500 & TotalSnowCount < 12500] <- "Average(7.5 to 12.5)"
StationList_Precipitation_Snow$Snow_Type_in_1000mm[TotalSnowCount >= 12500] <- "High(>12.5)"
detach(StationList_Precipitation_Snow)


Pie_ChartData_Precipitation = sqldf("Select distinct(Precipitation_Type_in_10000mm),count(Precipitation_Type_in_10000mm)
                                         from StationList_Precipitation_Snow group by Precipitation_Type_in_10000mm")

Pie_ChartData_Snow = sqldf("Select distinct(Snow_Type_in_1000mm),count(Snow_Type_in_1000mm)
                                         from StationList_Precipitation_Snow group by Snow_Type_in_1000mm")

# StandardCount = sqldf("Select count(Precipitation_Type) from StationList_Precipitation where Precipitation_Type ='Standard'")
# AboveAvgCount = sqldf("Select count(Precipitation_Type) from StationList_Precipitation where Precipitation_Type ='Above Average'")
# listOfLables  = sqldf("Select Distinct(Precipitation_Type) from StationList_Precipitation")

# Pie Chart for Distribution of Precipitation Type Recorded over all stations  of California 
#pie(Pie_ChartData[[2]], Pie_ChartData[[1]],   main="Pie Chart for Distribution of Precipitation Type Recorded over all stations  of California ")
#pie3D(Pie_ChartData[[2]], Pie_ChartData[[1]], explode =0.1 ,  main="Pie Chart for Distribution of Precipitation Type Recorded over all stations  of California ")


pie3D(Pie_ChartData_Precipitation[[2]],labels = Pie_ChartData_Precipitation[[1]],radius=0.8,height=0.1,theta=pi/5,
      start=1,border=par("fg"),labelcex=1.0,explode =0.2 , 
      main="Distribution of Precipitation (tenths of mm) Data Recorded \n over stations  of New York[in 10000mm]")

pie3D(Pie_ChartData_Snow[[2]],labels = Pie_ChartData_Snow[[1]],radius=0.8,height=0.1,theta=pi/5,
      start=1,border=par("fg"),labelcex=1.0,explode =0.2 , 
      main="Distribution of Snow (mm) Recorded \n over stations  of New York[in 1000mm]")



#............. = edges=NA,radius=1,height=0.2,theta=pi/6,start=0,border=par("fg"),
#     col=NULL,labels=NULL,labelpos=NULL,labelcol=par("fg"),labelcex=1.5,
#    sector.order=NULL,explode=0,shade=0.8,mar=c(4,4,4,4),pty="s",)
## ---------------------------------------------------------------------------------------------------------------

# High , Medium , Low graphs 
#ggplot (subset(StationList_Precipitation,Precipitation_Type =='Average'), aes(x=Station_Name, y = TotalPrecipitationCount)) + geom_histogram(stat ="identity")
#ggplot (subset(StationList_Precipitation,Precipitation_Type =='High'), aes(x=Station_Name, y = TotalPrecipitationCount)) + geom_histogram(stat ="identity")
#ggplot (subset(StationList_Precipitation,Precipitation_Type =='Standard'), aes(x=Station_Name, y = TotalPrecipitationCount)) + geom_histogram(stat ="identity")

## ----------------------------------------------------------------------------------------------------------------------
## Summaries  
#summary(subset(subset(StationList_Precipitation,Precipitation_Type =='Average(2.5 to 5.0)')))
#summary(subset(subset(StationList_Precipitation,Precipitation_Type =='High(>5.0)')))
#summary(subset(subset(StationList_Precipitation,Precipitation_Type =='Standard(<2.5)')))

## -----------------------------------------------------------------------------------------------------------------------
statistics =function (x){c(Sum= sum(x,na.rm = TRUE), Mean = mean(x,na.rm =TRUE) , Standard_Deviation = sd(x))}
summary_preci <- summaryBy (TotalPrecipitationCount~Precipitation_Type_in_10000mm, data = StationList_Precipitation_Snow, FUN = statistics)
summary_snow <- summaryBy (TotalSnowCount~Snow_Type_in_1000mm, data = StationList_Precipitation_Snow, FUN = statistics)

## -----------------------------------------------------------------------------------------------------------------------
# Maps : Precipitation , Snow , Summer and Winter
## First Map : Precipitation Heat Map 
map <- get_map(location = 'New York, United States', zoom = 6)
ggmap(map)

colormap <- c("Blue","Green","Red")
Prcp_HeatMap <- ggmap(map) %+% StationList_Precipitation_Snow +
  aes(x  =Longitude,y = Latitude,z = TotalPrecipitationCount) +
  stat_summary2d() +scale_fill_gradientn(name = "Precipitation Recorded(tenths of mm)",
                                         colours = colormap, space = "Lab") + 
  labs(x = "Longitude", y = "Latitude") +  coord_map()

print(Prcp_HeatMap)

# Second Map : Precipitation Recorded By Stations 
mapPoints <- ggmap(map) + geom_point(data = StationList_Precipitation_Snow ,aes(x = Longitude, y = Latitude, size = TotalPrecipitationCount), alpha = 0.7,colour="blue")
mapPoints

# Third Map : Snow Heat Map 
Snow_HeatMap <- ggmap(map) %+% StationList_Precipitation_Snow +
  aes(x  =Longitude,y = Latitude,z = TotalSnowCount) +
  stat_summary2d() +scale_fill_gradientn(name = "Snow Recorded(mm)",
                                         colours = colormap, space = "Lab") + 
  labs(x = "Longitude", y = "Latitude") +  coord_map()

print(Snow_HeatMap)

# Fourth Map : Snow Recorded By Stations 
mapPointsSnow <- ggmap(map) + geom_point(data = StationList_Precipitation_Snow ,aes(x = Longitude, y = Latitude, size = TotalSnowCount), alpha = 0.7,colour="blue")
mapPointsSnow

## --------------------------------Temperature Analysis  --------------------------------------------------------------
# Query For Summer Data 

Sum_Map_NewYork <- get_map(location = 'New York, United States', zoom = 6)
ggmap(Sum_Map_NewYork)

Temp_Data_Summer = sqldf("select STATION_NAME, LATITUDE AS Latitude, LONGITUDE as Longitude,DATE, AVG(TMAX) AS TMAX 
                         from csvdata_NewYork where DATE >= 20060401 AND DATE <=20060731 
                         or DATE >= 20070401 AND DATE <=20070731
                         or DATE >= 20080401 AND DATE <=20080731
                         or DATE >= 20090401 AND DATE <=20090731
                         or DATE >= 20100401 AND DATE <=20100731
                         or DATE >= 20110401 AND DATE <=20110731
                         or DATE >= 20120401 AND DATE <=20120731
                         GROUP BY STATION_NAME, LATITUDE,LONGITUDE  ORDER BY STATION_NAME")

Summer_Map_NewYork_Total <- ggmap(Sum_Map_NewYork) %+% subset(Temp_Data_Summer,TMAX != 'NA') +
  aes(x  =Longitude,
      y = Latitude,
      z = TMAX) +
  stat_summary2d() + 
  scale_fill_gradientn(name = "Temperture Recorded (in tenths of Degree C)",
                       colours = colormap,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()

print(Summer_Map_NewYork_Total)


# Query For Winter Data 
Winter_Map_NewYork <- get_map(location = 'New York, United States', zoom = 6)
ggmap(Winter_Map_NewYork)

Temp_Data_Winter = sqldf("select STATION_NAME, LATITUDE AS Latitude, LONGITUDE as Longitude,DATE, AVG(TMIN) AS TMIN 
                         from csvdata_NewYork where DATE >= 20061101 AND DATE <=20070228 
                         or DATE >= 20071101 AND DATE <=20080228
                         or DATE >= 20081101 AND DATE <=20090228
                         or DATE >= 20091101 AND DATE <=20100228
                         or DATE >= 20101101 AND DATE <=20110228
                         or DATE >= 20111101 AND DATE <=20120228
                         or DATE >= 20121101 AND DATE <=20130228
                         GROUP BY STATION_NAME, LATITUDE,LONGITUDE  ORDER BY STATION_NAME")

Winter_Map_NewYork_Total <- ggmap(Winter_Map_NewYork) %+% subset(Temp_Data_Winter,TMIN != 'NA') +
  aes(x  =Longitude,
      y = Latitude,
      z = TMIN) +
  stat_summary2d() + 
  scale_fill_gradientn(name = "Temperture Recorded (in tenths of Degree C)",
                       colours = colormap,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()

print(Winter_Map_NewYork_Total)


# K- Means : Precipitation Clustering 
x <- data.frame(StationList_Precipitation_Snow$Latitude, StationList_Precipitation_Snow$Longitude)
mat = as.matrix(x)
kclus <- kmeans(mat,5)
mat2 = data.frame(x)
map = get_map(location = 'New York, United States', zoom = 6)
mapPoints_KMeans <- ggmap(map) +
  geom_point(data = mat2 ,aes(x = StationList_Precipitation_Snow.Longitude,
                              y = StationList_Precipitation_Snow.Latitude , col =kclus$cluster ))
mapPoints_KMeans



## Bar - plots 

# TMAX and TMIN
Years  = c("2006", "2007","2008","2009","2010","2011","2012" )
# rm(Year7_Kansas)
Year1_NewYork = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_NewYork where Date like '2006%'")
Year2_NewYork = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_NewYork where Date like '2007%'")
Year3_NewYork = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_NewYork where Date like '2008%'")
Year4_NewYork = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_NewYork where Date like '2009%'")
Year5_NewYork = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_NewYork where Date like '2010%'")
Year6_NewYork = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_NewYork where Date like '2011%'")
Year7_NewYork = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_NewYork where Date like '2012%'")

Year1_NewYork_Total <- rbind( Year1_NewYork, Year2_NewYork, Year3_NewYork, Year4_NewYork, Year5_NewYork, Year6_NewYork, Year7_NewYork)

TMAX = Year1_NewYork_Total$TMAX
TMIN = Year1_NewYork_Total$TMIN

Rate = matrix(c(TMAX,TMIN), nrow = length (Years))
Rate2 = t(Rate)

barplot(Rate2, beside=TRUE, names.arg=c("2006", "2007","2008","2009","2010","2011","2012"),
        col=c("blue", "red"),border="black", main=c("Maximum and Minimum Temperatures in New York \n Over the Years ( 2006 - 2012)"),xlab="Year",ylab="Temperature (in tenths of Degree Celsius) ",font.lab=3)


# Precipitation and Snow 

Prcp1_NewYork = sqldf("select SUM(PRCP) as PRCP from csvdata_NewYork where Date like '2006%'")
Prcp2_NewYork = sqldf("select SUM(PRCP) as PRCP  from csvdata_NewYork where Date like '2007%'")
Prcp3_NewYork = sqldf("select SUM(PRCP) as PRCP  from csvdata_NewYork where Date like '2008%'")
Prcp4_NewYork = sqldf("select SUM(PRCP) as PRCP  from csvdata_NewYork where Date like '2009%'")
Prcp5_NewYork = sqldf("select SUM(PRCP) as PRCP  from csvdata_NewYork where Date like '2010%'")
Prcp6_NewYork = sqldf("select SUM(PRCP) as PRCP  from csvdata_NewYork where Date like '2011%'")
Prcp7_NewYork = sqldf("select SUM(PRCP) as PRCP  from csvdata_NewYork where Date like '2012%'")

PRCP_Total <- rbind( Prcp1_NewYork, Prcp2_NewYork, Prcp3_NewYork, Prcp4_NewYork, Prcp5_NewYork, Prcp6_NewYork, Prcp7_NewYork)

PRCP = PRCP_Total$PRCP

Rate = matrix(c(PRCP), nrow = length (Years))
Rate2 = t(Rate)

barplot(Rate2, beside=TRUE, names.arg=c("2006", "2007","2008","2009","2010","2011","2012"),
        col=c("Blue"),border="black", main=c("Total Precipitation(in tenths of mm) Recorded in New York \n Over the Years ( 2006 - 2012)"),xlab="Year",ylab="Precipitation (in tenths of mm) ",font.lab=3)


# Snow 
Winter_years =c(" W2006", "W2007","W2008","W2009","W2010","W2011","W2012" )
Snow1_NewYork = sqldf("select SUM(Snow) as Snow from csvdata_NewYork where DATE >= 20061101 AND DATE <=20070228")
Snow2_NewYork = sqldf("select SUM(Snow) as Snow  from csvdata_NewYork where DATE >= 20071101 AND DATE <=20080228")
Snow3_NewYork = sqldf("select SUM(Snow) as Snow  from csvdata_NewYork where DATE >= 20081101 AND DATE <=20090228")
Snow4_NewYork = sqldf("select SUM(Snow) as Snow  from csvdata_NewYork where DATE >= 20091101 AND DATE <=20100228")
Snow5_NewYork = sqldf("select SUM(Snow) as Snow  from csvdata_NewYork where DATE >= 20101101 AND DATE <=20110228")
Snow6_NewYork = sqldf("select SUM(Snow) as Snow  from csvdata_NewYork where DATE >= 20111101 AND DATE <=20120228")
Snow7_NewYork = sqldf("select SUM(Snow) as Snow  from csvdata_NewYork where DATE >= 20121101 AND DATE <=20130228")

Snow_Total <- rbind( Snow1_NewYork, Snow2_NewYork, Snow3_NewYork, Snow4_NewYork, Snow5_NewYork, Snow6_NewYork, Snow7_NewYork)

Snow= Snow_Total$Snow

Rate3 = matrix(c(Snow), nrow = length (Winter_years))
Rate4 = t(Rate3)

barplot(Rate4, beside=TRUE, names.arg=c(" W2006", "W2007","W2008","W2009","W2010","W2011","W2012" ),
        col=c("Blue"),border="black", main=c("Total Snow Recorded in New York \n Over the Years ( 2006 - 2012)"),xlab="Year",ylab="Snow (in mm) ",font.lab=3)

# Snow Depth 

Winter_years =c(" Winter 2006", "Winter 2007","Winter 2008","Winter 2009","Winter 2010","Winter 2011","Winter 2012" )
SnowDep1_NewYork = sqldf("select SUM(SNWD) as SnowDepth from csvdata_NewYork where DATE >= 20061101 AND DATE <=20070228")
SnowDep2_NewYork = sqldf("select SUM(SNWD) as SnowDepth  from csvdata_NewYork where DATE >= 20071101 AND DATE <=20080228")
SnowDep3_NewYork = sqldf("select SUM(SNWD) as SnowDepth  from csvdata_NewYork where DATE >= 20081101 AND DATE <=20090228")
SnowDep4_NewYork = sqldf("select SUM(SNWD) as SnowDepth  from csvdata_NewYork where DATE >= 20091101 AND DATE <=20100228")
SnowDep5_NewYork = sqldf("select SUM(SNWD) as SnowDepth  from csvdata_NewYork where DATE >= 20101101 AND DATE <=20110228")
SnowDep6_NewYork = sqldf("select SUM(SNWD) as SnowDepth  from csvdata_NewYork where DATE >= 20111101 AND DATE <=20120228")
SnowDep7_NewYork = sqldf("select SUM(SNWD) as SnowDepth  from csvdata_NewYork where DATE >= 20121101 AND DATE <=20130228")

SnowDepth_Total <- rbind( SnowDep1_NewYork, SnowDep2_NewYork, SnowDep3_NewYork, SnowDep4_NewYork, SnowDep5_NewYork, SnowDep6_NewYork, SnowDep7_NewYork)

SnowDepth= SnowDepth_Total$SnowDepth
SnowDepth_Total
Rate5 = matrix(c(SnowDepth), nrow = length (Winter_years))
Rate6 = t(Rate5)

barplot(Rate5, beside=TRUE, names.arg=c(" W2006", "W2007","W2008","W2009","W2010","W2011","W2012" ),
        col=c("Blue"),border="black", main=c("Total Snow Depth Recorded in New York \n Over the Years ( 2006 - 2012)"),xlab="Year",ylab="Snow Depth(in mm) ",font.lab=3)

#------------------------------------------------------------------------------------------------------------------------







