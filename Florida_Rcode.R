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
csvdata_florida  = read.csv(file ="C:\\Users\\MangeshVilas\\Documents\\Spring2014\\DIC\\Project1\\FL.csv", header = TRUE,sep = delim, dec = dec, stringsAsFactors=FALSE )
csvdata_florida$LATITUDE = as.numeric(csvdata_florida$LATITUDE)
csvdata_florida$LONGITUDE = as.numeric(csvdata_florida$LONGITUDE)
csvdata_florida$PRCP[csvdata_florida$PRCP ==-9999] = 0
csvdata_florida$PRCP[csvdata_florida$PRCP ==9999] = 0

# csvdata_florida$DATE  <- as.Date(as.character(csvdata_florida$DATE) ,'%Y%m%d')


#StationList =  sqldf("select distinct(STATION_NAME) from csvdata_florida")
#head(StationList)

# Precipitation 

# head(csvdata_florida,10)
#head(StationList_Precipitation)
StationList_Precipitation  = sqldf("Select distinct(STATION_NAME) as Station_Name, 
                                   sum(PRCP) as Total_Precipitation_Count, LATITUDE as Latitude, LONGITUDE as Longitude 
                                   from csvdata_florida where LATITUDE <> 'unknown' and LONGITUDE <> 'unknown'
                                   group by STATION_NAME, LATITUDE, LONGITUDE")
#head(StationList_Precipitation)
# floor(StationList_Precipitation$LONGITUDE)
# General Graph of Precipitation : GGPlot 
# ggplot(StationList_Precipitation, aes(x = Station_Name, y = Total_Precipitation_Count)) + geom_histogram(stat ="identity")
# barplot(StationList_Precipitation)
# abline(h=mean(StationList_Precipitation$Total_Precipitation_Count))
# General Graph of Precipitation : GGBoxPlot 
# ggplot(StationList_Precipitation, aes(x = Station_Name, y = Total_Precipitation_Count)) + geom_boxplot()

#------------------------------------------ Pie Chart--------------------------- 
attach(StationList_Precipitation)
StationList_Precipitation$Precipitation_Type_in_1000mm[Total_Precipitation_Count < 25000] <- "Mild(<2.5)"
StationList_Precipitation$Precipitation_Type_in_1000mm[Total_Precipitation_Count >= 25000 & Total_Precipitation_Count < 50000] <- "Average(2.5 to 5.0)"
StationList_Precipitation$Precipitation_Type_in_1000mm[Total_Precipitation_Count >= 50000] <- "High(>5.0)"
detach(StationList_Precipitation)

Pie_ChartData = sqldf("Select distinct(Precipitation_Type_in_1000mm),count(Precipitation_Type_in_1000mm) from StationList_Precipitation group by Precipitation_Type_in_1000mm")
# StandardCount = sqldf("Select count(Precipitation_Type) from StationList_Precipitation where Precipitation_Type ='Standard'")
# AboveAvgCount = sqldf("Select count(Precipitation_Type) from StationList_Precipitation where Precipitation_Type ='Above Average'")
# listOfLables  = sqldf("Select Distinct(Precipitation_Type) from StationList_Precipitation")

# Pie Chart for Distribution of Precipitation Type Recorded over all stations  of florida 
#pie(Pie_ChartData[[2]], Pie_ChartData[[1]],   main="Pie Chart for Distribution of Precipitation Type Recorded over all stations  of florida ")
#pie3D(Pie_ChartData[[2]], Pie_ChartData[[1]], explode =0.1 ,  main="Pie Chart for Distribution of Precipitation Type Recorded over all stations  of florida ")


pie3D(Pie_ChartData[[2]],labels = Pie_ChartData[[1]],radius=0.8,height=0.1,theta=pi/4,
      start=1,border=par("fg"),labelcex=1.0,explode =0.2 , 
      main="Distribution of Precipitation (tenths of mm) Data Recorded \n over stations for Florida[in 1000mm]")

#............. = edges=NA,radius=1,height=0.2,theta=pi/6,start=0,border=par("fg"),
#     col=NULL,labels=NULL,labelpos=NULL,labelcol=par("fg"),labelcex=1.5,
#    sector.order=NULL,explode=0,shade=0.8,mar=c(4,4,4,4),pty="s",)
## ---------------------------------------------------------------------------------------------------------------

# High , Medium , Low graphs 
#ggplot (subset(StationList_Precipitation,Precipitation_Type =='Average'), aes(x=Station_Name, y = Total_Precipitation_Count)) + geom_histogram(stat ="identity")
#ggplot (subset(StationList_Precipitation,Precipitation_Type =='High'), aes(x=Station_Name, y = Total_Precipitation_Count)) + geom_histogram(stat ="identity")
#ggplot (subset(StationList_Precipitation,Precipitation_Type =='Standard'), aes(x=Station_Name, y = Total_Precipitation_Count)) + geom_histogram(stat ="identity")

## ----------------------------------------------------------------------------------------------------------------------
## Summaries  
#summary(subset(subset(StationList_Precipitation,Precipitation_Type =='Average(2.5 to 5.0)')))
#summary(subset(subset(StationList_Precipitation,Precipitation_Type =='High(>5.0)')))
#summary(subset(subset(StationList_Precipitation,Precipitation_Type =='Standard(<2.5)')))

## -----------------------------------------------------------------------------------------------------------------------
statistics =function (x){c(Sum= sum(x,na.rm = TRUE), Mean = mean(x,na.rm =TRUE) , Standard_Deviation = sd(x))}
summary_preci <- summaryBy (Total_Precipitation_Count~Precipitation_Type, data = StationList_Precipitation, FUN = statistics)

## First Map 
map <- get_map(location = 'florida, United States', zoom = 7)
#ggmap(map)

#colormap <- c("Violet","Blue","Green","Yellow","Red","White")
colormap <- c("Blue","Green","Red")
Prec_Florida <- ggmap(map) %+% StationList_Precipitation +
  aes(x  =Longitude,y = Latitude,z = Total_Precipitation_Count) +
  stat_summary2d() +scale_fill_gradientn(name = "Precipitation Recorded (tenths of mm)",
                                         colours = colormap, space = "Lab") + 
  labs(x = "Longitude", y = "Latitude") +  coord_map()

print(Prec_Florida)

# Second Map : : Point Map
mapPoints <- ggmap(map) + geom_point(data = StationList_Precipitation ,aes(x = Longitude, y = Latitude, size = Total_Precipitation_Count), alpha = 0.7,colour="blue")
mapPoints


## --------------------------------Temperature Analysis  --------------------------------------------------------------

# Query For Summer Data 

Sum_Map_florida <- get_map(location = 'florida, United States', zoom = 7)
#ggmap(Sum_Map_florida)

Temp_Data_Summer = sqldf("select STATION_NAME, LATITUDE AS Latitude, LONGITUDE as Longitude,DATE, AVG(TMAX) AS TMAX 
                         from csvdata_florida where DATE >= 20060401 AND DATE <=20060731 
                         or DATE >= 20070401 AND DATE <=20070731
                         or DATE >= 20080401 AND DATE <=20080731
                         or DATE >= 20090401 AND DATE <=20090731
                         or DATE >= 20100401 AND DATE <=20100731
                         or DATE >= 20110401 AND DATE <=20110731
                         or DATE >= 20120401 AND DATE <=20120731
                         GROUP BY STATION_NAME, LATITUDE,LONGITUDE  ORDER BY STATION_NAME")

Summer_Map_florida_Total <- ggmap(Sum_Map_florida) %+% subset(Temp_Data_Summer,TMAX != 'NA') +
  aes(x  =Longitude,y = Latitude,z = TMAX) +
  stat_summary2d() +  scale_fill_gradientn(name = "Temperture Recorded \n (in tenths of Degree C)",
                                           colours = colormap,space = "Lab") + 
  labs(x = "Longitude",y = "Latitude") +
  coord_map()

print(Summer_Map_florida_Total)


# Query For Winter Data 
Winter_Map_florida <- get_map(location = 'florida, United States', zoom = 7)
#ggmap(Winter_Map_florida)

Temp_Data_Winter = sqldf("select STATION_NAME, LATITUDE AS Latitude, LONGITUDE as Longitude,DATE, AVG(TMIN) AS TMIN 
                         from csvdata_florida where DATE >= 20061101 AND DATE <=20070228 
                         or DATE >= 20071101 AND DATE <=20080228
                         or DATE >= 20081101 AND DATE <=20090228
                         or DATE >= 20091101 AND DATE <=20100228
                         or DATE >= 20101101 AND DATE <=20110228
                         or DATE >= 20111101 AND DATE <=20120228
                         or DATE >= 20121101 AND DATE <=20130228
                         GROUP BY STATION_NAME, LATITUDE,LONGITUDE  ORDER BY STATION_NAME")

Winter_Map_florida_Total <- ggmap(Winter_Map_florida) %+% subset(Temp_Data_Winter,TMIN != 'NA') +
  aes(x  =Longitude,y = Latitude,z = TMIN) +
  stat_summary2d() +   scale_fill_gradientn(name = "Temperture Recorded \n (in tenths of Degree C)",
                                            colours = colormap,space = "Lab") + 
  labs(x = "Longitude",y = "Latitude") +coord_map()

print(Winter_Map_florida_Total)


## K - means --------------------------------not completed-------------------------------------
x <- data.frame(StationList_Precipitation$Latitude, StationList_Precipitation$Longitude)
mat = as.matrix(x)
kclus <- kmeans(mat,5)
mat2 = data.frame(x)
map = get_map(location = 'Florida, United States', zoom = 6)
mapPoints_KMeans <- ggmap(map) +
  geom_point(data = mat2 ,aes(x = StationList_Precipitation.Longitude,
                              y = StationList_Precipitation.Latitude , col =kclus$cluster ))
mapPoints_KMeans

##
------------------------------------------------------------------------------------------------------
  #head(csvdata_florida)
  
  ## Bar - plots 
  
  # TMAX and TMIN
Years  = c("2006", "2007","2008","2009","2010","2011","2012" )

Year1_florida = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_florida where Date like '2006%'")
Year2_florida = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_florida where Date like '2007%'")
Year3_florida = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_florida where Date like '2008%'")
Year4_florida = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_florida where Date like '2009%'")
Year5_florida = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_florida where Date like '2010%'")
Year6_florida = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_florida where Date like '2011%'")
Year7_florida = sqldf("select avg(TMAX) as TMAX, avg(TMIN) as TMIN from csvdata_florida where Date like '2012%'")

Year1_florida_Total <- rbind( Year1_florida, Year2_florida, Year3_florida, Year4_florida, Year5_florida, Year6_florida, Year7_florida)

TMAX = Year1_florida_Total$TMAX
TMIN = Year1_florida_Total$TMIN

Rate = matrix(c(TMAX,TMIN), nrow = length (Years))
Rate3 = t(Rate)
#-----------------------------
legend.text = TRUE
args.legend = list(x = "topright", bty = "n")
barplot(Rate3, beside=TRUE, names.arg=c("2006", "2007","2008","2009","2010","2011","2012"),
        col=c("blue", "red"),border="black", main=c("Maximum and Minimum Temperatures in florida \n Over the Years ( 2006 - 2012)"),
        xlab="Year",ylab="Temperature (in tenths of Degree Celsius) ",font.lab=3)

#axis(2, at = 0:10, labels = 0:10)
#legend("topright", colnames(mydata), fill = colors, bty = "n")
# legend("topright",legend = c("Max Temp.", "Min Temp."),cex=1,fill = c("darkblue", "red"),bty = "n")

#--------------------------------
# Precipitation 

Prcp1_florida = sqldf("select SUM(PRCP) as PRCP from csvdata_florida where Date like '2006%'")
Prcp2_florida = sqldf("select SUM(PRCP) as PRCP  from csvdata_florida where Date like '2007%'")
Prcp3_florida = sqldf("select SUM(PRCP) as PRCP  from csvdata_florida where Date like '2008%'")
Prcp4_florida = sqldf("select SUM(PRCP) as PRCP  from csvdata_florida where Date like '2009%'")
Prcp5_florida = sqldf("select SUM(PRCP) as PRCP  from csvdata_florida where Date like '2010%'")
Prcp6_florida = sqldf("select SUM(PRCP) as PRCP  from csvdata_florida where Date like '2011%'")
Prcp7_florida = sqldf("select SUM(PRCP) as PRCP  from csvdata_florida where Date like '2012%'")

PRCP_Total <- rbind( Prcp1_florida, Prcp2_florida, Prcp3_florida, Prcp4_florida, Prcp5_florida, Prcp6_florida, Prcp7_florida)

PRCP = PRCP_Total$PRCP

Rate = matrix(c(PRCP), nrow = length (Years))
Rate2 = t(Rate)

barplot(Rate2, beside=TRUE, names.arg=c("2006", "2007","2008","2009","2010","2011","2012"),
        col=c("Blue"),border="black", main=c("Total Precipitation Recorded(in tenths of mm) in florida \n Over the Years ( 2006 - 2012)"),xlab="Year",ylab="Precipitation (in tenths of mm) ",font.lab=3)

------------------------------------------------------------------------------------------------------------------------------------------------














