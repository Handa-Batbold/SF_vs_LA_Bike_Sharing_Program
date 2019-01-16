library(data.table)
library(lubridate)

setwd("~/Desktop/bikes")

## 1

#BA.trips$trip_id=as.factor(BA.trips$trip_id) #make it to same format
#BA.trips$end_date=strptime(BA.trips$end_date, "%Y-%m-%d %H:%M:%S")
#View(BA.trips$end_date)
#BA.trips$start_date= strptime(BA.trips$start_date, "%Y-%m-%d %H:%M:%S")
#BA.trips$start_station_id = as.factor(BA.trips$start_station_id)
#BA.trips$end_station_id = as.factor(BA.trips$end_station_id)
#BA.trips$bike_number= as.factor(BA.trips$bike_number)

Bay_trips_rds = function (outpath){ #discussion code
  df = fread("sf_bikeshare_trips.csv")
  df$end_date = ymd_hms(df$end_date)
  df$start_date = ymd_hms(df$start_date)
  df$start_station_id = as.factor(df$start_station_id) 
  df$bike_number = as.factor(df$bike_number)
  df$end_station_id = as.factor(df$end_station_id)
  df$trip_id = as.factor(df$trip_id)
  saveRDS(df, outpath)
}

#Execute the function

Bay_trips_rds("BA_trips.rds")

Bay_Stations_rds = function (outpath) {
  df = read.csv("sf_bike_share_stations.csv")
  df$station_id = as.factor(df$station_id)
  df$installation_date = ymd(df$installation_date)
  saveRDS(df, outpath)
}

Bay_Stations_rds("BA_stations.rds")

## 2
#install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
#install.packages(c("maps", "mapdata"))
#devtools::install_github("dkahle/ggmap")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggrepel)

BA_stations = readRDS("BA_stations.rds")
BA_trips = readRDS("BA_trips.rds")

#Step 1: Subset to only SF stations

SF_stations<-BA_stations[BA_stations$landmark=="San Francisco",]

#Also, remove duplicate stations by their ID:

SF_stations<-SF_stations[duplicated(SF_stations$station_id)==FALSE,]

#Step 2: Subset trips to only be in San Francisco

SF_trips<-BA_trips[BA_trips$start_station_id %in% SF_stations$station_id,]

#Re-factor the start station variable so we remove the id's for non-SF stations
SF_trips$start_station_id<-factor(SF_trips$start_station_id)

#Step 3: Get the total number of trips by start station id

trip_counts<-as.data.frame(table(SF_trips$start_station_id))
names(trip_counts)<-c("station_id","trips")

#Step 4: join the trip counts data frame to the stations dataset

SF_stations<-plyr::join(SF_stations,trip_counts,"station_id","inner")


station_map<- get_map(location = "Downtown San Francisco", zoom = 13)

SF_loc = ggmap(station_map, extent = "device", ylab = "Latitude", xlab = "Longtitude")
SF_loc=SF_loc+geom_point(data = SF_stations, aes(x = longitude, y = latitude, size=trips), 
                         color="red",alpha=.5)
SF_loc=SF_loc+geom_text(data = SF_stations, aes(x= longitude, y= latitude,label = name), size = 2)
SF_loc

## duplicate(station_id)
## to add names of stations library(ggrepel)then use geom_text_repel()

###Question 3

###LA trips

LA_trip_files<-list.files()[grep("la_metro",list.files())]

RDS_names<-paste0("LAmetro",1:5,".rds")

readLAmetro<-function(infile,outpath){
  df<-fread(infile)
  #Step 1: Remove the '_id' part from start/end_station_id
  names(df)<-gsub("station_id","station",names(df))
  #Step 2: Convert the datetime variables for the dataset
  df$start_time<-parse_date_time(df$start_time,orders=c("m/d/y H:M","y-m-d H:M:S"))
  df$end_time<-parse_date_time(df$end_time,orders=c("m/d/y H:M","y-m-d H:M:S"))
  #Step 3: Save the RDS file
  saveRDS(df, outpath)
}

#Execute the function 5 times

sapply(1:5,function(x)readLAmetro(LA_trip_files[x],RDS_names[x]))

#Combine these all into a single data frame

metro_list<-lapply(RDS_names,readRDS)

la_metro_full<-do.call(rbind,metro_list)

#Finally, save this combined dataset as an RDS

saveRDS(la_metro_full,"LAmetrofull.rds")

####

LA.stations <- read.csv("metro-bike-share-stations-2017-10-20.csv")

readLAstation<-function(outpath){
  df<-read.csv("metro-bike-share-stations-2017-10-20.csv")
  df$Go_live_date<-parse_date_time(df$Go_live_date,orders=c("y-m-d","m/d/y"))
  saveRDS(df,outpath)
}

readLAstation("LAstation.rds")

#Q4: Make the same map but for LA.

#We'll need the trip counts once again, also we only want DTLA stations

LAtrips<-readRDS("LAmetrofull.rds")
LAstations<-readRDS("LAstation.rds")

#Step 1: Subset to DTLA stations

DTLAstations<-LAstations[LAstations$Region=="DTLA",]

#Step 2: Subset to DTLA trips

DTLAtrips<-LAtrips[LAtrips$start_station %in% DTLAstations$Station_ID,]

#Also, re-factor the start station variable

DTLAtrips$start_station<-factor(DTLAtrips$start_station)

#Also, make the start lat and lon numeric

DTLAtrips$start_lat<-as.numeric(DTLAtrips$start_lat)
DTLAtrips$start_lon<-as.numeric(DTLAtrips$start_lon)

#Step 3: Derive the estimated location of the stations
station_lon<-aggregate(start_lon~start_station,DTLAtrips,median)
station_lat<-aggregate(start_lat~start_station,DTLAtrips,median)

station_loc<-plyr::join(station_lon,station_lat,"start_station")
names(station_loc)[1]<-c("Station_ID")

#Fix the DTLA stations id variable
DTLAstations$Station_ID<-factor(DTLAstations$Station_ID)

DTLAstations<-plyr::join(DTLAstations,station_loc,"Station_ID")

#Step 4: Get the start counts for each station
LA_trip_counts<-as.data.frame(table(DTLAtrips$start_station))
names(LA_trip_counts)<-c("Station_ID","trips")

DTLAstations<-plyr::join(DTLAstations,LA_trip_counts,"Station_ID")

#Also, remove the last row. It contains no data.
DTLAstations<-DTLAstations[-66,]

#Finally, make the map

la_map<- get_map(location = "Downtown Los Angeles", zoom = 13)

LA_loc = ggmap(la_map, extent = "device", ylab = "Latitude", xlab = "Longtitude")
LA_loc=LA_loc+geom_point(data = DTLAstations, aes(x = start_lon, y=start_lat, size=trips), 
                         color="red",alpha=.5)
LA_loc=LA_loc+geom_text(data = DTLAstations, aes(x= start_lon, y=start_lat,label = Station_Name), size = 2)
LA_loc

#Question 5

library(geosphere)

#Different times of day: create an 'hour' variable that ranges from 0-23 (12:00 AM-11:00 PM)


#Trip Frequency

BA_trips$hour<-hour(BA_trips$start_date)
LAtrips$hour<-hour(LAtrips$start_time)

sf_trip_freq<-as.data.frame(table(BA_trips$hour))
la_trip_freq<-as.data.frame(table(LAtrips$hour))

sf_vs_la_freq<-plyr::join(sf_trip_freq,la_trip_freq,"Var1")
names(sf_vs_la_freq)<-c("Hour","Bay Area","Los Angeles")

par(mfrow=c(1,2))
plot(sf_vs_la_freq[,1],sf_vs_la_freq[,2],xlab="Hour",ylab="Bike trips",main="Bay Area bike trips")
plot(sf_vs_la_freq[,1],sf_vs_la_freq[,3],xlab="Hour",ylab="Bike trips",main="Los Angeles bike trips")

#Bay Area bike rides peak at the beginning and end of commute hours.
#LA bike rides peak only during leisure hours.

#Trip Distance
BA_start_loc<-BA_stations[match(BA_trips$start_station_id,BA_stations$station_id),4:3]
BA_end_loc<-BA_stations[match(BA_trips$end_station_id,BA_stations$station_id),4:3]

#Get the distance between start point and end point for both sets of data
BA_dist<-distGeo(BA_start_loc,BA_end_loc)

BA_trips$distance<-BA_dist

#Los Angeles
#Make the latitude and longitude numeric
LAtrips$start_lat<-as.numeric(LAtrips$start_lat)
LAtrips$start_lon<-as.numeric(LAtrips$start_lon)
LAtrips$end_lat<-as.numeric(LAtrips$end_lat)
LAtrips$end_lon<-as.numeric(LAtrips$end_lon)

LA_start_loc<-LAtrips[,c(7:6)]
LA_end_loc<-LAtrips[,c(10:9)]

LA_dist<-distGeo(LA_start_loc,LA_end_loc)

LAtrips$distance<-LA_dist

#Get the median trip distance by hour for both BA and LA

BA_med_dist<-aggregate(distance~hour,BA_trips,median)

LA_med_dist<-aggregate(distance~hour,LAtrips,median)

par(mfrow=c(1,2))
plot(BA_med_dist,xlab="Hour",ylab="Median distance (meters)",main="Bay Area bike trip distance",type='l')
plot(LA_med_dist,xlab="Hour",ylab="Median distance (meters)",main="Los Angeles bike trip distance",type='l')

#Bike trips are longer during the commute for people in the bay area. The distances drop because sometimes...
#...their tech comapny can't afford catering so they have to go to a food truck instead.
#LA bike rides also peak in the morning, but later. Perhaps even after work starts.
#This may be explained by the fact that the riders are getting their 'morning exercise' on the bike.
#But there also may be a commute effect as well.

#Trip Duration

BA_med_dur<-aggregate(duration_sec~hour,BA_trips,median)

LAtrips$duration_sec<-LAtrips$end_time-LAtrips$start_time

LA_med_dur<-aggregate(duration_sec~hour,LAtrips,median)

par(mfrow=c(1,2))
plot(BA_med_dur,xlab="Hour",ylab="Median duration (seconds)",main="Bay Area bike trip duration",type='l')
plot(LA_med_dur,xlab="Hour",ylab="Median duration (seconds)",main="Los Angeles bike trip duration",type='l')

#Bike trips in the Bay Area are very long in the morning, they might be stuck behind cars while they go to work.
#The durations drop off right after work starts.
#The after-work peak is much smaller as well, indicating that the bike ride from work may be shorter.
#For LA, there is an unusual dip in bike durations around 4:00 pm. People may be riding to metro stations..
#...which takes them very little time.

#Question 6

BA_bearing<-bearing(BA_start_loc,BA_end_loc)

BA_trips$bearing<-BA_bearing

LA_bearing<-bearing(LA_start_loc,LA_end_loc)

LAtrips$bearing<-LA_bearing

BA_med_bear<-aggregate(bearing~hour,BA_trips,median)
LA_med_bear<-aggregate(bearing~hour,LAtrips,median)

par(mfrow=c(1,2))
plot(BA_med_bear,xlab="Hour",ylab="Median bearing (degrees)",main="Bay Area bike trip duration",type='l')
plot(LA_med_bear,xlab="Hour",ylab="Median bearing (degrees)",main="Los Angeles bike trip duration",type='l')

#For SF, there's a difference in how people head at the beginning and end of work.
#In LA, this pattern is very consistent. People are entering and exiting work in the same direction.

#Q6:
#I know that I need to use the bearing() function
#Read the documentation
?bearing
#I know it asks for a set of 2 dimensional points in each argument
#I can use the fact that the bike sharing datasets should have pairs of start points and end points
#So in my head I see the function as bearing(start_points,end_points)
#This will give me a single number for each observation
#Then, I need to show how this changes by time of day.
#I know that I have the hour variable
#So I need to compare bearing by hour.
#If I treat hour as a group, I'm looking for "group statistics" for bearing
#When I want group statistics, I always use the aggregate() function
#Now, I visualize: aggregate(bearing~hour,data,median)
#I know from before that there are outliers in the data, so it's safer to choose median here
