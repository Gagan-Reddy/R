library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

#Unzipping DublinBusGTFS file nto the working directory 
unzip("DublinBusGTFS.zip", exdir = ".")
list.files(path = ".")

#reading induvidual datasets 
routes <- read_csv("routes.txt")
stop_times <- read_csv("stop_times.txt")
stops <- read_csv("stops.txt")
trips <- read_csv("trips.txt")
agency <- read_csv("agency.txt")
calendar <- read_csv("calendar.txt")
calendar_dates <- read_csv("calendar_dates.txt")
shapes <- read_csv("shapes.txt")
transfers <- read_csv("transfers.txt")

#######################################################################################################################
#Question 3.1

#creating one large dataset with selected columns
Bus_Data <- routes %>% left_join(trips) %>%  left_join(stop_times) %>% left_join(stops)%>% 
  select(route_id, route_short_name , route_long_name, route_type, 
         service_id, trip_id, stop_id, stop_name, arrival_time, departure_time, stop_sequence, direction_id)

#Exploratory analysis of the Data, focussing on all the routes.
#Finding number of trips per Route
No_Trips <- Bus_Data %>% group_by( route_short_name, trip_id) %>% summarise() %>% summarise(count = n())


#plotting a graph for Number of trips vs Route name
ggplot(No_Trips, aes(route_short_name, count, fill = route_short_name)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Route Name") + ylab("Number of Trips") + ggtitle("Number of Trips per Route") +labs(fill="Route Name")

#######################################################################################################################
#Question 3.2


#fetching information for Route 15B
#Route 15B in Direction 1
Bus_forward <- filter(Bus_Data, route_short_name == "15B" & direction_id == 0)
#Route 15B in Direction 2
Bus_reverse <- filter(Bus_Data, route_short_name == "15B" & direction_id == 1)


#Findind the stop Sequence number for Home stop
Home_Seq <- filter(Bus_forward, stop_name == "Ballyboden Way")
Home_Seq[1,c(8,11)]
#finding the stop Sequencr number for Work Stop
Work_Seq <- filter(Bus_reverse, stop_name == "Charlotte Way")
Work_Seq[1,c(8,11)]


#Trips only from the time period of 06:00:00 till 10:00:00 in Direction 1
Trip_morning <- filter(Bus_forward, arrival_time > hms("06:00:00") & arrival_time < hms("10:00:00"))
#Trips only from the time period of 18:00:00 till 22:00:00 in Direction 2
Trip_evening <- filter(Bus_forward, arrival_time > hms("18:00:00") & arrival_time < hms("22:00:00"))


#top 50 trips for route 15B
Trip_50FOR <- Bus_forward[1:(max(Bus_forward$stop_sequence)*50), ]
Trip_50REV <- Bus_reverse[1:(max(Bus_reverse$stop_sequence)*50), ]


#plot to show trips in all time frames
ggplot(Trip_50FOR, aes(stop_sequence, arrival_time , col = trip_id)) + geom_line() + geom_point() +
  xlab("Stop Number") + ylab("Arrival Time") + ggtitle("House Bus timings for majority of the Day")
ggplot(na.omit(Trip_50REV), aes(stop_sequence, arrival_time , col = trip_id)) + geom_line() + geom_point() +
  xlab("Stop Number") + ylab("Arrival Time") + ggtitle("Work Bus timings for majority of the Day")


#plot to show trips in the morning 
ggplot(Trip_morning, aes(stop_sequence, arrival_time , col = trip_id)) + geom_line() + geom_point()  + 
  scale_x_continuous(breaks = seq(0,55,by = 5)) +
  xlab("Stop Number") + ylab("Arrival Time") + ggtitle("Bus timings in the morning") + labs(col="Trip ID")
#plot to show the trip in the evening
ggplot(Trip_evening, aes(stop_sequence, arrival_time , col = trip_id)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(0,55,by = 5)) + 
  xlab("Stop Number") + ylab("Arrival Time") + ggtitle("Bus timings in the evening") + labs(col="Trip ID")
