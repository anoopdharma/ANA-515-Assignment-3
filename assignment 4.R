library(readr)
storm<-read_csv("StormEvents_details-ftp_v1.0_d1994_c20190920.csv")

##########Limit the dataframe to: .................
storm=storm[,c(18,20,7:10,16,14,15,13,27,45:48)]


#############Convert the beginning and ending dates to a "date-time" class
storm$BEGIN_DATE_TIME=as.Date(storm$BEGIN_DATE_TIME,format="%m/%d/%Y %H:%M")
storm$END_DATE_TIME=as.Date(storm$END_DATE_TIME,format="%m/%d/%Y %H:%M")

#################Change state and county names to title case
library(stringr)
storm$STATE=str_to_title(storm$STATE)
storm$CZ_NAME=str_to_title(storm$CZ_NAME)

#################Limit to the events listed by county FIPS (
storm=subset(storm,CZ_TYPE="C")
storm=storm[,-8]

#############Pad the state and county FIPS with a "0" at the beginning
storm$STATE_FIPS=str_c("0",storm$STATE_FIPS,sep="")
storm$CZ_FIPS=ifelse(str_length(storm$CZ_FIPS)==2,str_c("0",storm$CZ_FIPS,sep=""),
                     storm$CZ_FIPS)
############## then unite the two columns to make one fips column..............
storm$FIPS=str_c(storm$STATE_FIPS,storm$CZ_FIPS)

############Change all the column names to lower case
library(dplyr)
storm=rename_all(storm,tolower)


##################create a dataframe with the state name, area, and region
library(datasets)
data(state)
state=data.frame(state_name=state.name,state_area=state.area,state_region=state.region)

#######Create a dataframe with the number of events per state in the year of your birth.
event_number=table(storm$state)
event_number=data.frame(event_number)
View(event_number)
names(event_number)=c("state","events")

###########Merge in the state information
storm_per_state=merge(state,event_number,by.x="state_name",by.y="state",all.x=T,all.y=F)

############Create the following plot
library(ggplot2)
ggplot(storm_per_state,aes(x=state_area,y=events,color=state_region))+
  geom_point()+
  labs(y="# of events in 2017",x="land area (square miles)")