## Starbucks Challenge - given a set of addresses for Starbucks
## and city centers, determine which are within 20 miles, 50 miles of 
## a city center.
## Code by Melanie Vinton

library(ggmap)
library(dplyr)
library(leaflet)


#Read in CSV data files
cities<-read.csv("~/DataScience/Practice/cities.csv")
starbucks<-read.csv("~/DataScience/Practice/Starbucks.csv")



#Reduce cities to just DC
citydc<-subset(cities,City=="Washington, DC",na.rm=TRUE)

# Function to Calculate distance in kilometers between two points from
#https://conservationecology.wordpress.com/2013/06/30/distance-between-two-points-in-r/

earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c * .62137119
  return(d)
}

#Add Distance from each city to each Starbucks in starbucks dataframe
sbDist<-mutate(starbucks,DistDC=earth.dist(cities$Lon[1],cities$Lat[1],Longitude,Latitude))
sbDist<-mutate(sbDist,DistPhil=earth.dist(cities$Lon[2],cities$Lat[2],Longitude,Latitude))
sbDist<-mutate(sbDist,DistRich=earth.dist(cities$Lon[3],cities$Lat[3],Longitude,Latitude))
sbDist<-mutate(sbDist,DistBalt=earth.dist(cities$Lon[4],cities$Lat[4],Longitude,Latitude))
sbDist<-mutate(sbDist,DistAnnap=earth.dist(cities$Lon[5],cities$Lat[5],Longitude,Latitude))

#Filter for locations proximate to DC
sbDC<-filter(sbDist,DistDC<200)
sbDC<-arrange(sbDC,DistDC)
sbDC$Distance <- cut(sbDC$DistDC, c(0, 25, 50, 100, 200),labels=c("25 miles", "50 miles", "100 miles", "200 miles"))


# List of locations in each group
sbDC20<-filter(sbDist,DistDC<20)
sbDC50<-filter(sbDist,DistDC>20 & DistDC<50)
sbDC100<-filter(sbDist,DistDC>50 & DistDC<100)
sbDC200<-filter(sbDist,DistDC>100 & DistDC<200)

#create map and plot
dcmap<-qmap("Washington DC", zoom=6)
dcmap
dcmap+geom_point(aes(x=Longitude,y=Latitude,color=DistDC),data=sbDC)+
  scale_colour_gradientn(colours=rainbow(3),name="Miles",breaks=c(25,50,100,150),labels=c(25,50,100,150))+
  ggtitle("Starbucks Within 200 Miles of Washington DC")

#Create CSV file Washington DC
write.csv(sbDC, "DCStarbucks.csv", row.names=FALSE, na="")


#dcmap+geom_point(aes(x = Longitude, y = Latitude), data = sbDC20,colour="green")+
#geom_point(aes(x = Longitude, y = Latitude), data = sbDC50, colour="yellow")+
#geom_point(aes(x = Longitude, y = Latitude), data = sbDC100, colour="red")+
#geom_point(aes(x = Longitude, y = Latitude), data = sbDC200,colour="black")+
  #ggtitle("Starbucks Near DC")

#Create Leaflet interactive map
#binDist<-colorBin("Reds",sbDC$DistDC,c(25,50,100,200)) #colors based on bins
CatDist<-colorFactor(topo.colors(5),sbDC$Distance)
leafmap<-leaflet(sbDC) %>% addTiles() %>% 
  addCircleMarkers(lat=~Latitude,lng=~Longitude,radius=0.5,color=~CatDist(Distance)) %>%
  addLegend("bottomright",pal=CatDist,values=~Distance,title="Miles from DC")
leafmap


#Plot all points on Google map of US/Canada
map<-get_map(location='United States', zoom=4)
mapSB<-ggmap(map)+geom_point(aes(x=Longitude,y=Latitude),data=starbucks)
mapSB


#Density of Starbucks in US
#Ref: http://www.r-bloggers.com/contour-and-density-layers-with-ggmap/
map_bw<-get_map(location='United States', zoom=3,color="bw")
#map2<-get_map(location='Florida', zoom=6,color="bw")

ggmap(map_bw, extent="normal", maprange=FALSE) +
  geom_density2d(data=starbucks,aes(x=Longitude,y=Latitude))+
  stat_density2d(data=starbucks,aes(x=Longitude,y=Latitude,fill=..level..,alpha=..level..),
                 size=.01,bins=16,geom='polygon')+
  scale_fill_gradient(low="green",high="red")+
  scale_alpha(range=c(0.00,0.25),guide=FALSE)+
  coord_map(projection="mercator",xlim=c(attr(map,"bb")$ll.lon,attr(map,"bb")$ur.lon),
            ylim=c(attr(map,"bb")$ll.lat,attr(map,"bb")$ur.lat))+
  theme(legend.position="none",axis.title=element_blank(),text=element_text(size=12))



