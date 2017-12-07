#Goal is to create a map by state of the number of Starbucks
#and color code the states (like a choropleth map)

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(ggmap)
library(scales)



#Read in CSV data files
starbucks<-read.csv("~/DataScience/Practice/Starbucks.csv")

SBstate<-separate(starbucks,Address,c("street","state"),sep=",")
SBstate<-separate(SBstate,state,c("state","zip"),sep=" ")
#Results in the state falling into the zip column
#change the name of the column "zip" with:
names(SBstate)[names(SBstate)=="zip"]<-"ST"
#Remove the column state
SBstate$state<-NULL

#Improved method from above
SBstate2<-separate(starbucks,Address,c("street","state"),sep=",")
SBstate2<-separate(SBstate2,state,c("none","state","zip"),sep=" ")
#Remove the excess columns
SBstate2$none<-NULL
SBstate2$zip<-NULL

#Number of rows with each unique state
STnums<-count(SBstate2,state)

#Remove rows with NA
STnums<-na.omit(STnums)

#add column that converts state abbreviation to state name
STnums2<-mutate(STnums,region=state.name[match(STnums$state,state.abb)])

#Elimate rows with no state name (like Canadian provinces)
STnums2<-na.omit(STnums2)

#convert state names to lower case so can match with map_data("state")
STnums2$region<-tolower(STnums2$region)

#create map of CONUS states - doesn't include alaska and hawaii
states<-map_data("state")

#merge Starbucks data and states map
SBmerged<-merge(states,STnums2,sort=FALSE,by="region")
SBmerged<-SBmerged[order(SBmerged$order), ]

#create map (CONUS only)
ggplot()+
  geom_polygon(data=SBmerged,aes(x=long,y=lat,group=group,fill=n),color="black",size=0.2)+
  coord_map()+ #fixes the shape distortion
  scale_fill_distiller(palette="Spectral")+ 
  #distiller (instead of brewer) for continuous values
  labs(title="Number of Starbucks by State",fill="")+ #fill="" removes label from legend (was="freq")
  theme_nothing(legend=TRUE)  #get rid of axis labels (in ggmap package)
  
  
  
  
# Create map of all 50 states
states48 <- map_data("state")
  fix_AK <- map_data("world") %>% filter(., region == "USA" & long > 100) %>%
  mutate(., long = long * (-1))
states50_map <- map_data("world") %>% filter(., region == "USA" & long< 100)%>% 
  bind_rows(fix_AK)


AKnums <- filter(STnums2, state == "AK"|state == "HI")
AKnums <- rename(AKnums, subregion = region)
states50_map$subregion <- tolower(states50_map$subregion)
AKmerged <- merge(states50_map, AKnums, sort = FALSE, by = "subregion")
AKmerged <- AKmerged[order(AKmerged$order), ]

ggplot()+geom_polygon(data=AKmerged, aes(x=long,y=lat, group=group, fill = n),colour="black", size = .2)+
  geom_polygon(data=SBmerged, aes(x=long,y=lat, group=group, fill = n), colour="black", size = .2)+
  coord_map() +
  scale_fill_distiller(palette="Spectral")+ 
  #distiller (instead of brewer) for continuous values
  labs(title="Number of Starbucks by State",fill="")+ #fill="" removes label from legend (was="freq")
  theme_nothing(legend=TRUE)  #get rid of axis labels (in ggmap package)
  



