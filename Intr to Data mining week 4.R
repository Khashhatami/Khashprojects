library(ggplot2)

# Creating a simple map of US
# about "map_id" function" from stack overflow : (( map_id can be any column that hold the identifier for other layers. 
#Your first call to geom_map() should (usually) be the "base layer", similar to what you'd do with a full-on GIS program,
#that has the polygon outlines and perhaps a base fill.))
# Also pay attention to how when drwaing a mapp X and Y function as defacto longtitude and altitude 

us<-map_data("state")

dummyDF<- data.frame(state.name, stringsAsFactors = FALSE)
dummyDF$state<-tolower(dummyDF$state.name)

map.simple<- ggplot(dummyDF,aes(map_id=state))
map.simple<- map.simple + geom_map(map=us,fill="white",color="black")
map.simple<- map.simple + expand_limits(x= us$long,y=us$lat)
map.simple<- map.simple + coord_map()+ggtitle("Basic Map of contenintal USA")
map.simple

# This code is from the last semester 

urlToRead <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv" 
# Numberize() - Gets rid of commas and other junk and 
# converts to numbers 
# Assumes that the inputVector is a list of data that 
# can be treated as character strings 
# this summerizes the data munging that was done back in chapter 6 into a single function  
Numberize <- function(inputVector) 
{ 
        # Get rid of commas 
        inputVector<-gsub(",","",inputVector) 
        # Get rid of spaces 
        inputVector<-gsub(" ","", inputVector) 
        return(as.numeric(inputVector)) 
} 
readCensus <- function() { 
        urlToRead <-"http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv" 
        #read the data from the web 
        testFrame <- read.csv(url(urlToRead)) 
        #remove the first 8 rows ('header information') 
        testFrame<-testFrame[-1:-8,] 
        #only keep the first 5 columns 
        testFrame<-testFrame[,1:5] 
        #rename the first column 
        testFrame$stateName <- testFrame[,1] 
        testFrame<-testFrame[,-1] 
        #remove the last rows (tail info) 
        testFrame<-testFrame[-52:-58,] 
        #remove the 'dot' from the state name 
        testFrame$stateName <- gsub("\\.","",testFrame$stateName) 
        #convert the columns to actual numbers and rename 
        #columns 
        testFrame$april10census <-Numberize(testFrame$X) 
        testFrame$april10base <-Numberize(testFrame$X.1) 
        testFrame$july10pop <-Numberize(testFrame$X.2) 
        testFrame$july11pop <-Numberize(testFrame$X.3) 
        testFrame <- testFrame[,-1:-4] 
        #remove the old rownames, which are now confusing 
        rownames(testFrame) <- NULL 
        return(testFrame) 
} 

dfStates<- readCensus()

#For ggplot to recognize the name of states, they all have to lowercase 
# here we want to create map of US that the fill color of each state shows the population density of that state
# the brighter the color, the more populated that state is

dfStates$state<-tolower(dfStates$stateName)

map.popcolor<- ggplot(dfStates, aes(map_id=state))
map.popcolor<- map.popcolor+ geom_map(map=us, (aes(fill=dfStates$july11pop))
map.popcolor<- map.popcolor + expand_limits(x=us$long, y=us$lat)
map.popcolor<- map.popcolor + coord_map()+ggtitle("state population")
map.popcolor


# Now we want to add points to our map 
# here we specify a lag and lat to exactly pinpoint a picticular part of our map (in this case on southern Texas)
map.simple+ geom_point(aes(x=-100,y=30))

# Here instead of giving a specific lag and lat, we want to use a logical location. This is a more advanced way of asking our map to 
# show us a location. In this instance, we are asking our map to show a paticular location in newyork by just naming it
# When using geocode function you get a vector with two element 1. longtitude and 2. altitude, We use these two elements as our X and Y to 
#pinpoint to a specific location our map "map.popcolor"

library(ggmap)
register_google(key=)

latlon<-geocode("syracuse, ny")

map.popcolor + geom_point(aes(x=latlon$lon,y=latlon$lat,color= "darked",size=3))


# Now we want to add another point on Colerado State (misspelled)
# Here we have converted  latlon into a dataframe that has three points in it which all will be dispalyed on our map 

df.latlon<- data.frame(latlon)

latlon<- geocode("colorado")

df.latlon[2,]<- latlon
df.latlon[3,]<-geocode("denver,colorado")

# Here it gives us an FUN error stating that state column does not exist given that the original ggplot operated based on state. SO we 
#are going to create a dummy column "state" in latlon dataframe to trick R to give us the points on the map anyway 
map.simple+ geom_point(data=df.latlon,aes(x=lon,y=lat))

#Creating a dummy "state" column
#after this function, we should have three dots on our us map. One in newyork and two in colarado 
df.latlon$state<- "?"
map.simple+ geom_point(data=df.latlon,aes(x=lon,y=lat),alpha=0.5,color="darkred",size=3)


# Now lets conduct some geocode analysis on a larger data set
# this is a list of companies and where they are located at.

urlFile<- "https://raw.githubusercontent.com/GovLab/OpenData500/master/static/files/us/us_companies_all.csv"
od.comapnies<-read.csv(url(urlFile))

# through this function we remove all companies that do not have their "city" specidied. If the "city" column is empty, this function gets rid 
#of them. Here we use [] which is for looking specific column and row. We leave the rows section empty [x,] so it applies to all rows
# then we write condition that all elements of our data frame (od.companies) should have thier "city" column not empty(city!="")
# this means that if the city column is empty, the relavent element will dropped out of our data frame. 
# by conduting this function we see that the number of companies in our data frame is being reduced from 695 to 629. This means that 
#something like 60 of the comapnies did not have a specified city in the city column

od.comapnies<- od.comapnies[od.comapnies$city!="",]

# Now we want to first make sure all the states are characters
od.comapnies$state<- as.character(od.comapnies$state)

# Now we remove washington DC from the states becuase it is not a state and confuses ggplot when graphing 
od.comapnies<- od.comapnies[od.comapnies$state!="DC",]

# Aslo we want to change the abbriviation for Kansas from "KS" to "KA" becuae postal service has uses a diffrent abbeirivation for this state
# This is a condutional statment. It says "if you find, KA in state column, change that to KS in od.copanies dataframe"
od.comapnies$state<- ifelse(od.comapnies$state== "KA","KS",od.comapnies$state)

# In order to use geocode we need both city and state columns. However they are in seperate columns. So what we are going to do is to combine them
# into a singular column
#paste() method in R programming is used to concatenate the two string values by separating with delimiters
od.comapnies$cityState<- paste(od.comapnies$city,od.comapnies$state)

#now we want to create a separete column in our data frame that contains the geo location of the companies 
od.comapnies$geoCode<-geocode(od.comapnies$cityState)

# now that we have the geocode of all companies in our data set, we can see them on our map 
map.simple+ geom_point(data=od.comapnies,aes(x=geoCode$lon,y=geoCode$lat),shape=1)

# as we can see on the map, there is a dot that is far far away. 
#apearntly google has misundretood where one of our companies are. 
# to find that company we need look for a company that in its geocode has a longtitude greater than -25
bad<- od.comapnies[od.comapnies$geoCode$lon> -25,]

# now lets find out what city this geocode correspond to 
bad$cityState

# R console says "VITORIA GASTEIZ PR". Whatever place that this is, Google does not seem to be able to find this city.
# there are several ways to deal with this problem but the book suggest writing a code that ognores all cities beyond
# -24 longtitude
#Look at the operaters sing (<) and how it is used to isolate that city
od.comapnies<-od.comapnies[od.comapnies$geoCode$lon< -25,]

#let us try again !
# This map shows all the companies that are in our data frame on the map
map.simple+ geom_point(data=od.comapnies,aes(x=geoCode$lon,y=geoCode$lat),shape=1)

#Now let the fill of each circle represent the size of each comapny (in terms of how many people are employed within that comapny)
#first lets get some colors 
library(RColorBrewer)

# Here we are creating a new column in our data frame to break the comapnies in accordance to their number of their full time employess
#into categorical sizes so we can color the dots on our map
od.comapnies$sizes<- factor(od.comapnies$full_time_employees,levels=c("1-10","11-50","51-200","201-500","501-1000","5001-10000","10001+"))

# this function designate numbers for each categorical level of company sizes
numSizes <- length(levels(od.comapnies$sizes))

#brewer.pal provides color pallets to color our dots
# we apply different shades of Red on each of those numbers
myColors<- brewer.pal(numSizes,"Reds")

names(myColors)<- levels(od.comapnies$sizes)

# we test to see if there is a corresponding color to each company size
myColors[1:3]

# This provides our final map. This is the map of united states in which the color each state correspond to the population of that state
# also the dots show where the companies are and the colroing of each dot shows the size of the company. 
# the darker the color of the dot, the greater is the size of the company
map.popcolor + geom_point(data=od.comapnies,aes(x= geoCode$lon, y= geoCode$lat, color= sizes))+ scale_colour_manual(name="sizeOfCompnay",values=myColors)+ ggtitle("Open Data Company Analysis")
