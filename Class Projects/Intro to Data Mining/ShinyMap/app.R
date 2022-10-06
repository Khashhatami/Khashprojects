#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggmap)
library(ggplot2)




    Myui <- fluidPage(
        selectInput("variable","Variable:",c("july population" = "july11pop","region of country" = "region", "percentChange", "change in population"="popChange", "Percent change in population" = "percentChange")),plotOutput("plot"))
   

Myserver <- function(input, output) {
    us <- map_data("state")
    
    dfStates<- readCensus()
    
    dfStates <- dfStates[dfStates$stateName != "District of Columbia",]
    
    dfStates$region <- state.region
    
    dfStates$stateName <- tolower(dfStates$stateName)
    
    dfStates$popChange <- dfStates$july11pop - dfStates$july10pop
    
    dfStates$percentChange <- dfStates$popChange/dfStates$july10pop*100
    
    dfStates$state<-dfStates$stateName 
                        
    output$plot<- renderPlot(ggplot(dfStates, aes(map_id = state))+ geom_map(map=us,aes(fill=dfStates[,input$variable])) + expand_limits(x= us$long, y=us$lat) + coord_map() + ggtitle("state population") + guides (fill= guide_legend(title=input$variable)))}  

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


# Run the application 
shinyApp(ui = Myui, server = Myserver)

