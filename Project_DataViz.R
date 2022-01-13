library(tidyverse)
library(dplyr)
library(dslabs)
library(countrycode)
library(stringr)

#album <- read.csv("wasabi_albums.csv")
#songs <- read.delim("wasabi_songs.csv", sep = "\t")
#artist <- read.csv("wasabi_artists.csv")
#artist$yearbegin <- as.numeric(format(as.Date(artist$lifeSpan.begin, format="%Y-%m-%d"),"%Y"))
#artist$yearend <- as.numeric(format(as.Date(artist$lifeSpan.end, format="%Y-%m-%d"),"%Y"))
#artist$yearbegin[is.na(artist$yearbegin)] <- 2021
#artist$yearend[is.na(artist$yearend)] <- 2021
#artist$period <- abs(artist$yearend-artist$yearbegin)


artist <- read.csv("wasabi_artists.csv")

df <- artist %>% group_by(location.country)%>% summarise(n=n()) %>% arrange(desc(n))
group_df <- aggregate(deezerFans ~ location.country, data = artist, sum)
#group_df$Country <- str_sub(group_df$locationInfo,3,10)
for(i in 1:length(group_df$locationInfo)) {       # for-loop over columns
  group_df$experience[i] <- strsplit(group_df$locationInfo, ",")[[i]][1]
}
group_df_c <- aggregate(deezerFans ~ location.country, data = artist, sum)
df <- group_df_c %>% 
  rename(
    region = location.country
  )
mapdata <- map_data("world")
map_data <- left_join(mapdata,df,by= "region")
map_data_na <- map_data %>% filter(!is.na(map_data$deezerFans))

###########################################################

library(shiny)
library(highcharter)
library(dplyr)
wmap <- hcmap()

ui <- fluidPage(
  
  titlePanel("MapBubble"), # Application title
  sidebarLayout(
    sidebarPanel(     
      
      sliderInput('deezerFans','Magnitude more than(Richter Scale)', min = 150,max = 60000,step = 0.5,value = 0),
      
      sliderInput('bublesize','Adjust bubble Size',min = 2,max = 10,step = 1,value = 6)      
    ),
    
    # Display a Map Bubble
    mainPanel(
      highchartOutput('eqmap',height = "500px")         
    )
  )
)


server <- function(input, output) { 
  data <- reactive(map_data_na %>% 
                     filter(deezerFans >= input$deezerFans) %>%
                     rename(z = input$bubble))
  
  output$eqmap <-renderHighchart(
    
    wmap %>% hc_legend(enabled = F) %>%
      
      hc_add_series(data = data(), type = "mapbubble", name = "", maxSize = paste0(input$bublesize,'%')) %>% #bubble size in perc %
      
      hc_tooltip(useHTML = T,headerFormat='',pointFormat = paste('Location :{point.place} 
 Time: {point.time} 
',input$bubble,': {point.z}')) %>%
      
      hc_title(text = "Global Seismic Activity") %>%
      hc_subtitle(text = paste('No of obs:', nrow(data()),sep = '')) %>%
      hc_mapNavigation(enabled = T)
  )
}

# Run the application 
shinyApp(ui = ui, server = server)




