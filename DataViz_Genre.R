library(shiny)
library(highcharter)
library(dplyr)
edata <- read.csv('fans1.csv')

# View(edata)

wmap <- hcmap()

ui <- fluidPage(
               titlePanel("DeezerFans"),
               tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: aqua;  color:black}
    .tabbable > .nav > li > a[data-value='Introduction'] {background-color: red;   color:white}
    .tabbable > .nav > li > a[data-value='Visualisation'] {background-color: blue;  color:white}
    .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
               ")),
               tabsetPanel(
                 tabPanel("Introduction", # The user will have to read the description before using the app
                          p("This visualization is created for the Data Visualization project of the master of Data Science & AI.
                          This visualization was created by Ayoub Youssoufi using WASABI dataset.It is a part of group project.
                          The WASABI dataset is a large corpus of songs enriched with metadata extracted from music databases on the Web, 
                          and resulting from the processing of song lyrics and from audio analysis.
                          This can be useful for streaming music apps such as Spotify or Deezer to run a marketing compaign in the continents 
                          where there is a lack of use of the app. 
    
                          It is also interesting for people who want to see the distribution of the DeezerFans around the world."),
                          p("the ", em("Visualisation tab"), "contains mainly two panels.
                   The left panel contains filters to select according to Artist type and gender.
                   For the Artist type we can choose  between Group or Person. If the user select <<Group>>, it is required to select 
                   <<Both>> in the gender tab. However, if the user choose between <<Male>> or <<Female>> in the gender tab, he will need to select <<Person>> in Artist type tab."
                          ),
                     p("As a result, the output given displays the distribution of the DeezerFans by Artist type")     
                 ),
    tabPanel("Visualisation", # this tab will contain the visualisation
                # Application title
                sidebarLayout(
                  sidebarPanel(
                    
                    radioButtons('bubble','Bubble Size indicates',choices = c('deezerFans')),
                    selectInput('genreInput','Artist type',choices = c('Group', 'Person'),selected = 'Group'),
                    selectInput('genderInput','gender',choices = c('Male', 'Female','Both','Other'),selected = 'Both'),
                    sliderInput('bublesize','Adjust bubble Size',min = 2,max = 10,step = 2,value = 6)
                  ),
                  
                  # Display a Map Bubble
                  mainPanel(highchartOutput('eqmap',height = "500px")),
                  # dataTableOutput("results")
                  
                )))
)

server <- function(input, output) {
  
  
  df <- reactive(
    edata %>%
      filter(type == input$genreInput,
             gender== input$genderInput)%>%
      rename(z = input$bubble)
  )
  
  output$eqmap <-renderHighchart(
    
    wmap %>% hc_legend(enabled = F) %>%
      
      hc_add_series(data = df(), type = "mapbubble", name = "", maxSize = paste0(input$bublesize,'%')) %>% #bubble size in perc %
      
      hc_tooltip(useHTML = T,headerFormat='',pointFormat = paste('Location :{point.place}
 Time: {point.time}
',input$bubble,': {point.z}')) %>%
      
      hc_title(text = "Deezer Fans by continent") %>%
      hc_subtitle(text = paste('No of obs:', nrow(df()),sep = '')) %>%
      hc_mapNavigation(enabled = T) %>%
      hc_add_theme(hc_theme_flatdark())

      
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)