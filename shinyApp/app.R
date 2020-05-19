########################################################################################################
#
# AUTHOR: Gary Gasperino
# DATE: 12/13/2019
# DESCRIPTION: Shiny app to display birth and death data openly available from the UN data repository
#
########################################################################################################

#libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)

#load data
df <- read.csv("un_data.csv")
df$date <- as.Date(df$date,"%Y-%m-%d")
df$X <- NULL

#ui begin
ui <- fluidPage(
  titlePanel("International Births & Deaths Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      
    #description text
    helpText("Data was curated by the UN and is available at their ",a("data website.",href="http://data.un.org/")),
    helpText("The data was captured for over 200 countries. Data begins in various years depending on the country, with the earliest
              year being 1967 and the latest year being 2017. Using the filters below, the birth and death rates
             can be visualized across individual countries, whole continents, or globally."),
    
    #geographic aggregation level filter
    selectInput("geographic_agg",
                "Pick geographical level of aggregation:",
                choices=c("Countries","Continents","Global"),
                selected="Countries",
                multiple=FALSE,
                selectize=TRUE
                ),
    
    #display variable select filter
    selectInput("variable_select",
                "Select variable to display:",
                choices=c("Births","Deaths"),
                selected=c("Births","Deaths"),
                multiple=TRUE,
                selectize=TRUE
                ),
    
    #condition country selector
    uiOutput("lower_select")
    
    ),#end of sidebarPanel
  
    mainPanel(
      plotOutput("chart")
    )#end of mainPanel
    
  )#end of sideBarLayout
) #end of fluidPage for UI      


#start of server
server <- function(input, output, session) {
  
  output$lower_select <- renderUI({
    if(input$geographic_agg=="Countries"){
      selectInput("country_select",
                  "Select country data to display:",
                  choices=unique(df$country),
                  selected="United States of America",
                  multiple=FALSE
                  )
    } else if(input$geographic_agg=="Continents") {
      selectInput("continent_select",
                  "Select continent data to display:",
                  choices=unique(df$continent),
                  selected="North America",
                  multiple=FALSE)
      
    } else {
      
    }
  })
  
  output$chart <- renderPlot({
    if(input$geographic_agg=="Countries"){
      #first filter by variables to show
      plot_df <- subset(df, dataType %in% input$variable_select)
      
      #next filter by country selected
      plot_df <- subset(plot_df, country==input$country_select)
      
      #now display plot
      plot <- ggplot(plot_df,aes(x=date,y=value,color=dataType))
      plot <- plot + geom_point()
      plot <- plot + geom_smooth(se=FALSE)
      plot <- plot + labs(color="Measure Type")
      plot <- plot + xlab("Date of")
      plot <- plot + ylab("Number of")
      plot <- plot + theme_minimal()
      plot
    } else if(input$geographic_agg=="Continents"){
      #group data by continent
      plot_df <- df %>%
        group_by(continent,dataType,date) %>%
        summarize(value=sum(value))

      #subset datatype
      plot_df <- subset(plot_df,dataType %in% input$variable_select)
      
      #subset continent
      plot_df <- subset(plot_df,continent==input$continent_select)
      
      #create plot
      plot <- ggplot(plot_df,aes(x=date,y=value,color=dataType))
      plot <- plot + geom_point()
      plot <- plot + geom_smooth(se=FALSE)
      plot <- plot + labs(color="Measure Type")
      plot <- plot + xlab("Date of")
      plot <- plot + ylab("Number of")
      plot <- plot + theme_minimal()
      plot
    } else {
      #group data so it is on a global level by month/year
      plot_df <- df %>%
        group_by(date,dataType) %>%
        summarize(value = sum(value))
      
      #subset by variable select
      plot_df <- subset(plot_df,dataType %in% input$variable_select)
      
      #create plot
      plot <- ggplot(plot_df,aes(x=date,y=value,color=dataType))
      plot <- plot + geom_point()
      plot <- plot + geom_smooth(se=FALSE)
      plot <- plot + labs(color="Measure Type")
      plot <- plot + xlab("Date of")
      plot <- plot + ylab("Number of")
      plot <- plot + theme_minimal()
      plot
    }
  })
  
} #end of Server

shinyApp(ui=ui, server=server)