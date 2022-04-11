
library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)


video_game_sales=read.csv('Video_Games_Sales_as_at_22_Dec_2016.csv')
complete_cases=complete.cases(video_game_sales)
data=video_game_sales[complete_cases,] #delete rows with NA
data$Year_of_Release = as.numeric(data$Year_of_Release)

#The goal is to show the average sales across different binned critics' score
#categories, allowing the user to choose between different genre, platform and
#geographical region

# Define UI 
ui <- fluidPage(
  
  navbarPage("Video Game Market",
             tabPanel("Average Sales Revenue VS Critics Ratings",
                      
                      column(4,
                             # Range Input
                             selectInput("genrefilter",    #select genre
                                         label = h3("Genre"),
                                         c("Sports" = "Sports",
                                           "Racing" = "Racing",
                                           "Role-Playing" = "Role-Playing",
                                           "Puzzle" = "Puzzle",
                                           "Shooter" = "Shooter",
                                           "Action"='Action',
                                           "Fighting"="Fighting",
                                           "Adventure"="Adventure",
                                           "Strategy"="Strategy"
                                         ),
                                         multiple=FALSE,
                                         selected = "Shooter"  #default choice
                             ),
                             selectInput("areafilter",  #selects region
                                         h3("Continents: "),
                                         c("North America" = "NA_Sales",
                                           "Europe" = "EU_Sales",
                                           "Japan" = "JP_Sales",
                                           "Other" = "Other_Sales",
                                           "Global" = "Global_Sales"
                                         ),
                                         multiple=FALSE,
                                         selected = "Global"
                             ),
                             selectInput("platformfilter", #selects platform
                                         h3("Platform: "),
                                         c("PS2" = "PS2",
                                           "X360" = "X360",
                                           "PS3" = "PS3"
                                         ),
                                         multiple=FALSE,
                                         selected = "Wii"
                             )
                      ),
                      
                      column(8,
                             plotOutput("hist")
                      )
             ) #end of window
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$hist <- renderPlot({
    
    #filter data by selected genre and platform,
    #then keep the Critic_Score, and whatever column corresponding to the
    #region the user selected (NA_Sales, JP_Sales, etc, each contains USD sales)
    data <- data %>%
      filter(Genre==input$genrefilter[1] & Platform==input$platformfilter[1])%>%
      select(Critic_Score,input$areafilter[1])
    
    
    #bin the critic's scores and store into new column 'binned_score'
    data$binned_score=cut(data$Critic_Score, c(0,50,60,70,80,90,1000),labels=c(50,60,70,80,90,100))
    
    colnames(data)=c('Critic_Score','area_sales','binned_score') #rename columns
    
    #grouping by binned_score, calculate the average sales 
    data<-data%>%select(area_sales,binned_score)%>%
      group_by(binned_score)%>%summarise(sale=mean(area_sales))
    
    ggplot(data, aes(x=binned_score, y=sale)) + geom_bar(stat="identity", fill = "#FF6666") + 
      xlab("Critic Score Ranges") + ylab("Average Sales, in Millions of USD") +
      labs(title = "Average Sales VS Critic Score", colour = "Continents", subtitle  = "Critic ratings are binned from 50-60, 60-70, etc") +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
