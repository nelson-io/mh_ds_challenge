#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(naniar)
library(lubridate)
library(janitor)

choices_eda <- c('Missingness', 'Transactions over time', 
                 'Transactions by day of the week', 'Transactions by device', 
                 'Transactions by browser')
choices_clust <- c('Model Selection', 't-SNE representation')

#import data
load('plots/plot_list.RData')

df <- read_csv('data/data.csv') %>% 
    mutate(date = ymd(date),
           weekday = wday(date,label = T,abbr = F,week_start = 1)) %>% 
    clean_names()

plot_list[['missingness']] <- gg_miss_upset(df)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tabsetPanel(
        tabPanel('EDA',

            # Application title
            titlePanel("Exploratory Data Analysis"),
        
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel(
                    selectInput("eda_input",
                                "Analysis",
                                choices = choices_eda,
                                selected = 'Missingness',
                                multiple = F,
                                selectize = T)
                ),
        
                # Show a plot of the generated distribution
                mainPanel(
                   plotOutput("eda_plot"),
                   verbatimTextOutput('eda_text')
                )
            )
        ),
        tabPanel('Clustering',
                 
                 # Application title
                 titlePanel("K-Prototypes clustering"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("clust_input",
                                     "Analysis",
                                     choices = choices_clust,
                                     selected = 'Model Selection',
                                     multiple = F,
                                     selectize = T)
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("clust_plot"),
                         verbatimTextOutput('clust_text')
                     )
                 )
        )
    )    
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    selected_plot <- reactive({
        case_when(
            input$eda_input == choices_eda[1] ~ list(plot_list[1], text_list[[1]]),
            input$eda_input == choices_eda[2] ~ list(plot_list[2], text_list[[2]]),
            input$eda_input == choices_eda[3] ~ list(plot_list[3], text_list[[3]]),
            input$eda_input == choices_eda[4] ~ list(plot_list[4], text_list[[4]]),
            input$eda_input == choices_eda[5] ~ list(plot_list[5], text_list[[5]])
        )

    })

    selected_cplot <- reactive({
        case_when(
            input$clust_input == choices_clust[1] ~ list(plot_list[6], text_list[[6]]),
            input$clust_input == choices_clust[2] ~ list(plot_list[7], text_list[[7]])
        )
        
    })
    
    output$eda_plot <- renderPlot({
        selected_plot()[1]
       
    })
    
    output$eda_text <- renderText({
        selected_plot()[[2]]
    })
    
    output$clust_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        selected_cplot()[1]

    })
    
    output$clust_text <- renderText({
        selected_cplot()[[2]]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
