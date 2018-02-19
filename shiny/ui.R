library(shiny)
library(shinythemes) 
shinyUI(navbarPage( theme = shinytheme("sandstone"),
                    
                    h5("InauguralSpeeches visualization system"),
                    
                    tabPanel(h6("Basic graphs"),
                             sidebarLayout(
                               
                               sidebarPanel( 
                                 
                                 h5("Which graph want to plot?"),
                                 radioButtons("v3", 
                                                    label = h3(""), 
                                                    choices = list("beeswarm" = 1, 
                                                                   "heatmap" = 2,
                                                                   "Horizonal barplot" = 3,
                                                                   "fviz_cluster" = 4,
                                                                   "Vertical barplot" = 5,
                                                                   "corrplot" = 6,
                                                                   "wordCloud"=7,
                                                                   "neuralnet"=8
                                                                    ),
                                                    selected = 1),
                           submitButton(h4("Plot"))
                               ),                                   
                               
                               mainPanel( 
                                 tabsetPanel( 
                                   tabPanel("Graph Show pannel", plotOutput("fig"))
                               )
                             )   
                             ))
                
))