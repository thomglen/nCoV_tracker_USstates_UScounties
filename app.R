## COVID-2019 interactive mapping tool
## Edward Parker, London School of Hygiene & Tropical Medicine (edward.parker@lshtm.ac.uk), February 2020

if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
#if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
#if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")


### SHINY UI ###
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 "COVID-19 tracker", id="nav",
              
                 
                 tabPanel("About this site",
                          tags$div(
                            tags$h4("Site moved"), 
                            
                            "Site reconstructed as a fork of the original COVID-19 tracker site.",tags$br(),
                            "Please visit (and re-bookmark):",
                            tags$br(),
                            tags$a(href="https://graeber.shinyapps.io/nCoV_tracker/", "https://graeber.shinyapps.io/nCoV_tracker/"),
                            tags$br(),tags$br(),
                            "Code repository:",tags$br(),
                            tags$a(href="https://github.com/thomglen/nCoV_tracker", "https://github.com/thomglen/nCoV_tracker"),
                            tags$br()
                            
                            

                          )
                 )
                 
)


### SHINY SERVER ###

server = function(input, output, session) { 
   
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")