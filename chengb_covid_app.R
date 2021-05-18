# Load R packages
setwd("C:\\Users\\bchen\\Desktop\\JHU-R-Dashboard")
library(shiny)
library(shinythemes)
library(gridExtra)
library(plotly)


# Define UI
ui <- fluidPage(theme = shinytheme("spacelab"),
                titlePanel(title=div(img(height = 100,
                                         width = 100,
                                         src = "coronavirus_package.png"), "Covid-19 Confirmed Cases Plots")),
                navbarPage(
                  div(img(height = 30,
                          width = 45,
                          src = "chengb_pic.jpg"),"Bo Cheng"),
                  tabPanel("ARIMA(p,d,q) Model")
                  
                  
                ), # navbarPage
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("num", "Forecast Weeks:",min = 1, max = 4,step=1,
                                value=1, round = TRUE)
                    
                  ), # sidebarPanel
                  mainPanel(
                    tabsetPanel(id = "tabs",
                                tabPanel(value = "tab1", title = "Forecast Plot",
                                         fluidRow(plotOutput("plot1")),
                                         verbatimTextOutput("summary")
                                ),
                                tabPanel(value = "tab2", title = "Map Plot",
                                         fluidRow(plotOutput("plot2"))
                                ),
                                tabPanel(value = "tab3", title = "Treemap Plot",
                                         fluidRow(plotlyOutput("plot3"))
                                )
                    )
                  ) # mainPanel
                )
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  dat <- reactive({
    n_days <- input$num*7
    n_days
  })
  # remotes::install_github("joachim-gassen/tidycovid19")
  library(tidycovid19)
  
  library(dplyr)
  
  coronavirus<-download_jhu_csse_covid19_data(cached = TRUE)
  coronavirus<-as.data.frame(coronavirus)
  coronavirus_us <- coronavirus %>% filter(country == "US") %>% select(c("date", "confirmed"))
  p2<-map_covid19(coronavirus, type = "confirmed", region = "North America")
  conf_df <- coronavirus %>% 
    group_by(country) %>%
    summarise(total_confirmed_cases = sum(confirmed)) %>%
    arrange(-total_confirmed_cases) %>%
    mutate(parents = "Global Confirmed Cases") %>%
    ungroup()
  p3<-plot_ly(data = conf_df,
              type= "treemap",
              values = ~total_confirmed_cases,
              labels= ~country,
              parents=  ~parents,
              domain = list(column=0),
              name = "Confirmed",
              textinfo="label+value+percent parent")
  
  library(tidyverse)
  library(fpp2)
  library(zoo)
  library(xts)
  library(fabletools)
  library(scales)
  
  daily_confirmed_us<-xts(coronavirus_us[,-1], order.by = coronavirus_us[,1])
  # names(daily_confirmed_us) <- "confirmed_cases"
  # head(daily_confirmed_us,1)
  # autoplot(daily_confirmed_us)
  # str(daily_confirmed_us)
  
  # us_confirmed_ses<-ses(daily_confirmed_us, alpha = 0.99, h = 7)
  observeEvent(input$tabs, {
    if(input$tabs == "tab1") {
      print("Tab 1 code is run")
    }
    if(input$tabs == "tab2") {
      print("Tab 2 code is run")
    }
    if(input$tabs == "tab3") {
      print("Tab 3 code is run")
    }
    
    output$plot1<-renderPlot({
      us_confirmed_arima<-auto.arima(daily_confirmed_us)
      arima_forecast<-forecast(us_confirmed_arima,dat())
      p1<-autoplot(arima_forecast, main="Confirmed Cases Forecast") + autolayer(us_confirmed_arima$fitted, series="ARIMA Model") +
        ylab("Confirmed Cases") + xlab("Days Since 2020-01-22") + scale_y_continuous(labels = comma)
      grid.arrange(p1, ncol = 1)
    })
    output$summary<- renderPrint({
      us_confirmed_arima<-auto.arima(daily_confirmed_us)
      arima_forecast<-forecast(us_confirmed_arima,dat())
      summary(arima_forecast)
    })
    output$plot2<-renderPlot({
      p2
    })
    output$plot3<-renderPlotly({
      p3
    })
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)