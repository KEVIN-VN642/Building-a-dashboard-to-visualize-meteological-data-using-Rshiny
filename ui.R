#create directory to read a file

t<- read.csv("Data/Sites.csv")
site_list <- t$Site_Name #get site names

vars <- c("wind_speed","air_temperature","rltv_hum","visibility")
aggs <- c("Raw Hourly Data","Daily Averages","Monthly Averages",
         "Daily Maxima","Daily Minima")
Months<- c(1:12)
library(shiny)
fluidPage(
  column(12,offset=4,titlePanel(h2("Visualization Meteorological Data in UK in 2020"))),
  column(2,
         wellPanel(
           h4("Visualize Data"),
           selectizeInput("selected_sites","Select sites to plot",choices=site_list,
                          selected="Sumburgh",multiple=TRUE,size=8,options=list(maxItems=5)),
           radioButtons("variable","Reported Variable",selected="air_temperature",choices=vars),
           radioButtons("agg_type","Aggregation Type",selected="Monthly Averages",choices=aggs),
           radioButtons("time_type","Time Handling",selected="Calenda Time",
                        choices=c("Calenda Time","Day in Week","Hour in Day"))
         ),
         wellPanel(
           selectizeInput("HT_site","Hutton: Select a Site",choices=site_list,selected="Boulmer"),
           selectizeInput("m_month","Select Month",choices=Months,selected=6)
         ),
         wellPanel(downloadButton("report","Download Report",icon="Download"))
         
         ),
  column(10,
         tabsetPanel(
           tabPanel("Data Visualization",
                    uiOutput("ui"),
                    fluidRow(h3("      ")),
                    column(5,h4("Summary Daily Average Data"),
                           DT::DTOutput("table1")),
                    
                    column(4,offset = 2, textOutput("t"),
                           tags$head(tags$style("#t{font-size:18px}")),
                           DT::DTOutput("table2")
                           )
           ),
           tabPanel("Sites Location",
                    fluidRow(
                      column(6,plotOutput("map_plot")),
                      column(6,tableOutput("sites_info"))
                    )
                    
                )
            )
         
         )

)
  