library(ggplot2)
library(shiny)
library(DT)
library(tidyverse)
library(lubridate)
library(maps)
library(rmarkdown)

source("visualize.R")
source("HT.R")
load("sites_data.RData")

sites<- read.csv("Data/Sites.csv")

shinyServer(function(input,output){
  #getting data for chosen sites and chosen quantity
  chosen_data <- reactive({
    selected_site_data(input$selected_sites,input$variable)
  })
  
  #-------------------------------------------VISUALIZE DATA-------------------------------------
  #Display the graph
  output$ui <- renderUI({
    if (input$agg_type=="Monthly Averages")
      output <- plotOutput("monthly")
    
    if (input$agg_type=="Raw Hourly Data")
      output <- plotOutput("raw_hour_dat")
    if(input$agg_type=="Daily Averages")
      output <- plotOutput("daily_avg_out")
    if(input$agg_type=="Daily Maxima")
      output <- plotOutput("daily_max_out")
    if(input$agg_type=="Daily Minima")
      output <- plotOutput("daily_min_out")
    
    output

  })
  
  #---------------------visualize for Monthly Average
  output$monthly <- renderPlot({
    monthly_plot(input$selected_sites,input$variable)
    
  })
  #---------------------visualize for raw_hour_data
  output$raw_hour_dat <- renderPlot({
    hourly_plot(input$selected_sites,input$variable)
    
  })
  
  #---------------------visualize for daily data
  output$daily_avg_out <- renderPlot({
    daily_average(input$selected_sites,input$variable,input$time_type)
  })
  
  #---------------------visualize for daily min
  output$daily_min_out <- renderPlot({
    daily_min(input$selected_sites,input$variable,input$time_type)
  })
  
  
  #---------------------visualize for daily max
  output$daily_max_out <- renderPlot({
    daily_max(input$selected_sites,input$variable,input$time_type)
  })
  
  #Display last 7 rows of daily average data of each site
  summary_data <- reactive({
    data <- selected_site_data(input$selected_sites,input$variable)
    data <- data %>% group_by(Site_Name,d_date) %>% summarise(mean_measure=round(mean(Measure),1))
    data<- data %>% group_by(Site_Name) %>% do(tail(.,7))
    colnames(data) <- c("Site_Name","Date",paste("Mean",input$variable))
    data
  })
  
  output$table1 <- renderDT({
    summary_data()
  },rownames=FALSE,options=list(columnDefs=list(list(className='dt-center',targets=0:2)),pageLength=7,lengthMenu=c(5,7,10,15,20)))
  
  #Display Hutton data
  output$t <- renderText({
    paste("Hutton Days   Site:",input$HT_site)
    
  })
  
  ht_data <- reactive({
    HT_days(input$HT_site,input$m_month)
  })
  
  output$table2 <- renderDT({
    ht_data()
  },rownames=FALSE,options=list(columnDefs=list(list(className='dt-center',targets=0:1)),pageLength=7,lengthMenu=c(5,7,10,15,20)))
  
  
  #------------------------------------MAP-SITE LOCATION---------------------------------
    #show sites location and map
  output$map_plot <- renderPlot({
  maps::map("world","UK")
      points(sites$Longitude,sites$Latitude,pch=16,cex=1,col="red")+
        text(sites$Longitude,sites$Latitude,sites$Site_ID,col="blue",cex=0.8,pos=1)
    })
  output$sites_info <- renderTable({
    sites%>% select(Site_Name,Latitude,Longitude)
  })
  
  #-----------------------------------GENERATE REPORT------------------------------------
  output$report <- downloadHandler(
    filename = "report.docx",
    content=function(file){
      render("report.Rmd",output_format="word_document",
                        output_file=file,
                        params=list(measure=input$variable,
                                    sites=input$selected_sites,
                                    agg_type=input$agg_type,
                                    time_type=input$time_type,
                                    ht_site=input$HT_site,
                                    m_month=input$m_month)
                                                                )
    }
  )
  
  
})
