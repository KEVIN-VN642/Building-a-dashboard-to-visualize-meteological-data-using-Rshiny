
load("sites_data.RData")  #load sites_data
selected_site_data <- function(selected_sites,measure){
  validate(need(selected_sites !="","Please select one to five sites")) #prevent errors when no sites selected
  dat <- sites_data %>% filter(Site_Name %in% selected_sites)
  
  names(dat)[names(dat)==measure] <-"Measure" # Name the chosen quantiy 
  #(temperature, humidity..) by common name Measure
  dat
}

#function support to draw plot from daily average data
daily_average <- function(selected_sites,measure,time_type){
  #'selected_sites: sites need visualize
  #'measure: quantity needs to visualize
  #'time_type: which time option is selected
  data <- selected_site_data(selected_sites,measure)
  data <- data %>% group_by(d_date,Site_Name) %>% summarize(mean_measure=mean(Measure,na.rm=TRUE))
  data["mean_measure"] <- lapply(data["mean_measure"], function(x) round(x,1))
  
  if ((time_type=="Calenda Time") | (time_type=="Hour in Day")){
    #For daily average, option Hour in Day make no sense then we keep it the same with Calenda Time
    
    plt <- ggplot(data)+geom_line(aes(d_date,mean_measure,col=Site_Name),size=1,alpha=0.5)+
      labs(x="Date",y=paste("Daily Average",measure),title =paste("Daily Average",measure))
    
  }
  
  if (time_type=="Day in Week"){
    plt <- ggplot(data)+geom_point(aes(wday(as.Date(d_date),label=TRUE),mean_measure,col=Site_Name),size=5,alpha=0.5,
                                   position=position_jitter(w=0.07,h=0.05))+
      #position_jitter: to support visualize overlapping points
      labs(x="Day",y=paste("Daily Average",measure),title =paste("Daily Average",measure))
    
  }
  plt+theme(axis.title = element_text(size = 25,face="bold"),axis.text = element_text(size=20),
            plot.title = element_text(size=32,face="bold"),legend.text = element_text(size=20),
            legend.title = element_text(size=25,face="bold"))
  
} 


daily_min <- function(selected_sites,measure,time_type){
  #'selected_sites: sites need visualize
  #'measure: quantity needs to visualize
  #'time_type: which time option is selected
  data <- selected_site_data(selected_sites,measure)
  g <- data %>% group_by(d_date,Site_Name) %>% summarize(mean_measure=min(Measure,na.rm=TRUE))
  data <- data %>% left_join(g) %>% subset(mean_measure==Measure)
  
  
  if ((time_type=="Calenda Time") | (time_type=="Day in Week")){
    
    plt <- ggplot(data)+geom_line(aes(d_date,mean_measure,col=Site_Name),size=1,alpha=0.5)+
      labs(x="Date",y=paste("Daily Min",measure),title =paste("Daily Min",measure))
    
  }
  
  if (time_type=="Day in Week"){
    plt <- ggplot(data)+geom_count(aes(wday(as.Date(d_date),label=TRUE),as.integer(mean_measure),col=Site_Name),alpha=0.5,
                                   position=position_jitter(w=0.07,h=0.05))+
      #position_jitter: to support visualize overlapping points
      labs(x="Day",y=paste("Daily Min",measure),title =paste("Daily Min",measure),size="Count")
    
  }
  
  if (time_type=="Hour in Day"){
    plt <- ggplot(data)+geom_count(aes(h_hour,as.integer(mean_measure),col=Site_Name),alpha=0.5,
                                   position=position_jitter(w=0.07,h=0.05))+
      #position_jitter: to support visualize overlapping points
      labs(x="Hour",y=paste("Daily Min",measure),title =paste("Daily Min",measure),size="Count")
    
  }
  
  plt+theme(axis.title = element_text(size = 25,face="bold"),axis.text = element_text(size=20),
            plot.title = element_text(size=32,face="bold"),legend.text = element_text(size=20),
            legend.title = element_text(size=25,face="bold"))
} 




daily_max <- function(selected_sites,measure,time_type){
  #'selected_sites: sites need visualize
  #'measure: quantity needs to visualize
  #'time_type: which time option is selected
  data <- selected_site_data(selected_sites,measure)
  g <- data %>% group_by(d_date,Site_Name) %>% summarize(mean_measure=max(Measure,na.rm=TRUE))
  data <- data %>% left_join(g) %>% subset(mean_measure==Measure)
  
  
  if ((time_type=="Calenda Time") | (time_type=="Day in Week")){
    
    plt <- ggplot(data)+geom_line(aes(d_date,mean_measure,col=Site_Name),size=1,alpha=0.5)+
      labs(x="Date",y=paste("Daily Max",measure),title =paste("Daily Max",measure))
    
  }
  
  if (time_type=="Day in Week"){
    plt <- ggplot(data)+geom_count(aes(wday(as.Date(d_date),label=TRUE),as.integer(mean_measure),col=Site_Name),alpha=0.5,
                                   position=position_jitter(w=0.07,h=0.05))+
      #position_jitter: to support visualize overlapping points
      labs(x="Day",y=paste("Daily Max",measure),title =paste("Daily Max",measure),size="Count")
    
  }
  
  if (time_type=="Hour in Day"){
    plt <- ggplot(data)+geom_count(aes(h_hour,as.integer(mean_measure),col=Site_Name),alpha=0.5,
                                   position=position_jitter(w=0.07,h=0.05))+
      labs(x="Hour",y=paste("Daily Max",measure),title =paste("Daily Max",measure),size="Count")
    
  }
  
  plt+theme(axis.title = element_text(size = 25,face="bold"),axis.text = element_text(size=20),
            plot.title = element_text(size=32,face="bold"),legend.text = element_text(size=20),
            legend.title = element_text(size=25,face="bold"))
} 



monthly_plot <- function(selected_sites,measure){
  
  #'selected_sites: sites needs to visualize
  #'measure: parameter needs to visualize
  #'monthly_plot: plot monthly data from selected measure (take monthly average)
  data<- selected_site_data(selected_sites,measure)
  #data <- data %>% mutate(X_Date=m.months)
  data <- data %>% group_by(m_month,Site_Name) %>% summarize(mean_measure=mean(Measure,na.rm=TRUE))
  #data["mean_measure"] <- lapply(data["mean_measure"], function(x) round(x,1))
  
  
  ggplot(data)+geom_line(aes(m_month,mean_measure,col=Site_Name),size=1,inherit.aes = FALSE)+
    scale_x_continuous(breaks = 1:12,labels = c("Jan","Feb","Mar","Apr","May",
                                                "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
    labs(x="Month",y=paste("Monthly",measure))+
    ggtitle(paste("Monthly Average",measure))+
    theme(axis.title = element_text(size = 25,face="bold"),axis.text = element_text(size=20),
          plot.title = element_text(size=32,face="bold"),legend.text = element_text(size=20),
          legend.title = element_text(size=25,face="bold"))
}


hourly_plot <- function(selected_sites,measure){
  #'selected_sites: sites needs to visualize
  #'measure: parameter need to visualize
  #'hourly_plot: plot hourly data from selected measure
  data <- selected_site_data(selected_sites,measure)
  ggplot(data)+geom_count(aes(h_hour,as.integer(Measure),col=Site_Name),alpha=0.5,
                          position=position_jitter(w=0.12,h=0.07))+
    #position_jitter: to support visualize overlapping points
    labs(x="Hour",y=paste("Hourly",measure),size="Count")+ggtitle(paste("Hourly",measure))+
    theme(axis.title = element_text(size = 25,face="bold"),axis.text = element_text(size=20),
          plot.title = element_text(size=32,face="bold"),legend.text = element_text(size=20),
          legend.title = element_text(size=25,face="bold"))
}

