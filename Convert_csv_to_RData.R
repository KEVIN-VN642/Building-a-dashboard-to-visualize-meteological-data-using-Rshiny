library(tidyverse)

sites <- read.csv("Data/Sites.csv")
head(sites)

first_site <- read.csv(paste("Data/Site_",sites$Site_ID[1],".csv",sep=""))
first_site[6:7] <- lapply(first_site[6:7],as.numeric) #ensure consistent format for temp and humidiy
first_site <- first_site %>% mutate(d_date=as.Date(paste(2020,month,day,sep="-","%Y-%m-%d"))) #ensure all sites 
#have same time format
sites_data<- first_site #data will combine all sites


for (i in 2:length(sites$Site_ID)){
  s <- read.csv(paste("Data/Site_",sites$Site_ID[i],".csv",sep=""))
  s[6:7] <- lapply(s[6:7],as.numeric)
  s <- s %>% mutate(d_date=as.Date(paste(2020,month,day,sep="-","%Y-%m-%d")))
  sites_data <- rbind(sites_data,s)
}

sites_data <- sites_data %>% left_join(sites,by=c("Site"="Site_ID"))


#change names of columns to avoid potential conflict with times related function
names(sites_data)[names(sites_data)=="hour"] <- "h_hour"
names(sites_data)[names(sites_data)=="month"] <- "m_month"
names(sites_data)[names(sites_data)=="day"] <- "d_day"

sites_data <- sites_data %>% mutate(Date_label=wday(as.Date(d_date),label = TRUE))
head(sites_data)
save(sites_data,file="sites_data.RData")
