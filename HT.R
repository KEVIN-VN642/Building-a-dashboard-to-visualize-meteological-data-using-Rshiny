load("sites_data.RData")
HT <- function(x){
  #'@HT function calculate vector z which are sum of two consecutive elements of 
  #'vector x to determine whether or not two consecutive days of variable x 
  #'meet Hutton Criteria, after that shift index of elements of z one position
  #'In later step (not in this function), which elements of z equal to 2 will 
  #'be the date that variable x meet Hutton
  #'x=(x1,x2,x3,x4,x5) then HT(x)=(0,x1,x1+x2,x2+x3,x3+x4)
  n<- length(x)
  y<-x
  y[2:n]<- x[1:n-1]
  y[1]=0
  z<- x+y
  z[2:n]<-z[1:n-1]
  z[1]<-0
  z
}

HT_days <- function(site,mmonth){
  #'site: site_name needs to analyze Hutton
  #'mmonth: month needs to analyze Hutton
  #'HT_days: return the dataframe containing dates for Hutton
  data <- sites_data %>% filter(Site_Name == site & m_month==mmonth)
  data <- data %>% group_by(d_date) %>% summarise(temp=min(air_temperature)>=10,humi=sum(rltv_hum>=90)>=6)
  data <- data %>% mutate(ht_temp=HT(temp),ht_humi=HT(humi)) %>% filter(ht_temp==2 & ht_humi==2)
  data["Site_Name"]<- site
  data <- data %>% select(Site_Name,d_date)
  colnames(data) <- c("Site_Name","Hutton_Date")
  data
  
}