library(tidyverse)
library(lubridate)
# change date_time while import

PM25 = PM25 %>% mutate(date_by_week = round_date(Date, unit="week")) # organise by week,requires lubridate


?diff

month(PM25$Date, label = TRUE)
wday(PM25$Date, label = TRUE)

PM25= PM25 %>%  mutate( month= month(PM25$Date, label = TRUE), day= wday(PM25$Date, label = TRUE))

PM25_month= PM25%>% select(month,day,Concentration) %>% group_by(day,month) %>% summarise(num = n(), mean_conc = mean(Concentration)) %>% arrange(desc(mean_conc))

PM25_month %>% ggplot(aes(y=mean_conc,x=reorder(day,mean_conc)))+geom_bar(stat = "identity")

PM25_month %>% ggplot(aes(y=mean_conc,x=day))+geom_bar()


PM25_day = PM25 %>% select(day,Concentration) %>% group_by(day) %>% summarise(num=n(),mean_conc=mean(Concentration))


PM25_day %>% ggplot(aes(y=mean_conc,x=reorder(day,mean_conc),fill=day))+geom_bar(stat = "identity") +coord_flip()+ labs(
  title = " PM2.5 average concentration daywise Anand Vihar",
  subtitle= " PM25 rises on Friday and saturday",
  caption = " data from cpcb.in",
  x= "Day",
  y= "Concentration"
)

PM25_mon= PM25%>% select(month,Concentration) %>% group_by(month) %>% summarise(num = n(), mean_conc = mean(Concentration)) %>% arrange()

PM25_mon %>% ggplot(aes(y=mean_conc,x=reorder(month,mean_conc),fill=month))+geom_bar(stat = "identity") +coord_flip()+ labs(
  title = " PM2.5 average concentration monthwise Anand Vihar",
  subtitle= " PM25 peaks in winters ",
  caption = " data from cpcb.in",
  x= "Month",
  y= "Concentration"
)


PM25_month %>% filter(month=="Mar") %>% ggplot(aes(y=mean_conc,x=day,fill=day))+geom_bar(stat = "identity") +coord_flip()+ labs(
  title = " PM2.5 average concentration daywise Anand Vihar",
  subtitle= " PM25 variation",
  caption = " data from cpcb.in",
  x= "Day",
  y= "Concentration"
)



monther = function(x){
  PM25_month %>% filter(month==x) %>% ggplot(aes(y=mean_conc,x=day,fill=day))+geom_bar(stat = "identity") +coord_flip()+ labs(
    title = x,
    subtitle= " PM25 variation by day",
    caption = " data from cpcb.in",
    x= "Day",
    y= "Concentration"
  )
  
}

monther("Apr")

x1 = levels(PM25_month$month)
            
 x2=  x1 %>% map(.,monther)    
  
  paths <- stringr::str_c(x1, ".png")
  
  pwalk(list(paths, x2), ggsave, path = getwd())
  
  paths
  getwd()
  
  
  monther1 = function(x){
    PM25_month %>% filter(month==x) %>% ggplot(aes(x=reorder(day,mean_conc),y=mean_conc,fill=day))+geom_bar(stat = "identity") +coord_flip()+ labs(
      title = x,
      subtitle= " PM25 variation by day",
      caption = " data from cpcb.in",
      x= "Day",
      y= "Concentration"
    )
    
  }
  
  monther1("Apr")
  
  
  x3=  x1 %>% map(.,monther)    
  
  paths <- stringr::str_c(x1, ".png")
  
  pwalk(list(paths, x3), ggsave, path = getwd())
  
  library(tidyverse)
  # oddeven1
  PM25  %>%filter(Date >= "2015-10-31", Date<= "2015-11-30") %>% ggplot(aes(x=Date,y=Concentration))+geom_line(color="steel blue")+
    geom_vline(xintercept=as.numeric(as.Date("2015-11-11"))) +
 
  labs(
    title= " PM 2.5 variation in October-November 2015 at Bandra ",
    subtitle = "PM 2.5 was rising pre- Diwali in 20015  due to other factors ",
    y = " PM 2.5",
    x = "Date",
    caption = "source cpcb.in"
  )
  # https://stackoverflow.com/questions/5388832/how-to-get-a-vertical-geom-vline-to-an-x-axis-of-class-date
  
  
  #odd even2
  
  PM25  %>%filter(Date >= "2016-04-01", Date<= "2016-05-15") %>% ggplot(aes(x=Date,y=Concentration))+geom_line() 
  
  
  
  
  PM25  %>%filter(Date >= "2016-12-25", Date<= "2017-01-31") %>% ggplot(aes(x=Date,y=Concentration))+geom_line()
  
  
  PM25$windspeed= windspeed$Concentration
  
  summary(is.na(windspeed$Concentration))
  summary(is.na(PM25$Concentration))
  length(unique(windspeed$Date))
  length(unique(PM25$Date))
  
  rm(windspeed)
  
  k2 = left_join(PM25, windspeed, by=c("Date"))
  
  summary(lm(Concentration.x~Concentration.y,data=k2))
  
  k2 = k2 %>% mutate(odd_even = case_when(
    Date >= "2016-01-01" & Date<= "2016-01-16" ~1,
    Date >= "2016-04-15" & Date<= "2016-04-30" ~1,
    TRUE ~ 0))
    
  summary(k2$odd_even)
  
  summary(as.factor(k2$odd_even))
  
  summary(lm(Concentration.x~Concentration.y*odd_even,data=k2))
  
