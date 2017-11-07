library(magick)
getwd()

# http://www.sthda.com/english/articles/2-r/6-create-an-animated-gif-image-with-r-and-imagemagick/
# simple command to make gifs with magick 
?system

?

# set a separate working directory
# first write a custom function for ggplot
xk = function(d){
  
  m1= k4%>% filter(time==d)
  p+ geom_point(data=m1,aes(x=lo,y=la,size=pollx,color=name)) +labs(title=d) 
}

# first generate 100 ggplot images ina separate file like rplot1,rplot2etc
# animate ( requirement no pacakge, but imagemagick hass to be installed on system)

summary(k4$pollx)
getwd()

setwd("/home/anupam/Documents/rmaps/multi")

aa = c(1:100)
# check if function works,here it is xk
xk(1)
plots <-  map(aa,xk)
paths <- stringr::str_c("Rplot",1:length(plots), ".png")
#produce plots

pwalk(list(paths, plots), ggsave, path = getwd())
# save plots into animation with imagemagick

system("convert -delay 100 *.png pollution.gif")

# 1 image is 100 kb and 100 images, gif=9.1MB  right

# 1,2,3,4,is nt appropriate always append extra name to file other 7 will come in between 69 n 70 eg. 69,7,70 so better 7,71


#https://stackoverflow.com/questions/29966582/ggplot-geom-point-with-colors-based-on-specific-discrete-values


library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) + 
  geom_point(aes(colour = cut(qsec, c(-Inf, 17, 19, Inf))),
             size = 5) +
  scale_color_manual(name = "qsec",
                     values = c("(-Inf,17]" = "black",
                                "(17,19]" = "yellow",
                                "(19, Inf]" = "red"),
                     labels = c("<= 17", "17 < qsec <= 19", "> 19"))



xx =k4 %>% filter(time==5) 

p+ geom_point(data=xx,aes(x=lo,y=la,color=code,size=6))+scale_colour_manual(values = cols)
xx
library(ggrepel)

# Better plot

# First factor code your continuous variable in data frame
k4= k4 %>% mutate(code= case_when(
  pollx<100 ~ "low",
  pollx>100&pollx<150 ~"moderate",
  pollx>150&pollx<200 ~"high",
  pollx>200 ~"dangerous",
  TRUE ~ "NA"
))

# create color code
cols <- c("dangerous" = "red", "low" = "green", "high" = "yellow", "moderate" = "orange","NA"="white")
label1 = c(">200","<100","150-200","100-150","NA")
# can change labels also



p1= p + 
  geom_point(data=xx,aes(x=lo,y=la,color=code,size=6))+scale_colour_manual(name="pollution",values = cols)+
  geom_text_repel(data=xx,aes(x=lo,y=la,label=name))+
  annotate("text",x=77.1,y=28.6,label="DELHI")

# in this size is seen in legend to remove it place it out of aes
#https://stackoverflow.com/questions/11714951/remove-extra-legends-in-ggplot2

xx =k4 %>% filter(time==96)
p + 
  geom_point(data=xx,aes(x=lo,y=la,color=code),size=6)+scale_colour_manual(name="pollution",values = cols)+
  geom_text_repel(data=xx,aes(x=lo,y=la,label=name))+
  annotate("text",x=77.1,y=28.6,label="DELHI")

xx
xx

label1 = c(">200", "<100", "100-150","150-200",">300")
  p+geom_point(data=xx,aes(x=lo,y=la,colour=pollx))+
scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),values=c(100,150,200,250,300,350))
    #http://ggplot2.tidyverse.org/reference/annotate.html)

getwd()

?cut
setwd("/home/anupam/Documents/rmaps")

  
