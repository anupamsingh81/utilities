library(tidyverse)
library(gganimate)

life= na.omit(lifex)


names(life[4])= "health_expenditure_dollars" # change name
 
life=life %>% rename( health_expenditure_dollars = `Health expenditure (2010 int-$) (2010 int-$)`, population = `Total population (Gapminder)`)

summary(as.factor(life$Entity))

life=life %>% mutate(dollar_exp_ratio = health_expenditure_dollars/`Life expectancy at birth (years)`)

life=life %>% rename(life_expectancy=`Life expectancy at birth (years)`)

life %>% group_by(Entity,Year,dollar_exp_ratio) %>% summarise() %>% arrange(desc(dollar_exp_ratio))

p3 <- ggplot(life, aes(y=life_expectancy,x= health_expenditure_dollars, frame = Year)) +geom_path(aes(cumulative = TRUE, group = as.factor(Entity))) +scale_x_log10() +facet_wrap(~as.factor(Entity))

gganimate(p3)

life=life %>% mutate(country= as.factor(Entity))
ggplot(life, aes(y=life_expectancy,x= health_expenditure_dollars,color=country))+geom_line() +facet~wrap(~year)

p2 <- ggplot(life, aes(x=health_expenditure_dollars,y=life_expectancy)) +
  
  geom_point(aes(frame = Year,color = country) ) +
  scale_x_log10()

gganimate(p2)

p3= ggplot(life, aes(x=health_expenditure_dollars,y=life_expectancy,frame=Year)) +
  
  geom_path(aes(cumulative=TRUE,group = country,color=country) ) +
  scale_x_log10()

gganimate(p3)

gganimate(p3, "output.gif")
