# save multiple box plots at once

library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())


plots= master%>% select_if(is.numeric) %>% names() %>% map(function(y)ggplot(subset(master,!is.na(DEATH)),aes(DEATH))+ geom_boxplot(aes_string(y=y)))
paths <- stringr::str_c("DEATH",1:length(plots), ".png")
pwalk(list(paths, plots), ggsave, path = getwd())

paths
tempdir()

getwd()

length(plots)



# dodge plots(factor plots)


plots2= master %>% select_if(is.factor) %>% map(~ggplot(master, aes(REBLEED, ..count..)) + geom_bar(aes(fill = .), position = "dodge")+ labs(title=.))
paths2 <- stringr::str_c("REBLEED",fac,1:length(plots2), ".png")
pwalk(list(paths2, plots2), ggsave, path = getwd())

