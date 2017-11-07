# Multiple crosstable and chi square
library(desc)

chichi = master %>% select_if(is.factor) %>% map(~CrossTable(.,master$REBLEED,chisq=TRUE))

print(chichi)

didi = master %>% select_if(is.factor) %>% map(~CrossTable(.,master$DEATH,chisq=TRUE))

sink("chi.txt") # print long output to file inwd
chichi
sink() # go back to terminal output

sink("di.txt") # print long output to file inwd
didi
sink() # g

?xtabs

library(tidyverse)

master =master %>% 
  # got stuck at spaces so changed all col name white space to _ 
  
  # Bulk change 
  nn = names(master)

library(stringr)
patt = " "

mm = str_replace_all(nn,patt,"_")
mm

names(master)= mm

names(master)


allcomb %>%
  purrr::pmap(.f = test_wrapper, tbl=master)


master= master %>% mutate(outcome_31 = case_when(
  is.na(outcome_30)~"a",
  outcome_30=="d"~"d",
  TRUE~"a"))


summary(master$outcome_31)



master$outcome_31= factor(master$outcome_31,levels=c("a","d"))
summary(master$outcome_31)

master = master %>% mutate(outcome_30=outcome_31)
master$outcome_31<- NULL








summary(master)


