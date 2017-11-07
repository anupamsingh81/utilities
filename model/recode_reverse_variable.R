
library(stringr)
# https://stackoverflow.com/questions/38088328/how-to-recode-and-revers-code-variables-in-columns-with-dplyr
# https://stackoverflow.com/questions/38809509/recode-and-mutate-all-in-dplyr

h1 =hematemesis %>% select(contains("n=1")) %>% map(~as.factor(.)) %>% as.data.frame() %>% add_rownames(var ="patient")

i1 = hematemesis %>% select(-contains("n=1")) %>%  add_rownames(var ="patient")

l1 =inner_join(h1,i1)

summary(l1)


cor.test(hematemesis$`abg- bicarbonate`,hematemesis$lactate)

summary(as.factor(hematemesis$`DIAGNOSIS`))

carcinoma = c("oma","can","hcc","ca gb","malignancy")
duodenum = c()

#https://stackoverflow.com/questions/26659198/detect-multiple-strings-with-dplyr-and-stringr
hematemesis$Etiology = ifelse(str_detect(hematemesis$DIAGNOSIS,"cld"),"Cirrhosis",
                             ifelse(str_detect(hematemesis$DIAGNOSIS,paste(carcinoma,collapse = '|')),"Neoplasia",
                        ifelse(str_detect(hematemesis$DIAGNOSIS,"weiss"),"Mallory_Weiss",
                           ifelse(str_detect(hematemesis$DIAGNOSIS,"duo"),"Duodenal",  
                                  ifelse(str_detect(hematemesis$DIAGNOSIS,"esophagitis"),"Esophagitis",  
                                         "Others")))))
hematemesis$Cause = ifelse(str_detect(hematemesis$DIAGNOSIS,"cld"),"Cirrhosis",
                              ifelse(str_detect(hematemesis$DIAGNOSIS,paste(carcinoma,collapse = '|')),"Neoplasia",
                                     ifelse(str_detect(hematemesis$DIAGNOSIS,"weiss"),"Mallory_Weiss",  
                                                          "Others")))

summary(as.factor(hematemesis$Etiology))

summary(as.factor(hematemesis$Cause))
