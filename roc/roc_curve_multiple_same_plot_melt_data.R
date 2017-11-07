
# ROC CURVE
#https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html
# 
library(tidyverse)
library(plotROC)

master_roc = master2 %>% select(smear,xpert,culture,specimen)

table(master_roc$xpert,master_roc$culture)
table(master_roc$smear,master_roc$culture)

test = master_roc %>% mutate( microscopy = ifelse(smear=="Negative",0,1), gene_xpert = ifelse(xpert=="Negative",0,1), afb_culture = ifelse(culture=="Negative",0,1)) %>% 
  select(microscopy,gene_xpert,afb_culture,specimen)

table(test$gene_xpert,test$afb_culture)
  
head(test)
longtest4 <- melt_roc(test4, "afb_culture", c("microscopy", "gene_xpert"))
head(longtest2)


roc1 =ggplot(longtest1, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()
roc2 =ggplot(longtest2, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()
roc3 = ggplot(longtest3, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()
roc4 = ggplot(longtest4, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()

library(gridExtra)

grid.arrange(roc1,roc2,roc3,roc4)
library(pROC)
smear_roc = roc(test4$afb_culture,test4$microscopy ,data=test4)

xpert_roc = roc(test4$afb_culture,test4$gene_xpert ,data=test4)

xpert_roc$specificities[2]
xpert_roc$sensitivities[2]
smear_roc$specificities[2]
smear_roc$sensitivities[2]

table(test4$gene_xpert,test4$afb_culture)

test1 = test %>% filter(specimen=="BAL")
test2 = test %>% filter(specimen=="Sputum")
test3 = test %>% filter(specimen=="Pleural")

test4= test %>% filter(specimen=="Others")

roc.test(smear_roc,xpert_roc)


plot(smear_roc,col="red")
plot(xpert_roc,add=TRUE,col="blue")

legend('bottomright', names(dat)[c(3:4)] , 
       lty=1, col=c('red', 'blue'),  cex=.75)


title(main =" Comparison Of Area under Curves of gene-xpert and smear microscopy",line = 3.0)


table(dat$smear,dat$culture)

table(dat$xpert,dat$culture)


table(dat$smear,dat$xpert,dat$culture)


