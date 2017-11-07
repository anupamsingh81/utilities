# ROC

library(pROC)
library(OptimalCutpoints)

unglROC = ungl %>% filter(diagnoses=="iAKI"| diagnoses=="HRS")
unglROC1 = ungl %>% filter(diagnoses=="Prerenal"| diagnoses=="HRS")

unglROC$diagnoses= as.factor(unglROC$diagnoses)

a=  roc(unglROC$diagnoses,unglROC$NGAL)

b=  roc(unglROC$diagnoses,unglROC$feNa)

c=  roc(unglROC$diagnoses,unglROC$Urine.na)

unglnames= ungl %>% select(NGAL,feNa,Urine.na,diagnoses)




plot(a,col="red")
plot(b,add=TRUE,col="blue")
plot(c,add=TRUE,col="green")

legend('bottomright', names(unglnames)[c(1:3)] , 
       lty=1, col=c('red', 'blue', 'green'),  cex=.75)


title(main ="   Biomarkers for  differentiating between HRS and Prerenal ",line = 3.0)

auc(a)
auc(b)
auc(c)
