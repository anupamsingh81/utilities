library(tableone)

master=hematemesis
# Extract Factor
fac =master %>% select_if(is.factor) %>% names()

str(fac)

fac[2]

# Extract numeric
num =master %>% select_if(is.numeric) %>% names()

str(num)
master$diagnoses = ifelse(master$CKD ==1,"CKD",
                          ifelse(master$HRS==1,"HRS",
                                 ifelse(master$`Intrinsic renal`==1,"iAKI", 
                                        ifelse(master$Prerenal==1,"Prerenal", "Control"))))      

master$diagnoses = factor(master$diagnoses, levels = c("Control","CKD","Prerenal","HRS","iAKI"))

str(master$diagnoses)

summary(master$diagnoses)

vars = c(num,fac)
vars

library(tableone)
tab2 <- CreateTableOne(vars = vars, strata = "REBLEED" , data = master)

#Error due to bad name in table one

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

num =master %>% select_if(is.numeric) %>% names()

fac =master %>% select_if(is.factor) %>% names()

vars = c(num,fac)
vars
num
fac
library(tableone)
tab2 <- CreateTableOne(vars = vars, strata = "REBLEED" , data = master)
# stuck at pulse
master= master %>% rename(Pulse=`PULSE>100`)

num =master %>% select_if(is.numeric) %>% names()
num
fac =master %>% select_if(is.factor) %>% names()

vars = c(num,fac)

tab2 <- CreateTableOne(vars = vars, strata = "DEATH" , data = master)
print(tab2,quote = TRUE,noSpaces = TRUE)





master$
  summary(master)



num
tab2

print(tab2,quote = TRUE,noSpaces = TRUE)

