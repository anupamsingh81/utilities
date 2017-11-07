

# dca

source("dca.r")

dca(data=master_rock2,outcome="diagnoses",predictors = "ngalpred")



a1= glm(diagnoses~NGAL,family=binomial(),data=master_rock2)
a1$fitted.values

master_rock2$ngalpred = predict(a1,type="response")

b1= glm(diagnoses~feNa,family=binomial(),data=master_rock2)

master_rock2$feNapred = predict(b1,type="response")


c1= glm(diagnoses~urine_na,family=binomial(),data=master_rock2)

master_rock2$urinenapred = predict(c1,type="response")
dca(data=master_rock2,outcome="diagnoses",predictors = c("feNapred","ngalpred","urinenapred"))


# master_roc3
dca(data=master_rock2,outcome="diagnoses",predictors = "ngalpred")



a1= glm(diagnoses~NGAL,family=binomial(),data=master_rock3)


master_rock3$ngalpred = predict(a1,type="response")

b1= glm(diagnoses~feNa,family=binomial(),data=master_rock3)

master_rock3$feNapred = predict(b1,type="response")


c1= glm(diagnoses~urine_na,family=binomial(),data=master_rock3)

master_rock3$urinenapred = predict(c1,type="response")
dca(data=master_rock3,outcome="diagnoses",predictors = c("feNapred","ngalpred","urinenapred"))


                               
