regcoef = function(x,data){
  df1 = data %>% dplyr::select(x,Diabetes)
  #df1
  a = glm(as.factor(REBLEED.EPISODE)~.,family=binomial(),data=df1)
  b= exp(a$coefficient[-1])
  
  d =  exp(confint(a)[-1,]) # removing first row intercept for easy printing numeric
  list(b,d)
}

regcoef1 = function(x){
  df1 = hem2 %>% dplyr::select(x,REBLEED.EPISODE)
  df1
  a = glm(as.factor(REBLEED.EPISODE)~.,family=binomial(),data=df1)
  b= exp(a$coefficient)
  
  d =  exp(confint(a)) # retain intercept for factor for first level
  list(b,d)
}
