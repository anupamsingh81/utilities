logitloess <- function(x, y, s) {
  
  col1 = deparse(substitute(x))
  col2 = gsub(col1,"hematemesis")
  logit <- function(pr) {
    log(pr/(1-pr))
  }
  
  if (missing(s)) {
    locspan <- 0.7
  } else {
    locspan <- s
  }
  
  loessfit <- predict(loess(y~x,span=locspan))
  pi <- pmax(pmin(loessfit,0.9999),0.0001)
  logitfitted <- logit(pi)
  
  plot(x, logitfitted, ylab="logit",xlab=col1)
  
}

summary(hematemesis$REBLEED.EPISODE1)
hematemesis$REBLEED = ifelse(hematemesis$REBLEED.EPISODE==1,0,1)
summary(hematemesis$REBLEED)
library(tidyverse)

hematemesis %>% select_if(is.numeric) %>% select(-c(REBLEED)) %>% map(~logitloess(.,hematemesis$REBLEED))
                                              
logitloess(hematemesis$lactate,hematemesis$REBLEED)

hematemesis %>% select_if(is.numeric) %>% select(-c(REBLEED)) %>% names()

