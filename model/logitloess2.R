logitloess <- function(x, y, s) {
  col1 <- deparse(substitute(x))

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

summary(as.factor(hematemesisfinal$REBLEED))

hematemesisfinal$REBLEED = ifelse(hematemesisfinal$REBLEED=="y",1,0)
logitloess(x=hematemesisfinal$Lactate,y=hematemesisfinal$REBLEED)

logitloess(x=hematemesisfinal$GBS.SCORE,y=hematemesisfinal$REBLEED)



logitloess(x=hematemesis$PNED.SCORE,y=as.numeric(hematemesis$REBLEED.EPISODE))




str(hematemesis$REBLEED.EPISODE)


set.seed(12345)
n <- 1000
x <- rnorm(n)
xb <- -2+x^2
pr <- exp(xb)/(1+exp(xb))
y <- 1*(runif(n) < pr)
logitloess(x,y)
