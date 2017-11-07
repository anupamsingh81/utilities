

predictfinal = function(x,y){
construct_download_url <- function(url, format='csv', sheetid = NULL){
  key <- stringr::str_extract(url, '[[:alnum:]_-]{30,}')
  if(is.null(sheetid) & stringr::str_detect(url, 'gid=[[:digit:]]+')){
    sheetid <- as.numeric(stringr::str_extract(stringr::str_extract(url,'gid=[[:digit:]]+'),'[[:digit:]]+'))
  }
  address <- paste0('https://docs.google.com/spreadsheets/export?id=',key,'&format=',format)
  if(!is.null(sheetid)){
    address <- paste0(address, '&gid=', sheetid)
  }
  return(address)
}

gsheet2text <- function(url, format='csv', sheetid = NULL){
  address <- construct_download_url(url=url, format=format, sheetid = sheetid)
  page <- httr::GET(address)
  if(stringr::str_detect(page$headers$`content-type`, stringr::fixed('text/html'))){
    stop("Unable to retrieve document. Is 'share by link' enabled for this sheet?")
  }
  content <- httr::content(page, as='text')
  return(content)
}

predictci =function(k,kp){preds <- predict(k, newdata = kp, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

fit2 <- k$family$linkinv(fit)
upr2 <- k$family$linkinv(upr)
lwr2 <- k$family$linkinv(lwr)

x = paste("The  probability of outcome  within 30 days  in percentage is ",round(100*fit2,digits = 2),"(",round(100*lwr2,digits=2),"-",round(100*upr2,digits=2),")")
x}
url<-"https://docs.google.com/spreadsheets/d/1ZkIWMmDF9bJgn21jrwhy7sQaVSbHJMbUa5Ajetov52o/edit?usp=sharing"

hematemesis1 = read.csv(text=gsheet2text(url, format= 'csv'), stringsAsFactors=FALSE)

k = glm(as.factor(DEATH)~PNED.SCORE+Lactate,data = hematemesis1,family = binomial(link = "logit"))

k1= glm(as.factor(REBLEED)~PNED.SCORE+Lactate,data = hematemesis1,family = binomial(link = "logit"))
dd = predictci(k=k,kp=data.frame(PNED.SCORE=x,Lactate=y))
rebl = predictci(k=k1,kp=data.frame(PNED.SCORE=x,Lactate=y))

a=c(dd,rebl)
a

}



predictfinal(x=4,y=4)

