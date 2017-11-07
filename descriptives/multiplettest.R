# save multiple plots at once

library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())

# map2 2 arguments for t test ( Multiple t test)

# write function

sum_wrapper <- function (tbl=NULL, var1=NULL, var2=NULL) {
  tbl_pair <- tbl %>%
    select_(var1, var2)
  x <- tbl_pair %>% pull(var1)
  y <- tbl_pair %>% pull(var2)
  sum = x+y
  sum
}

#check
sum_wrapper(var1="HB",var2="BUN",tbl=master)



# execute
allcomb_var <- expand.grid(var1 = c("HB","lactate"), var2 = c("BUN"),
                           stringsAsFactors = FALSE)

allcomb_var %>% 
  purrr::pmap(.f = sum_wrapper, tbl=master)

# ttest - multiple

test_wrapper <- function (tbl=NULL, var1=NULL, var2=NULL) {
  tbl_pair <- tbl %>%
    select_(var1, var2)
  x <- tbl_pair %>% pull(var1)
  y <- tbl_pair %>% pull(var2)
  f=t.test(x~y) # there was fault if no formula mode
  g= colnames(tbl_pair) # get col_names
  h=list(g,f)
  h
}

#check
colnames(master$Name)

test_wrapper(var1="HB", var2 = "DEATH",tbl=master) 


master$REBLEED=as.factor(master$REBLEED)
summary(master$REBLEED.EPISODE)
num
allcomb <- expand.grid(var1 = num, var2 = c("DEATH","REBLEED"),
                       stringsAsFactors = FALSE)

allcomb %>% purrr::pmap(.f = test_wrapper, tbl=master)

