
# Net Benefit analysis overall

library(rmda)

dat$culture = ifelse(dat$culture=="negative",0,1)


smear.model <- decision_curve(culture1~smear, 
                              
                              data = master2,
                              study.design = "cohort",
                              policy = "opt-in", #default
                              bootstraps = 50)


add_gene_xpert.model <- decision_curve(culture1~smear+xpert, 
                                       
                                       data = master2,
                                       study.design = "cohort",
                                       policy = "opt-in", #default
                                       bootstraps = 50)

xpert.model <- decision_curve(culture1~xpert, 
                              
                              data = master2,
                              study.design = "cohort",
                              policy = "opt-in", #default
                              bootstraps = 50)



plot_decision_curve( list(smear.model, add_gene_xpert.model),
                     curve.names = c("smear microscopy", "smear and gene xpert"), xlim =
                       c(0, 1), legend.position = "bottomright")

plot_decision_curve( list(smear.model, xpert.model),
                     curve.names = c("smear microscopy", "gene xpert"), xlim =
                       c(0, 1), legend.position = "bottomright")


master2 %>% filter(culture=="Negative",ct=="L") %>%  summarize(n=n())

master2$culture1 = ifelse(master2$culture=="Negative",0,1)

