hematemesis%>% select_if(is.factor) %>% map(~chisq.test(.,hematemesis$DEATH)$p.value) %>% as.data.frame %>% gather %>% 
  mutate(signf= ifelse(value<0.05,"significant","non-significant")) %>% 
  ggplot(aes(x=reorder(key,value),y=value))+
  geom_point(aes(colour=signf))+
  coord_flip()+ylab("p value")+xlab("Factor")

hematemesis %>% select_if(is.numeric) %>% map(~t.test(.~hematemesis$REBLEED.EPISODE)$p.value) %>% as.data.frame %>% gather %>% 
  mutate(signf= ifelse(value<0.05,"significant","non-significant")) %>% 
  ggplot(aes(x=reorder(key,value),y=value))+
  geom_point(aes(colour=signf))+
  coord_flip()+ylab("p value")+xlab("Factor")


