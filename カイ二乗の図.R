n<-100000
chi.x<-matrix(nrow=n,ncol=30)
for(i in 1:30){
  chi.x[,i]<-rchisq(n=n,df=i) #自由度iのカイ二乗分布からn個の乱数発生させる
}


chig <-as.data.frame(chi.x) %>% 
  select(V1,V3,V5,V10,V15,V20) %>% 
  rename("df=1" = V1,
         "df=3" = V3,
         "df=5" = V5,
         "df=10" = V10,
         "df=15" = V15,
         "df=20" = V20)

chig1 <- cbind(ID = 1:nrow(chig), chig)

chig2 <- chig1 %>% 
  pivot_longer(-ID) %>% 
  mutate(name = factor(name,
                       levels=c("df=1","df=3","df=5",
                                "df=10","df=15","df=20")))

chi2g <- chig2 %>% 
  ggplot(aes(value)) +
  geom_density() + 
  geom_hline(yintercept = 0.05 , color = "red",  linetype="dashed") + 
  theme_bw(base_size = 12) + 
  facet_wrap(~name) + 
  ylim(0,1) + 
  labs(x = expression(paste ({χ^2}, "-value" ,sep="") ),
       y = "帰無仮説が支持される確率")
chi2g

ggsave("カイ2分布.png",chi2g, dpi=300, width = 10, height = 5)





