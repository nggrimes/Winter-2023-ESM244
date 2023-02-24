library(tidyverse)

palmetto<-read_csv(here::here("assignments","assignment-2","palmetto.csv")) %>% 
  select(height,length,width,green_lvs,species)

palm<-palmetto %>% 
  mutate(name=ifelse(species==2,"Sabal etonia","Serenoa repens")) %>% 
  mutate(species=as.factor(species))

ggplot(data=palm)+
  geom_point(aes(x=height,y=length,color=name))

ggplot(data=palm)+
  geom_boxplot(aes(x=name,y=height))+
  theme_classic()+
  labs(y="Height (cm)",x="Palmetto Species")

ggplot(data=palm,aes(x=name,y=height))+
  geom_violin(fill='white',linewidth=3)+
  geom_jitter(aes(color=name),alpha=0.2,width=0.1)+
  theme_classic()
  
