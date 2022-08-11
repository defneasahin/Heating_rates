library(dplyr)
library(ggplot2)
library(tidyr)


theme_d <- theme_classic() +
  theme(text = element_text(color = "black", size = 14),
        strip.text = element_text(color = "black", size = 14),
        axis.text  = element_text(color = "black", size = 12),
        axis.ticks = element_line(color = "black"),
        legend.text = element_text(size = 12),
        line = element_line(color = "black"),
        plot.background  = element_rect(fill = "white", color = "transparent"),
        panel.background = element_rect(fill = "white", color = "black"),
        strip.background = element_rect(fill = "white", color = "transparent"),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),
        legend.background = element_rect(fill = "white", color = "transparent"),
        legend.key = element_rect(fill = "white", color = "transparent"))



Pver<-read.csv("Pversipora.csv")%>%
  glimpse()
Pdam<-read.csv("Pdamicornis.csv")%>%
  glimpse()

e<-Pdam%>%
  select(Allocation, chla)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(chla), sd= sd(chla), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(Chlorophyll~a,(ug~cm^-2))))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  theme(axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), breaks= seq(0.5, 8, 1), limits = c(0,5.1), position = "right")+
  theme_d

f<-Pdam%>%
  select(Allocation, chla_cell)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(chla_cell), sd= sd(chla_cell), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(Chlorophyll~a,(pg~cell^-1))))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  theme(axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), breaks= seq(1, 8, 2), limits = c(0,6), position = "right")+
  theme_d
g<-Pdam%>%
  select(Allocation, zoox)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(zoox), sd= sd(zoox), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(Symbiont~density,(10^6~cells~cm^-2))))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  theme(axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), breaks= seq(0.2, 2, 0.4), limits = c(0,1.5), position = "right")+
  theme_d

h<-Pdam%>%
  select(Allocation, biomass)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(biomass), sd= sd(biomass), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(Tissue~biomass,(mg~cm^-2))))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  theme(axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), breaks= seq(1, 20, 1), limits = c(0,4.1), position = "right")+
  theme_d

a<-Pver%>%
  select(Allocation, chla)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(chla), sd= sd(chla), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(Chlorophyll~a,(ug~cm^-2))))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  theme(axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), breaks= seq(2, 20, 2), limits = c(0,11.5))+
  theme_d

b<-Pver%>%
  select(Allocation, chla_cell)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(chla_cell), sd= sd(chla_cell), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(Chlorophyll~a,(pg~cell^-1))))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  theme(axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), breaks= seq(0.5, 4, 0.5), limits = c(0,2.3))+
  theme_d


c<-Pver%>%
  select(Allocation, zoox)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(zoox), sd= sd(zoox), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(Symbiont~density,(10^6~cells~cm^-2))))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  theme(axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), breaks= seq(2, 8, 2), limits = c(0,7))+
  theme_d

d<-Pver%>%
  select(Allocation, biomass)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(biomass), sd= sd(biomass), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(Tissue~biomass,(mg~cm^-2))))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  theme(axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), breaks= seq(2, 16, 4), limits = c(0,15))+
  theme_d

cowplot::plot_grid(a,e, b,f,
                   c,g,d,h, ncol = 2)

a<-Pdam%>%
  select(Allocation, P)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(P), sd= sd(P), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(P)))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  scale_y_continuous(expand = c(0, 0), breaks= seq(0.5, 1.5, 0.5), limits = c(0,1.5))+
  theme(axis.title.x = element_blank())+
  theme_d

b<-Pdam%>%
  select(Allocation, P_R)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(P_R), sd= sd(P_R), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(P:R)))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  scale_y_continuous(expand = c(0, 0), breaks= seq(1, 4, 1), limits = c(0,4.5))+
  theme(axis.title.x = element_blank())+
  theme_d

c<-Pdam%>%
  select(Allocation, R)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(R), sd= sd(R), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(R)))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  scale_y_continuous(expand = c(0, 0), breaks= seq(0.2, 0.8, 0.2), limits = c(0,0.7))+
  theme(axis.title.x = element_blank())+
  theme_d

d<-Pver%>%
  select(Allocation, P)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(P), sd= sd(P), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(P)))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  scale_y_continuous(expand = c(0, 0), breaks= seq(0.5, 1.5, 0.5), limits = c(0,1.8),position = "right")+
  theme(axis.title.x = element_blank())+
  theme_d

e<-Pver%>%
  select(Allocation, P_R)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(P_R), sd= sd(P_R), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(P:R)))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  scale_y_continuous(expand = c(0, 0), breaks= seq(1, 4, 1), limits = c(0,4.5),position = "right")+
  theme(axis.title.x = element_blank())+
  theme_d

f<-Pver%>%
  select(Allocation, R)%>%
  drop_na()%>%
  group_by(Allocation)%>%
  dplyr::summarise(meanB = mean(R), sd= sd(R), n = n(), se = sd/sqrt(n))%>%
  ggplot(aes(x=Allocation,y=meanB,fill=Allocation)) +
  ylab(expression(atop(R)))+
  xlab('')+
  geom_errorbar(aes(ymin=meanB-se, ymax=meanB+se), width=0.3,size=0.5)+
  geom_bar(stat = "identity",show.legend=FALSE, color = "black")+
  scale_y_continuous(expand = c(0, 0), breaks= seq(0.2, 0.8, 0.2), limits = c(0,0.7),position = "right")+
  theme(axis.title.x = element_blank())+
  theme_d


cowplot::plot_grid(a,d, c,f,
                   b,e, ncol = 2)

