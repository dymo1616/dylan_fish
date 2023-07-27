#permanova Zfish_ord<-wcmdscale(fish_ord, method="euclidean", eig = FALSE, add = FALSE, x.ret = FALSE)

Yfish_ord<-avgdist(ord_fish_cal,10)

Zfish_ord<-wcmdscale(fish_ord)
Fsh<-cmdscale(fish_ord, k = 2, eig = T)

Fsh_plot<-as.data.frame(Fsh$points)

as.tibble(Fsh_ord)
colnames(Fsh_plot) <- c("axis_1", "axis_2")

as.tibble(Fsh_ord)
Fsh_plot$Site <- rownames(Fsh_plot)

Fsh$eig[1]/(sum(Fsh$eig))

Fsh$eig[2]/(sum(Fsh$eig))
Fsh_ord_plot<-Fsh_ord
as.data.frame(Fsh_ord_plot)

class(Fsh)

 ggplot(fshplot_gulf, aes(x = "axis_1", y = "axis_2")) +
  geom_point() +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  theme_bw() + 
  xlab("PCoA 1 (30.8%)") +
  ylab("PCoA 2 (22.3%)") +
  annotate(geom = 'text', label = 'Bray-Curtis', x = Inf, y = -Inf, hjust = 1.15, vjust = -1)



wcmfish<- as.data.frame(Zfish_ord

wcmdscale(fish_ord)
fish_ord



permanova<-set.seed(10),arkan_red_ord, k=2,try=10,trymax = 10) %>%
scores()%>%
  
  permanova<-nmds$points %>%
  as_tibble(rownames = "site_year_bout")

 arkredcentroid<-arkred_ord_env %>%
  group_by(Bout)%>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))
 
 yeargulfcentroid<-gulf_env_dist_second_run %>%
   group_by(Years)%>%
   summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))
 
 arka %>% ggplot(aes(x=NMDS1,y=NMDS2,color=temporal))+
   geom_point()+
   stat_ellipse()+
   geom_point(data=arkredcentroid,size=5,shape=21,color="black",aes(fill=plot))
 Atemporallonghornova<-adonis2(as.dist(vegdprin_permanova)~texas_nmds_plot$temporal,permutations = 10001)

library(ggplot2)

gulf_env_dist_second_run %>% 
  ggplot(aes(x=NMDS1,y=NMDS2,color=Years))+
  geom_point()+
  stat_ellipse(show.legend = FALSE)+
  geom_point(data = yeargulfcentroid, size=5,shape=21,color="black",aes(fill=Years),show.legend = FALSE)+
labs(title ="Beta Diversity Differences in First 3 Years V last 3 years of Fish Sampling at Gulf Draining NEON Stream Sites")+
theme(plot.title = element_text(size =08))+
theme(plot.title = element_text(hjust = 0.5))

as.matrix(gulf_env_dist_second_run)
adonis2(as.dist(gulf_env_dist_second_run)~gulf_env_dist_second_run)
#Atlantic
atlanticcentroid<-AtlanticNMDS%>%
  group_by(Season)%>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

Yearatlanticcentroid<-AtlanticNMDS%>%
  group_by(Years)%>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

AtlanticNMDS %>% 
  ggplot(aes(x=NMDS1,y=NMDS2,color=Season))+
  geom_point()+
  stat_ellipse(show.legend = FALSE)+
  geom_point(data = atlanticcentroid, size=5,shape=21,color="black",aes(fill=Season),show.legend = FALSE)+
  labs(title ="Beta Diversity Differences Between Early and Late Season Fish Sampling at Gulf Draining NEON Stream Sites")+
  theme(plot.title = element_text(size =08))+
  theme(plot.title = element_text(hjust = 0.5))

#carribean 

seasoncaribeancentroid<-caribbeanNMDS%>%
  group_by(Season)%>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

Yearcaribbeancentroid<-caribbeanNMDS%>%
  group_by(Years)%>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

caribbeanNMDS %>% 
  ggplot(aes(x=NMDS1,y=NMDS2,color=Years))+
  geom_point()+
  stat_ellipse(show.legend = FALSE)+
  geom_point(data = Yearcaribbeancentroid, size=5,shape=21,color="black",aes(fill=Years),show.legend = FALSE)+
  labs(title ="Beta Diversity Differences Between First 3 years and Latest 3 Years Fish Sampling at Caribbean Draining NEON Stream Sites")+
  theme(plot.title = element_text(size =08))+
  theme(plot.title = element_text(hjust = 0.5))

#Pacific

seasonpacificcentroid<-PacificNMDS%>%
  group_by(Season)%>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

Yearpacificcentroid<-PacificNMDS%>%
  group_by(Years)%>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

PacificNMDS %>% 
  ggplot(aes(x=NMDS1,y=NMDS2,color=Years))+
  geom_point()+
  stat_ellipse(show.legend = FALSE)+
  geom_point(data = seasonpacificcentroid, size=5,shape=21,color="black",aes(fill=Season),show.legend = FALSE)+
  labs(title ="Beta Diversity Differences Between Early and Late Season Fish Sampling at Pacific Draining NEON Stream Sites")+
  theme(plot.title = element_text(size =08))+
  theme(plot.title = element_text(hjust = 0.5))



#new try

library(vegan)
library(dplyr)

mungedonarkanred530<-read.csv('C:/Users/dmonahan/Documents/diversity/finalcrunch/mungedonarkanred530d.csv', fileEncoding="UTF-8-BOM",header=TRUE)


arkred_site<-mungedonarkanred530 %>% column_to_rownames("site_year_bout")

set.seed(1)
arkred_dist<-vegdist(arkred_site)

set.seed(16)
arkred_nmds<-metaMDS(arkred_dist)%>%
  scores()%>%
  as.tibble(rownames="site_year_bout")

arkred_nmds_plot<-inner_join(arkred_nmds,earkred)

temporalcentroidarkred<-arkred_nmds_plot%>%
  group_by(temporal) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))
  
arkred_nmds_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=temporal))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=temporalcentroidarkred,size=5,shape=21,color="black",aes(fill=temporal))
  

boutoucuova<-adonis2(as.dist(arkred_dist)~arkred_nmds_plot$temporal,permutations = 10001)

  MAD<-mungedonarkanred530 %>% column_to_rownames("site_year_bout")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(vegan)



#prin

prin_permanova<-read.csv('C:/Users/dmonahan/Documents/diversity/finalcrunch/prin_permanova.csv', fileEncoding="UTF-8-BOM",header=TRUE)

vegdprin_permanova<-prin_permanova %>% column_to_rownames("site_year_bout")

texas_gulf_dis<-vegdist(vegdprin_permanova)

set.seed(16)
texas_gulfmds<-metaMDS(texas_gulf_dis)%>%
  scores()%>%
  as.tibble(rownames="site_year_bout")

texas_nmds_plot<-inner_join(texas_gulfmds,meta_data_prin_permanova)

temporalcentroidtenn<-texas_nmds_plot%>%
  group_by(temporal) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

texas_nmds_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=temporal))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=temporalcentroidtexas,size=5,shape=21,color="black",aes(fill=plot))
Atemporallonghornova<-adonis2(as.dist(vegdprin_permanova)~texas_nmds_plot$temporal,permutations = 10001)

boutoucuova<-adonis2(as.dist(test)~earkred$realbout,permutations = 10001)

templonghornova$aov.tab
temporaloucuova$`Pr(>F)`
temporaloucuova$aov.tab


#beta
library(vegan)
#group A
beta_bold_east_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=state))+
  +   geom_point()+
  +   stat_ellipse()+
  +   geom_point(data=stateeastcentroid,size=5,shape=21,color="black",aes(fill=state))

beta_bold_central<-read.csv('C:/Users/dmonahan/Documents/diversity/finalcrunch/beta_bold_central.csv', fileEncoding="UTF-8-BOM",header=TRUE)

beta_bold_central_permanova<-beta_bold_central %>% column_to_rownames("site_year_bout")

set.seed(16)
beta_bold_central_dist<-vegdist(beta_bold_central_permanova)

set.seed(1616)
beta_bold_central_nmds<-metaMDS(beta_bold_central_dist)%>%
  scores()%>%
  as.tibble(rownames="site_year_bout")

central_groupA_plot<-inner_join(beta_bold_central_nmds,Ebeta_bold_central)

statecentralcentroid<-central_groupA_plot%>%
  group_by(state) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

tiff('test.tiff', units="in", width=5, height=4, res=400, compression = 'lzw')

central_groupA_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=state))+
    geom_point()+
     stat_ellipse()+
    geom_point(data=statecentralcentroid,size=5,shape=21,color="black",aes(fill=state))


bout_col_ks_nova<-adonis2(as.dist(beta_bold_COl_KS_dist)~COLKS_plot$bout,permutations = 10001)

col_ks_nova$`Pr(>F)`
centralnova$`Pr(>F)`
east

centralnova$F
centralnova$SumOfSqs
bout_beta_bold_eastanova



spatial_beta_bold_centralanova$`Pr(>F)`
perm

# group b



group_b<-read.csv('C:/Users/dmonahan/Documents/diversity/finalcrunch/group_b.csv', fileEncoding="UTF-8-BOM",header=TRUE)

group_b_permanova<-group_b %>% column_to_rownames("site_year_bout")

set.seed(16)
group_b_dist<-vegdist(group_b_permanova)

set.seed(16161616)
group_b_nmds<-metaMDS(group_b_dist)%>%
  scores()%>%
  as.tibble(rownames="site_year_bout")

COLKS_plot<-inner_join(beta_bold_COl_KS_nmds,col_kans_env)

boutcentralcentroid<-beta_bold_central_plot%>%
  group_by(bout) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

beta_bold_east_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=state))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=stateeastcentroid,size=5,shape=21,color="black",aes(fill=state))


bout_groupA_nova<-adonis2(as.dist(beta_bold_central_dist)~beta_bold_central_plot$bout,permutations = 10001)

col_ks_nova$`Pr(>F)`
centralnova$`Pr(>F)`
east






















#texas gulf 

texasgulf_3run<-read.csv('C:/Users/dmonahan/Documents/diversity/finalcrunch/texasgulf_3run.csv', fileEncoding="UTF-8-BOM",header=TRUE)

texasgulf_3run_matrix<-texasgulf_3run %>% column_to_rownames("site_year_bout")

texasgulf_3run_dist<-vegdist(texasgulf_3run_matrix)

set.seed(16)
texasgulf_3run_nmds<-metaMDS(texasgulf_3run_dist)%>%
  scores()%>%
  as.tibble(rownames="site_year_bout")

texgulfplot_plot<-inner_join(texasgulf_3run_nmds,meta_data_prin_permanova)

boutttexgul<-texgulfplot_plot%>%
  group_by(bout) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

temporaltexgulf3<-texgulfplot_plot%>%
  group_by(temporal) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

texgulfplot_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=temporal))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=temporaltexgulf3,size=5,shape=21,color="black",aes(fill=temporal))




#Atlantic

midatla_cpue<-read.csv('C:/Users/dmonahan/Documents/diversity/finalcrunch/midatla_cpue.csv', fileEncoding="UTF-8-BOM",header=TRUE)

midatla_cpue_matrix<-midatla_cpue %>% column_to_rownames("site_year_bout")


midatla_cpue_dist<-vegdist(midatla_cpue_matrix)

set.seed(16)
midatla_cpue_nmds<-metaMDS(midatla_cpue_dist)%>%
  scores()%>%
  as.tibble(rownames="site_year_bout")

midatla_plot<-inner_join(midatla_cpue_nmds,env_midatla_cpue)

bouttmidatlacentroid<-midatla_plot%>%
  group_by(bout) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

tempormidatlacentroid<-midatla_plot%>%
  group_by(temporal) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

midatla_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=bout))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=bouttmidatlacentroid,size=5,shape=21,color="black",aes(fill=bout))




midatlanovabout<-adonis2(as.dist(midatla_cpue_dist)~ midatla_plot$bout,permutations = 10001)

#kansas

kanasas_cpue<-read.csv('C:/Users/dmonahan/Documents/diversity/finalcrunch/kanasas_cpue.csv', fileEncoding="UTF-8-BOM",header=TRUE)

kanasas_cpue_matrix<-kanasas_cpue %>% column_to_rownames("site_year_bout")


kanasas_cpue_dist<-vegdist(kanasas_cpue_matrix)

set.seed(16)
kanasas_cpue_nmds<-metaMDS(kanasas_cpue_dist)%>%
  scores()%>%
  as.tibble(rownames="site_year_bout")

kanasas_plot<-inner_join(kanasas_cpue_nmds,env_kanasas)

bouttkanasascentroid<-kanasas_plot%>%
  group_by(bout) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

temporkanasascentroid<-kanasas_plot%>%
  group_by(temporal) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

kanasas_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=bout))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=bouttkanasascentroid,size=5,shape=21,color="black",aes(fill=bout))


kanasas_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=temporal))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=temporkanasascentroid,size=5,shape=21,color="black",aes(fill=temporal))



kanasasanovtemporal<-adonis2(as.dist(kanasas_cpue_dist)~ env_kanasas$temporal,permutations = 10001)

kanasasanovabout$`Pr(>F)`

kanasasanovtemporal$`Pr(>F)`

#Caribbean

caribbean_cpue<-read.csv('C:/Users/dmonahan/Documents/diversity/finalcrunch/caribbean_cpue.csv', fileEncoding="UTF-8-BOM",header=TRUE)

caribbean_cpue_matrix<-caribbean_cpue %>% column_to_rownames("site_year_bout")


caribbean_cpue_dist<-vegdist(caribbean_cpue_matrix)

set.seed(16)
caribbean_cpue_nmds<-metaMDS(caribbean_cpue_dist)%>%
  scores()%>%
  as.tibble(rownames="site_year_bout")

caribbean_plot<-inner_join(caribbean_cpue_nmds,env_caribbean_cpue)

bouttcaribbeancentroid<-caribbean_plot%>%
  group_by(bout) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

temporcarribeancentroid<-caribbean_plot%>%
  group_by(temporal) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

caribbean_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=bout))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=bouttcaribbeancentroid,size=5,shape=21,color="black",aes(fill=bout))


caribbean_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=temporal))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=temporcarribeancentroid,size=5,shape=21,color="black",aes(fill=temporal))



caribbeananovtemporal<-adonis2(as.dist(caribbean_cpue_dist)~ env_caribbean_cpue$temporal,permutations = 10001)

caribbeananovbout<-adonis2(as.dist(caribbean_cpue_dist)~ env_caribbean_cpue$bout,permutations = 10001)

caribbeananovbout$`Pr(>F)`

caribbeananovtemporal$`Pr(>F)`

#new england 

hopb_cpue<-read.csv('C:/Users/dmonahan/Documents/diversity/finalcrunch/hopb_cpue.csv', fileEncoding="UTF-8-BOM",header=TRUE)

hopb_cpue_matrix<-hopb_cpue %>% column_to_rownames("site_year_bout")


hopb_cpue_dist<-vegdist(hopb_cpue_matrix)

set.seed(16)
hopb_cpue_nmds<-metaMDS(hopb_cpue_dist)%>%
  scores()%>%
  as.tibble(rownames="site_year_bout")

hbo_plot<-inner_join(hopb_cpue_nmds,env_hopb)

bouthbocentroid<-hbo_plot%>%
  group_by(bout) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

temporhpbocentroid<-hbo_plot%>%
  group_by(temporal) %>%
  summarize(NMDS1=mean(NMDS1),NMDS2=mean(NMDS2))

hbo_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=bout))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=bouthbocentroid,size=5,shape=21,color="black",aes(fill=bout))


hbo_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=temporal))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=temporhpbocentroid,size=5,shape=21,color="black",aes(fill=temporal))



hpboanovtemporal<-adonis2(as.dist(hopb_cpue_dist)~ env_hopb$temporal,permutations = 10001)

hpboanovbout<-adonis2(as.dist(hopb_cpue_dist)~ env_hopb$bout,permutations = 10001)

hpboanovtemporal$`Pr(>F)`

hpboanovbout$`Pr(>F)`


#arkansa_red

texas_nmds_plot %>% ggplot(aes(x=NMDS1,y=NMDS2,color=temporal))+
  geom_point()+
  stat_ellipse()+
  geom_point(data=temporalcentroidtexas,size=5,shape=21,color="black",aes(fill=plot))
Atemporallonghornova<-adonis2(as.dist(vegdprin_permanova)~texas_nmds_plot$temporal,permutations = 10001)






















test %>%
  as.matrix()%>%
  as.tibble(rownames="site_year_bout") %>%
  pivot_longer(-site_year_bout)%>%
  filter(site_year_bout>name)%>%
  mutate(last=str_replace(site_year_bout,".*B",""))%>%
  mutate(first=str_replace(name,".*B",""))%>%
  mutate(boutnumber=as.numeric(str_replace(last,"spring","1")))%>%
           mutate(boutnumber=as.numeric(str_replace(last,"fall","3")))%>%
  group_by(site_year_bout,last,boutnumber)%>%
  summarize(median=median(value))%>%
  ungroup()%>%
  ggplot(aes(x=boutnumber,y=median,color=last))+
  geom_line()





set.seed(1)
metaMDS(lasttryARKRED)%>%
  scores() %>%
  as_tibble(rownames = sites)



mungedpissedarkred$season<-substr(munged_ordination_arkan_red$site_year_bout,11,16)



