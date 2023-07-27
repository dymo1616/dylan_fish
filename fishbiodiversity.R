
install(vegan)
library(vegan)
library(betapart)
library(rda)
BiodiversityRGUI(changeLog = FALSE, backward.compatibility.messages = FALSE)

#delete below 

fsh_perfish_with_boutslake_stream<-fsh_perfish_with_bouts$domainID
TOOK_remove_fsh_perfish_with_bouts<-fsh_perfish_with_bouts
TOOK_remove_fsh_perfish_with_bouts<-fsh_perfish_with_bouts %>% slice(-c("TOOK"))
TOOK_remove_fsh_perfish_with_bouts<-subset(fsh_perfish_with_bouts,fsh_perfish_with_bouts$siteID!="TOOK")
TOOK_remove_fsh_perfish_with_bouts<-subset(fsh_perfish_with_bouts,fsh_perfish_with_bouts$siteID!="TOOK")
fish_withboutstookr[I!(fish_withboutstookr$siteID !== "TOOK"),]
TOOK_remove_fsh_perfish_with_bouts[!(TOOK_remove_fsh_perfish_with_bouts$siteID=="TOOK"),] %>% head()
fsh


library(dplyr)
#count of all fish in fish per fish -this will need to be joined with bulk-this is for single pass and all pass, could also be by species if you want

## just single pass
fsh_perFish_noDups$gear<-substr(fsh_perFish_noDups$eventID,20,30)
fsh_bulkCount_noDups$gear<-substr(fsh_bulkCount_noDups$eventID,20,30)


#Lake
A_fish_lake<-fsh_perFish_noDups %>% filter(passNumber %in% c(1,4,5))

fsh_bulk_lake<-fsh_bulkCount_noDups %>% filter(passNumber %in% c(1,4,5))

B_fish_lake<-A_fish_lake%>% group_by(siteID,scientificName,year,site_year_bout,site_bout,gear,eventID)%>%
  count("scientificName")

AC_bulk_lake<-fsh_bulk_lake %>% group_by("siteID","scientificName","eventID","year","site_year_bout","site_bout","gear","bulkFishCount") %>% summarise(n=sum(bulkFishCount))

AC_bulk_lake<-fsh_bulk_lake %>% group_by(siteID,scientificName,eventID,year,site_year_bout,site_bout,gear,bulkFishCount) %>% 
  summarise(n=sum(bulkFishCount))

C_bulk_lake$n<-C_bulk_lake$bulkFishCount


fish_speciescount$count<-fish_speciescount$n
bulk_join$count<-bulk_join$bulkFishCount

class(fish_speciescount$count)
class(bulk_join$count)

#lake 
lake_total_fish<-rbind(B_fish_lake,AC_bulk_lake)

ef_total<-subset(lake_total_fish,lake_total_fish$gear=="e-fisher")
ef_lake_total<-ef_total%>% filter(str_detect(siteID,"TOOK|CRAM|LIRO|PRPO|PRLA"))

ef_lake_fish<-ef_lake_total %>% group_by(scientificName,site_year_bout) %>%
  summarise(tot_fish=sum(n))

fsh_perPass_noDups

fyke_lake_total<-subset(lake_total_fish,lake_total_fish$gear=="fyke")


#lake pass
lake_fish_pass_onepass<-subset(fsh_perPass_noDups,fsh_perPass_noDups$passNumber==1)

lake_pass_ef <- lake_fish_pass_onepass %>% group_by(site_year_bout) %>%
  summarise(totalseconds=sum(efTime))

class(lake_pass_ef$totalseconds)

lake_pass_ef$gear<-substr(lake_pass_ef$eventID,20,30)
only_lake_pass_ef<-lake_pass_ef%>% filter(str_detect(siteID,"TOOK|CRAM|LIRO|PRPO|PRLA"))
only_lake_pass_only_ef<-subset(only_lake_pass_ef,only_lake_pass_ef$gear=="e-fisher")

lake_ef_cpu<-rbind(lake_pass_ef,lake_fish_site_total)

lake_fish_site_total<- ef_lake_fish %>%  group_by(site_year_bout, scientificName) %>%
  summarise(totalF=sum(tot_fish))

ef_lake_ef_cpu<-subset(lake_ef_cpu,lake_ef_cpu$gear=="e-fisher")

count_ef_lake<-group_by()

lake_ef_cpu$cpue<-paste(lake_ef_cpu$n/(lake_ef_cpu$totalseconds/3600))
as.data.frame()


dive_fish_onep<-subset(diversity_table_fish,diversity_table_fish$passNumber==1)
dive_fishpass_onepass<-subset(DE_fish_pass_onepass,DE_fish_pass_onepass$passNumber==1)



#dive_total_fish_onepass<- dive_fish_onep %>% group_by(siteID,site_year_bout,year,scientificName) %>%
#summarise(tf_caught =sum(n))

dive_fish_onep_ef<-merge(dive_fishpass_onepass,dive_fish_onep)


ef_lake_total$cpue<-paste(dive_fish_onep_ef$tf_caught/dive_fish_onep_ef$totalseconds)
dive_fish_onep_ef$cpueperhour<-paste(dive_fish_onep_ef$tf_caught/(dive_fish_onep_ef$totalseconds/3600))


first_pass_total_fish_perfish_bulk<-dive_fish_onep_ef %>% group_by(site_year_bout,scientificName) %>%
  summarise(total_fish_PH=sum(cpueperhour))
class(first_pass_total_fish_perfish_bulk$total_fish_PH)

#stream

A_fish_all_fistpass<- subset(fsh_perFish_noDups,fsh_perFish_noDups$passNumber==1)

B_fish_pone_speciescount<-A_fish_all_fistpass%>% group_by(siteID,scientificName,year,site_year_bout,site_bout,gear)%>%
  count("scientificName")

cpue_fshpfsh<-A_fish_all_fistpass%>% group_by(siteID,scientificName,year,site_year_bout,site_bout,gear)%>%
  count("scientificName")

## all pass
fish_speciescount<-fsh_perFish_noDups%>% group_by(siteID,scientificName,eventID,year,passNumber,namedLocation,site_year_bout,site_bout)%>%count("scientificName")





#just single pass
fsh_bulk_onepass<-subset(fsh_bulkCount_noDups,fsh_bulkCount_noDups$passNumber==1)

C_bulk_onepass<-fsh_bulk_onepass %>% select(c("siteID","scientificName","eventID","year","site_year_bout","site_bout","gear","bulkFishCount"))


##all pass

bulk_join<-fsh_bulkCount_noDups %>% select(c("siteID","scientificName","year","eventID","namedLocation","passNumber","site_year_bout","site_bout","bulkFishCount"))

#join process
# prep of fish bulk data to join with fish per fish

fish_speciescount$count<-fish_speciescount$n
bulk_join$count<-bulk_join$bulkFishCount

class(fish_speciescount$count)
class(bulk_join$count)

total_fish_all<-rbind(fish_speciescount,bulk_join)

class(total_fish_all$count)

# 
## fish per fish count  to DF

##one pass

C_bulk_onepass$n<-C_bulk_onepass$bulkFishCount

j_fish_count_onepass<-B_fish_pone_speciescount %>% select(c(siteID,scientificName,year,site_year_bout,site_bout,gear,n))
j_bulk_onepass<-C_bulk_onepass %>% select(c(siteID,scientificName,year,site_year_bout,site_bout,gear,n))

##all pass
total_fish_onepass<-rbind(j_fish_count_onepass,j_bulk_onepass)

#summarise all fish by event id ext for depletion 

##count fish caught species
depletion_fish<- total_fish_all %>% group_by(site_year_bout,siteID,year,namedLocation,eventID,passNumber) %>%
  summarise(tf_caught=sum(count))

diversity_table_fish<- total_fish_all %>% group_by(site_year_bout,siteID,year,namedLocation,eventID,passNumber,scientificName) %>%
  summarise(tf_caught=sum(count))


##join pass number

Pass_for_fixed<-select(fsh_fieldData_noDups,c(fixedRandomReach ,namedLocation))

depletion_fish_count<-merge(depletion_fish,Pass_for_fixed)

depletion_fish_count$method<-substr(depletion_fish_count$eventID,20,27)

# depletion just fixed 

depletion_fixed<-subset(depletion_fish_count,depletion_fish_count$fixedRandomReach=="fixed")
depletion_fixed_ef<-subset(depletion_fixed,depletion_fixed$method=="e-fisher")



depletion_fixed_ef_dup_removed<-depletion_fixed_ef[!duplicated(depletion_fixed_ef$eventID), ]

depletion_fixed_ef_dup_removed$site_reach<-substr(depletion_fixed_ef_dup_removed$eventID,1,16)
dp_per<-depletion_fixed_ef_dup_removed


dpu<-unique(dp_per$site_reach)
out<-data.frame()
for(i in 1:length(dpu)){
  data.i<-dp_per[dp_per$site_reach==dpu[i],]
  if(nrow(data.i)<3){next}
  t1_2<-ifelse(data.i$tf_caught[data.i$passNumber==1]<data.i$tf_caught[data.i$passNumber==2],1,0)
  t1_3<-ifelse(data.i$tf_caught[data.i$passNumber==1]<data.i$tf_caught[data.i$passNumber==3],1,0)
  t2_3<-ifelse(data.i$tf_caught[data.i$passNumber==2]<data.i$tf_caught[data.i$passNumber==3],1,0)
  out.i<-cbind(dpu[i],t1_2,t1_3,t2_3)
  out<-rbind(out,out.i)
}







  

depletion_df<-data_frame(depletion_fixed_no_dup)
  df2 <- df[!duplicated(df), ]

  
  ## vegan diversity index 
  
  #cpue-  
  
  
  
  # just for lakes 
  
  total_fish_onepass_lakes<-lake_total_fish %>% filter(siteID %in% c("TOOK","CRAM","LIRO","PRPO","PRLA"))
  
  
  # getting pass data ready 
  
  
  
  fsh_perPass_noDups$gear<-substr(fsh_perPass_noDups$eventID,20,30)
  
  fsh
  
  # fish 
  
  ef_fsh_fsh_lake<-subset(total_fish_onepass_lakes,total_fish_onepass_lakes$gear=="e-fisher")
  fyke_fsh_lakes<-subset(total_fish_onepass_lakes,total_fish_onepass_lakes$gear=="fyke")
 
  
  
  #pass
  
  passdata_lakes<-fsh_perPass_noDups %>% filter(siteID %in% c("TOOK","CRAM","LIRO","PRPO","PRLA"))
  lake_fsh_pass_ef<-subset(passdata_lakes,passdata_lakes$gear=="e-fisher")
  
  lake_pass_eftime <- fsh_pass_ef%>% group_by(site_year_bout,siteID,year,eventID,passNumber) %>%
    summarise(totalseconds=sum(efTime))
  
  #net time 
  fyke_lake_passdata<-subset(passdata_lakes,passdata_lakes$gear=="fyke")
  
  fyke_time_pass_data<-fyke_lake_passdata %>% group_by(site_year_bout,siteID,year,eventID,passNumber) %>%
    summarise(netDeploymentTime=sum(netDeploymentTime))
  
  
  #ef_join
  ef_lake_div<-merge(dive_fishpass_onepass,dive_fish_onep)   
  
  
 
  
  
  
  
  
                        
                        fsh_perPass_noDups$efTime2[is.na(fsh_perPass_noDups$efTime2)] = 0
                        #fsh_perPass_with_bouts$efTimeSecond<-fsh_perPass_with_bouts$efTime2
  
                        DE_fish_pass_onepass <- fsh_perPass_noDups %>% group_by(site_year_bout,siteID,year,eventID,passNumber) %>%
                        summarise(totalseconds=sum(efTime+efTime2))


                        dive_fish_onep<-subset(diversity_table_fish,diversity_table_fish$passNumber==1)
                        dive_fishpass_onepass<-subset(DE_fish_pass_onepass,DE_fish_pass_onepass$passNumber==1)
                          


                          #dive_total_fish_onepass<- dive_fish_onep %>% group_by(siteID,site_year_bout,year,scientificName) %>%
                          #summarise(tf_caught =sum(n))
                          
                          dive_fish_onep_ef<-merge(dive_fishpass_onepass,dive_fish_onep)   
                          
                          #calc cpue
                          dive_fish_onep_ef$cpue<-paste(dive_fish_onep_ef$tf_caught/dive_fish_onep_ef$totalseconds)
                          dive_fish_onep_ef$cpueperhour<-paste(dive_fish_onep_ef$tf_caught/(dive_fish_onep_ef$totalseconds/3600))
                          
                          #already done
                          #fsh_perPass_with_bouts$efTime2[is.na(fsh_perPass_with_bouts$efTime2)] = 0
                          #fsh_perPass_with_bouts$efTimeSecond<-fsh_perPass_with_bouts$efTime2
                          #D_fish_pass_onepass <-subset(fsh_perPass_with_bouts,fsh_perPass_with_bouts$passNumber==1)
                          #D_fish_pass_onepass$efTime2[is.na(D_fish_pass_onepass$efTime2)] = 0
                          #D_fish_pass_onepass$efTime2<-replace(D_fish_pass_onepass,is.na(D_fish_pass_onepass$efTime2),0)
                          #D_fish_pass_onepass$efTimeB<-D_fish_pass_onepass$e
                          class(dive_fish_onep_ef$cpueperhour)
                          dive_fish_onep_ef$cpueperhour<-as.numeric(as.character(dive_fish_onep_ef$cpueperhour))
                          #xyz_fish$fish_phour<-as.numeric(as.character(xyz_fish$fish_phour))
                          #xyz_fish$fish_phour<-paste((xyz_fish$cpue)*3600)
                          #class(xyz_fish$fish_phour)
                        
                          #xyz_fish$cpueperhour<-paste((xyz_fish$totalfish_sp/xyz_fish$totalseconds)*60)
                          #xyz_fish<-xyz_fish$cpue %>% mutate_if(is.character,as.numeric)
                          #DE_fish_pass_onepass <- D_fish_pass_onepass %>% group_by(site_year_bout,siteID,year) %>%
                           # summarise(totalseconds=sum(efTime+efTime2))
                          
                          
                          
                          first_pass_total_fish_perfish_bulk<-dive_fish_onep_ef %>% group_by(site_year_bout,scientificName) %>%
                            summarise(total_fish_PH=sum(cpueperhour))
                          class(first_pass_total_fish_perfish_bulk$total_fish_PH)
                          
                          Atest_wide<-pivot_wider(first_pass_total_fish_perfish_bulk,
                                                 id_cols = scientificName,
                                                 id_expand = FALSE,
                                                 names_from = site_year_bout,
                                                 names_prefix = "",
                                                 names_sep = "_",
                                                 names_glue = NULL,
                                                 names_sort = FALSE,
                                                 names_vary = "fastest",
                                                 names_expand = FALSE,
                                                 names_repair = "check_unique",
                                                 values_from = total_fish_PH,
                                                 values_fill = NULL,
                                                 values_fn = NULL,
                                                 unused_fn = NULL
                          )
                          
                          data_long <- gather(sitename_total_fish_wide), condition, measurement, control:cond2, factor_key=TRUE)
                          data_long
                          
         
              #vegan beta diversity             
                          
           betatry<-site_total_fish_wide[-25,-32,-22,-34,-35,-105,106,-107,-108,-169,-170,-171,-172,-178,-179,-180,-181,-202,-203,-210,]
          stream_beta_nositena<-stream_beta %>% select(c[-1,-2])
          stream_beta_nositena= subset(stream_beta, select = -c(site,X,site_year_bout) )
                       
          betatry.bd.j<-beta.div.comp(beta_test,coef = "J",quant = T)
          betatry.bd.j$part
          betatry.bd.j$part
          
          betatry.bd.s<-beta.div.comp(beta_test,coef = "S",quant = T)
          betatry.bd.s$part
          
          
          
           #
        #beta for just stream sites 
          
          stream_beta_nositena.bd.j<-beta.div.comp(stream_beta_nositena,coef = "J",quant = T)
          stream_beta_nositena.bd.j$part
          
          
          
          local.repl<-LCBD.comp(stream_beta_nositena.bd.j$repl,sqrt.D = T)
          
          local.rich<-LCBD.comp(stream_beta_nositena.bd.j$rich,sqrt.D = T)
          
         class(beta_test$Ameiurus.melas)
         
          
          local.repl$LCBD
          
          local.rich$LCBD
          
          
          scbd<-beta.div(stream_beta_nositena,method = "hellinger")
          
          scbd$LCBD
          
          Ordination.model1 <- metaMDS(stream_beta_nositena, distance='bray', k=2, trymax=1, 
                                       autotransform=TRUE, noshare=0.1, expand=TRUE, trace=1, plot=FALSE)
          
          plot1 <- ordiplot(Ordination.model1, choices=c(1,2,3,4))
          
          #divstance 
          
         
           rownames(Afish_dist)<-Afish_dist$site_year_bout
          matrix_Afish<-as.matrix(Afish_dist)
          avgdi
          
          beta_test<-stream_beta_nositena
          beta_test_interger<-tibble(beta_test)
          OTU<-Afish_dist
          OTU<- as.numeric(unlist(OTU))
          beta_test_interger<-as.integer(beta_test_interger)
          as.matrix(beta_test_interger)
          Bfish_dist<-Afish_dist %>% select(-site_year_bout)
          class(Afish_dist$Ameiurus.melas)
          
         #Afish_dist<-fish_beta_form_05152023[-208,]
          
          #ordination by ecoregion 
          
          library(dplyr)
          library(vegan)
          arkan_red_ord<-ordination_arkan_red %>% column_to_rownames("site_year_bout")




          set.seed(1200)
          disarkanred<-avgdist(arkan_red_ord,dmethod = "bray", sample = 20 )
          
          set.seed(1)
          nmds<-metaMDS(arkan_red_ord)
          
          nmds$points
          scores(nmds)
          plot(nmds)
          
          #set.seed(2)
          #metaMDS(fish_distance)
          
          library(ggplot2)
          library(dplyr)
          library(vegan)
          
          arkordgraph<-nmds%>%
            as_tibble(rownames = "site_year_bout") %>%
            inner_join(.,arkred_ord_env,by="site_year_bout") %>%
            ggplot(aes(x=NMDS1,y=NMDS2,color=nmds)) +
            geom_point()
            
            as.data.frame(scores)
          
            #variation parti
            nmds$
          graph_nmds<-scores
          
          graph_divide(nmds) %>%
            as_tibble(rownames = "site_year_bout")
          
        arkred  %>%
            as_tibble(rownames = "site_year_bout")
        
        as.data.frame(arkred)
          
          class(beta_test_interger$)
          
          
          
          #alternative to ordination
          
         arkred<-nmds$points
          nmds
          
          
          
          beta.pair.abund(site_total_fish_wide, index.family = "bray")
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                         # to graph alpha  
                          
                          library(dplyr)
                          library(phyloseq)
                          # maybe not for fish 
                          
                          #remove charter string 
                         site_total_fish_wide<- subset(site_total_fish_wide,select = -c(site_year_bout))
                          # na to zeros
                         sitename_total_fish_wide %>% mutate_if(is.character,as.numeric)
                         Atest_wide[is.na(Atest_wide)] = 0
                         setwd('C:/Users/dmonahan/Documents/diversity/eftime_corrected')
                         
                         stream_div_formate=read.csv('C:/Users/dmonahan/Documents/diversity/eftime_corrected/fish_beta_form_05152023.csv', fileEncoding="UTF-8-BOM",header=TRUE)
                         stream_run<-stream_run[-10,]
                         
                         cpue_stream<- cpue_stream %>% select(-c(scientific_n))
                         
                         sampledata_namecor_D<-  subset(sampledata_namecor_D,select = -c(site_year_bout))
                         
                         df2 <- df %>% select(-c(id, name, chapters))
                         
                         species<-Atest_wide %>% select(c["scientificName"])
                         
                         species= subset(Atest_wide, select = c("scientificName"))
                         
                         setdiff(rownames(cpue_stream),taxon_stream$scientific_n)
                         
                         dplyr::setdiff(rownames(Atest_wide),species$scientificName)
                         
                         class(species$scientificName)
                         class(rownames(Atest_wide))
                         
                         rownames(taxon_stream) <- taxon_stream$scientific_n
                         taxon_stream =  as.matrix(taxon_stream)
                         
                         rownames(sampledata_namecor_D)<-sampledata_namecor_D$site_year_bout
                         sampledata_namecor_D =  as.matrix(sampledata_namecor_D)
                         
                         phyloseq_validate(cpue_stream)
                         
                         rownames(cpue_stream) <- cpue_stream$scientific_n
                         species =  as.matrix(species)
                         
                         #drian_meta<-cor_diversity_env_data %>% select(c(site_year_bout,drainage))
                         
                         ps=phyloseq(sample_data(sampledata_namecor_D))
                         
                         sampledata_namecor_D <- as.data.frame(sampledata_namecor_D)
                         
                         OTU = otu_table(otumat, taxa_are_rows = TRUE)
                         TAX = tax_table(taxmat)
                         sampledata = sample_data(sampledata_namecor_D)
                         
                         OTU = otu_table(cpue_stream, taxa_are_rows = TRUE)
                         TAX = tax_table(taxon_stream)
                         sampledata = sample_data(sampledata_namecor_D)
                         
                         
                         physeq1 = phyloseq(OTU, TAX, sampledata)
                         
                         physeq1 = phyloseq(cpue_stream,taxon_stream, drian_meta)
                         
                         sample_data(physeq1)$drainage <- factor((sample_data(physeq1)$drainage), levels=c("Mississippi","Caribbean","Atlantic","Artic"))
                         
                         physeq1 = transform_sample_counts(physeq1, as.integer)
                         richness <- estimate_richness(physeq1)
                         head(richness)
                         
                         intersect(rownames(stream_run),taxon_stream$scientific_n)
                         
                         taxa_names(stream_run)
                         
                         plot_richness(physeq1)
                         
                         plot_richness(OTU, x="drainage", measures="Shannon", color = "drainage")+
                           geom_boxplot(alpha=0.6)+ 
                           theme(legend.position="none", axis.text.x=element_text(angle=45,hjust=1,vjust=1,size=12))
                         
                         OTU_T <- as.data.frame(OTU)
                         #above maybe not for fish
                         
                          #as.numeric(unlist(test_wide))
                          #test_wide %>% mutate_if(is.character,as.numeric)
                          
                          #class(header_total_fish_wide$`Lepomis cyanellus`)
                          
                          #env <- read_csv(here::here("data", "env-var.csv"))
                          
                          
                          # to use to correlate with the other data 
                          #header_total_fish_wide[is.na(header_total_fish_wide)] = 0
                          
                          #test_wide[] <- lapply(test_wide, function(x) as.numeric(replace(x, is.na(x), 0)))
                          library(FSA)
                          library(FSAdata)
                          # claclulating values 
                          #s<-apply(TW>0,1,sum)
                          #s
                          #diversity(TW)
                        #simpson$value <-(diversity(simpson,index="simpson"))
                        #shannon$value<-(diversity(shannon,index = "shannon"))
                        #richness$value<-(diversity(rich))
                        #head(value)
                        
                          #diversity(TW,index = )
                          
                          
                          
                          
                          #row.names(test_wide)<-test_wide$site_year_bout
                          
                          
                          
                          diver_calc_sheet<-site_total_fish_wide
                         
                        #simpson <- diver_calc_sheet 
                        #shannon<-test_wide
                        #richness<-test_wide
                        
                       
                         #shannon
                          
                          
                          
                        library(vegan)
                          
#latest data 05252023
#data streams

stream_beta_cpue<-cpue_streams_fishperfish_bulk_05252023 %>%
  column_to_rownames("site_year_bout")

library(vegan)

shannondiv <- diversity(stream_beta_cpue)
head(shannondiv)

shannon_stream_fish<-as.data.frame(shannondiv)

#simpson

simpsondiv<-diversity(stream_beta_cpue,index = "simpson")
head(simpsondiv)

simpson_stream_fish<-as.data.frame(simpsondiv)

#rich 

richness<-specnumber(stream_beta_cpue)
stream_richness<-as.data.frame(richness)

# getting site means 
## richness

richness_justsites_mean<-stream_richnessh_modified_052920223 %>% group_by(Site) %>%
summarize(mean=mean(Richness))


#hill div 
hilldiv::hill_div(stream_beta_cpue,2,hills_tree)

  
                          
                          
                          
                        shannondiv <- diversity(diver_calc_sheet)
                        head(shannondiv)
                        
                        shannon_fish<-as.data.frame(shannondiv)
                        
                        #shannon enthropy 
                        
                        shannon_enthropy<-diversity(diver_calc_sheet, index = "shannon")
                        
                        #simpson
                        simpsondiv<-diversity(diver_calc_sheet,index = "simpson")
                        head(simpsondiv)
                        simpson_fish<-as.data.frame(simpsondiv)
                        
                        #invers simpson
                        inversimpdive=1/simpson_fish
                          
                        #Beta Diversity 
                        library(adespatial)
                        library(ade4)
                        
                        drain_beta<- subset(drain_beta,select = -c(Drainage))
                        # na to zeros
                        drain_beta %>% mutate_if(is.character,as.numeric)
                        d[is.na(site_total_fish_wide)] = 0
                        
                        drain_beta<-ifelse(drain_beta$Drainage >0,1,0)
                        
                        dist<-beta.pair(drain_beta,index.family = 'jaccard')
                        bd<-betadisper(dist[[3]],groups)
                        plot(bd)
                        
                        diver_calc_sheet.bd.j<-beta.div.comp(diver_calc_sheet,coef="J",quant=T)
                        
                        site_species_div<-LCBD.comp(diver_calc_sheet, method="hellinger")
                        
                       LCBD.test<- LCBD.comp(diver_calc_sheet, sqrt.D = TRUE, save.D = FALSE)
                       
                       res = beta.div(diver_calc_sheet, "percentdiff", nperm=999, clock=TRUE)
                       
                       
                       signif = which(res$p.LCBD <= 0.05) # Which are the significant LCBD indices?
                       nonsignif = which(res$p.LCBD > 0.05) # Which are the non-significant LCBD indices?
                       g1 <- s.value(diver_calc_sheet[signif,], res$LCBD[signif], ppoint.alpha = 0.5, plegend.drawKey = FALSE,
                                     symbol = "circle", col = c("white", "red"), main="Map of mite LCBD (red = significant indices)")
                       g2 <- s.value(diver_calc_sheet[nonsignif,], res$LCBD[nonsignif], ppoint.alpha = 0.5,
                                     symbol = "circle", col = c("white", "blue"))
                       g2+g1
                       }
                        #ANOVA
                        
                        sppdiv_aov <- aov(shannondiv ~ drainage, data = drainiage)
                        summary(sppdiv_aov)
                        
                        
                        sppdiv_aov <- aov(shannondiv ~ landtype, data = site_type)
                        summary(sppdiv_aov)
                        
                        
                        as.data.frame(res$p.LCBD)
                        as.data.frame(res$SCBD)
                        
                        data(mite.xy)
                        s.value(mite.xy, res$LCBD, symbol = "circle", col = c("white", "brown"), main="Map of mite LCBD")
                        library(adegraphics)
                        if(require("vegan", quietly = TRUE) & require("adegraphics", quietly = TRUE)){
                          data(diver_calc_sheet)
                          res = beta.div(diver_calc_sheet, "hellinger", nperm=999)
                          
                          beta_div_sheet.bd.j<-beta.div.comp(beta_div_sheet,coef = "J",quant = T)
                          beta_div_sheet.bd.j$part
                         
                         diver_calc_sheet.bd.s<-beta.div.comp(diver_calc_sheet,coef = "J",quant = T)
                         diver_calc_sheet.bd.s$part
                         
                          
                          class(beta_div_sheet$V2)
                          
                           data(diver.xy)
                          s.value(mite.xy, res$LCBD, symbol = "circle", col = c("white", "brown"), main="Map of mite LCBD")
                          
                        
                        
                        
                        #heat map
                        install(heat_tree)
                        library(metacoder)
                        
                        devtools::install_github("ropensci/taxa")
                        devtools::install_github("grunwaldlab/metacoder")
                        
                        load("ps_obj.Rdata")
                        
                        
                        first_pass_total_fish_perfish_bulk %>%
                          taxa::filter(total_fish_PH > 10) %>% 
                          heat_tree(node_label = scientificName,
                                    node_size = site_year_bout, 
                                    node_color = site_year_bout, 
                                    layout = "da", initial_layout = "re", 
                                    title = "Taxa in fish")
                        
                        
                        
                        
                        
                        #below doesnt workk
                       #shannon<-diversity(diver_calc_sheet,"shannon") 
                       #head(shannon)
                       
                       #simpson$value<-diversity(simpson,"simpson")
                        #divers
                        
                        #simpson.unb(TW)
                          #class(test_wide$`Pimephales promelas`)
                          #diversitycomp(TW,y=NULL)
                          
                          #TW = subset(TW, select = -c(site_year_bout))
                          
                       #dive_shannon<- diversity(diver_calc_sheet,index = "shannon")
                       #class(dive_shannon)
                          
                        #  A_dive_shannon=as.data.frame.matrix(dive_shannon) 
                          
                         # new_shannon=as.data.frame.matrix(shannon)
                          #print("dataframe after conversion :")
                          #new_shannon
                        
                          #sitenames<-pivot_wider(List_fish_T_BD,
                           #                      id_cols = site_year_bout,
                            #                     id_expand = FALSE,
                             #                    names_from = scientificName,
                              #                   names_prefix = "",
                               #                  names_sep = "_",
                                #                 names_glue = NULL,
                                 #                names_sort = FALSE,
                                  #               names_vary = "fastest",
                                   #              names_expand = FALSE,
                                    #             names_repair = "check_unique",
                                     #            values_from = total_fish_PH,
                                      #
                        #values_fill = NULL,
                                            #     values_fn = NULL,
                                              #   unused_fn = NULL
                         # )
                          
                          #above doesnt work
                          library(ggplot2)
                        library(dplyr)
                        library(tidyverse)
                          simp_lat_drain$site_year_bout<-simp_lat_drain$?..site_year_bout
                          
                          p<-ggplot(data=cor_diversity_env_data,aes(shannondiv,simpsondiv))+
                            geom_point(aes(color=factor(drainage)))
                          
                          NEON_shannon_simp<- NEON_Shannon_and_Simpson_
                          NEON_shannon_simp$shannon<-NEON_shannon_simp$`shannon score`
                          NEON_shannon_simp$simpson<-NEON_shannon_simp$`simpson score`
                          
                          
                          
                          tiff('test.tiff', units="in", width=5, height=4, res=400, compression = 'lzw')
                          
                         p<- ggplot(data=NEON_shannon_simp,aes(shannon,simpson))+
                            geom_count(aes(color=factor(ecoregion)))+
                            coord_fixed()+
                            theme_bw()+
                            scale_size_area(breaks = c(1,2))
                         
                         
                         getwd()
                         
                         p<- ggplot(data=cor_diversity_env_data,aes(shannondiv,simpsondiv))+
                           geom_count(aes(color=factor(drainage))) +
                           coord_fixed() +
                           theme_bw() +
                           scale_size_area(breaks = c(1,2))
                          
                          ggplot(data=cor_diversity_env_data,aes(shannondiv,simpsondiv))+
                            geom_point(aes(color=factor(temp)))
                          
                          ggplot(data=cor_diversity_env_data,aes(shannondiv,simpsondiv))+
                            geom_count(aes(color=factor(`mean air temp`))) +
                            coord_fixed() +
                            theme_bw() +
                            scale_size_area(breaks = c(1,2))
                          
                          ggplot(data=cor_diversity_env_data,aes(shannondiv,simpsondiv))+
                            geom_point(aes(color=factor(elevation)))
                          
                          ggplot(data=NEON_shannon_simppson,aes(shannon,simpson))+
                            geom_count(aes(color=factor(ecoregion))) +
                            coord_fixed() +
                            theme_bw() +
                            scale_size_area(breaks = c(1,2))
                          
                          
                          ggplot(data=cor_diversity_env_data,aes(shannondiv,simpsondiv))+
                            geom_point(aes(color=factor(UTM_Northing)))
                          
                          ggplot(data=cor_diversity_env_data,aes(shannondiv,simpsondiv))+
                            geom_count(aes(color=factor(UTM_Northing))) +
                            coord_fixed() +
                            theme_bw() +
                            scale_size_area(breaks = c(1,2))
                          
                          ggplot(data=X_bout_cor_diversity_env_data,aes(shannondiv,simpsondiv))+
                            geom_point(aes(color=factor(bout_season)))
                          
                          diver_grow_miss<-subset(X_bout_cor_diversity_env_data,X_bout_cor_diversity_env_data$drainage=="Mississippi")
                          
                         p<- ggplot(data=diver_grow_miss,aes(shannondiv,simpsondiv))+
                            geom_point(aes(color=factor(bout_season)))
                         
                         ggplotly(p)
                         
                         library(ggplotlyExtra)
                         
                         #ttest
                         
                         cor_diversity_env_data$site<-substr(cor_diversity_env_data$site_year_bout,1,4)
                         library(ecolTest)
                         
                         
                         t.test(arik_shannon_simpson$Beginning_Shannon, arik_shannon_simpson$End_Shannon, var.equal=TRUE)
                         
                         
                         
                         
                        # Hutcheson_t_test(
                           shannon_beg_end_ttest$beginning,
                           shannon_beg_end_ttest$end,
                           shannon.base = exp(1),
                           alternative = "two.sided",
                           difference = 0
                         )
                          
              ## fish site numbers 
                          getwd()
                          setwd("C:/Users/dmonahan/Documents/diversity/eftime_corrected")
                          beta_div_sheet= read.csv('C:/Users/dmonahan/Documents/diversity/eftime_corrected/beta_div_sheet.csv', fileEncoding="UTF-8-BOM",header=FALSE)
                          
                          beta_div_sheet %>% mutate_if(is.character,as.numeric)
                          ggplot(sites_andfishnumbers[which()])
                          
                          
                          
                          
    ## beta diversity 
                          
                          
                       #wilcocks 
                          
                          wilcoks_test<-diversity_env_data
                          
                          wilcoks_test %>%
                             wilcox.test(shannondiv~site_year_bout)
                          
                          
                          class(wilcoks_test$shannondiv)
                          
                          wilcoks_test?
                            
                          
    ##RDA
                          
                          
                          
                          
                          
          rda.var1<-rda(diversity_env_data$shannondiv~var1,data)
                          