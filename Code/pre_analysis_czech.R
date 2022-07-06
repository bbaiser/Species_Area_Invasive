
rm(list=ls())

#load packages
library(sf)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

#full data set on species presence
raw_spec<-read.csv("Data/Czech_Republic_sp_lists.csv")


#convert to long form
data_long <- raw_spec %>%                                  
             gather(key= "site", value="presence",10:311)%>% 
             filter(!is.na(presence))%>% #get rid of species-site combos that don't occur
             mutate(ID =as.numeric(str_remove(site, "res_id_")))
data_long 



#make site by species matrix (realized I don;t need to)
#site_by_species<-raw_spec[,c(3,10:311)]%>% # remove rows that are not presence or species names
                # mutate_all(~replace(., is.na(.), 0))%>%#replace NAs with zeros
                 #t() %>% #transpose to site by species matrix
                 #as.data.frame() %>% #make into data frame
                 #setNames(raw_spec[,3]) %>% #make column names after species names from original data
                 #slice(-1)#remove first row of data which are column names



#How many invasive species are in the data set? (43 species)
int<-data_long%>%
    filter(Invasion.status.2012 =="invasive")

length(unique(int$species_name_2002))  


#How many naturalized species are in the data set? (221 species)
nat<-data_long%>%
  filter(Invasion.status.2012 =="naturalized")

length(unique(nat$species_name_2002))  

#How many "casual" species are in the data set? (78 species)
cas<-data_long%>%
  filter(Invasion.status.2012 =="casual")

length(unique(cas$species_name_2002))  



write.csv(data_long, "data/cleaned_czech_data.csv")


