
rm(list=ls())
#isa: had to install rcpp before
library(sf)
library(lubridate)
library(plyr)
library(dplyr)
 #You have loaded plyr after dplyr - this is likely to cause problems.If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
library(Ostats)
library(ggplot2)
library(iNEXT)
library(piecewiseSEM)
library(effects)
library(raster)
library(tidyverse)

#read in the Full species list for all parks in long format
full_sp<-read.csv("Data/sp_list_parks_cleaned.csv")%>%
         mutate(sp = replace(sp, sp == "Pennisetum polystachios", "Cenchrus polystachios"))%>%
         mutate(sp = replace(sp, sp == "Pennisetum setaceum", "Cenchrus setaceus"))%>%
         mutate(sp = replace(sp, sp == "Bauhinia variegata var. variegata", "Bauhinia variegata"))%>%
         mutate(sp = replace(sp, sp == "Schinus terebinthifolius", "Schinus terebinthifolia"))%>%
         mutate(sp = replace(sp, sp == "Pennisetum polystachion", "Cenchrus polystachios"))%>%
         mutate(sp = replace(sp, sp == "Pennisetum setaceum", "Cenchrus setaceus"))

  
fleppc<-read.csv("Data/FLEPPC_2019.csv")%>%
        dplyr::rename( sp = Scientific.Name)%>%#rename the species column to match the full_sp
        mutate(sp = replace(sp, sp == "Cenchrus purpureus (Pennisetum purpureum)", "Pennisetum purpureum"))%>%#harmonize names in fleppc list; differences 
        mutate(sp = replace(sp, sp == "Dolichandra unguis-cati (Macfadyena unguis-cati)", "Dolichandra unguis-cati"))%>%
        mutate(sp = replace(sp, sp == "Triadica sebifera (Sapium sebiferum)", "Triadica sebifera"))%>%
        mutate(sp = replace(sp, sp == "Aristolochia elegans (Aristolochia littoralis)", "Aristolochia elegans"))%>%
        mutate(sp = replace(sp, sp == "Cenchrus polystachios (Pennisetum polystachios)", "Cenchrus polystachios"))%>%
        mutate(sp = replace(sp, sp == "Cenchrus setaceus (Pennisetum setaceum)", "Cenchrus setaceus"))%>%
        mutate(sp = replace(sp, sp == "Dalechampia scandens*", "Dalechampia scandens"))%>%
        mutate(sp = replace(sp, sp == "Distimake tuberosus (Merremia tuberosa)", "Merremia tuberosa"))%>%
        mutate(sp = replace(sp, sp == "Dracaena hyacinthoides (Sansevieria hyacinthoides)", "Sansevieria hyacinthoides"))%>%
        mutate(sp = replace(sp, sp == "Epipremnum pinnatum cv.‘Aureum’", "Epipremnum pinnatum cv. Aureum"))%>%
        mutate(sp = replace(sp, sp == "Heteropterys brachtiata", "Heteropterys brachiata"))%>%
        mutate(sp = replace(sp, sp == "Koelreuteria elegans subsp. Formosana", "Koelreuteria elegans subsp. formosana"))%>%  
        mutate(sp = replace(sp, sp == "Melaleuca viminalis (Callistemon viminalis)", "Melaleuca viminalis "))%>% 
        mutate(sp = replace(sp, sp == "Talipariti tiliaceum", "Hibiscus tiliaceus var. tiliaceus"))%>% 
        mutate(sp = replace(sp, sp == "Urochloa maxima (Panicum maximum)", "Panicum maximum"))
  

#####"Sporobolus pyramidalis"	"Sporobolus jacquemontii" #which is which/ask jenn


#Obtain a list of all species (i.e., no duplicates form the full species list along with status)

sp_list<-subset(full_sp, !duplicated(sp))%>%
         dplyr::select(sp, native_status,introduced_status,invasive_status, cultivated_status,status)%>%
         left_join( .,fleppc, by = "sp")#join fleppc list by species name

#compare flepcc names to those in the data set and find fleppc species not in the species list
unshared_names<-fleppc[!fleppc$sp %in% sp_list$sp,]


#How many invasive species are in the data set? (138 species)
int<-sp_list%>%
     filter(Category =="I"| Category =='II')%>%
     dplyr::count(.)    


#joined native/invasive designation to long format species list of park assemblages
cleaned_data<-sp_list %>%
          mutate(Category = ifelse(native_status == 'Native',"Native",Category))%>% #give native designation
          mutate_at(vars(Category), ~replace_na(., "Not Native"))%>%#giv non-native designation
          filter(native_status!="Possibly Native")%>%#remove the designation "Possibly Native ~20 sp
          mutate(CatagoryII = ifelse(Category =="I"| Category =='II',"Invasive",Category))%>%
          dplyr::select(sp, Category,CatagoryII)%>%#select only species and category 
          left_join( .,full_sp, by = "sp")#join to long format community data

       
#save out data frame

write.csv(cleaned_data, "data/cleaned_data.csv")
