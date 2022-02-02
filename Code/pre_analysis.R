library(tidyverse)

#isa: had to install rcpp before
library(sf)
library(lubridate)
library(plyr) #You have loaded plyr after dplyr - this is likely to cause problems.If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
library(Ostats)
library(ggplot2)
library(iNEXT)
library(piecewiseSEM)
library(effects)
library(raster)


#read in the Full species list for all parks in long format
full_sp<-read.csv("Data/sp_list_parks_cleaned.csv")

fleppc<-read.csv("Data/FLEPPC_2019.csv")%>%
        rename(., sp = Scientific.Name)#rename the species column to match the full_sp

#Obtain a list of all species (i.e., no duplicates form the full species list along with status)

sp_list<-subset(full_sp, !duplicated(sp))%>%
         dplyr::select(sp, native_status,introduced_status,invasive_status, cultivated_status,status)%>%
         left_join( .,fleppc, by = "sp")#join fleppc list by species name


#compare flepcc names to those in the data set
#filter out all duplicate rows (i.e., both pairs of the duplicate) from the svl data
unshared_names<-fleppc[!fleppc$sp %in% sp_list$sp,]

#diff names

#sp list                            #fleppc
"Bauhinia variegata var. variegata" == "Bauhinia variegata"# drop var....
"Pennisetum purpureum" == "Cenchrus purpureus (Pennisetum purpureum)" #drop Cenchrus purpureus
"Dolichandra unguis-cat" == "Dolichandra unguis-cati (Macfadyena unguis-cati)"#drop macadyena....
"Lantana camara" == "Lantana strigocamara" #change to strigocamara
"Schinus terebinthifolius" == "Schinus terebinthifolia"#replace folius with folia

"Sporobolus pyramidalis"	"Sporobolus jacquemontii" #which is which/ask jenn

"Triadica sebifera" =="Triadica sebifera (Sapium sebiferum)"# drop (Sapium sebiferum)

"Aristolochia elegans"== "Aristolochia elegans (Aristolochia littoralis)" #drop(Aristolochia littoralis)

"Pennisetum polystachion"== "	Cenchrus polystachios (Pennisetum polystachios)"#change both to Cenchrus polystachios
"Pennisetum setaceum"== "Cenchrus setaceus (Pennisetum setaceum)" #change both to Cenchrus setaceus 

"Dalechampia scandens" == "Dalechampia scandens*" # remove asterisk

"Merremia tuberosa" =="Distimake tuberosus (Merremia tuberosa)"# change Distimake tuberosus(Merremia tuberosa) to Merremia tuberosa) to
"Sansevieria hyacinthoides" == "Dracaena hyacinthoides (Sansevieria hyacinthoides)"# change Dracaena hyacinthoides (Sansevieria hyacinthoides to Sansevieria hyacinthoides

"Epipremnum pinnatum cv. Aureum" == "Epipremnum pinnatum cv.‘Aureum’"# remove quotations from fleppc

"Heteropterys brachiata"=="Heteropterys brachtiata" # fleppc mispelled

"Koelreuteria elegans subsp. formosana"== 	" Koelreuteria elegans subsp. Formosana" #change capitl for F
"Melaleuca viminalis"== "Melaleuca viminalis (Callistemon viminalis)" # DROP (Callistemon viminalis)
"Hibiscus tiliaceus var. tiliaceus"       ==      "Talipariti tiliaceum"# Change to Hibiscus tiliaceus var. tiliaceus

"Panicum maximum" =="Urochloa maxima (Panicum maximum)"# change to Panicum maximum
  
  
  
  #How many invasive species are in the data set?
int<-sp_list%>%
     filter(Category =="I"| Category =='II')%>%
     count(.)    

int #122 invasive species 

#sp_list[sp_list$Category == "NA" & sp_list$native_status == "Native", "NA"] <- "Native"

new<-sp_list %>%
    mutate(Category = ifelse(native_status == 'Native',"Native",Category))%>%
    mutate_at(vars(Category), ~replace_na(., "Not Native"))
    
    mutate(Category = ifelse(native_status == 'Not Native' & Category!="I"| Category !='II',"Not Native",Category))
  
str(new)

sp_list %>% mutate(Category = ifelse(native_status == "Native", "Native", Category))

sp_list[sp_list$native_status=="Native" ,] <- 3


#filter taxa list keeping targeted rodents with species or subspecies designations (Note that T&E species do not have sp designations)
tax_reduced2<-tax%>%
  filter(order == 'Rodentia',taxonProtocolCategory == 'target', taxonRank%in% c('species','subspecies'))#keeping target, rodents with species designation


###WHAT TO DO WITH SUBSPECIES? HERE WE KEEP THEM


#make tibbles
mam_dat<-lapply(neondata, as_tibble)
head(mam_dat)

#variable descriptions from NEON, i.e. metadata
#vars<-mam_dat$variables_10072

#this is the tibble with the itv data 
itv_mammal_data<-mam_dat$mam_pertrapnight
colnames(itv_mammal_data)

####clean data#####

mammal_dataT <- itv_mammal_data%>%
  filter(taxonID%in%tax_reduced2$taxonID)%>% #take taxa from neon taxa filtered list #isa: is tax_reduced2 and not tax_reduced
  mutate(year = year(collectDate), logweight=log(weight))%>% #CREATE YEAR COLUMN and log weight column
  filter(lifeStage=="adult")%>% #Keep only adults
  group_by(tagID) %>% #group individuals by tag (i.e., recaptures) 
  filter(collectDate==min(collectDate))%>% #for recaptures, take the earliest record
  ungroup()%>%
  mutate(tax_Site = paste(taxonID, siteID, sep = "_")) #make a unique species-by-site identifier called "tax_Site"

mammal_dataT <- mammal_dataT %>%
  filter(!is.na(logweight))