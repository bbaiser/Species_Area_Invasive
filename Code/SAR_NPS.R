library(tidyverse)

#import NPS species and park data from Daijiang Li

nps_data<-load("Data/nps_plant_invasive.RData")
  
#species data
species_data<-nps_plants_present2%>%
              rename(Park=park_abbr)

#park info
park_data<-park_info%>%
           rename(Park=UNIT_CODE)


rich<-species_data %>%
      count(status,Park)%>%#get species richness for each provenance by park combo
      left_join( .,park_data, by = "Park")%>%#join with park info (i.e., area, etc)
     # mutate_at(vars(status), as.factor)%>%#make category a factor
      mutate(log_area = log(area_km2))



#find sites without all three  
missing<-as.data.frame(sort(table(rich$Park)))%>%
         filter(Freq<3)
missing

#1 ALFL    2
#2 BOWA    2
#3 DEPO    2
#4 GLBA    2
#5 SITK    2
#6 SUCR    2



#test for different z-values
ancova_model <- aov(log(n) ~ log(area_km2) * status, data = rich)

car::Anova(ancova_model, type="III")


#pairwise posthoc
z<-emmeans::emtrends(ancova_model,"status", var = "log(area_km2) ")
pairs(z)

###with random effect for site
mod<-lmerTest::lmer(log(n) ~ log_area * status+(1|Park), data = rich)
summary(mod)
car::Anova(mod)

#pairwise posthoc
z<-emmeans::emtrends(mod,"status", var = "log_area")
pairs(z)

#subset each type of species for models and plots
native<-rich%>%
  filter(status=="Native")

non_native<-rich%>%
  filter(status=="Non-native")

Invasive<-rich%>%
  filter(status=="Invasive")

#Exotic<-rich%>%
 # filter(CatagoryII=="Not Native"| CatagoryII =='Invasive')




#linear models and plots

#native
summary(reg<-lm(log(n)~log(area_km2), data=native))
plot(log(n)~log(area_km2), data=native)  
abline(reg)

#non_native
summary(reg<-lm(log(n)~log(area_km2), data=non_native))
plot(log(n)~log(area_km2), data=non_native)  
abline(reg)
#invasive
summary(reg<-lm(log(n)~log(area_km2), data=Invasive))
plot(log(n)~log(area_km2), data=Invasive) 
abline(reg)
#exotic




#Plot all three lines ggplot
ggplot() +
  geom_smooth(aes(x = log(area_km2), y = log(n)), data = native, 
              method = "lm", se = FALSE, color = "green") + 
  geom_smooth(aes(x = log(area_km2), y = log(n)), data = non_native, 
              method = "lm", se = FALSE, color = "blue") + 
  geom_smooth(aes(x = log(area_km2), y = log(n)), data = Invasive, 
              method = "lm", se = FALSE, color = "purple") 



#write.csv(rich, "data/park_rich.csv")
