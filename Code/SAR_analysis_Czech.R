####Run SAR analysis#######
library(sf)
library(lubridate)
library(plyr)
library(dplyr)
library(car)
library(multcomp)
library(ggplot2)
library(rstatix)
library(emmeans)

#import long format data from "pre_analysis.R" file
long_dat<-read.csv("Data/cleaned_czech_data.csv", row=1)

park_data<-read.csv("Data/Czech_Republic_Parks.csv")
colnames(park_data)


#get species richness for native, exotic, invasive (not including origin)

rich<-long_dat %>%
      count(Invasion.statusII,ID)%>%#get species richness for each provenance by park combo
      left_join( .,park_data, by = "ID")%>%#join with park info (i.e., area, etc)
      mutate_at(vars(Invasion.statusII), as.factor)%>%#make category a factor
      mutate(log_area = log(Area..ha.))

#get species richness for native, exotic, invasive including origin

rich_org<-long_dat %>%
      count(Invasion.statusIII,ID)%>%#get species richness for each provenance by park combo
      left_join( .,park_data, by = "ID")%>%#join with park info (i.e., area, etc)
      mutate_at(vars(Invasion.statusIII), as.factor)%>%#make category a factor
      mutate(log_area = log(Area..ha.))

#####################
#FOR EXOTIC RICHNESS (combine invasive and non-native)
#rich_exotic<-reduced_long_dat %>%
 #            count(CatagoryIII,park_name)%>%#get species richness for each provenance by park combo
  #           left_join( .,reduced_park_data, by = "park_name")%>%#join with park info (i.e., area, etc)
   #          mutate_at(vars(CatagoryIII), as.factor)%>%#make catagoryII a factor
    #         mutate(log_area = log(size))
######################################
#find sites without all three  
missing<-as.data.frame(sort(table(rich$ID)))%>%
        filter(Freq<3)
missing



#test for different z-values
ancova_model <- aov(log(n) ~ log_area * Invasion.statusIII, data = rich_org)

Anova(ancova_model, type="III")



#pairwise posthoc
z<-emtrends(ancova_model,"Invasion.statusIII", var = "log_area")
pairs(z)




#subset each type of species for models and plots from rich_org (i.e., not considering origin)
native2<-rich_org%>%
         filter(Invasion.statusIII=="native native")

non_native_arc<-rich_org%>%
                filter(Invasion.statusIII=="Exotic archaeophyte")

Invasive_arc<-rich_org%>%
              filter(Invasion.statusIII=="invasive archaeophyte")

non_native_neo<-rich_org%>%
                filter(Invasion.statusIII=="Exotic neophyte")

Invasive_neo<-rich_org%>%
              filter(Invasion.statusIII=="invasive neophyte")

#linear models and plots

#native
summary(lm(log(n)~log_area, data=native2))
plot(log(n)~log_area, data=native2) 

#non_native_arc
summary(lm(log(n)~log_area, data=non_native_arc))
plot(log(n)~log_area, data=non_native_arc)  

#invasive_arc
summary(lm(log(n)~log_area, data=Invasive_arc))
plot(log(n)~log_area, data=Invasive_arc) 

#non_native_neo
summary(lm(log(n)~log_area, data=non_native_neo))
plot(log(n)~log_area, data=non_native_neo)  

#invasive_neo
summary(lm(log(n)~log_area, data=Invasive_neo))
plot(log(n)~log_area, data=Invasive_neo) 


#subset each type of species for models and plots from rich (i.e., not considering origin)
native<-rich%>%
        filter(Invasion.statusII=="native")

non_native<-rich%>%
            filter(Invasion.statusII=="Exotic")

Invasive<-rich%>%
          filter(Invasion.statusII=="invasive")


#linear models and plots

#native
summary(lm(log(n)~log_area, data=native))
plot(log(n)~log_area, data=native)  

s=79*600000^0.12836
log(s)

#non_native
summary(lm(log(n)~log_area, data=non_native))
plot(log(n)~log_area, data=non_native)  

#invasive
summary(lm(log(n)~log_area, data=Invasive))
plot(log(n)~log_area, data=Invasive) 

#exotic
summary(lm(log(n)~log_area, data=Exotic))
plot(log(n)~log_area, data=Exotic) 



#Plot all three lines ggplot
ggplot() +
  geom_smooth(aes(x = log_area, y = log(n)), data = native, 
              method = "lm", se = FALSE, color = "green") + 
  geom_smooth(aes(x = log_area, y = log(n)), data = non_native, 
              method = "lm", se = FALSE, color = "blue") + 
  geom_smooth(aes(x = log_area, y = log(n)), data = Invasive, 
              method = "lm", se = FALSE, color = "purple") 



 # geom_point(aes(x = year, y = slr), data = brest1, color = "red") + 
  #geom_point(aes(x = year, y = slr), data = brest2, color = "blue")
#write.csv(rich, "data/park_rich.csv")
