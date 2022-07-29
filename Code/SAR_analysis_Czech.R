###Run SAR analysis###
library(sf)
library(lubridate)
library(plyr)
library(dplyr)
library(car)
library(multcomp)
library(ggplot2)
library(rstatix)
library(emmeans)
library(lme4)
library(lmerTest)

####call in and clean data####
#import long format data from "pre_analysis.R" file
long_dat<-read.csv("Data/cleaned_czech_data.csv", row=1)

#import park data
park_data<-read.csv("Data/Czech_Republic_Parks.csv")


#get species richness for native, exotic, invasive not including age (i.e., archeophyte, neophyte)
rich<-long_dat %>%
      count(Invasion.statusII,ID)%>%#get species richness for each provenance by park combo
      left_join( .,park_data, by = "ID")%>%#join with park info (i.e., area, etc)
      mutate_at(vars(Invasion.statusII), as.factor)%>%#make category a factor
      mutate(log_area = log(Area..ha./100))#log transform area

#get species richness for native, exotic, invasive including age (i.e., archeophyte, neophyte)

rich_age<-long_dat %>%
      count(Invasion.statusIII,ID)%>%#get species richness for each provenance by park combo
      left_join( .,park_data, by = "ID")%>%#join with park info (i.e., area, etc)
      mutate_at(vars(Invasion.statusIII), as.factor)%>%#make category a factor
      mutate(log_area = log(Area..ha.))#log transform area

###
#FOR EXOTIC RICHNESS (combine invasive and non-native)
#rich_exotic<-reduced_long_dat %>%
 #            count(CatagoryIII,park_name)%>%#get species richness for each provenance by park combo
  #           left_join( .,reduced_park_data, by = "park_name")%>%#join with park info (i.e., area, etc)
   #          mutate_at(vars(CatagoryIII), as.factor)%>%#make catagoryII a factor
    #         mutate(log_area = log(size))
###


####analysis not considering age####

dat<-rich# not considering age

#test for different z-values
ancova_model <- aov(log(n) ~ log_area * Invasion.statusII, data = dat)
Anova(ancova_model, type="III")

#pairwise posthoc
z<-emtrends(ancova_model,"Invasion.statusII", var = "log_area")
pairs(z)


###with random effect for site
mod<-lmerTest::lmer(log(n) ~ log_area * Invasion.statusII+(1|ID), data = dat)
summary(mod)
Anova(mod)

#pairwise posthoc
z<-emtrends(mod,"Invasion.statusII", var = "log_area")
pairs(z)


###subset each type of species for models and plots from rich (i.e., not considering origin)
native<-dat%>%
        filter(Invasion.statusII=="native")

non_native<-dat%>%
            filter(Invasion.statusII=="non-native")

Invasive<-dat%>%
          filter(Invasion.statusII=="invasive")



#######linear models and plots

#native
summary(lm(log(n)~log_area, data=native))
plot(log(n)~log_area, data=native)  
abline(lm(log(n)~log_area, data=native))

#non_native
summary(lm(log(n)~log_area, data=non_native))
plot(log(n)~log_area, data=non_native)  
abline(lm(log(n)~log_area, data=non_native))

#invasive
summary(lm(log(n)~log_area, data=Invasive))
plot(log(n)~log_area, data=Invasive) 
abline(lm(log(n)~log_area, data=Invasive))

#exotic
#summary(lm(log(n)~log_area, data=Exotic))
#plot(log(n)~log_area, data=Exotic) 

#to calculate species richness from sar equation
s=79*600000^0.12836
log(s)

#Plot all three lines ggplot
ggplot() +
  geom_smooth(aes(x = log_area, y = log(n)), data = native, 
              method = "lm", se = FALSE, color = "green") + 
  geom_smooth(aes(x = log_area, y = log(n)), data = non_native, 
              method = "lm", se = FALSE, color = "blue") + 
  geom_smooth(aes(x = log_area, y = log(n)), data = Invasive, 
              method = "lm", se = FALSE, color = "purple")

#####################################################################

###data inputs for all analyses below

#subset data for neophytes

dat<-rich_age%>%
     filter(Invasion.statusIII=='non-native neophyte'| Invasion.statusIII=='invasive neophyte'|Invasion.statusIII=='native native')
  
      
#subset data for archeophytes

dat<-rich_age%>%
     filter(Invasion.statusIII=='non-native archaeophyte'| Invasion.statusIII=='invasive archaeophyte'|Invasion.statusIII=='native native')




#test for different z-values
ancova_model <- aov(log(n) ~ log_area * Invasion.statusIII, data = dat)
Anova(ancova_model, type="III")

#pairwise posthoc
z<-emtrends(ancova_model,"Invasion.statusIII", var = "log_area")
pairs(z)


###with random effect for site
mod<-lmerTest::lmer(log(n) ~ log_area * Invasion.statusIII+(1|ID), data = dat)
summary(mod)
Anova(mod)

#pairwise posthoc
z<-emtrends(mod,"Invasion.statusIII", var = "log_area")
pairs(z)

#subset each type of species for models and plots from rich_org (i.e.,  considering age)
native2<-dat%>%
         filter(Invasion.statusIII=="native native")

non_native_arc<-dat%>%
                filter(Invasion.statusIII=="non-native archaeophyte")

Invasive_arc<-dat%>%
              filter(Invasion.statusIII=="invasive archaeophyte")

non_native_neo<-dat%>%
                filter(Invasion.statusIII=="non-native neophyte")

Invasive_neo<-dat%>%
              filter(Invasion.statusIII=="invasive neophyte")

#linear models and plots
#native
summary(lm(log(n)~log_area, data=native2))
plot(log(n)~log_area, data=native2) 
abline(lm(log(n)~log_area, data=native2))

#non_native_arc
summary(lm(log(n)~log_area, data=non_native_arc))
plot(log(n)~log_area, data=non_native_arc)  
abline(lm(log(n)~log_area, data=non_native_arc))

#invasive_arc
summary(lm(log(n)~log_area, data=Invasive_arc))
plot(log(n)~log_area, data=Invasive_arc) 
abline(lm(log(n)~log_area, data=Invasive_arc))

#non_native_neo
summary(lm(log(n)~log_area, data=non_native_neo))
plot(log(n)~log_area, data=non_native_neo)  
abline(lm(log(n)~log_area, data=non_native_neo))

#invasive_neo
summary(lm(log(n)~log_area, data=Invasive_neo))
plot(log(n)~log_area, data=Invasive_neo) 
abline(lm(log(n)~log_area, data=Invasive_neo))


#Plot all three lines ggplot
#arc


6.925e-02 +6.648e-03 

#neo
ggplot() +
  geom_smooth(aes(x = log_area, y = log(n)), data = native2, 
              method = "lm", se = FALSE, color = "green") + 
  geom_smooth(aes(x = log_area, y = log(n)), data = non_native_neo, 
              method = "lm", se = FALSE, color = "blue") + 
  geom_smooth(aes(x = log_area, y = log(n)), data = Invasive_neo, 
              method = "lm", se = FALSE, color = "purple") 
#arc
ggplot() +
  geom_smooth(aes(x = log_area, y = log(n)), data = native2, 
              method = "lm", se = FALSE, color = "green") + 
  geom_smooth(aes(x = log_area, y = log(n)), data = non_native_arc, 
              method = "lm", se = FALSE, color = "blue") + 
  geom_smooth(aes(x = log_area, y = log(n)), data = Invasive_arc, 
              method = "lm", se = FALSE, color = "purple") 

