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

####call in and clean data####

#species data
long_dat<-read.csv("Data/cleaned_data.csv")

#park data
park_data<-read.csv("Data/SFL_meta_data_cleaned.csv")%>%
           rename(size = size..acres.)
  
#expert information about sites with insufficient data
steve_data<-read.csv("Data/park_issues..SWWcomments.csv")%>%#verification of sites with data i
            filter(Keep.=="no")%>%
            dplyr::select(park_name)

#remove parks that have insufficient data from the long_data
reduced_long_dat<-long_dat[!long_dat$park_name %in% steve_data$park_name,] 


#remove parks that have insufficient data from the park_data
reduced_park_data<-park_data[!park_data$park_name %in% steve_data$park_name,] 


#Obtain richness for native/invasive/nonnative for each park and join park data
rich<-reduced_long_dat %>%
      count(CatagoryII,park_name)%>%#get species richness for each provenance by park combo
      left_join( .,reduced_park_data, by = "park_name")%>%#join with park info (i.e., area, etc)
      mutate_at(vars(CatagoryII), as.factor)%>%#make catagoryII a factor
      mutate(log_area = log(size/247.1))#convert acre to km2

#same as above but lumping non-native and invasive into 
rich_exotic<-reduced_long_dat %>%
             count(CatagoryIII,park_name)%>%#get species richness for each provenance by park combo
             left_join( .,reduced_park_data, by = "park_name")%>%#join with park info (i.e., area, etc)
             mutate_at(vars(CatagoryIII), as.factor)%>%#make catagoryII a factor
             mutate(log_area = log(size))

#make sure all sites have native/non-native/invasive species
missing<-as.data.frame(sort(table(rich$park_name)))%>%
        filter(Freq<3)
missing



###test for differnces in z-values with random effect for site
mod<-lmerTest::lmer(log(n) ~ log_area * CatagoryII+ (1|park_name), data = rich)
summary(mod)
Anova(mod)

#pairwise post-hoc test
z<-emtrends(mod,"CatagoryII", var = "log_area")
pairs(z)

#subset each group of species (native/non-native/invasive/exotic) for models and plots
native<-rich%>%
  filter(CatagoryII=="Native")

non_native<-rich%>%
  filter(CatagoryII=="Not Native")

Invasive<-rich%>%
  filter(CatagoryII=="Invasive")

Exotic<-rich%>%
  filter(CatagoryII=="Not Native"| CatagoryII =='Invasive')


#linear models and plots
#native
summary(lm(log(n)~log(size), data=native))
plot(log(n)~log(size), data=native)  

#non_native
summary(lm(log(n)~log(size), data=non_native))
plot(log(n)~log(size), data=non_native)  

#invasive
summary(lm(log(n)~log(size), data=Invasive))
plot(log(n)~log(size), data=Invasive) 

#exotic
summary(lm(log(n)~log(size), data=Exotic))
plot(log(n)~log(size), data=Exotic) 



#Plot all three (native) lines ggplot

SAR_fisf_plot <- ggplot() +
                labs(y = "log (Species Richness)", x = expression(log(Area)~km^2)) +
                geom_smooth(aes(x = log(size), y = log(n)), data = native, 
                            method = "lm", se = FALSE, color = "green") + 
                geom_smooth(aes(x = log(size), y = log(n)), data = non_native, 
                            method = "lm", se = FALSE, color = "blue") + 
                geom_smooth(aes(x = log(size), y = log(n)), data = Invasive, 
                            method = "lm", se = FALSE, color = "purple")






