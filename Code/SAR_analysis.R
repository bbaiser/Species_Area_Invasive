####Run SAR analysis#######
library(sf)
library(lubridate)
library(plyr)
library(dplyr)
library(car)
library(multcomp)
library(ggplot2)
library(rstatix)


#import long fromat data from "pre_analysis.R" file
long_dat<-read.csv("Data/cleaned_data.csv")

park_data<-read.csv("Data/SFL_meta_data_cleaned.csv")

steve_data<-read.csv("Data/park_issues..SWWcomments.csv")%>%#verification of sites with data i
            filter(Keep.=="no")%>%
            dplyr::select(park_name)

#remove parks that have insufficient data according to steve from the long_data
reduced_long_dat<-long_dat[!long_dat$park_name %in% steve_data$park_name,] 


#remove parks that have insufficient data according to steve from the long_data
reduced_park_data<-park_data[!park_data$park_name %in% steve_data$park_name,] 


####to separate mainland and keys data
mainland_park_data<-reduced_park_data%>%
          filter(Keys=="Keys")

mainland_long_dat<-reduced_long_dat[reduced_long_dat$park_name %in% mainland_park_data$park_name,]

rich<-mainland_long_dat %>%
      count(CatagoryII,park_name)%>%#get species richness for each provenance by park combo
      left_join( .,mainland_park_data, by = "park_name")%>%#join with park info (i.e., area, etc)
      mutate_at(vars(CatagoryII), as.factor)%>%#make category a factor
      mutate(log_area = log(size))

#not removing keys data
rich<-reduced_long_dat %>%
     count(CatagoryII,park_name)%>%#get species richness for each provenance by park combo
      left_join( .,reduced_park_data, by = "park_name")%>%#join with park info (i.e., area, etc)
      mutate_at(vars(CatagoryII), as.factor)%>%#make catagoryII a factor
      mutate(log_area = log(size))

#FOR EXOTIC RICHNESS
rich_exotic<-reduced_long_dat %>%
             count(CatagoryIII,park_name)%>%#get species richness for each provenance by park combo
             left_join( .,reduced_park_data, by = "park_name")%>%#join with park info (i.e., area, etc)
             mutate_at(vars(CatagoryIII), as.factor)%>%#make catagoryII a factor
             mutate(log_area = log(size))

#find sites without all three  
missing<-as.data.frame(sort(table(rich$park_name)))%>%
        filter(Freq<3)
missing


exp(.14)

(1.14^0.14– 1) * 100 = 3.7 

(1.14^0.14 -1)*100
#test for different z-values
ancova_model <- aov(log(n) ~ log_area * CatagoryII, data = rich)

Anova(ancova_model, type="III")



#pairwise posthoc

z<-emtrends(ancova_model,"CatagoryII", var = "log_area")
pairs(z)




#subset each type of species for models and plots
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
s=79*600000^0.12836
log(s)
#non_native
summary(lm(log(n)~log(size), data=non_native))
plot(log(n)~log(size), data=non_native)  

#invasive
summary(lm(log(n)~log(size), data=Invasive))
plot(log(n)~log(size), data=Invasive) 

#exotic
summary(lm(log(n)~log(size), data=Exotic))
plot(log(n)~log(size), data=Exotic) 



#Plot all three lines ggplot
ggplot() +
  geom_smooth(aes(x = log(size), y = log(n)), data = native, 
              method = "lm", se = FALSE, color = "green") + 
  geom_smooth(aes(x = log(size), y = log(n)), data = non_native, 
              method = "lm", se = FALSE, color = "blue") + 
  geom_smooth(aes(x = log(size), y = log(n)), data = Invasive, 
              method = "lm", se = FALSE, color = "purple") 



 # geom_point(aes(x = year, y = slr), data = brest1, color = "red") + 
  #geom_point(aes(x = year, y = slr), data = brest2, color = "blue")
#write.csv(rich, "data/park_rich.csv")
