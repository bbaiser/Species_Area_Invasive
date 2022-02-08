####Run SAR analysis#######

#import long fromat data from "pre_analysis.R" file
long_dat<-read.csv("Data/cleaned_data.csv")

park_data<-read.csv("Data/SFL_meta_data_cleaned.csv")

rich<-long_dat %>%
      count(CatagoryII,park_name)%>%#get species richness for each provenance by park combo
      left_join( .,park_data, by = "park_name")#join with park info (i.e., area, etc)

#find sites without 
missing<-as.data.frame(sort(table(rich$park_name)))%>%
       filter(Freq<3)




#subset each type of species
native<-rich%>%
        filter(CatagoryII=="Native")

non_native<-rich%>%
            filter(CatagoryII=="Not Native")

Invasive<-rich%>%
            filter(CatagoryII=="Invasive")

#linear models

summary(lm(log(n)~log(size), data=native))

summary(lm(log(n)~log(size), data=non_native))
summary(lm(log(n)~log(size), data=Invasive))

plot(log(n)~log(size), data=non_native)  

plot(park_data$sp_num~log(park_data$size)

     