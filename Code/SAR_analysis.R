####Run SAR analysis#######
library(sf)
library(lubridate)
library(plyr)
library(dplyr)
library(car)
library(multcomp)



#import long fromat data from "pre_analysis.R" file
long_dat<-read.csv("Data/cleaned_data.csv")

park_data<-read.csv("Data/SFL_meta_data_cleaned.csv")

steve_data<-read.csv("Data/park_issues..SWWcomments.csv")%>%#verification of sites with data i
            filter(Keep.=="no")%>%
            dplyr::select(park_name)

reduced_long_dat<-long_dat[!long_dat$park_name %in% steve_data$park_name,]  
reduced_park_data<-park_data[!park_data$park_name %in% steve_data$park_name,]  

rich<-reduced_long_dat %>%
      count(CatagoryII,park_name)%>%#get species richness for each provenance by park combo
      left_join( .,reduced_park_data, by = "park_name")%>%#join with park info (i.e., area, etc)
      mutate_at(vars(CatagoryII), as.factor)#make catagoryII a factor

#find sites without all three  
missing<-as.data.frame(sort(table(rich$park_name)))%>%
       filter(Freq<3)




#subset each type of species
native<-rich%>%
        filter(CatagoryII=="Native")

non_native<-rich%>%
            filter(CatagoryII=="Not Native")

Invasive<-rich%>%
          filter(CatagoryII=="Invasive")

Exotic<-rich%>%
        filter(CatagoryII=="Invasive"| CatagoryII =='Not Native')


#test for different z-values
ancova_model <- aov(log(n) ~ log(size)+CatagoryII, data = rich)
Anova(ancova_model, type="III")

#pairwise posthoc
postHocs <- glht(ancova_model, linfct = mcp(CatagoryII = "Tukey"))

TukeyHSD(ancova_model, conf.level=.95)
summary(postHocs)

#linear models

summary(lm(log(n)~log(size), data=native))

summary(lm(log(n)~log(size), data=non_native))
summary(lm(log(n)~log(size), data=Invasive))
summary(lm(log(n)~log(size), data=Exotic))
plot(log(n)~log(size), data=Invasive)  

plot(park_data$sp_num~log(park_data$size))

write.csv(rich, "data/park_rich.csv")
