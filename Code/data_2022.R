# Things get complicated with more and more data.
# overall, 3 source of data: park data of SFL, county data of SFL, county data of FL (Atlas)
# sp status (native/introduced/cultivated) was decided from the 1st and 3rd above source;
# with native/introduced status from Atlas as the main source, i.e. when one sp present in
# both park data of SFL and Atlas, we use the status from the Atlas.
# These two status are mostly the same.

# At the end, here are the objects important for later analyses:

### sp list data
# sp_all_status_atlas_sfl #-- status for all sp from all source
# sp_list_all_cleaned     #-- sp list of parks of SFL, updated with status
# sp_list_county_cleaned  #-- sp list of counties of SFL
# county_FL               #-- sp list of all counties of FL, from Atlas

### site data
# meta_data_combined      #-- meta data for parks of SFL
# habitats_long           #-- vegetation type for parks of SFL
# area                    #-- area of all counties and states of USA
# area_county_FL          #-- area of all counties in FL



############ split line #################################################################
## Read data ----

#### read data from SFL parks
domain = "https://regionalconservation.org/ircs/database/"

sfl_all = readLines("https://regionalconservation.org/ircs/database/site/ConservationAreas.asp")
str(sfl_all)
cons_area_names = sfl_all[str_detect(string = sfl_all,
                                     pattern = "^.*/plants/ByConsArea\\.asp\\?SiteID=.*$")] %>%
  str_replace(pattern = ".*(plants/ByConsArea.*)\">.*$",
              replacement = "\\1")
park_names = gsub(x = cons_area_names, pattern = ".*SN=(.*)$", "\\1")

websites = paste0(domain, cons_area_names %>% str_replace_all("'", "%27")
                  %>% str_replace_all(" ", "%20"))
# 27% for '; %20 for space

# conservation 2020 sites ### are problematic and have no data, removed
results = vector(mode = "list", length = length(websites))
names(results) = park_names
for(i in seq_along(websites)){
  # for(i in 366:432){
  print(i)
  url = websites[i]
  # get the total number of sp and park name
  park_name = park_names[i]
  web = rvest::read_html(url)
  sp_num = web %>% html_element(css = ".titleMain") %>% html_text2() # number of sp reported
  sp_num = as.numeric(gsub(pattern = "^.*There are (.*) taxa reported.*$",
                           replacement = "\\1", x = sp_num))
  web_tables = web %>% html_table(header = TRUE) # number of sp reported
  
  metadata = names(web_tables[[2]])
  metadata = str_remove_all(metadata, "\\s") %>% 
    str_replace("Size", ";Size") %>% 
    str_replace("Latitude", ";Latitude") %>% 
    str_replace("Longitude", ";Longitude") %>% 
    str_replace("Section", ";Section") %>% 
    str_replace("Township", ";Township") %>% 
    str_replace("Range", ";Range") %>% 
    str_replace("Notes", ";Notes") %>% 
    str_replace_all(":", ": ") 
  
  metadata_df = tibble(
    county = gsub(pattern = ".*County: ([^;]*);.*", "\\1", x = metadata),
    size = gsub(pattern = ".*Size: ([^;]*);.*", "\\1", x = metadata),
    latitude = gsub(pattern = ".*Latitude: ([^;]*);.*", "\\1", x = metadata),
    longitude = gsub(pattern = ".*Longitude: ([^;]*);.*", "\\1", x = metadata),
    sp_num = sp_num, park_name = park_name
  )
  
  if(length(web_tables) >= 4){
    # main species list
    sp_park = web_tables[[4]]
    names(sp_park) = gsub(":", "", names(sp_park))
    names(sp_park) = gsub(" ", "_", names(sp_park)) %>% tolower()
    sp_park$scientific_name = gsub("^.*Scientific Name: (.*)Occurrence.*$", "\\1", 
                                   x = sp_park$scientific_name) %>% 
      str_trim()
    # some parks do not have Occurrence, e.g. Amberjack Slough
    sp_park$scientific_name = gsub("^.*Scientific Name: (.*)Reference.*$", "\\1", 
                                   x = sp_park$scientific_name) %>% 
      str_trim()
    
  } else {
    sp_park = NA
  }
  
  output = list(meta_data = metadata_df, sp_list = sp_park)
  
  results[[i]] = output
  # names(results)[i] = park_name
}

saveRDS(results, file = "output_data/Sounth_FL_plant_list.rds")

results = readRDS("output_data copy/SFL_County/Sounth_FL_plant_list.rds")
results[[1]]
map_chr(meta_data, class) %>% unname()

# combine all meta data into one file
meta_data = map_dfr(results, "meta_data")

sp_list = results[!is.na(map(results, "sp_list"))] %>% 
  map("sp_list") %>% 
  bind_rows(.id = "park_name")

write_csv(sp_list, file = "SFL_sp_list.csv")
write_csv(meta_data, file = "SFL_meta_data.csv")

library(tidyverse)
meta_data = read_csv("SFL_meta_data.csv")
(filter(meta_data, !is.na(sp_num)))
meta_data = filter(meta_data, !is.na(sp_num))
hist(meta_data$sp_num)
sort(table(meta_data$sp_num))
sort(meta_data$sp_num)
# remove parks with less than 5 species
meta_data = filter(meta_data, sp_num >= 5)

meta_data$size[meta_data$park_name == "Castellow Hammock parcel 31"] = "112acres"
# http://www.miamidade.gov/parks/castello-hammock.asp
meta_data$size[meta_data$park_name == "John Kunkel Small Pineland"] = "0.7acres"
# http://regionalconservation.org/ircs/pdf/publications/2008_2.pdf   page 10
meta_data$size[meta_data$park_name == "LaBelle Nature Park"] = "9.0acres"
# http://www.labellenaturepark.org/?page=about   the nine-acre park...
meta_data$size[meta_data$park_name == "Nancy Payton Preserve"] = "71acres"
# http://www.colliergov.net/your-government/divisions-a-e/conservation-collier/preserve-information/nancy-payton-preserve
meta_data$size[meta_data$park_name == "Railhead Scrub Preserve"] = "132acres"
# http://www.colliergov.net/your-government/divisions-a-e/conservation-collier/preserve-information/railhead-scrub-preserve
meta_data$size[meta_data$park_name == "Wet Woods Preserve"] = "26.77acres"
# http://www.colliergov.net/home/showdocument?id=18810 wetlands 15.53, uplands 11.24

filter(meta_data, !grepl("acres", size)) # remove 13 parks with no size area
meta_data = filter(meta_data, grepl("acres", size))
meta_data$size = str_extract(meta_data$size, "[.0-9]+acres") %>% 
  str_remove("acres") %>% 
  as.numeric()
meta_data$county = str_replace_all(meta_data$county, pattern = "County", replacement =  " ") %>% 
  str_replace_all(" *,", ", ") %>% 
  str_trim() %>% 
  str_replace_all("PalmBeach", "Palm Beach")
str_detect(meta_data$latitude, "º")
grep("º", meta_data$latitude, value = T, invert = F)
meta_data = mutate(meta_data,
                   latitude = ifelse(str_detect(latitude, "º"), latitude, NA),
                   latitude = str_replace(latitude, "º", ""),
                   latitude = as.numeric(latitude))
grep("º", meta_data$longitude, value = T, invert = F)
meta_data = mutate(meta_data,
                   longitude = ifelse(str_detect(longitude, "º"), longitude, NA),
                   longitude = str_extract(longitude, "-[0-9.]*º"),
                   longitude = str_replace(longitude, "º", ""),
                   longitude = as.numeric(longitude))
write_csv(meta_data, file = "SFL_meta_data_cleaned.csv")

###### Sp list data for all parks #############
sp_list_all = read_csv("SFL_sp_list.csv")
names(sp_list_all)
# get rid of parks without meta data
length(intersect(unique(sp_list_all$park_name), meta_data$park_name)) # 278
sp_list_all = filter(sp_list_all, park_name %in% meta_data$park_name)

table(sp_list_all$occurrence)
sp_list_all = filter(sp_list_all, !occurrence %in% c("Absent", "Recorded as Present in Error"))
table(sp_list_all$native_status)
table(sp_list_all$introduced_status)
sp_list_all = filter(sp_list_all, !native_status %in% 
                       c("Undetermined Nativity", # 1
                         "Possibly Extirpated", # 468
                         "Presumed Extirpated", # 142
                         "Native, Extirpated, now Reintroduced" # only 1
                       ))
write.csv(sp_list_all, file = "sp_list_parks_not_cleaned.csv", row.names = F)

## clean sp native/introduced status ----
# sp list from parks data in SFL
sp_list_status = select(sp_list_all, sp = scientific_name,
                        native_status, introduced_status, invasive_status,
                        cultivated_status) %>% unique() %>% arrange(sp)
n_distinct(sp_list_status$sp) # 2420
head(sp_list_status)
tail(sp_list_status)
# unique(sp_list_status$native_status)
# unique(sp_list_status$introduced_status)
# unique(sp_list_status$invasive_status)
# unique(sp_list_status$cultivated_status)

sp_list_status$naturalized_status = NA
sp_list_status$extirpated_status = NA
table(sp_list_status$native_status)
sp_list_status$native_status[sp_list_status$native_status %in%
                               c("Doubtfully Native", "Assumed to be Native")] = "Possibly Native"
sp_list_status$native_status[sp_list_status$native_status == "Presumed Extirpated"] = "Possibly Extirpated"
sp_list_status$native_status[sp_list_status$native_status == "Native, Extirpated, now Cultivated Only"] = "Native, Cultivated Only"
sp_list_status$cultivated_status[sp_list_status$native_status == "Not Native, Cultivated Only"] = "Cultivated"
sp_list_status$cultivated_status[sp_list_status$native_status == "Native, Cultivated Only"] = "Cultivated"
sp_list_status$native_status[sp_list_status$native_status == "Not Native, Cultivated Only"] = "Not Native"
sp_list_status$native_status[sp_list_status$native_status == "Native, Cultivated Only"] = "Native"
sp_list_status$naturalized_status[sp_list_status$native_status == "Not Native, Naturalized"] = "Naturalized"
sp_list_status$native_status[sp_list_status$native_status == "Not Native, Naturalized"] = "Not Native"
sp_list_status$extirpated_status[sp_list_status$native_status == "Possibly Extirpated"] = "Possibly Extirpated"
sp_list_status$native_status[sp_list_status$introduced_status == "Not Introduced" & sp_list_status$invasive_status == "Native" &
                               sp_list_status$native_status == "Possibly Extirpated"] = "Native"
sp_list_status$native_status[sp_list_status$introduced_status == "Not Introduced" &
                               sp_list_status$native_status == "Possibly Extirpated"] = "Native"
sp_list_status$native_status[sp_list_status$introduced_status == "Introduced" &
                               sp_list_status$native_status == "Possibly Extirpated"] = "Not Native"
sp_list_status$native_status[sp_list_status$introduced_status == "Introduced, Presumed Extirpated" &
                               sp_list_status$native_status == "Possibly Extirpated"] = "Not Native"
sp_list_status$native_status[sp_list_status$invasive_status == "Native" &
                               sp_list_status$native_status == "Possibly Extirpated"] = "Native"
sp_list_status$native_status[sp_list_status$native_status %in% c("", "-", "Null", "NC",
                                                                 "Possibly Extirpated",
                                                                 "Recorded as Native in Error")] = NA
unique(sp_list_status$native_status) # "Not Native"   NA   "Native"  "Possibly Native"

unique(sp_list_status$introduced_status)
filter(sp_list_all, introduced_status == "U")
filter(sp_list_status, introduced_status == "C")
sp_list_status$extirpated_status[sp_list_status$introduced_status == "Introduced, Presumed Extirpated"] = "Possibly Extirpated"
sp_list_status$introduced_status[sp_list_status$introduced_status == "Introduced, Presumed Extirpated"] = "Introduced"
sp_list_status$extirpated_status[sp_list_status$introduced_status == "Introduced, Possibly Extirpated"] = "Possibly Extirpated"
sp_list_status$introduced_status[sp_list_status$introduced_status == "Introduced, Possibly Extirpated"] = "Introduced"
sp_list_status$introduced_status[sp_list_status$introduced_status %in% c("CA", "U", "R", "C")] = "Introduced" # not native in native_status
sp_list_status$introduced_status[sp_list_status$introduced_status == "X"] = "Not Introduced" # not native
sp_list_status$introduced_status[sp_list_status$introduced_status == "Assumed to be Introduced"] = "Possibly Introduced" # not native
sp_list_status$introduced_status[sp_list_status$introduced_status %in% c("", "Null", "No Information")] = NA # not native in native_status



unique(sp_list_status$invasive_status)
filter(sp_list_all, invasive_status == "Ruderal")
filter(sp_list_status, invasive_status == "C")
sp_list_status$invasive_status[sp_list_status$invasive_status == "#PlantsByConsArea(\"InvasiveStatus\")#"] = "Null" # no information
sp_list_status$cultivated_status[sp_list_status$invasive_status %in% c("Cultivated Only",
                                                                       "Formerly Cultivated",
                                                                       "Cultivated")] = "Cultivated"
sp_list_status$invasive_status[sp_list_status$invasive_status %in% c("Cultivated Only",
                                                                     "Formerly Cultivated",
                                                                     "Cultivated", "Native")] = "Not Invasive" # assuming...
sp_list_status$invasive_status[sp_list_status$invasive_status %in% c("Potentially Invasive",
                                                                     "Assumed to be Invasive")] = "Possibly Invasive" # not native in native_status
sp_list_status$invasive_status[sp_list_status$invasive_status %in% c("", "Null")] = NA

unique(sp_list_status$cultivated_status)
filter(sp_list_all, cultivated_status == "Ruderal")
filter(sp_list_status, cultivated_status == "a")
sp_list_status$cultivated_status[sp_list_status$cultivated_status == "C"] = "Cultivated" # no information
sp_list_status$cultivated_status[sp_list_status$cultivated_status %in% c("Doubtfully Cultivated",
                                                                         "Formerly Cultivated",
                                                                         "Assumed to be Cultivated")] = "Cultivated"
sp_list_status$cultivated_status[sp_list_status$cultivated_status %in% c("", "Null", "a")] = NA

str(sp_list_status)
sp_list_status = unique(sp_list_status)
str(sp_list_status)
sp_list_status = mutate(sp_list_status, no_info = is.na(native_status) & is.na(introduced_status) &
                          is.na(invasive_status) & is.na(cultivated_status) & is.na(naturalized_status) &
                          is.na(extirpated_status)) %>%
  group_by(sp) %>%
  filter(no_info == FALSE) ## removed sp without any info about status
# write.csv(sp_list_status, file = "output_data/sp_status.csv", row.names = F)

clean_status = function(x){
  native_s = unique(x$native_status)
  introduced_s = unique(x$introduced_status)
  invasive_s = unique(x$invasive_status)
  cultivated_s = unique(x$cultivated_status)
  naturalized_s = unique(x$naturalized_status)
  extirpated_s = unique(x$extirpated_status)
  # if only one value
  if(length(native_s) == 1) native_s2 = native_s
  if(length(introduced_s) == 1) introduced_s2 = introduced_s
  if(length(invasive_s) == 1) invasive_s2 = invasive_s
  if(length(cultivated_s) == 1) cultivated_s2 = cultivated_s
  if(length(naturalized_s) == 1) naturalized_s2 = naturalized_s
  if(length(extirpated_s) == 1) extirpated_s2 = extirpated_s
  # remove NA
  if(length(native_s) > 1) native_s = na.omit(native_s)
  if(length(introduced_s) > 1) introduced_s = na.omit(introduced_s)
  if(length(invasive_s) > 1) invasive_s = na.omit(invasive_s)
  if(length(cultivated_s) > 1) cultivated_s = na.omit(cultivated_s)
  if(length(naturalized_s) > 1) naturalized_s = na.omit(naturalized_s)
  if(length(extirpated_s) > 1) extirpated_s = na.omit(extirpated_s)
  # if only one left after removing NA
  if(length(native_s) == 1) native_s2 = native_s
  if(length(introduced_s) == 1) introduced_s2 = introduced_s
  if(length(invasive_s) == 1) invasive_s2 = invasive_s
  if(length(cultivated_s) == 1) cultivated_s2 = cultivated_s
  if(length(naturalized_s) == 1) naturalized_s2 = naturalized_s
  if(length(extirpated_s) == 1) extirpated_s2 = extirpated_s
  # if more than one value left after removing NA
  if(length(native_s) > 1){
    freq_native = sort(table(na.omit(x$native_status)), decreasing = T)
    if(freq_native[1] > freq_native[2]){ # more of this
      native_s2 = names(freq_native[1])
    } else { # even number
      if(any(native_s == "Native")){
        native_s2 = "Native"
      } else {
        if(any(native_s == "Not Native")){
          native_s2 = "Not Native"
        } else {
          if(any(native_s == "Possibly Native")) native_s2 = "Possibly Native"
        }
      }
    }
  }
  
  if(length(introduced_s) > 1){
    freq_introduced = sort(table(na.omit(x$introduced_status)), decreasing = T)
    if(freq_introduced[1] > freq_introduced[2]){ # more of this
      introduced_s2 = names(freq_introduced[1])
    } else { # even number
      if(any(introduced_s == "Introduced")){
        introduced_s2 = "Introduced"
      } else {
        if(any(introduced_s == "Not Introduced")){
          introduced_s2 = "Not Introduced"
        } else {
          if(any(introduced_s == "Possibly Introduced")) introduced_s2 = "Possibly Introduced"
        }
      }
    }
  }
  
  if(length(invasive_s) > 1){
    freq_invasive = sort(table(na.omit(x$invasive_status)), decreasing = T)
    if(freq_invasive[1] > freq_invasive[2]){ # more of this
      invasive_s2 = names(freq_invasive[1])
    } else { # even number
      if(any(invasive_s == "Invasive")){
        invasive_s2 = "Invasive"
      } else {
        if(any(invasive_s == "Not Invasive")){
          invasive_s2 = "Not Invasive"
        } else {
          if(any(invasive_s == "Possibly Invasive")) invasive_s2 = "Possibly Invasive"
        }
      }
    }
  }
  
  if(length(cultivated_s) > 1){
    freq_cultivated = sort(table(na.omit(x$cultivated_status)), decreasing = T)
    if(freq_cultivated[1] > freq_cultivated[2]){ # more of this
      cultivated_s2 = names(freq_cultivated[1])
    } else { # even number
      if(any(cultivated_s == "Cultivated")){
        cultivated_s2 = "Cultivated"
      } else {
        if(any(cultivated_s == "Not Cultivated")){
          cultivated_s2 = "Not Cultivated"
        } else {
          if(any(cultivated_s == "Possibly Cultivated")) cultivated_s2 = "Possibly Cultivated"
        }
      }
    }
  }
  
  if(length(naturalized_s) > 1){
    freq_naturalized = sort(table(na.omit(x$naturalized_status)), decreasing = T)
    if(freq_naturalized[1] > freq_naturalized[2]){ # more of this
      naturalized_s2 = names(freq_naturalized[1])
    } else { # even number
      if(any(naturalized_s == "Naturalized")){
        naturalized_s2 = "Naturalized"
      } else {
        if(any(naturalized_s == "Not Naturalized")){
          naturalized_s2 = "Not Naturalized"
        } else {
          if(any(naturalized_s == "Possibly Naturalized")) naturalized_s2 = "Possibly Naturalized"
        }
      }
    }
  }
  
  if(length(extirpated_s) > 1){
    freq_extirpated = sort(table(na.omit(x$extirpated_status)), decreasing = T)
    if(freq_extirpated[1] > freq_extirpated[2]){ # more of this
      extirpated_s2 = names(freq_extirpated[1])
    } else { # even number
      if(any(extirpated_s == "Extirpated")){
        extirpated_s2 = "Extirpated"
      } else {
        if(any(extirpated_s == "Not Extirpated")){
          extirpated_s2 = "Not Extirpated"
        } else {
          if(any(extirpated_s == "Possibly Extirpated")) extirpated_s2 = "Possibly Extirpated"
        }
      }
    }
  }
  
  data.frame(native_status = native_s2, introduced_status = introduced_s2,
             invasive_status = invasive_s2, cultivated_status = cultivated_s2,
             naturalized_status = naturalized_s2, extirpated_status = extirpated_s2)
}

sp_list_status_cleaned = group_by(sp_list_status, sp) %>%
  do(clean_status(.)) %>% ungroup()
filter(sp_list_status_cleaned, native_status == "Native" & introduced_status == "Introduced") # 10...
filter(sp_list_status_cleaned, native_status == "Not Native" & introduced_status == "Not Introduced" &
         cultivated_status != "Cultivated")
filter(sp_list_status_cleaned, sp == "Acer rubrum")
sp_list_status_cleaned$native_status[sp_list_status_cleaned$invasive_status %in%
                                       c("Ruderal", "Invasive") &
                                       sp_list_status_cleaned$introduced_status == "Introduced"] = "Not Native"
sp_list_status_cleaned$native_status[sp_list_status_cleaned$native_status == "Native" &
                                       sp_list_status_cleaned$introduced_status == "Introduced"] = "Not Native"

## pick one crietia and try
unique(sp_list_status_cleaned$native_status)
unique(sp_list_status_cleaned$introduced_status)
unique(sp_list_status_cleaned$invasive_status)
unique(sp_list_status_cleaned$cultivated_status)
unique(sp_list_status_cleaned$naturalized_status)
unique(sp_list_status_cleaned$extirpated_status)

sp_list_status_cleaned$status = NA
sp_list_status_cleaned$status[sp_list_status_cleaned$native_status == "Native" &
                                sp_list_status_cleaned$introduced_status == "Not Introduced"] = "Native"
sp_list_status_cleaned$status[sp_list_status_cleaned$native_status == "Not Native" &
                                sp_list_status_cleaned$introduced_status == "Introduced"] = "Introduced"
filter(sp_list_status_cleaned, native_status == "Not Native",
       introduced_status == "Not Introduced",
       cultivated_status == "Cultivated")
sp_list_status_cleaned$status[sp_list_status_cleaned$sp %in%
                                filter(sp_list_status_cleaned, native_status == "Not Native",
                                       introduced_status == "Not Introduced",
                                       cultivated_status == "Cultivated")$sp] = "Cultivated"
filter(sp_list_status_cleaned, is.na(status), cultivated_status == "Cultivated")
sp_list_status_cleaned$status[sp_list_status_cleaned$sp %in%
                                filter(sp_list_status_cleaned, is.na(status),
                                       cultivated_status == "Cultivated")$sp] = "Cultivated"
filter(sp_list_status_cleaned, is.na(status)) #%>% write.csv(file = "problematic_sp_status.csv", row.names = F)
table(sp_list_status_cleaned$status)
# Cultivated Introduced     Native
#        220        655       1399 
any(duplicated(sp_list_status_cleaned$sp))

sp_list_all_cleaned = select(sp_list_all, sp = scientific_name, occurrence, 
                             reference, voucher, park_name) %>%
  left_join(sp_list_status_cleaned, by = "sp")
write.csv(sp_list_all_cleaned, file = "sp_list_parks_cleaned.csv", row.names = F)



