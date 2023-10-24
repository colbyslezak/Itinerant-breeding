# install.packages("move")
# install.packages("remotes")
# install.packages("dplyr")
# install.packages("rlang")
# install.packages("Rtools")
# install.packages("adehabitatLT")
# install.packages("extrafont")
# install.packages("stringr")
# install.packages("broom")
# install.packages("ggpmisc")
# remotes::install_github("picardis/nestR", build_vignettes = TRUE, force=TRUE)
# remotes::install_github('bbc/bbplot')
# install.packages("cowplot")
# install.packages("NatParksPalettes")
# install.packages("lubridate")
# install.packages("writexl")

library(nestR)
library(dplyr)
library(move)
library(ggplot2)
library(adehabitatLT)
library(rgdal)
library(sp)
library(proj4)
library(lubridate)
library(scales)
library(bbplot)
library(extrafont)
library(stringr)
library(broom)
library(ggpmisc)
library(ggpubr)
library(AICcmodavg)
library(cowplot)
library(lubridate)
library(writexl)
library(NatParksPalettes)

#login to movebank, this gives me full access to the whole dataset
login <- movebankLogin(username = "colby_slezak", password="Mamajama12!")
amwo_data <- getMovebankData(study = "American Woodcock Migration Ecology in Eastern North America", 
                             login = login, 
                             removeDuplicatedTimestamps=TRUE)

#This will give you the nesting data in "move" package format; if you want it as a traditional dataframe, use the following code
amwo_data <- as.data.frame(amwo_data)


amwo_data %>% 
  transmute(burst = local_identifier,
            date = timestamp,
            long = location_long,
            lat = location_lat,
            tagtype = comments,
            sex = sex,
            age = taxon_detail,
            altitude = height_above_msl,
            accuracy=location_error_text, 
            start=timestamp_start,
            end=timestamp_end,
            mortality=mortality_status) ->
  amwo_data

# get the dead birds filtered so I can exclude nests on or after dead date
dead <- amwo_data %>% filter(mortality=="1")
unique(dead$burst)


# filter out females from 2019-2022 (there are 272 hens excluding Maine 2017)
femalesonly <- amwo_data %>% filter(sex=="f")
femalesonly <- femalesonly %>% filter(start<="2022-06-01")
str(femalesonly)
uniquefemales <- data.frame(unique(femalesonly$burst))
str(uniquefemales)

# I wanted to pull out the first 2 letters from the burst column in the unique females data frame so that I can summarize number of tags from each state and province
uniquefemales$stateprov <- substr(uniquefemales$unique.femalesonly.burst. , start = 1 , stop = 2 )
uniquefemales$stateprov <- as.factor(uniquefemales$stateprov)

uniquefemales$year <- substr(uniquefemales$unique.femalesonly.burst. , start = 4 , stop = 8 )

# summarize number of tags per state and province----
stateprovsummary <- uniquefemales %>% group_by(stateprov) %>% group_by(year) %>% count(stateprov)
str(stateprovsummary)
sum(stateprovsummary$n,na.rm=TRUE)

# figure out start date range for each province and state for each year 
femalesonly$stateprov <- substr(femalesonly$burst , start = 1 , stop = 2 )
femalesonly$year <- format(as.POSIXct(femalesonly$start), "%Y")
femalesonly$month <- format(as.POSIXct(femalesonly$start), "%m")

# separate into catching seasons becuase some states caught in more than one. 
femalesonlysummarysummerandfall <- femalesonly %>% filter(month=="07"|month=="08"|month=="09"|month=="10"|month=="11") %>% group_by(year, stateprov)%>% reframe(range = range(start))
femalesonlysummarywinterspring <- femalesonly %>% filter(month=="04"|month=="05"|month=="06"|month=="12"|month=="01"|month=="02"|month=="03") %>% group_by(year, stateprov)%>% reframe(range = range(start))
# there is still 272 females excluding Maine 2017
sum(stateprovsummary$n,na.rm=TRUE)

# write.csv(femalesonlysummarysummerandfall, "./Data/femalesonlysummarysummerandfall.csv", row.names = FALSE)
# write.csv(femalesonlysummarywinterspring, "./Data/femalesonlysummarywinterspring.csv", row.names = FALSE)

# summarize number of tags per season per state and province in summer and fall (155 hens excluding 2 hens from Maine tagged  in 2017-- in 2018 only one was still transmitting at time of Jan 1 so excluded 2017 all together)
countsbystatesummerandfall<- femalesonly %>% filter(month=="07"|month=="08"|month=="09"|month=="10"|month=="11") %>%group_by(year, stateprov)%>% summarise(count = n_distinct(burst))
sum(countsbystatesummerandfall$count)

# summarize number of tags per season per state and province in winter and spring (119 hens) Virginia birds are wonky in this summary cuz they caught in January 2020 and then again in December 2020 so I just manually figured out start and end by looking through 2020 start dates in femalesonly file, I also moved the onwe RI bird from 2020 that started 1 Dec to the spring totals
countsbystatewinterspring<- femalesonly %>% filter(month=="04"|month=="05"|month=="06"|month=="12"|month=="01"|month=="02"|month=="03") %>%group_by(year, stateprov)%>% summarise(count = n_distinct(burst))
sum(countsbystatewinterspring$count)

# there are still 272 hens excluding Maine 2017

# filter date periods 2018-2022, there were no females in 2017, 1 in 2018, but since there are no nesting hens in 2018 it isnt included. 

# 2018, 1 female
datefilter2018 <- femalesonly %>% 
  filter(date >= "2018-01-01" & date <= "2018-09-01") %>% 
  droplevels()
count2018 <- unique(datefilter2018$burst)

# for 2018 I wanted to see which females that started collecting points prior to Feb 20th and were tracked past June 1 so I filtered additionally below. I looked what % of these females nested >1 time
datefilter2018pastjune <- femalesonly %>% filter(date >= "2018-01-01" & date <= "2018-09-01") %>% filter(start<= "2018-02-20") %>%  filter(end>="2018-06-01") %>% droplevels()
count2018pastjune <- unique(datefilter2018pastjune$burst)

# 2019, 42 females
datefilter2019 <- femalesonly %>% 
  filter(date >= "2019-01-01" & date <= "2019-09-01") %>% 
  droplevels()
count2019 <- unique(datefilter2019$burst)
str(datefilter2019)

# for 2019 I wanted to see which females that started collecting points prior to Feb 20th and were tracked past June 1 so I filtered additionally below. I looked what % of these females nested >1 time
datefilter2019pastjune <- femalesonly %>% filter(date >= "2019-01-01" & date <= "2019-09-01") %>% filter(start<= "2019-02-20") %>%  filter(end>="2019-06-01") %>% droplevels()
count2019pastjune <- unique(datefilter2019pastjune$burst)

# 2020, 75 females
datefilter2020 <- femalesonly %>% 
  filter(date >= "2020-01-01" & date <= "2020-09-01") %>% 
  droplevels()
count2020 <- unique(datefilter2020$burst)

# for 2020 I wanted to see which females that started collecting points prior to Feb 20th and were tracked past June 1 so I filtered additionally below. I looked what % of these females nested >1 time
datefilter2020pastjune <- femalesonly %>% filter(date >= "2020-01-01" & date <= "2020-09-01") %>%  filter(start<= "2020-02-20") %>% filter(end>="2020-06-01") %>%  droplevels()
count2020pastjune <- unique(datefilter2020pastjune$burst)

# 2021, 62 females 
datefilter2021 <- femalesonly %>% 
  filter(date >= "2021-01-01" & date <= "2021-09-01") %>%
  droplevels()
count2021 <- unique(datefilter2021$burst)

# for 2021 I wanted to see which females that started collecting points prior to Feb 20th and were tracked past June 1 so I filtered additionally below. I looked what % of these females nested >1 time
datefilter2021pastjune <- femalesonly %>% filter(date >= "2021-01-01" & date <= "2021-09-01") %>%  filter(start<= "2021-02-20") %>% filter(end>="2021-06-01") %>% droplevels()
count2021pastjune <- unique(datefilter2021pastjune$burst)

# 2022, 58 females
datefilter2022 <- femalesonly %>% 
  filter(date >= "2022-01-01" & date <= "2022-09-01") %>% 
  droplevels()
count2022 <- unique(datefilter2022$burst)

# for 2022 I wanted to see which females that started collecting points prior to Feb 20th and were tracked past June 1 so I filtered additionally below. I looked what % of these females nested >1 time
datefilter2022pastjune <- femalesonly %>% filter(date >= "2022-01-01" & date <= "2022-09-01") %>%  filter(start<= "2022-02-20") %>%  filter(end>="2022-06-01") %>% droplevels()
count2022pastjune <- unique(datefilter2022pastjune$burst)

# figuring out how often tags were transmitting for publication (precision) from Erik Blomberg code 
## add ordinal date term 

# # combine all files from top of this code where I filtered just to 1 Jan-1 Sept into one dataframe
erik <- rbind(datefilter2018, datefilter2019, datefilter2020, datefilter2021, datefilter2022)
str(erik)

erik$burst <- as.factor(erik$burst)

# get ordinal date term
erik$Ordinal<- yday(erik$date)

erik$Mid.transmit<- NA

for (i in 1:nrow(erik)){
  
  erik$Mid.transmit[i]<- ifelse(erik$burst[i]==erik$burst[i+1], 
                                 (erik$Ordinal[i+1]+erik$Ordinal[i])/2,
                                 NA)
}
  
## calculate the mean precision of locations as the distance between the mid-point
## and the actual location date.

erik$precision<- erik$Mid.transmit-erik$Ordinal

# multiply precision x4 to get number of days to collect 3 points on average 
erik$precision2 <- erik$precision*4

mean(erik$precision2, na.rm=TRUE)#4.32
sd(erik$precision2, na.rm=TRUE)#5.417
min(erik$precision2, na.rm=TRUE)#0
max(erik$precision2, na.rm=TRUE)#122
quantile(erik$precision2, probs=c(seq(0,1,.05)), na.rm=TRUE)
# 95% of data 12 days or less to get 3 points

# now look at just the majority of breeding season 20 Feb-1 June
datefilter2019_2 <- femalesonly %>% 
  filter(date >= "2019-02-20" & date <= "2019-06-01") %>% 
  droplevels()

datefilter2020_2 <- femalesonly %>% 
  filter(date >= "2020-02-20" & date <= "2020-06-01") %>% 
  droplevels()

datefilter2021_2 <- femalesonly %>% 
  filter(date >= "2021-02-20" & date <= "2021-06-01") %>% 
  droplevels()


datefilter2022_2 <- femalesonly %>% 
  filter(date >= "2022-02-20" & date <= "2022-06-01") %>% 
  droplevels()

erik2 <- rbind(datefilter2019_2, datefilter2020_2, datefilter2021_2, datefilter2022_2)
str(erik2)

erik2$burst <- as.factor(erik2$burst)

# get ordinal date term
erik2$Ordinal<- yday(erik2$date)

erik2$Mid.transmit<- NA

for (i in 1:nrow(erik2)){
  
  erik2$Mid.transmit[i]<- ifelse(erik2$burst[i]==erik2$burst[i+1], 
                                (erik2$Ordinal[i+1]+erik2$Ordinal[i])/2,
                                NA)
}

## calculate the mean precision of locations as the distance between the mid-point
## and the actual location date.

erik2$precision<- erik2$Mid.transmit-erik2$Ordinal

# multiply precision x4 to get number of days to collect 3 points on average 
erik2$precision2 <- erik2$precision*4

mean(erik2$precision2, na.rm=TRUE)#3.490
sd(erik2$precision2, na.rm=TRUE)#3.779
min(erik2$precision2, na.rm=TRUE)#0
max(erik2$precision2, na.rm=TRUE)#112
quantile(erik2$precision2, probs=c(seq(0,1,.05)), na.rm=TRUE)
# 95% of data 8 days or less to get 3 points

# I want to see how many hens were actually captured from 2019-22 using the capture log found on google drive 
femalecaptured <- read.csv("./Data/female_catpure.csv")
femalecaptured$Date <- as.Date(femalecaptured$Date, format='%Y-%m-%d')
femalecaptures <- femalecaptured %>% filter(Sex=="Female")
femalecapturessept <- femalecaptures %>% filter(Date>="2018-09-01"&Date<="2022-06-01")

# write this to a csv
# write.csv(erik, "./Data/erik.csv", row.names = FALSE)


# NestR package stuff
# vignette("nestR")
# View(woodstorks)
# str(woodstorks)
# require(dplyr)
# use the find_nest function on your data with parameters specified from your system 
# min_pts is how many points need to be in a buffer before it is considered a potential nest site and revisitiation stats are calculated 
# min_d_fix set to 2 because that way if there is a buffer that anything with less than 2 points won't interfere with the number of consec days for a buffer. 
# the CART helps modify min_top_att- minimum number of points on the day of top attendance which is always 100 bc of our course duty cycle, min_consec, perc_days_visit (the number of days it went back to that nest site from the time it was first seen there and the last time it was seen there), and perc_top_visit (which is always 100 bc of our course duty cycle). Perc_top_visit is the percent of time the bird spent at the nest 

# this is from the help....On days when no visit was recorded, two cases are possible: either the nest was truly not visited, or visits were missed. On days with few fixes, there is a higher chance of missing a visit given that it happened. Missed visit detections can interrupt an otherwise continuous strike of days visited. To counteract possible issues due to missed visit detections, the user can define min_d_fix as the minimum number of fixes that have to be available in a day with no visits for that day to be retained when counting consecutive days visited. If a day with no visits and fewer fixes than min_d_fix interrupts a sequence of consecutive days visited, it does not get considered and the sequence gets counted as uninterrupted.
# this means that we would want min_d_fix set to 2 because we never have more than one location per day 

# using these parameters we only miss one nest
# 2018 nests 
# ws_output_1 <- nestR::find_nests(gps_data = datefilter2018,
#                           sea_start = "01-01",
#                           sea_end = "09-01",
#                           nest_cycle = 21,
#                           buffer = 20,
#                           min_pts = 2,
#                           min_d_fix = 2,
#                           min_consec = 6,
#                           min_top_att = 1,
#                           min_days_att = 1,
#                           discard_overlapping = TRUE)
# nests2018 <- ws_output_1$nests
# # explore_nests(ws_output_1$nests)
# 
# # 2019 nests
# ws_output_2 <- nestR::find_nests(gps_data = datefilter2019,
#                                  sea_start = "01-01",
#                                  sea_end = "09-01",
#                                  nest_cycle = 21,
#                                  buffer = 20,
#                                  min_pts = 2,
#                                  min_d_fix = 2,
#                                  min_consec = 6,
#                                  min_top_att = 1,
#                                  min_days_att = 1,
#                                  discard_overlapping = TRUE)
# nests2019 <- ws_output_2$nests
# # explore_nests(ws_output_2$nests)
# 
# # 2020 nests 
# ws_output_3 <- nestR::find_nests(gps_data = datefilter2020,
#                                  sea_start = "01-01",
#                                  sea_end = "09-01",
#                                  nest_cycle = 21,
#                                  buffer = 20,
#                                  min_pts = 2,
#                                  min_d_fix = 2,
#                                  min_consec = 6,
#                                  min_top_att = 1,
#                                  min_days_att = 1,
#                                  discard_overlapping = TRUE)
# nests2020 <- ws_output_3$nests
# # explore_nests(ws_output_3$nests)
# 
# # 2021 nests 
# ws_output_4 <- nestR::find_nests(gps_data = datefilter2021,
#                                  sea_start = "01-01",
#                                  sea_end = "09-01",
#                                  nest_cycle = 21,
#                                  buffer = 20,
#                                  min_pts = 2,
#                                  min_d_fix = 2,
#                                  min_consec = 6,
#                                  min_top_att = 1,
#                                  min_days_att = 1,
#                                  discard_overlapping = TRUE)
# nests2021 <- ws_output_4$nests
# # explore_nests(ws_output_4$nests)
# 
# # 2022 nests
# ws_output_5 <- nestR::find_nests(gps_data = datefilter2022,
#                                  sea_start = "01-01",
#                                  sea_end = "09-01",
#                                  nest_cycle = 21,
#                                  buffer = 20,
#                                  min_pts = 2,
#                                  min_d_fix = 2,
#                                  min_consec = 6,
#                                  min_top_att = 1,
#                                  min_days_att = 1,
#                                  discard_overlapping = TRUE)
# nests2022 <- ws_output_5$nests
# View(ws_output_5)
# explore_nests(datefilter2022)

# # combine all nest files into one dataframe
# combinednests26 <- rbind(nests2018, nests2019, nests2020, nests2021, nests2022)
# write.csv(combinednests, "./Data/allnestsflyway.csv", row.names = FALSE)

# # make ggplot of latitude by 2019
# ggplot(nests2019, aes(x = lat, y =attempt_start, color = burst)) +
#   geom_point(position = position_dodge2(width = 0.5, preserve = "single"), size=2) +
#   labs(title = "Nest Initiation by Latitude 2019", x = "Latitude", y = "Date") +
#   theme_classic() +
#   theme(text = element_text(family = "serif")) +
#   theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), axis.text = element_text(size = 14, colour = "black"), axis.title.y = element_text(size = 15, color = "black", vjust = 2.25), axis.ticks.x = element_blank(), legend.position = "none")

# # make ggplot of latitude by 2020
# ggplot(nests2020, aes(x = lat, y =attempt_start, color = burst)) +
#   geom_point(position = position_dodge2(width = 0.5, preserve = "single"), size=2) +
#   labs(title = "Nest Initiation by Latitude 2020", x = "Latitude", y = "Date") +
#   theme_classic() +
#   theme(text = element_text(family = "serif")) +
#   theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), axis.text = element_text(size = 14, colour = "black"), axis.title.y = element_text(size = 15, color = "black", vjust = 2.25), axis.ticks.x = element_blank(), legend.position = "none")
# 
# # make ggplot of latitude by 2021
# ggplot(nests2021, aes(x = lat, y =attempt_start, color = burst)) +
#   geom_point(position = position_dodge2(width = 0.5, preserve = "single"), size=2) +
#   labs(title = "Nest Initiation by Latitude 2021", x = "Latitude", y = "Date") +
#   theme_classic() +
#   theme(text = element_text(family = "serif")) +
#   theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), axis.text = element_text(size = 14, colour = "black"), axis.title.y = element_text(size = 15, color = "black", vjust = 2.25), axis.ticks.x = element_blank(), legend.position = "none")
# 
# # make ggplot of latitude by 2022
# ggplot(nests2022, aes(x = lat, y =attempt_start, color = burst)) +
#   geom_point(position = position_dodge2(width = 0.5, preserve = "single"), size=2) +
#   labs(title = "Nest Initiation by Latitude 2022", x = "Latitude", y = "Date") +
#   theme_classic() +
#   theme(text = element_text(family = "serif")) +
#   theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), axis.text = element_text(size = 14, colour = "black"), axis.title.y = element_text(size = 15, color = "black", vjust = 2.25), axis.ticks.x = element_blank(), legend.position = "none")
# 
# # with min_d_fix as 1 and min_consec 6
# # 2018 nests 
# ws_output_116 <- nestR::find_nests(gps_data = datefilter2018,
#                                  sea_start = "01-01",
#                                  sea_end = "09-01",
#                                  nest_cycle = 21,
#                                  buffer = 20,
#                                  min_pts = 2,
#                                  min_d_fix = 1,
#                                  min_consec = 6,
#                                  min_top_att = 1,
#                                  min_days_att = 1,
#                                  discard_overlapping = TRUE)
# nests201816 <- ws_output_116$nests
# # explore_nests(ws_output_1$nests)
# 
# # 2019 nests
# ws_output_216 <- nestR::find_nests(gps_data = datefilter2019,
#                                  sea_start = "01-01",
#                                  sea_end = "09-01",
#                                  nest_cycle = 21,
#                                  buffer = 20,
#                                  min_pts = 2,
#                                  min_d_fix = 1,
#                                  min_consec = 6,
#                                  min_top_att = 1,
#                                  min_days_att = 1,
#                                  discard_overlapping = TRUE)
# nests201916 <- ws_output_216$nests
# # explore_nests(ws_output_2$nests)
# 
# # 2020 nests 
# ws_output_316 <- nestR::find_nests(gps_data = datefilter2020,
#                                  sea_start = "01-01",
#                                  sea_end = "09-01",
#                                  nest_cycle = 21,
#                                  buffer = 20,
#                                  min_pts = 2,
#                                  min_d_fix = 1,
#                                  min_consec = 6,
#                                  min_top_att = 1,
#                                  min_days_att = 1,
#                                  discard_overlapping = TRUE)
# nests202016 <- ws_output_316$nests
# # explore_nests(ws_output_3$nests)
# 
# # 2021 nests 
# ws_output_416 <- nestR::find_nests(gps_data = datefilter2021,
#                                  sea_start = "01-01",
#                                  sea_end = "09-01",
#                                  nest_cycle = 21,
#                                  buffer = 20,
#                                  min_pts = 2,
#                                  min_d_fix = 1,
#                                  min_consec = 6,
#                                  min_top_att = 1,
#                                  min_days_att = 1,
#                                  discard_overlapping = TRUE)
# nests202116 <- ws_output_416$nests
# # explore_nests(ws_output_4$nests)
# 
# # 2022 nests
# ws_output_516 <- nestR::find_nests(gps_data = datefilter2022,
#                                  sea_start = "01-01",
#                                  sea_end = "09-01",
#                                  nest_cycle = 21,
#                                  buffer = 20,
#                                  min_pts = 2,
#                                  min_d_fix = 1,
#                                  min_consec = 6,
#                                  min_top_att = 1,
#                                  min_days_att = 1,
#                                  discard_overlapping = TRUE)
# nests202216 <- ws_output_516$nests
# 
# combinednests16 <- rbind(nests201816, nests201916, nests202016, nests202116, nests202216)
# # write.csv(combinednests16, "./Data/combinednests16.csv", row.names = FALSE)
# 
# 
# # with min_d_fix 1 and min_consec 4
# # 2018 nests 
# ws_output_114 <- nestR::find_nests(gps_data = datefilter2018,
#                                   sea_start = "01-01",
#                                   sea_end = "09-01",
#                                   nest_cycle = 21,
#                                   buffer = 20,
#                                   min_pts = 2,
#                                   min_d_fix = 1,
#                                   min_consec = 4,
#                                   min_top_att = 1,
#                                   min_days_att = 1,
#                                   discard_overlapping = TRUE)
# nests201814 <- ws_output_114$nests
# # explore_nests(ws_output_1$nests)
# 
# # 2019 nests
# ws_output_214 <- nestR::find_nests(gps_data = datefilter2019,
#                                   sea_start = "01-01",
#                                   sea_end = "09-01",
#                                   nest_cycle = 21,
#                                   buffer = 20,
#                                   min_pts = 2,
#                                   min_d_fix = 1,
#                                   min_consec = 4,
#                                   min_top_att = 1,
#                                   min_days_att = 1,
#                                   discard_overlapping = TRUE)
# nests201914 <- ws_output_214$nests
# # explore_nests(ws_output_2$nests)
# 
# # 2020 nests 
# ws_output_314 <- nestR::find_nests(gps_data = datefilter2020,
#                                   sea_start = "01-01",
#                                   sea_end = "09-01",
#                                   nest_cycle = 21,
#                                   buffer = 20,
#                                   min_pts = 2,
#                                   min_d_fix = 1,
#                                   min_consec = 4,
#                                   min_top_att = 1,
#                                   min_days_att = 1,
#                                   discard_overlapping = TRUE)
# nests202014 <- ws_output_314$nests
# # explore_nests(ws_output_3$nests)
# 
# # 2021 nests 
# ws_output_414 <- nestR::find_nests(gps_data = datefilter2021,
#                                   sea_start = "01-01",
#                                   sea_end = "09-01",
#                                   nest_cycle = 21,
#                                   buffer = 20,
#                                   min_pts = 2,
#                                   min_d_fix = 1,
#                                   min_consec = 4,
#                                   min_top_att = 1,
#                                   min_days_att = 1,
#                                   discard_overlapping = TRUE)
# nests202114 <- ws_output_414$nests
# # explore_nests(ws_output_4$nests)
# 
# # 2022 nests
# ws_output_514 <- nestR::find_nests(gps_data = datefilter2022,
#                                   sea_start = "01-01",
#                                   sea_end = "09-01",
#                                   nest_cycle = 21,
#                                   buffer = 20,
#                                   min_pts = 2,
#                                   min_d_fix = 1,
#                                   min_consec = 4,
#                                   min_top_att = 1,
#                                   min_days_att = 1,
#                                   discard_overlapping = TRUE)
# nests202214 <- ws_output_514$nests
# 
# combinednests14 <- rbind(nests201814, nests201914, nests202014, nests202114, nests202214)
# # write.csv(combinednests14, "./Data/combinednests14.csv", row.names = FALSE)
# 
# 
# # with min daily fix as 2 and min_consec 4
# # 2018 nests 
# ws_output_124 <- nestR::find_nests(gps_data = datefilter2018,
#                                   sea_start = "01-01",
#                                   sea_end = "09-01",
#                                   nest_cycle = 21,
#                                   buffer = 20,
#                                   min_pts = 2,
#                                   min_d_fix = 2,
#                                   min_consec = 4,
#                                   min_top_att = 1,
#                                   min_days_att = 1,
#                                   discard_overlapping = TRUE)
# nests201824 <- ws_output_124$nests
# # explore_nests(ws_output_1$nests)
# 
# # 2019 nests
# ws_output_224 <- nestR::find_nests(gps_data = datefilter2019,
#                                   sea_start = "01-01",
#                                   sea_end = "09-01",
#                                   nest_cycle = 21,
#                                   buffer = 20,
#                                   min_pts = 2,
#                                   min_d_fix = 2,
#                                   min_consec = 4,
#                                   min_top_att = 1,
#                                   min_days_att = 1,
#                                   discard_overlapping = TRUE)
# nests201924 <- ws_output_224$nests
# # explore_nests(ws_output_2$nests)
# 
# # 2020 nests 
# ws_output_324 <- nestR::find_nests(gps_data = datefilter2020,
#                                   sea_start = "01-01",
#                                   sea_end = "09-01",
#                                   nest_cycle = 21,
#                                   buffer = 20,
#                                   min_pts = 2,
#                                   min_d_fix = 2,
#                                   min_consec = 4,
#                                   min_top_att = 1,
#                                   min_days_att = 1,
#                                   discard_overlapping = TRUE)
# nests202024 <- ws_output_324$nests
# # explore_nests(ws_output_3$nests)
# 
# # 2021 nests 
# ws_output_424 <- nestR::find_nests(gps_data = datefilter2021,
#                                   sea_start = "01-01",
#                                   sea_end = "09-01",
#                                   nest_cycle = 21,
#                                   buffer = 20,
#                                   min_pts = 2,
#                                   min_d_fix = 2,
#                                   min_consec = 4,
#                                   min_top_att = 1,
#                                   min_days_att = 1,
#                                   discard_overlapping = TRUE)
# nests202124 <- ws_output_424$nests
# # explore_nests(ws_output_4$nests)
# 
# # 2022 nests
# ws_output_524 <- nestR::find_nests(gps_data = datefilter2022,
#                                   sea_start = "01-01",
#                                   sea_end = "09-01",
#                                   nest_cycle = 21,
#                                   buffer = 20,
#                                   min_pts = 2,
#                                   min_d_fix = 2,
#                                   min_consec = 4,
#                                   min_top_att = 1,
#                                   min_days_att = 1,
#                                   discard_overlapping = TRUE)
# nests202224 <- ws_output_524$nests
# 
# combinednests24 <- rbind(nests201824, nests201924, nests202024, nests202124, nests202224)
# # write.csv(combinednests24, "./Data/combinednests24.csv", row.names = FALSE)


# with min daily fix as 2 and min_consec 5
# 2018 nests 
ws_output_125 <- nestR::find_nests(gps_data = datefilter2018,
                                   sea_start = "01-01",
                                   sea_end = "09-01",
                                   nest_cycle = 21,
                                   buffer = 20,
                                   min_pts = 2,
                                   min_d_fix = 2,
                                   min_consec = 5,
                                   min_top_att = 1,
                                   min_days_att = 1,
                                   discard_overlapping = TRUE)
nests201825 <- ws_output_125$nests
# explore_nests(ws_output_1$nests)

# 2019 nests
ws_output_225 <- nestR::find_nests(gps_data = datefilter2019,
                                   sea_start = "01-01",
                                   sea_end = "09-01",
                                   nest_cycle = 21,
                                   buffer = 20,
                                   min_pts = 2,
                                   min_d_fix = 2,
                                   min_consec = 5,
                                   min_top_att = 1,
                                   min_days_att = 1,
                                   discard_overlapping = TRUE)
nests201925 <- ws_output_225$nests
# explore_nests(ws_output_2$nests)

# save kfoldlme2 output (my top model) so that you don't have to run it again
# saveRDS(nests201925, file = "./ObjectsAndModels/nests201925.rds")

# read model back in to environment
nests201925 <- readRDS("./ObjectsAndModels/nests201925.rds")

# 2020 nests 
ws_output_325 <- nestR::find_nests(gps_data = datefilter2020,
                                   sea_start = "01-01",
                                   sea_end = "09-01",
                                   nest_cycle = 21,
                                   buffer = 20,
                                   min_pts = 2,
                                   min_d_fix = 2,
                                   min_consec = 5,
                                   min_top_att = 1,
                                   min_days_att = 1,
                                   discard_overlapping = TRUE)
nests202025 <- ws_output_325$nests
# explore_nests(ws_output_3$nests)

# save kfoldlme2 output (my top model) so that you don't have to run it again
# saveRDS(nests202025, file = "./ObjectsAndModels/nests202025.rds")

# read model back in to environment
nests202025 <- readRDS("./ObjectsAndModels/nests202025.rds")

# 2021 nests 
ws_output_425 <- nestR::find_nests(gps_data = datefilter2021,
                                   sea_start = "01-01",
                                   sea_end = "09-01",
                                   nest_cycle = 21,
                                   buffer = 20,
                                   min_pts = 2,
                                   min_d_fix = 2,
                                   min_consec = 5,
                                   min_top_att = 1,
                                   min_days_att = 1,
                                   discard_overlapping = TRUE)
nests202125 <- ws_output_425$nests
# explore_nests(ws_output_4$nests)

# save kfoldlme2 output (my top model) so that you don't have to run it again
# saveRDS(nests202125, file = "./ObjectsAndModels/nests202125.rds")

# read model back in to environment
nests202125 <- readRDS("./ObjectsAndModels/nests202125.rds")

# 2022 nests
ws_output_525 <- nestR::find_nests(gps_data = datefilter2022,
                                   sea_start = "01-01",
                                   sea_end = "09-01",
                                   nest_cycle = 21,
                                   buffer = 20,
                                   min_pts = 2,
                                   min_d_fix = 2,
                                   min_consec = 5,
                                   min_top_att = 1,
                                   min_days_att = 1,
                                   discard_overlapping = TRUE)
nests202225 <- ws_output_525$nests

# save kfoldlme2 output (my top model) so that you don't have to run it again
# saveRDS(nests202225, file = "./ObjectsAndModels/nests202225.rds")

# read model back in to environment
nests202225 <- readRDS("./ObjectsAndModels/nests202225.rds")


# there are a total of 350 nests before filtering dead
combinednests25 <- rbind(nests201925, nests202025, nests202125, nests202225)

# write.csv(combinednests25, "./Data/combinednests25fulldata.csv", row.names = FALSE)

# read data back in
combinednests25 <- read.csv("./Data/combinednests25fulldata.csv")

# filter out nests from hens that were dead via Liam's HMM model, there are now 336 nests 
combinednests25 <- combinednests25 %>% filter(!(burst=="NC-2021-21" & loc_id == 33)) %>% filter(!(burst=="LA-2022-14" & loc_id == 59)) %>% filter(!(burst=="NJ-2019-26" & loc_id == 23))%>% filter(!(burst=="NY-2019-19" & loc_id == 5))%>% filter(!(burst=="NY-2022-46" & loc_id == 4)) %>% filter(!(burst=="RI-2018-02"& loc_id== 7)) %>% filter(!(burst=="RI-2021-52" & loc_id == 10)) %>% filter(!(burst=="VA-2019-26" & loc_id == 3)) %>% filter(!(burst=="VA-2020-82"))%>% filter(!(burst=="NC-2020-09")) %>% filter(!(burst=="VA-2019-16"))%>% filter(!(burst=="VA-2019-24")) %>% filter(!(burst=="LA-2022-14" & loc_id == 59)) %>% filter(!(burst=="NJ-2019-25" & loc_id == 72)) %>% filter(!(burst=="NJ-2019-25" & loc_id == 74)) 
# write.csv(combinednests25, "./Data/combinednests25all337nests.csv", row.names = FALSE)

unique(dead$burst)

# see which hens had a nest so that we can see why we did not find nests for ca. 119 females in the dataset (most likely they collected few points)----
hensthatnested <- data.frame(unique(combinednests25$burst))
names(hensthatnested)[1] <- "burst"
# anti_join looks for ids that are in x but do not also appear in the y dataframe
joinedids <- anti_join(femalesonly, hensthatnested)
joinedids <-  joinedids %>% filter(burst!="ME-2017-02"& burst!="ME-2017-03")
# there are 119 hens that never nested in the dataset
unique(joinedids$burst)
counthensthatdidntnest <- joinedids %>%  group_by(burst) %>% summarise(burst = n())
# 59 of the 119 hens had less than 35 points 
counthensthatdidntnestless30 <- counthensthatdidntnest %>% filter(burst >= 35)
range(counthensthatdidntnest)
# on average the hens that didn't nest had 43 points
mean(counthensthatdidntnest$burst)

#Calculating 95% confidence intervals on mean step length
sample.mean<- mean(counthensthatdidntnest$burst,na.rm=TRUE)
sample.n <- nrow(counthensthatdidntnest)

samplen.n<-119
samplen.sd<-sd(counthensthatdidntnest$burst)
samplen.se <- samplen.sd/sqrt(sample.n)

alphan = 0.05
degrees.freedomn = samplen.n - 1
t.scoren = qt(p=alphan/2, df=degrees.freedomn,lower.tail=F)
print(t.scoren)
margin.errorn <- t.scoren * samplen.se

lower.boundn <- sample.mean - margin.errorn
upper.boundn <- sample.mean + margin.errorn
print(c(lower.boundn, upper.boundn))

# see how many hens nested before Feb 20th annually there are 72 of 336 nests (~21% nests before 20 Feb)----
combinednests25before20feb <- combinednests25 %>% filter(attempt_start<="2019-02-20"&attempt_start>="2019-01-01"|attempt_start<="2020-02-20"&attempt_start>="2020-01-01"|attempt_start<="2021-02-20"&attempt_start>="2021-01-01"|attempt_start<="2022-02-20"&attempt_start>="2022-01-01")

# export for list of states in paper 
# write.csv(combinednests25before20feb, "./Data/combinednests25before20feb.csv", row.names = FALSE)

# see how many hens did this
uniquehensnestdatabeforefeb20 <- unique(combinednests25before20feb$burst)

# there were a total of 153 of the 282 tagged as part of project that nested
uniquehensnestdata <- unique(combinednests25$burst)

# number of nests successfully identified by NestR package after careful checking of data is 92% of nests----
24/26

# with min daily fix as 1 and min_consec 5
# 2018 nests 
# ws_output_115 <- nestR::find_nests(gps_data = datefilter2018,
#                                    sea_start = "01-01",
#                                    sea_end = "09-01",
#                                    nest_cycle = 21,
#                                    buffer = 20,
#                                    min_pts = 2,
#                                    min_d_fix = 1,
#                                    min_consec = 5,
#                                    min_top_att = 1,
#                                    min_days_att = 1,
#                                    discard_overlapping = TRUE)
# nests201815 <- ws_output_115$nests
# # explore_nests(ws_output_1$nests)
# 
# # 2019 nests
# ws_output_215 <- nestR::find_nests(gps_data = datefilter2019,
#                                    sea_start = "01-01",
#                                    sea_end = "09-01",
#                                    nest_cycle = 21,
#                                    buffer = 20,
#                                    min_pts = 2,
#                                    min_d_fix = 1,
#                                    min_consec = 5,
#                                    min_top_att = 1,
#                                    min_days_att = 1,
#                                    discard_overlapping = TRUE)
# nests201915 <- ws_output_215$nests
# # explore_nests(ws_output_2$nests)
# 
# # 2020 nests 
# ws_output_315 <- nestR::find_nests(gps_data = datefilter2020,
#                                    sea_start = "01-01",
#                                    sea_end = "09-01",
#                                    nest_cycle = 21,
#                                    buffer = 20,
#                                    min_pts = 2,
#                                    min_d_fix = 1,
#                                    min_consec = 5,
#                                    min_top_att = 1,
#                                    min_days_att = 1,
#                                    discard_overlapping = TRUE)
# nests202015 <- ws_output_315$nests
# # explore_nests(ws_output_3$nests)
# 
# # 2021 nests 
# ws_output_415 <- nestR::find_nests(gps_data = datefilter2021,
#                                    sea_start = "01-01",
#                                    sea_end = "09-01",
#                                    nest_cycle = 21,
#                                    buffer = 20,
#                                    min_pts = 2,
#                                    min_d_fix = 1,
#                                    min_consec = 5,
#                                    min_top_att = 1,
#                                    min_days_att = 1,
#                                    discard_overlapping = TRUE)
# nests202115 <- ws_output_415$nests
# # explore_nests(ws_output_4$nests)
# 
# # 2022 nests
# ws_output_515 <- nestR::find_nests(gps_data = datefilter2022,
#                                    sea_start = "01-01",
#                                    sea_end = "09-01",
#                                    nest_cycle = 21,
#                                    buffer = 20,
#                                    min_pts = 2,
#                                    min_d_fix = 1,
#                                    min_consec = 5,
#                                    min_top_att = 1,
#                                    min_days_att = 1,
#                                    discard_overlapping = TRUE)
# nests202215 <- ws_output_515$nests
# 
# combinednests15 <- rbind(nests201815, nests201915, nests202015, nests202115, nests202215)
# write.csv(combinednests15, "./Data/combinednests15.csv", row.names = FALSE)

# Calculating step lengths (distance between nesting attempts) for 25

# Convert to UTM projection
# EPSG:102008 North America Albers Equal Area Conic
# +proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 


# Project in UTM 19N ----
# UTM zone 19N = EPSG:26919
# PROJ.4 = "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# WGS 84 conversion
str(combinednests25)

# Pull out just long-lat columns from original data
datacoord <- cbind(combinednests25$long, combinednests25$lat)
# Project long-lat coordinates in UTM
datacoordMetric <- data.frame(project(datacoord, "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
# Rename UTM x,y coordinate columns to whatever you want (I did x, y ... you could do easting, northing, whatever ya want)
colnames(datacoordMetric) <- c("x","y")
# Tack on UTM coordinate columns to original dataframe
amwo2022 <- cbind(combinednests25, datacoordMetric)
str(amwo2022)

plot(amwo2022$long, amwo2022$lat)
plot(amwo2022$x, amwo2022$y)

# Format date for adehabitat ----
str(amwo2022)
amwo2022$timestamp <- as.POSIXct(amwo2022$attempt_end, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


# Generate ltraj objects ----
# Generate trajectory
trajamwo <- as.ltraj(xy = amwo2022[,c("x","y")], date = amwo2022$timestamp, id = amwo2022$burst, burst = amwo2022$burst)
# Convert to dataframe so easier to work with
trajamwo_df <- ld(trajamwo)
#   mutate(individual = as.factor(id)) %>% 
#   select(-id)

# total number of hens is 153 in dataset
totalnumberofhensindataset <- unique(trajamwo_df$burst)

# make a km column
trajamwo_df$distkm <- trajamwo_df$dist/1000

# see how many of the 336 nests were renests by removing nas, there are 183 renests
trajamwo_dfnonas <- trajamwo_df %>% tidyr::drop_na(dist)

# write csv of trajamwo_df for combining with attempts and nests 2
# write.csv(trajamwo_df, "./Data/trajamwo_df.csv", row.names = FALSE)

# calculate mean movement length, 442.13 km
mean(trajamwo_df$distkm,na.rm=TRUE)

#Calculating 95% confidence intervals on mean step length
sample.meannest<- mean(trajamwo_df$distkm,na.rm=TRUE)
naomit <- trajamwo_df %>% tidyr::drop_na(dist)
sample.n <- nrow(naomit)

samplen.n<-183
samplen.sd<-sd(naomit$distkm)
samplen.se <- samplen.sd/sqrt(sample.n)

alphan = 0.05
degrees.freedomn = samplen.n - 1
t.scoren = qt(p=alphan/2, df=degrees.freedomn,lower.tail=F)
print(t.scoren)
margin.errorn <- t.scoren * samplen.se

lower.boundn <- sample.meannest - margin.errorn
upper.boundn <- sample.meannest + margin.errorn
print(c(lower.boundn, upper.boundn))

# see how many renests were greater than 50km from the prior nest (102 of 183)
kmgreat50 <- trajamwo_df %>% filter(distkm>50)

# what percent of renests were greater than 50km from last (55.74%)
percgreaterthan50 <- (102/183)*100

# see how many birds made movements greater than 50km (71 of 153) (46.41%)
idsgreat50 <- unique(kmgreat50$id)
idsgreat50whole <- unique(trajamwo_df$id)
percbirdsgreat50 <- (71/153)*100

# see how many renests were greater than 100km from the last (51.91%)
kmgreat100 <- trajamwo_df %>% filter(distkm>100)
# what percent nests more than 100km from last
percgreaterthan100 <- (95/183)*100

# see how many birds made movements greater than 100km (68 of 153) (44.44%)
idsgreat100 <- unique(kmgreat100$id)
idsgreat100whole <- unique(trajamwo_df$id)
percbirdsgreat100 <- (68/153)*100

# Visualize distribution of step lengths ----
  ggplot(trajamwo_df, aes(x = log(distkm))) +
  geom_histogram(fill="darkgreen") +
  labs(x = "Step length (km)") +
  theme_classic()+
  theme(text=element_text(family="serif"))+
  theme(text = element_text(size = 14))

# ggplot of latitiude by distance moved between attempts, I also did by date as suggested by Scott----

# need to add nest attempt number to the combinednests25 file
attemptsandnests <- combinednestswattempts %>% dplyr::select(attempt,attempt_start,burst) %>% unique()
attemptsandnests2 <- left_join(combinednests25,attemptsandnests)
# write.csv(attemptsandnests2, "./Data/attemptsandnests2.csv", row.names = FALSE)

# I took the above file and the trajamwo_df and combined them in excel because i couldn't get it to work in R because no common identifier that would make it happen...
# reread in the attemptsandnests2 now with trajamwo_df distkm values
attemptsandnests2withdistkm <- read.csv("./Data/attemptsandnests2.csv")
str(attemptsandnests2withdistkm)
# make ggplot now
attemptsandnests2withdistkm$attempt <- as.factor(attemptsandnests2withdistkm$attempt)

# geom_smooth for each attempt by distance moved w facet wrap----
# remove nas
naomitnestsandattempts <- na.omit(attemptsandnests2withdistkm)

# faceted version
ggplot(naomitnestsandattempts, aes(x = lat, y = distkm, colour = attempt, group = attempt)) + 
    geom_smooth(aes(group = attempt), method = "lm", se = TRUE)+
    scale_color_manual(values=natparks.pals("KingsCanyon",5), name="Nest Attempt")+
    labs(x = "Latitude") +
    labs(y= "Distance Moved Between Nest Attempts (km)")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme_bw()+
    guides(col= guide_legend(title= "Nest Attempt"))+
    theme(text=element_text(family="serif"))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title=element_text(size= 15))+
    facet_wrap(~attempt)


# distance moved by latitude all in one smoothed lowess line version----
latplot <- ggplot(naomitnestsandattempts, aes(x = lat, y = distkm)) + 
  scale_color_manual()+
  geom_smooth(color=natparks.pals("CraterLake",1))+
  labs(x = "Latitude") +
  labs(y= "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_classic(base_size=40)+
  theme(text=element_text(family="serif"))+
  scale_x_continuous(breaks=c(30,35,40,45,50),labels=c("30\n(Jacksonville, FL)", "35\n(Hiawassee, NC)", "40\n(Philadelphia, PA)", "45\n(Perry, ME)", "50\n(Sept-Iles, QC)"))

# with linear model version
linearlat <- lm(distkm~lat, data=naomitnestsandattempts)
summary(linearlat)


ggplot(naomitnestsandattempts, aes(x = lat, y = distkm)) + 
  stat_smooth(method="lm", level=0.95, method.args = list(family=binomial), color=natparks.pals("CraterLake",1))+
  stat_regline_equation(aes(label =  paste(..eq.label.., sep = "~~~~")), size=6, family="serif")+
  labs(x = "Latitude") +
  labs(y= "Distance moved between nest attempts (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_classic()+
  theme(text=element_text(family="serif"))+
  theme(text = element_text(size = 12))+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size= 15))+
  scale_x_continuous(breaks=c(30,35,40,45,50),labels=c("30\n(Jacksonville, FL)", "35\n(Hiawassee, NC)", "40\n(Philadelphia, PA)", "45\n(Perry, ME)", "50\n(Sept-Iles, QE)"))


# distance moved by date all in one smoothed lowess line version-----
naomitnestsandattempts$attempt_start <- as.Date(naomitnestsandattempts$attempt_start,format='%m/%d/%Y')
naomitnestsandattempts<- naomitnestsandattempts %>% mutate(week=week(attempt_start))   

weekplot <- ggplot(naomitnestsandattempts, aes(x = week, y = distkm)) + 
  scale_color_manual()+
    geom_smooth(color=natparks.pals("CraterLake",1))+
    labs(x = "Week of nesting season") +
    labs(y= "Distance moved between nesting attempts (km)")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme_classic(base_size=40)+
    theme(text=element_text(family="serif"))+
    scale_x_continuous(breaks=c(1,5,10,15,20,25,29),labels=c("1\n(Jan 1st)","5\n(Feb 5th)", "10\n(Mar 12th)", "15\n(Apr 16th)", "20\n(May 21st)", "25\n(Jun 24th)", "28\n(Jul 15th)"))

# with linear model version
linearweek <- lm(distkm~week, data=naomitnestsandattempts)
summary(linearweek)

ggplot(naomitnestsandattempts, aes(x = week, y = distkm)) + 
  stat_smooth(method="lm", level=0.95, method.args = list(family=binomial), color=natparks.pals("CraterLake",1))+
  stat_regline_equation(aes(label =  paste(..eq.label.., sep = "~~~~")), size=6, family="serif")+
  labs(x = "Week of Nesting Season") +
  labs(y= "Distance Moved Between Nest Attempts (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_classic()+
  theme(text=element_text(family="serif"))+
  theme(text = element_text(size = 12))+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size= 15))+
  scale_x_continuous(breaks=c(1,5,10,15,20,25,29),labels=c("1\n(Jan. 1st)","5\n(Feb. 5th)", "10\n(March 12th)", "15\n(April 16th)", "20\n(May 21st)", "25\n(June 24th)", "28\n(July 15th)"))
    
        
        
# see smoothed line equation that geom_smooth made https://stackoverflow.com/questions/67650151/how-to-easily-show-the-equation-behind-ggplots-geom-smooth
attempt1lm <- stats::loess(distkm ~ lat, data = naomitnestsandattempts %>% filter(attempt=="1"))
attempt2lm <- stats::loess(distkm ~ lat, data = naomitnestsandattempts %>% filter(attempt=="2"))
attempt3lm <- stats::loess(distkm ~ lat, data = naomitnestsandattempts %>% filter(attempt=="3"))
attempt4lm <- stats::loess(distkm ~ lat, data = naomitnestsandattempts %>% filter(attempt=="4"))
attempt5lm <- stats::loess(distkm ~ lat, data = naomitnestsandattempts %>% filter(attempt=="5"))


# scatter plot 
windowsFonts()
attemptsandnests2withdistkm$attempt <- as.factor(attemptsandnests2withdistkm$attempt)
ggplot(attemptsandnests2withdistkm,aes(x=lat, y=distkm)) +
    geom_point(aes(color=attempt))+
    labs(x = "Latitude") +
    labs(y= "Distance Moved (km)")+
    scale_color_manual(values=natparks.pals("KingsCanyon",6), name="Nest Attempt")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme_bw()+
    theme(text=element_text(family="serif"))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title = element_text(size = 15))

# check for linearity, it's linear (I think)
# make linear model for lat and distkm----
latanddistkmlm <- lm(distkm ~ lat, data = attemptsandnests2withdistkm)
summary(latanddistkmlm)
model.diag.metrics <- augment(latanddistkmlm)
head(model.diag.metrics)

ggplot2::ggplot(model.diag.metrics, aes(lat, distkm)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = lat, yend = .fitted), color = "red", size = 0.3)

par(mfrow = c(2, 2))
plot(latanddistkmlm)

hist(latanddistkmlm$residuals)


# ggplot for distance moved by time spent nesting----

# calculate time spent nesting
str(attemptsandnests2withdistkm)
attemptsandnests2withdistkm$attempt_start <- as.Date(attemptsandnests2withdistkm$attempt_start,format='%m/%d/%Y')
attemptsandnests2withdistkm$attempt_end <- as.Date(attemptsandnests2withdistkm$attempt_end,format='%m/%d/%Y')

attemptsandnests2withdistkm$totalnesttime <- difftime(attemptsandnests2withdistkm$attempt_end,attemptsandnests2withdistkm$attempt_start, units = c("days"))

attemptsandnests2withdistkm$attempt <- attemptsandnests2withdistkm$attempt

windowsFonts()
ggplot(attemptsandnests2withdistkm,aes(x=totalnesttime, y=distkm)) +
    geom_smooth(color=natparks.pals("KingsCanyon",1)) +
    # scale_x_continuous(limits = c(0, 40)) +
    # geom_density(alpha = 0.3, color = "#FF6666", fill = "#FF6666") +
    labs(x = "Nest Attempt Duration in Days") +
    labs(y= "Distance Moved (km)")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(text=element_text(family="serif"))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title= element_text(size= 15))

# ggplot of year by distance moved between attempts----
trajamwo_df$year <- format(as.POSIXct(trajamwo_df$date), "%Y")

ggplot(trajamwo_df,aes(x=distkm, y=year, col=year)) +
    geom_point() +
    labs(x = "Distance Moved (km)") +
    labs(y= "Date")+
    scale_color_manual(values=natparks.pals("Yellowstone",4), name="Nest Attempt")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(text=element_text(family="serif"))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title = element_text(size = 15))
  
# ANOVA for distance moved by year, there is no difference
year.aov <- aov(distkm ~ year, data = trajamwo_df)
summary(year.aov)
TukeyHSD(year.aov)


# filter out dataset by attempt for measuring renesting intervals
# filter out first nests
firstnests <- combinednests25 %>% group_by(burst) %>% arrange(attempt_start) %>% filter(row_number()==1)
firstnests$attempt <- 1

# filter out second nests
secondnests <- combinednests25 %>% group_by(burst) %>% arrange(attempt_start) %>%  filter(row_number()==2)
secondnests$attempt <- 2 

# filter out third nests
thirdnests <- combinednests25 %>% group_by(burst) %>% arrange(attempt_start) %>% filter(row_number()==3)
thirdnests$attempt <- 3

# filter out fourth nests
fourthnests <- combinednests25 %>% group_by(burst) %>% arrange(attempt_start) %>% filter(row_number()==4)
fourthnests$attempt <- 4

# filter out fifth nests
fifthnests <- combinednests25 %>% group_by(burst) %>% arrange(attempt_start) %>% filter(row_number()==5)
fifthnests$attempt <- 5

# filter out sixth nests
sixthnests <- combinednests25 %>% group_by(burst) %>% arrange(attempt_start) %>% filter(row_number()==6)
sixthnests$attempt <- 6

# make a combined datset with all attempts now as a column
combinednestswattempts <- rbind(firstnests, secondnests, thirdnests, fourthnests, fifthnests, sixthnests)

# write this to a csv to calculate days between nest failure and initiation of the next nest 
# write.csv(combinednestswattempts, "./Data/renestinginterval.csv", row.names = FALSE)

# reread renestinginterval back in because I calculated renesting intervals in excel 
renestinginterval <- read.csv("./Data/renestinginterval.csv")
naremove <- renestinginterval %>% filter(!is.na(renesttime))
naremove2 <- naremove %>% filter(attempt=="2"|attempt=="3"|attempt=="4"|attempt=="5"|attempt=="6") %>% droplevels()

# mean renesting interval 20.89 days, range is 1-85 days----
mean(naremove2$renesttime,na.rm=TRUE)
range(naremove2$renesttime)

# check for birds that nested between two years and delete their distances from excel csv, VA 2019-22 nested in 2019 twice and then again in 2020 so I made 2020 nest attempt 1st for that year in teh csv and deleted the distance, the other 2 birds > 75 days were fine. I then reread the csv in
naremove22 <- naremove2 %>% filter(renesttime>75)

# ggplot of renest interval in days by attempt----
# make attempt a factor
naremove2$attempt <- as.factor(naremove2$attempt)

ggplot(naremove2, aes(x=renesttime,stat="count", color=attempt))+
    geom_density(show_guide=FALSE, size=1.5)+
    stat_density(geom="line",position="identity", size = 0.8)+
    scale_color_manual(name="Renest Interval",labels=c("1-2", "2-3", "3-4","4-5","5-6"),values=natparks.pals("KingsCanyon",6))+
    labs(x = "Number of Days") +
    labs(y= "Proportion of Nests")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme_bw()+
    theme(text=element_text(family="serif"))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title = element_text(size = 15))


# ANOVA for renest time by attempt is not different
renestaov <- aov(renesttime~attempt, data=naremove2)
summary(renestaov)
TukeyHSD(renestaov)
  
# I wanted to see mean nest initiation date for each attempt----
naremove$dateformatt <- as.Date(naremove$attempt_start,format='%m/%d')
naremove1st <- naremove %>% filter(attempt=="1")
naremove2nd <- naremove %>% filter(attempt=="2")
naremove3rd <- naremove %>% filter(attempt=="3")
naremove4th <- naremove %>% filter(attempt=="4")
naremove5th <- naremove %>% filter(attempt=="5")
naremove6th <- naremove %>% filter(attempt=="6")

# mean 1st attempt is March 1st
mean(naremove1st$dateformatt, na.rm=TRUE)

# mean 2nd attempt is April 9th 
mean(naremove2nd$dateformatt, na.rm=TRUE)

# mean 3rd attempt is April 29th
mean(naremove3rd$dateformatt, na.rm=TRUE)

# mean 4th attempt is May 25th
mean(naremove4th$dateformatt, na.rm=TRUE)

# mean 5th attempt is June 25th, altho only 12 nests
mean(naremove5th$dateformatt, na.rm=TRUE)

# mean 6th attempt is June 7th altho only 2 nests
mean(naremove6th$dateformatt, na.rm=TRUE)

# ggplot of renest interval by latitude----
ggplot(naremove2)+
    geom_smooth(aes(x=lat, y=renesttime),color=natparks.pals("KingsCanyon",1)) +
    # geom_density(alpha = 0.3, color = "#FF6666", fill = "#FF6666") +
    labs(x = "Latitude (Degrees)") +
    labs(y= "Renest Interval (Days)")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(text=element_text(family="serif"))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title = element_text(size = 15))

# filter out nests by year
# make year column
combinednests25$year <- format(as.POSIXct(combinednests25$attempt_end), "%Y")

# make date column
combinednests25$date <- format(as.POSIXct(combinednests25$attempt_start), "%m-%d")

# filter out 2019 nests 
combinednests252019 <- combinednests25 %>% filter(year=="2019") %>% droplevels()

# filter out 2020 nests 
combinednests252020 <- combinednests25 %>% filter(year=="2020")%>% droplevels()

# filter out 2021 nests 
combinednests252021 <- combinednests25 %>% filter(year=="2021")%>% droplevels()

# filter out 2022 nests 
combinednests252022 <- combinednests25 %>% filter(year=="2022")%>% droplevels()


# # ggplot of number of nests by year and initiation date----
# ggplot(combinednests252019,aes(x=attempt_start))+
#   geom_histogram() +
#   # geom_density(alpha = 0.3, color = "#FF6666", fill = "#FF6666") +
#   labs(x = "Date") +
#   labs(y= "Number of Nests")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# ggplot(combinednests252020,aes(x=attempt_start))+
#   geom_histogram() +
#   # geom_density(alpha = 0.3, color = "#FF6666", fill = "#FF6666") +
#   labs(x = "Date") +
#   labs(y= "Number of Nests")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# ggplot(combinednests252021,aes(x=attempt_start))+
#   geom_histogram() +
#   # geom_density(alpha = 0.3, color = "#FF6666", fill = "#FF6666") +
#   labs(x = "Date") +
#   labs(y= "Number of Nests")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# ggplot(combinednests252022,aes(x=attempt_start))+
#   geom_histogram() +
#   # geom_density(alpha = 0.3, color = "#FF6666", fill = "#FF6666") +
#   labs(x = "Date") +
#   labs(y= "Number of Nests")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# ggplot of density of nests by week using initiation date colored by attempt----
# making interval week rather than individual  days
# could also use ggarange in ggpubr package to make 4 panel plot with months rather than weeks 


# find average nest initiation date by year
combinednests252019$dateformat <- as.Date(combinednests252019$attempt_start,format='%Y-%m-%d')
# 2019 April 3rd
str(combinednests252019)
combinednests252019mean <- mean(combinednests252019$dateformat)

# 2020 April 5th
combinednests252020$dateformat <- as.Date(combinednests252020$attempt_start,format='%Y-%m-%d')
combinednests252020mean <- mean(combinednests252020$dateformat)

# 2021 March 16th
combinednests252021$dateformat <- as.Date(combinednests252021$attempt_start,format='%Y-%m-%d')
combinednests252021mean <- mean(combinednests252021$dateformat)

# 2022 April 10th
combinednests252022$dateformat <- as.Date(combinednests252022$attempt_start,format='%Y-%m-%d')
combinednests252022mean <- mean(combinednests252022$dateformat)

# all years

combinednestsnew <- combinednests25 %>% mutate(week=week(attempt_start))

# combine combined nest new with combinednestswattempts
combinednestsnewwattempts <- left_join(combinednestsnew, combinednestswattempts)

# need to make sure attempt is a factor so that ggplot will color by it
combinednestsnewwattempts$attempt <- as.factor(combinednestsnewwattempts$attempt)
                 
# ggplot of nest density by week 
ggplot(combinednestsnewwattempts %>% filter(attempt!="6"), aes(x=week,color=attempt))+
    geom_density(show_guide=FALSE, size=1.5) +
    stat_density(geom="line",position="identity", size = 1.1)+
    scale_color_manual(values=natparks.pals("KingsCanyon",5), name="Nest Attempt")+
    # geom_density(alpha = 0.3, color = "#FF6666", fill = "#FF6666") +
    labs(x = "Week of Nesting Season") +
    labs(y= "Proportion of Nests")+
    theme_bw()+
    theme(text=element_text(family="serif"))+
    theme(text = element_text(size = 12))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title = element_text(size = 15))+
    theme(axis.text = element_text(size = 12))+
    xlim(0,35)

   

# ggplot of density of nests by lat using initiation date colored by attempt----
ggplot(combinednestsnewwattempts %>% filter(attempt!="6"), aes(x=lat,color=attempt))+
    geom_density(show_guide = FALSE,size=1.1)+
    stat_density(geom="line",position="identity", size = 1.1)+
    scale_color_manual(values=natparks.pals("KingsCanyon",6), name="Nest Attempt")+
    labs(x = "Latitude") +
    labs(y= "Proportion of Nests")+
    ylim(0,.15)+
    xlim(25,55)+ 
    theme_bw()+
    scale_x_continuous(breaks=c(30,35,40,45,50,55))+
    theme(text = element_text(family="serif", size = 12),
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 15))

# save plot
# ggsave(plotnestattempts, file = "./Plots/plotnestattempts.png", width = 10) 
  
# Bar chart of number of nests by attempt, ggrepel package function geom_repel will add callout to the peaks----
  
# count number of rows (which is number of nests per female)
combinednestswattemptssummary <- combinednestswattempts %>% group_by(burst) %>% summarise(no_rows=length(burst))
str(combinednestswattemptssummary)
combinednestswattemptssummary$no_rows <- as.factor(combinednestswattemptssummary$no_rows)

# include hens that were tracked into June only so that most of nest season is included and we can see how many times these hens nested

  
# plot of number of nests per hen----
ggplot(data=combinednestswattemptssummary, aes(x=no_rows,fill=no_rows))+
    geom_bar() +
    scale_fill_manual(values=natparks.pals("KingsCanyon",n=6,type="discrete"), name="Nest Attempt")+
    labs(x = "Nest Attempt") +
    labs(y= "Number of Hens")+
    theme_bw()+
    theme(text=element_text(family="serif"))+
    theme(text = element_text(size = 12))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title=element_text(size= 15))


# see how many nests are successful or failed---- 
# there are 192 successful >=19 days
attemptsandnests2successfulnests <- attemptsandnests2withdistkm %>% filter(totalnesttime>=19)
attemptsandnests2successfulnests$outcome <- "success"
# there are 144 failed nests attempts <=18 days
attemptsandnests2failednests <- attemptsandnests2withdistkm %>% filter(totalnesttime<=18)
attemptsandnests2failednests$outcome <- "failed"

attemptsandnests2succfailnests <- full_join(attemptsandnests2successfulnests, attemptsandnests2failednests)

# ggplot of failed and successful nests 
ggplot(attemptsandnests2succfailnests, aes(outcome, stat="count")) +
    geom_bar(aes(fill = outcome))+
    labs(x = "Nest Outcome") +
    theme_classic()+
    labs(y= "Number of Nests")+
    scale_fill_manual(values=natparks.pals("KingsCanyon",n=2,type="discrete"))+
    theme(text=element_text(family="serif"))+
    theme(text = element_text(size = 20))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position="none")+
    theme(axis.text = element_text(size = 20))

# make logistic regression model to see if latitude or date predicts whether a successful nest or not----
attemptsandnests2succfailnests$fate <- attemptsandnests2succfailnests$outcome
str(attemptsandnests2succfailnests$fate)

attemptsandnests2succfailnests$outcome <- ifelse(attemptsandnests2succfailnests$fate=="success", "1", "0")

str(attemptsandnests2succfailnests)
attemptsandnests2succfailnests$outcome <- as.factor(attemptsandnests2succfailnests$outcome)

attemptsandnests2succfailnests$attempt_start <- as.Date(attemptsandnests2succfailnests$attempt_start,format='%m/%d/%Y')
attemptsandnests2succfailnests<- attemptsandnests2succfailnests %>% mutate(week=week(attempt_start))   

# convert attempt_start to Julian date(this is an alternative way to plot, but since I used week for all my other plots I decided to end up using week in this plot as well)
attemptsandnests2succfailnests$julian <- format(attemptsandnests2succfailnests$attempt_start, "%j")
attemptsandnests2succfailnests$julian <- as.numeric(attemptsandnests2succfailnests$julian)
attemptsandnests2succfailnests$week <- as.numeric(attemptsandnests2succfailnests$week)

# glm model shows that as latitude increases probability of surviving increases

null <- glm(as.factor(outcome)~1, data=attemptsandnests2succfailnests, family=binomial)
summary(null)

latitudeglmer <- glm(as.factor(outcome)~lat, data=attemptsandnests2succfailnests, family=binomial)
summary(latitudeglmer)

dateglmer <- glm(as.factor(outcome)~week, data=attemptsandnests2succfailnests, family=binomial)
summary.glm(dateglmer)
coef(dateglmer)

confint(dateglmer, 'week', level=0.95)
2.5 % 97.5 %

# Hosmer-Lemeshow Goodness of fit test to see how well logsitic model fits 
install.packages("ResourceSelection")
output <- ResourceSelection::hoslem.test(dateglmer$y, fitted(dateglmer), g=10)
output$p.value

# perform model selection
# make list of models
latanddate <- list(latitudeglmer,dateglmer,null)
#
# name models
latanddatenames <- c("Nest latitude","Week of nesting season", "null")
#
# run AIC on models
latanddateaic <- data.frame(aictab(latanddate, modnames = latanddatenames, second.ord = TRUE))

# write aic table to excel file 
# writexl::write_xlsx(latanddateaic, "./Figures/latanddateaictable.xlsx")

# make ggplot of logistic model, the first line forces the outcome to be 1 and 0 cuz it kept trying to order the number 1 & 2 so I had to do it manual until I found this forum: https://stackoverflow.com/questions/66005572/logistic-regression-error-computation-failed-in-stat-smooth-y-values-must
library(ggplot2)
 nestsurvivalplot <- ggplot(attemptsandnests2succfailnests, aes(week,as.numeric(outcome) - 1)) + 
  geom_smooth( method = "glm", se = TRUE, method.args = list(family = "binomial"), color=natparks.pals("CraterLake",1))+
  theme_classic(base_size=14)+
  stat_regline_equation(aes(label =  paste(..eq.label.., sep = "~~~~")), size=6, family="serif")+
  labs(x = expression(italic(z))) +
  labs(x = "Week of nesting season") +
  labs(y= "Probability of successful nest")+
  theme(text=element_text(family="serif"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme(axis.title = element_text(size = 15))+
  scale_x_continuous(breaks=c(1,5,10,15,20,25,28),labels=c("1\n(Jan. 1st)","5\n(Feb. 5th)", "10\n(March 12th)", "15\n(April 16th)", "20\n(May 21st)", "25\n(June 24th)", "28\n(July15th)"))

# ggsave(plot = nestsurvivalplot, filename = "./Figures/nestsurvivalplot.jpg", dpi = 350, width = 10, height = 8)


# coolors.co to get custom color palletes for plots, it gives you the color code numbers
# make plot of logistic model predictions---

# alternative way using actual model output predications

# make predictions from top model because geom_smooth would not work for some reason
plot_df <- augment(dateglmer, type.predict = "response", se_fit = TRUE)
head(plot_df)

# calculate upper and lower CIs
plot_df$upper <- plot_df$.fitted+1.96*plot_df$.se.fit
plot_df$lower <- plot_df$.fitted-1.96*plot_df$.se.fit

# # annotation for plot
# annotation <- data.frame(
#   label = c("y= 0.42+0.05(x)"))


library(ggpubr)
ggplot(plot_df, aes(x = week)) +
  geom_line(aes(y = .fitted), color=natparks.pals("Saguaro",1)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.4) + 
  labs(x = "Week of Nesting Season", y = "Probability of Successful Nest")+
  theme_classic(base_size = 14)+
  labs(x = "Week of Nesting Season") +
  labs(y= "Probability of a Successful Nest")+
  theme(text=element_text(family="serif"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme(axis.title = element_text(size = 15))+ 
  # geom_text(data=annotation, aes( x=3, y=0.8, label=label),color="black", size=7 , family="serif", fontface="italic")+
  scale_x_continuous(breaks=c(1,5,10,15,20,25,28),labels=c("1\n(Jan. 1st)","5\n(Feb. 5th)", "10\n(March 12th)", "15\n(April 16th)", "20\n(May 21st)", "25\n(June 24th)", "28\n(July15th)"))+
  ylim(.2,1)


# need to merge combined nests25 and the femalesonly dataframe so that I can look at how the age of hen effects nest movements----

# select just burst and age from the femalesonly dataframe so we don't have repeats in the left join
df_merge <- femalesonly %>% dplyr::select(burst,age) %>% unique()

# left join the two dataframes (the one I made in excel above in the code and the df_merge I did above)
dfmerge2 <- left_join(attemptsandnests2withdistkm,df_merge)
unique(dfmerge2$age)
dfmergewages <- dfmerge2 %>% filter(age=="After Hatch Year"|age=="After Second Year"|age=="Second Year")
# select just hatch  year
dfmergewageshy <- dfmerge2 %>% filter(age=="Hatch Year")

# rename hatch year second year
secondyear <- dfmergewageshy %>% 
  mutate(across('age', str_replace, 'Hatch Year', 'Second Year'))

# left join back with dfmergewages that contains the other 3 ages we want 
agesforplot <- full_join(dfmergewages, secondyear)
unique(agesforplot$age)

# ggplot of hen age by renest distance----
windowsFonts()
agesforplot$age <- as.factor(agesforplot)

ggplot(data=agesforplot,aes(x=distkm, y=age, color=age)) +
    geom_point(size=2)+
    scale_color_manual(values=natparks.pals("KingsCanyon", 3), name="Nest Attempt")+
    labs(x = "Distance Moved from Previous Nest Attempt (km)") +
    labs(y= "Hen Age")+
    theme_bw()+
    theme(text=element_text(family="serif"))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title=element_text(size=15))

# calculate mean movement length by age----

# second year birds, 375.21 km
sy <- agesforplot %>% filter(age=="Second Year")
mean(sy$distkm,na.rm=TRUE)

# After Second Year Birds, 478.36 km 
asy <- agesforplot %>% filter(age=="After Second Year")
mean(asy$distkm,na.rm=TRUE)

# After Hatch Year Birds, 677.626 km 
ahy <- agesforplot %>% filter(age=="After Hatch Year")
mean(ahy$distkm,na.rm=TRUE)

# run ANOVA to see if different age hens move different differences between nest attempts, it is only slightly significant between second year and ahy
age.aov <- aov(distkm ~ age, data = agesforplot %>% filter(age=="After Second Year"|age=="Second Year"))
summary(age.aov)
TukeyHSD(age.aov)

# run ANOVA to see if birds move different differences by attempt, first attempt different from attempts 2,3,4,5
agesforplot$attempt <- as.factor(agesforplot$attempt)
attempt.aov <- aov(distkm ~ attempt, data = agesforplot)
summary(attempt.aov)
TukeyHSD(attempt.aov)

# calculate mean distance moved by attempt, after 1st attempt on average moved 632.81 meters----
# there are 6th attempts but no 7th so there is no average distance moved 
onedist <- dfmerge2 %>% filter(attempt=="1")
twodist <- dfmerge2 %>% filter(attempt=="2")
threedist <- dfmerge2 %>% filter(attempt=="3")
fourdist <- dfmerge2 %>% filter(attempt=="4")
fivedist <- dfmerge2 %>% filter(attempt=="5")


mean(onedist$distkm, na.rm=TRUE)
mean(twodist$distkm, na.rm=TRUE)
mean(threedist$distkm, na.rm=TRUE)
mean(fourdist$distkm, na.rm=TRUE)
mean(fivedist$distkm, na.rm=TRUE)
mean(sixdist$distkm, na.rm=TRUE)


# ggplot of number of hens by age----
agesforplot$attempt <- as.factor(agesforplot$attempt)
agesforplot$age <- as.factor(agesforplot$age)

ggplot(agesforplot, aes(age, stat="count")) + 
    geom_bar(aes(fill = age))+
    scale_fill_manual(values=natparks.pals("KingsCanyon",n=3,type="discrete"))+
    labs(x = "Hen Age") +
    labs(y= "Number of Hens")+
    theme(text=element_text(family="serif"))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title=element_text(size=15))+
    theme(legend.position="none")

 
# count the number of attempts by bird by age 
numbersy <- agesforplot %>% filter(age=="Second Year") %>% group_by(burst) %>% summarise(n = n())
numberasy <- agesforplot %>% filter(age=="After Second Year") %>% group_by(burst) %>% summarise(n = n())
numberahy <- agesforplot %>% filter(age=="After Hatch Year") %>% group_by(burst) %>% summarise(n = n())

# merge into one dataframe all of the counts of attempts, there are 148 hens we have age info for 
countsbyage <- rbind(numbersy,numberasy,numberahy)

# see the average number of nests attempts, 2.22 nest attempts for all ages, however remember some birds had no age----
avgnestattempts <- mean(countsbyage$n,na.rm=TRUE)

#Calculating 95% confidence intervals 
sample.meanavg<-mean(countsbyage$n)
nrow(countsbyage)
samplen.navg<-147
samplen.sdavg<-sd(countsbyage$n)
samplen.seavg <- samplen.sdavg/sqrt(samplen.navg)

alphanavg = 0.05
degrees.freedomnavg = samplen.navg - 1
t.scorenavg = qt(p=alphanavg/2, df=degrees.freedomnavg,lower.tail=F)
print(t.scorenavg)
margin.errornavg <- t.scorenavg * samplen.seavg

lower.boundnavg <- sample.meanavg - margin.errornavg
upper.boundnavg <- sample.meanavg + margin.errornavg
print(c(lower.boundnavg, upper.boundnavg))


# see the average number of nest attempts by age 
# first need to combine counts of attempts dataframe with age dataframe
ages <- agesforplot %>% dplyr::select(burst,age) %>% unique()

# now combine with left join
attemptsandages <- left_join(countsbyage,ages)

# ggplot of number of attempts by age by id----
windowsFonts()
ggplot(attemptsandages,aes(x=burst, y=n, col=age)) +
    geom_point() +
    scale_color_manual(values=natparks.pals("KingsCanyon", 3), name="Hen Age")+
    labs(x = "Bird ID") +
    labs(y= "Number of Nest Attempts")+
    theme(text=element_text(family="serif", size=12))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title=element_text(size= 15))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# ggplot of sum of total nest attempts we found by age----
ggplot(attemptsandages, aes(age, stat="count")) +     # Using default colors
    geom_bar(aes(fill = age))+
    scale_fill_manual(values=natparks.pals("KingsCanyon",n=3,type="discrete"), name="Hen Age")+
    labs(x = "Hen Age") +
    labs(y= "Number of Nests")+
    theme(text=element_text(family="serif"))+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title=element_text(size=15))

# run ANOVA to see if there is a difference in the number of nest attempts by age, there is a very significant difference but only between second year and ahy 
numbernestsage.aov <- aov(n ~ age, data = attemptsandages)
summary(numbernestsage.aov)
TukeyHSD(numbernestsage.aov)

# calculate mean number of nest attempts by age----
# second year birds 2.41 nests
symean <- attemptsandages %>% filter(age=="Second Year") 
mean(symean$n, na.rm=TRUE)

# asy birds 2.60 nests
asymean <- attemptsandages %>% filter(age=="After Second Year") 
mean(asymean$n, na.rm=TRUE)

# ahy birds 1.47 nests 
ahymean <- attemptsandages %>% filter(age=="After Hatch Year") 
mean(ahymean$n, na.rm=TRUE)

# see how many hens have more than one nest attempt in the entire dataset, there are 85 of 153 hens disregarding whether they have an age or not (~56%)----
numberattemptsbybird <- attemptsandnests2withdistkm  %>% group_by(burst) %>% summarise(n = n())
numberattemptsgreaterthan1 <- numberattemptsbybird %>% filter(n>1) %>% unique
percentofhensmorethan1 <- (85/153)*100

# arrange plots for publication
loesslatandweek <- plot_grid(weekplot,latplot, labels = c('A', 'B'), label_fontfamily = "serif", label_size = 40)
# ggsave(plot = loesslatandweek, filename = "./Figures/allnestsloesslatandweek.jpg", dpi = 400, width = 35, height = 15)


