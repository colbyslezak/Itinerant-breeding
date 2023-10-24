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
# install.packages("NatParksPalettes")
# install.packages("cowplot")
# install.packages("lme4")
# install.packages("ggmap")
# install.packages("grid")
# install.packages("ggpubr")

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
library(NatParksPalettes)
library(ggdist)
library(lme4)
library(AICcmodavg)
library(cowplot)
library(ggmap)
library(grid)
library(ggpubr)

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

# no dead birds included in combinednests25
dead <- amwo_data %>% filter(mortality=="1")
unique(dead$burst)

# save all the amwo raw data from Movebank
# saveRDS(amwo_data, file = "./ObjectsAndModels/proportionplots/amwodata.rds")
amwo_data <- readRDS(file = "./ObjectsAndModels/proportionplots/amwodata.rds")

# filter out females from 2018-2022 (there are 274 hens)
femalesonly <- amwo_data %>% filter(sex=="f")
femalesonly <- femalesonly %>% filter(start<="2022-06-01")
str(femalesonly)
uniquefemales <- data.frame(unique(femalesonly$burst))
str(uniquefemales)

# filter date periods 2018-2022, there were no females in 2017, 1 in 2018


# for 2018 I wanted to see which females that started collecting points prior to Feb 20th and were tracked past June 1 so I filtered additionally below. I looked what % of these females nested >1 time
datefilter2018 <- femalesonly %>% filter(date >= "2018-01-01" & date <= "2018-09-01") %>% filter(start<= "2018-02-20") %>%  filter(end>="2018-06-01") %>% droplevels()
count2018 <- unique(datefilter2018$burst)


# for 2019 I wanted to see which females that started collecting points prior to Feb 20th and were tracked past June 1 so I filtered additionally below. I looked what % of these females nested >1 time
datefilter2019 <- femalesonly %>% filter(date >= "2019-01-01" & date <= "2019-09-01") %>% filter(start<= "2019-02-20") %>%  filter(end>="2019-06-01")  %>%  droplevels()
datefilter2019$date <- as.Date(datefilter2019$date, format="%Y-%m-%d")
count2019 <- unique(datefilter2019$burst)


# check to see how many points per bird 
summary2019 <- datefilter2019 %>% group_by(burst) %>% count(burst)

# check to see when first point was 
firstpoint2019 <- datefilter2019 %>% 
  group_by(burst) %>%
  arrange(date) %>%
  filter(row_number()==1)

# check for large gaps in data
differenceindayspoints2019 <- datefilter2019 %>%
  group_by(burst) %>%
  mutate(Diff = date - lag(date))%>% filter(Diff>=14) %>% filter(date<="2019-06-01")

# for 2020 I wanted to see which females that started collecting points prior to Feb 20th and were tracked past June 1 so I filtered additionally below. I looked what % of these females nested >1 time
datefilter2020 <- femalesonly %>% filter(date >= "2020-01-01" & date <= "2020-09-01") %>%  filter(start<= "2020-02-20") %>% filter(end>="2020-06-01") %>% filter(burst!="NS-2019-02") %>% droplevels()
datefilter2020$date <- as.Date(datefilter2020$date, format="%Y-%m-%d")
count2020 <- unique(datefilter2020$burst)

# see how many points collected per bird, QUE-2020-19 had less than 10 points so I excluded it below 
summary2020 <- datefilter2020 %>% group_by(burst) %>% count(burst)

# check to see when first point was 
firstpoint2020 <- datefilter2020 %>% 
  group_by(burst) %>%
  arrange(date) %>%
  filter(row_number()==1)

# check for large gaps in data
differenceindayspoints2020 <- datefilter2020 %>%
  group_by(burst) %>%
  mutate(Diff = date - lag(date))%>% filter(Diff>=14)%>% filter(date<="2020-06-01")

# for 2021 I wanted to see which females that started collecting points prior to Feb 20th and were tracked past June 1 so I filtered additionally below. I looked what % of these females nested >1 time
datefilter2021 <- femalesonly %>% filter(date >= "2021-01-01" & date <= "2021-09-01") %>%  filter(start<= "2021-02-20") %>% filter(end>="2021-06-01") %>% filter(burst!="QUE-2020-19") %>%  droplevels()
datefilter2021$date <- as.Date(datefilter2021$date, format="%Y-%m-%d")
count2021 <- unique(datefilter2021$burst)

# see how many points collected per bird
summary2021 <- datefilter2021 %>% group_by(burst) %>% count(burst)

# check to see when first point was 
firstpoint2021 <- datefilter2021 %>% 
  group_by(burst) %>%
  arrange(date) %>%
  filter(row_number()==1)

# check for large gaps in data
differenceindayspoints2021 <- datefilter2021 %>%
  group_by(burst) %>%
  mutate(Diff = date - lag(date))%>% filter(Diff>=14)%>% filter(date<="2021-06-01")

# for 2022 I wanted to see which females that started collecting points prior to Feb 20th and were tracked past June 1 so I filtered additionally below. I looked what % of these females nested >1 time
datefilter2022 <- femalesonly %>% filter(date >= "2022-01-01" & date <= "2022-09-01") %>%  filter(start<= "2022-02-20") %>%  filter(end>="2022-06-01") %>% droplevels()
datefilter2022$date <- as.Date(datefilter2022$date, format="%Y-%m-%d")
count2022 <- unique(datefilter2022$burst)

summary2022 <- datefilter2022 %>% group_by(burst) %>% count(burst)

# check to see when first point was 
firstpoint2022 <- datefilter2022 %>% 
  group_by(burst) %>%
  arrange(date) %>%
  filter(row_number()==1)

# check for large gaps in data
differenceindayspoints2022 <- datefilter2022 %>%
  group_by(burst) %>%
  mutate(Diff = date - lag(date)) %>% filter(Diff>=14)%>% filter(date<="2022-06-01")


# there are 55 hens total in the dataset tracked from Feb 20th- June 1st excluding QUE-2020-19 in 2021 which had few points (6) and NS-2019-02 in 2020 which had several large gaps in data collection----
febtojunehendata <- rbind(datefilter2018, datefilter2019, datefilter2020, datefilter2021, datefilter2022)

# see how many points per hen 
numberofpointsfebjunehens <- febtojunehendata %>% group_by(burst) %>% summarise(no_rows=length(burst))

# on average 84.727 points collected per hen +/- 9.50 points from 20 Feb until 1 Sept----
sample.meanavg<-mean(numberofpointsfebjunehens$no_rows)
nrow(numberofpointsfebjunehens)
samplen.navg<-55
samplen.sdavg<-sd(numberofpointsfebjunehens$no_rows)
samplen.seavg <- samplen.sdavg/sqrt(samplen.navg)

alphanavg = 0.05
degrees.freedomnavg = samplen.navg - 1
t.scorenavg = qt(p=alphanavg/2, df=degrees.freedomnavg,lower.tail=F)
print(t.scorenavg)
margin.errornavg <- t.scorenavg * samplen.seavg

lower.boundnavg <- sample.meanavg - margin.errornavg
upper.boundnavg <- sample.meanavg + margin.errornavg
print(c(lower.boundnavg, upper.boundnavg))
  
uniquefemales <- data.frame(unique(febtojunehendata$burst))
str(uniquefemales)

# I wanted to pull out the first 2 letters from the burst column in the unique females data frame so that I can summarize number of tags from each state and province
uniquefemales$stateprov <- substr(uniquefemales$unique.febtojunehendata.burst. , start = 1 , stop = 2 )
uniquefemales$stateprov <- as.factor(uniquefemales$stateprov)

uniquefemales$year <- substr(uniquefemales$unique.febtojunehendata.burst. , start = 4 , stop = 8 )

# summarize number of tags per state and province----
stateprovsummary <- uniquefemales %>% group_by(stateprov) %>% group_by(year) %>% count(stateprov)
str(stateprovsummary)

# there are still 55 hens
sum(stateprovsummary$n,na.rm=TRUE)

# figure out start date range for each province and state for each year 
febtojunehendata$stateprov <- substr(febtojunehendata$burst , start = 1 , stop = 2 )
febtojunehendata$year <- format(as.POSIXct(febtojunehendata$start), "%Y")
febtojunehendata$month <- format(as.POSIXct(febtojunehendata$start), "%m")

# separate into catching seasons becuase some states caught in more than one, and count number of hens per state province
str(febtojunehendata)
febtojunehendata$stateprov <- as.factor(febtojunehendata$stateprov)

febtojunesummarysummerandfall <- febtojunehendata %>% filter(month=="07"|month=="08"|month=="09"|month=="10"|month=="11") %>% dplyr::group_by(year, stateprov)%>% summarise(count = n_distinct(burst))
# there are 29 hens still transmitting from summer-fall
sum(febtojunesummarysummerandfall$count)
febtojunesummarywinterspring <- febtojunehendata %>% filter(month=="04"|month=="05"|month=="06"|month=="12"|month=="01"|month=="02"|month=="03") %>% dplyr::group_by(year, stateprov)%>% summarise(count = n_distinct(burst))
# there are 26 hens still transmitting from winter-spring
sum(febtojunesummarywinterspring$count)

# write.csv(femalesonlysummarysummerandfall, "./Data/femalesonlysummarysummerandfall.csv", row.names = FALSE)
# write.csv(femalesonlysummarywinterspring, "./Data/femalesonlysummarywinterspring.csv", row.names = FALSE)


# # combine all files into one dataframe
# combinedbeforeseptember <- rbind(datefilter2018, datefilter2019, datefilter2020, datefilter2021, datefilter2022)
# str(combinedbeforeseptember)

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

# save kfoldlme2 output (my top model) so that you don't have to run it again
# saveRDS(nests201825, file = "./ObjectsAndModels/proportionplots/nests201825.rds")

# # read model back in to environment
# nests201825 <- readRDS("./ObjectsAndModels/proportionplots/nests201825.rds")

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
# saveRDS(nests201925, file = "./ObjectsAndModels/proportionplots/nests201925.rds")

# read model back in to environment
nests201925 <- readRDS("./ObjectsAndModels/proportionplots/nests201925.rds")

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
# saveRDS(nests202025, file = "./ObjectsAndModels/proportionplots/nests202025.rds")

# read model back in to environment
nests202025 <- readRDS("./ObjectsAndModels/proportionplots/nests202025.rds")

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
# saveRDS(nests202125, file = "./ObjectsAndModels/proportionplots/nests202125.rds")

# read model back in to environment
nests202125 <- readRDS("./ObjectsAndModels/proportionplots/nests202125.rds")

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
# saveRDS(nests202225, file = "./ObjectsAndModels/proportionplots/nests202225.rds")

# read model back in to environment
nests202225 <- readRDS("./ObjectsAndModels/proportionplots/nests202225.rds")



# there are a total of 145 nests
combinednests25 <- rbind(nests201925, nests202025, nests202125, nests202225)

# write.csv(combinednests25, "./Data/proportionplots/combinednests25.csv", row.names = FALSE)


# filter out nests from hens that were dead via Liam's HMM model, there are now 144 nests 
combinednests25 <- combinednests25 %>% filter(!(burst=="NC-2021-21" & loc_id == 33)) %>% filter(!(burst=="LA-2022-14" & loc_id == 59)) %>% filter(!(burst=="NJ-2019-26" & loc_id == 23))%>% filter(!(burst=="NY-2019-19" & loc_id == 5))%>% filter(!(burst=="NY-2022-46" & loc_id == 4)) %>% filter(!(burst=="RI-2018-02"& loc_id== 7)) %>% filter(!(burst=="RI-2021-52" & loc_id == 10)) %>% filter(!(burst=="VA-2019-26" & loc_id == 3)) %>% filter(!(burst=="VA-2020-82"))%>% filter(!(burst=="NC-2020-09")) %>% filter(!(burst=="VA-2019-16"))%>% filter(!(burst=="VA-2019-24")) %>% filter(!(burst=="LA-2022-14" & loc_id == 59)) %>% filter(!(burst=="NJ-2019-25" & loc_id == 72)) %>% filter(!(burst=="NJ-2019-25" & loc_id == 74)) 

# 52 of the 55 hens we tracked Feb 20-June 1 nested 
uniquehensnestdata <- unique(combinednests25$burst)

# make date column
combinednests25$date <- format(as.POSIXct(combinednests25$attempt_start), "%m-%d")

# find earliest and latest nests-- earliest in January 9th
combinednestsbeforejan <- combinednests25 %>% filter(date<="01-31")


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
library(sf)
# Transform to NAD83(HARN)/UTM 19N (EPSG = 3749)
amwo2022 <- st_as_sf(combinednests25, coords = c("long", "lat"))
## amwo.wgs.84 is dataframe with coordinates in WGS84
## st_as_sf converts the dataframe to a spatial object
st_crs(amwo2022) <- 4326
## specify the projection of the spatial object with the EPSG code (not proj.4 string)
amwo202219n <- st_transform(amwo2022, "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs")
## Final formatting (b/c spatial object stores coordinates different than regular dataframe)
amwo202219n <- amwo202219n %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  glimpse()

str(amwo202219n)
plot(amwo202219n$x, amwo202219n$y)

# Format date for adehabitat ----
str(amwo202219n)
amwo2022$timestamp <- as.POSIXct(amwo202219n$attempt_end, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


# Generate ltraj objects ----
# Generate trajectory
trajamwo <- as.ltraj(xy = amwo202219n[,c("x","y")], date = amwo2022$timestamp, id = amwo2022$burst, burst = amwo2022$burst)
# Convert to dataframe so easier to work with
trajamwo_df <- ld(trajamwo)
#   mutate(individual = as.factor(id)) %>% 
#   select(-id)

# total number of hens is 52 in dataset
totalnumberofhensindataset <- unique(trajamwo_df$burst)

# make a km column
trajamwo_df$distkm <- trajamwo_df$dist/1000

# see how many of the 144 nests were renests by removing nas, there are 92 renests
trajamwo_dfnonas <- trajamwo_df %>% tidyr::drop_na(dist)

# write csv of trajamwo_df for combining with attempts and nests 2
# write.csv(trajamwo_df, "./Data/proportionplots/trajamwo_df.csv", row.names = FALSE)

# calculate mean movement length, 499.95 km
mean(trajamwo_df$distkm,na.rm=TRUE)

#Calculating 95% confidence intervals on mean step length
sample.meannest<- mean(trajamwo_df$distkm,na.rm=TRUE)
naomit <- trajamwo_df %>% tidyr::drop_na(dist)
sample.n <- nrow(naomit)

samplen.n<-92
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

# see how many renests were greater than 50km from the prior nest (58 of 92)
kmgreat50 <- trajamwo_df %>% filter(distkm>50)

# what percent of renests were greater than 50km from last (63.04%)
percgreaterthan50 <- (58/92)*100

# see how many birds made movements greater than 50km (39 of 52) (75%)
idsgreat50 <- unique(kmgreat50$id)
idsgreat50whole <- unique(trajamwo_df$id)
percbirdsgreat50 <- (39/52)*100

# see how many renests were greater than 100km from the last (57.61%)
kmgreat100 <- trajamwo_df %>% filter(distkm>100)
# what percent nests more than 100km from last
percgreaterthan100 <- (53/92)*100

# see how many birds made movements greater than 100km (37 of 52) (71.15%)
idsgreat100 <- unique(kmgreat100$id)
idsgreat100whole <- unique(trajamwo_df$id)
percbirdsgreat100 <- (37/52)*100

# Visualize distribution of step lengths ----
ggplot(trajamwo_df, aes(x = log(distkm))) +
  geom_histogram(fill="darkgreen") +
  labs(x = "Step length (km)") +
  theme_classic()+
  theme(text=element_text(family="serif"))+
  theme(text = element_text(size = 14))


# filter out dataset by attempt for measuring renesting intervals
# filter out first nests
firstnests <- combinednests25 %>% group_by(burst) %>% arrange(attempt_start) %>% filter(row_number()==1)
firstnests$attempt <- 1

# get range and mean of latitudes for first nesting attempts----
range(firstnests$lat)
mean(firstnests$lat)

# calculate 95% CI... yes 0.975 even tho for 95% CI
error <- qt(0.975,df=length(firstnests$lat)-1)*sd(firstnests$lat)/sqrt(length(firstnests$lat))

# Lower CI is 36.85 and Upper CI is 39.21 degrees 
mean(firstnests$lat)+error
mean(firstnests$lat)-error


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

# write this to a csv to calculate days between nest failure and initiation of the next nest and then I reread this in down below in the code
# write.csv(combinednestswattempts, "./Data/proportionplots/renestinginterval.csv", row.names = FALSE)

# ggplot of latitiude by distance moved between attempts, I also did by date as suggested by Scott----

# need to add nest attempt number to the combinednests25 file
attemptsandnests <- combinednestswattempts %>% dplyr::select(attempt,attempt_start,burst) %>% unique()
attemptsandnests2 <- left_join(combinednests25,attemptsandnests)
# write.csv(attemptsandnests2, "./Data/proportionplots/attemptsandnests2.csv", row.names = FALSE)

# I took the above file and the trajamwo_df and combined them in excel because i couldn't get it to work in R because no common identifier that would make it happen...
# reread in the attemptsandnests2 now with trajamwo_df distkm values
attemptsandnests2withdistkm <- read.csv("./Data/proportionplots/attemptsandnests2.csv")
str(attemptsandnests2withdistkm)
# make ggplot now
attemptsandnests2withdistkm$attempt <- as.factor(attemptsandnests2withdistkm$attempt)

# geom_smooth for each attempt by distance moved w facet wrap----
# remove nas
naomitnestsandattempts <- na.omit(attemptsandnests2withdistkm)

# faceted version
ggplot(naomitnestsandattempts %>% filter(attempt!="5"), aes(x = lat, y = distkm, colour = attempt, group = attempt)) + 
  geom_smooth(aes(group = attempt), method = "lm", se = TRUE)+
  scale_color_manual(values=natparks.pals("KingsCanyon",5), name="Nest attempt")+
  labs(x = "Latitude") +
  labs(y= "Distance moved (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_bw()+
  guides(col= guide_legend(title= "Nest Attempt"))+
  theme(text=element_text(family="serif"))+
  theme(axis.text = element_text(size = 12))+
  theme(axis.title=element_text(size= 15))+
  facet_wrap(~attempt)


# all in one smoothed lowess line version
ggplot(naomitnestsandattempts, aes(x = lat, y = distkm)) + 
  geom_smooth(color=natparks.pals("Saguaro",1))+
  labs(x = "Latitude") +
  labs(y= "Distance Moved (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_classic()+
  theme(text=element_text(family="serif"))+
  theme(text = element_text(size = 12))+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size= 15))

# with linear model version
linear <- lm(distkm~lat, data=naomitnestsandattempts)
summary(linear)

ggplot(naomitnestsandattempts, aes(x = lat, y = distkm)) + 
  stat_smooth(method="lm", se=TRUE, method.args = list(family=binomial), color=natparks.pals("Saguaro",1))+
  stat_regline_equation(aes(label =  paste(..eq.label.., sep = "~~~~")), size=6, family="serif")+
  labs(x = "Latitude") +
  labs(y= "Distance Moved (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_classic()+
  theme(text=element_text(family="serif"))+
  theme(text = element_text(size = 12))+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size= 15))




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
  scale_color_manual(values=natparks.pals("KingsCanyon",6), name="Nest attempt")+
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

# check for statistical difference in nest attempt duration between each nesting attempt 1-4----
attemptandnests2withdistkmm <- attemptsandnests2withdistkm %>% filter(attempt=="1"|attempt=="2"|attempt=="3"|attempt=="4")
attemptdurationaov <- aov(totalnesttime~attempt, data=attemptandnests2withdistkmm)
summary(attemptdurationaov)

# calculate mean nest duration time----
mean(attemptandnests2withdistkmm$totalnesttime)

# calculate 95% CI... yes 0.975 even tho for 95% CI
error <- qt(0.975,df=length(attemptandnests2withdistkmm$totalnesttime)-1)*sd(attemptandnests2withdistkmm$totalnesttime)/sqrt(length(attemptandnests2withdistkmm$totalnesttime))

# Lower CI is 15.65 & upper CI is 17.28 days
mean(attemptandnests2withdistkmm$totalnesttime)+error
mean(attemptandnests2withdistkmm$totalnesttime)-error

# see how many hens nests lasted less than 19 days 60 nests
attemptsandnests2withdistkmlessthan19 <- attemptsandnests2withdistkm %>% filter(totalnesttime<19)


windowsFonts()

# boxplot of time spent nesting by bird----
nestdurationplot <- ggplot(attemptsandnests2withdistkm,aes(x=attempt,y=totalnesttime,fill=attempt)) +
  geom_boxplot(size=1, outlier.size=5, outlier.shape="cross", color="black") +
  stat_summary(fun.y="mean", size=2, shape="diamond", alpha=4, color="black")+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 1.5), size = 5, alpha = 0.9)+
  stat_boxplot(geom='errorbar', size=1)+
  scale_y_continuous(breaks=seq(0,21,5))+
  theme_classic(base_size = 30)+
  scale_fill_manual(values=natparks.pals("Saguaro",6))+
  # scale_x_continuous(limits = c(0, 40)) +
  # geom_density(alpha = 0.3, color = "#FF6666", fill = "#FF6666") +
  labs(x = "Nesting attempt") +
  labs(y= "Nest duration (days)")+
  theme(text=element_text(family="serif"))+
  theme(axis.title= element_text(size= 30))+
  theme(legend.position = "none")

# boxplot of distance moved by attempt 
distmovedbyattemptplot <- ggplot(attemptsandnests2withdistkm %>% filter(attempt!="6"),aes(x=attempt,y=distkm,fill=attempt)) +
  geom_boxplot(size=1, outlier.size=5, outlier.shape="cross", color="black") +
  stat_summary(fun.y="mean", size=2, shape="diamond", alpha=4, color="black")+
  geom_point(position = position_jitterdodge(dodge.width = 1.5, jitter.width = 1.5), size = 5,alpha = 0.9)+
  stat_boxplot(geom='errorbar', size=1)+
  scale_y_continuous(breaks=seq(0,2500,250))+
  theme_classic(base_size = 30)+
  scale_fill_manual(values=natparks.pals("Saguaro",5))+
  scale_x_discrete(labels=c('1-2', '2-3', '3-4', '4-5', "5-6"))+
  # scale_x_continuous(limits = c(0, 40)) +
  # geom_density(alpha = 0.3, color = "#FF6666", fill = "#FF6666") +
  labs(x = "Renest interval") +
  labs(y= "Distance moved (km)")+
  theme(text=element_text(family="serif"))+
  theme(axis.title= element_text(size= 30))+
  theme(legend.position = "none")

# ggsave(plot = distmovedbynestinterval, filename = "./Figures/distmovedbynestinterval.jpg", dpi = 350, width = 10, height = 8)


# ggplot of year by distance moved between attempts----
trajamwo_df$year <- format(as.POSIXct(trajamwo_df$date), "%Y")

ggplot(trajamwo_df,aes(x=distkm, y=year, col=year)) +
  geom_point() +
  labs(x = "Distance Moved (km)") +
  labs(y= "Date")+
  scale_color_manual(values=natparks.pals("Saguaro",4), name="Nest Attempt")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="serif"))+
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size = 15))

# ANOVA for distance moved by year, there is no difference
year.aov <- aov(distkm ~ year, data = trajamwo_df)
summary(year.aov)
TukeyHSD(year.aov)

# reread renestinginterval back in because I calculated renesting intervals in excel 
renestinginterval <- read.csv("./Data/proportionplots/renestinginterval.csv")
naremove <- renestinginterval %>% filter(!is.na(renesttime))
naremove2 <- naremove %>% filter(attempt=="2"|attempt=="3"|attempt=="4"|attempt=="5"|attempt=="6") %>% droplevels()
naremove2anova <- naremove2 %>% filter(attempt=="2"|attempt=="3"|attempt=="4")
naremove2anova$attempt <- as.factor(naremove2anova$attempt)
# mean renesting interval 23.95 days, range is 1-72 days----
mean(naremove2anova$renesttime,na.rm=TRUE)
range(naremove2anova$renesttime)

# calculate 95% CIs for renest time----
error2 <- qt(0.975,df=length(naremove2anova$renesttime)-1)*sd(naremove2anova$renesttime)/sqrt(length(naremove2anova$renesttime))

# Lower CI is 20.04 & upper CI is 27.86 days
mean(naremove2anova$renesttime)+error2
mean(naremove2anova$renesttime)-error2

# check for birds that nested between two years and delete their distances from excel csv, there were none
naremove22 <- naremove2 %>% filter(renesttime>75)

# ggplot of renest interval in days by attempt:raincloud plot https://rpubs.com/rana2hin/raincloud----
# make attempt a factor
naremove2$attempt <- as.factor(naremove2$attempt)
ggplot(data=naremove2 %>% filter(attempt!="6"), aes(x=renesttime, y=attempt, fill=attempt))+
  scale_y_discrete(breaks = c(2,3,4,5), labels=c("1-2", "2-3", "3-4", "4-5"))+
  scale_fill_manual(values=natparks.pals("Saguaro",4),name="Renest Interval",labels=c('1-2', '2-3', '3-4', '4-5', "5-6"))+
  theme_classic(base_size=20)+
  theme(text=element_text(family="serif"))+
  stat_halfeye(# adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    scale = 0.5)+
  geom_boxplot(width = 0.12,
               # removing outliers
               outlier.color = NA,
               alpha = 0.5)+
  stat_summary(fun.y="mean")+
  stat_dots(# ploting on left side
    side = "left",
    # adjusting position
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = 0.70)+
  theme(axis.title = element_text(size = 15))+
  # Themes and Labels
  labs(
    x = "Renest Time (Days)",
    y = "Renest Interval",
    fill = "attempt")+
  theme(legend.position = "top")
  


# alternate version of renest interval in days by attempt
renestplot <- ggplot(data=naremove2, aes(x=attempt, y=renesttime, fill=attempt))+
  geom_boxplot(size=1, outlier.size=5, outlier.shape="cross", color="black") +
  stat_summary(fun.y="mean", size=2, shape="diamond", alpha=4, color="black")+
  stat_boxplot(geom='errorbar', size=1)+
  geom_point(position = position_jitterdodge(dodge.width = 1.5, jitter.width = 1.5), size = 5, alpha = 0.9)+
  theme_classic(base_size=30)+
  scale_fill_manual(values=natparks.pals("Saguaro",6))+
  scale_x_discrete(labels=c('1-2', '2-3', '3-4', '4-5', "5-6"))+
  labs(x = "Renest interval") +
  labs(y= "Renest time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title = element_text(size = 30))+
  theme(text=element_text(family="serif"))+
  theme(legend.position="none")


# ANOVA for renest time attempts 1-4 is not different
renestaov <- aov(renesttime~attempt, data=naremove2anova)
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

# mean 1st attempt is March 4th
mean(naremove1st$dateformatt, na.rm=TRUE)

# mean 2nd attempt is April 10th 
mean(naremove2nd$dateformatt, na.rm=TRUE)

# mean 3rd attempt is April 29th
mean(naremove3rd$dateformatt, na.rm=TRUE)

# mean 4th attempt is June 2nd
mean(naremove4th$dateformatt, na.rm=TRUE)

# mean 5th attempt is June 16th, altho only 5 nests
mean(naremove5th$dateformatt, na.rm=TRUE)

# mean 6th attempt is April 19th altho only 1 nests
mean(naremove6th$dateformatt, na.rm=TRUE)

# ggplot of renest interval by latitude----
ggplot(naremove2)+
  geom_smooth(aes(x=lat, y=renesttime),colour=natparks.pals("Glacier",1)) +
  # geom_density(alpha = 0.3, color = "#FF6666", fill = "#FF6666") +
  labs(x = "Latitude (Degrees)") +
  labs(y= "Renest Interval (Days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="serif"))+
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size = 15))

# see how mnay birds had a renest time >50 days to see whether there is indication of double brooding as per Scott. M. Suggestion, this seems to be rare because there are only 10 nests that had that long of a renest interval0----
renestgreat50days <- naremove2 %>% filter(renesttime>=50)

combinednests25$dateformat <- as.Date(combinednests25$attempt_start,format='%Y-%m-%d')

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

# 2019 April 6th
combinednests252019$dateformat <- as.Date(combinednests252019$dateformat,format='%Y-%m-%d')
combinednests252019mean <- mean(combinednests252019$dateformat)

# 2020 April 13th
combinednests252020$dateformat <- as.Date(combinednests252020$dateformat,format='%Y-%m-%d')
combinednests252020mean <- mean(combinednests252020$dateformat)

# 2021 April 9th
combinednests252021$dateformat <- as.Date(combinednests252021$dateformat,format='%Y-%m-%d')
combinednests252021mean <- mean(combinednests252021$dateformat)

# 2022 April 5th
combinednests252022$dateformat <- as.Date(combinednests252022$dateformat,format='%Y-%m-%d')
combinednests252022mean <- mean(combinednests252022$dateformat)

# all years

combinednestsnew <- combinednests25 %>% mutate(week=week(attempt_start))

# combine combined nest new with combinednestswattempts
combinednestsnewwattempts <- left_join(combinednestsnew, combinednestswattempts)

# need to make sure attempt is a factor so that ggplot will color by it
combinednestsnewwattempts$attempt <- as.factor(combinednestsnewwattempts$attempt)

# ggplot of nest density by week raincloud plot----
weekofnestingseasonplot <- ggplot(combinednestsnewwattempts %>% filter(attempt!="6"), aes(x=week,y=attempt,fill=attempt))+
  stat_halfeye(show.legend = FALSE,
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    scale = 0.5
  )+
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  )+
  stat_summary(fun.y="mean")+
  stat_dots(
    # ploting on left side
    side = "left",
    # adjusting position
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = 0.25,position = position_dodge(width = 0.75)
  )+
  scale_fill_manual(values=natparks.pals("Saguaro",5), name="Nest attempt")+
  # Themes and Labels
  labs(
    x = "Week of nesting season",
    y = "Nesting attempt",
    fill = "attempt"
  ) +
  theme_classic(base_size=40)+
  theme(text=element_text(family="serif"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title = element_text(size = 40))+
  theme(legend.position = "top")+
  scale_x_continuous(breaks=c(1,5,10,15,20,25,30),labels=c("1\n(Jan 1st)","5\n(Feb 5th)", "10\n(Mar 12th)", "15\n(Apr 16th)", "20\n(May 21st)", "25\n(Jun 24th)", "28\n(Jul 30th)"))
  


# alternate version ggplot of nest density by week 
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

latitudeplot <- ggplot(combinednestsnewwattempts %>% filter(attempt!="6"), aes(x=lat,y=attempt,fill=attempt))+
  stat_halfeye(show_guide = FALSE,
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    scale = 0.5
  )+
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  )+
  stat_summary(fun.y="mean")+
  stat_dots(
    # ploting on left side
    side = "left",
    # adjusting position
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = 0.15) +
  scale_fill_manual(values=natparks.pals("Saguaro",5), name="Nest attempt")+
  # Themes and Labels
  labs(
    x = "Latitude",
    y = "",
    fill = "attempt"
  ) +
  theme_classic(base_size=40)+
  theme(text=element_text(family="serif"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title = element_text(size = 40))+
  theme(legend.position = "top")+
  # theme(axis.ticks.y = element_blank())+
  # theme(axis.text.y = element_blank())+
  scale_x_continuous(breaks=c(30,35,40,45,50),labels=c("30\n(Jacksonville, FL)", "35\n(Hiawassee, NC)", "40\n(Philadelphia, PA)", "45\n(Perry, ME)", "50\n(Sept-Iles, QC)"))

# alternate version ggplot of density of nests by lat using initiation date colored by attempt
ggplot(combinednestsnewwattempts %>% filter(attempt!="6"), aes(x=lat,color=attempt))+
  geom_density(show_guide = FALSE,size=1.1)+
  stat_density(geom="line",position="identity", size = 1.1)+
  scale_color_manual(values=natparks.pals("KingsCanyon",6), name="Nest Attempt")+
  labs(x = "Latitude") +
  labs(y= "Proportion of Nests")+
  ylim(0,.16)+
  xlim(25,55)+ 
  theme_classic()+
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


# see how many hens had greater than 1 nest attempt
combinednestswattemptssummary$no_rows <- as.numeric(combinednestswattemptssummary$no_rows)
greaterthan1nest <- combinednestswattemptssummary %>% filter(no_rows>1)

# ggplot total number of nests per hen----
combinednestswattemptssummary$no_rows <- as.factor(combinednestswattemptssummary$no_rows)

numberofnestsperhen <- ggplot(data=combinednestswattemptssummary, aes(x=no_rows,fill=no_rows))+
  scale_fill_manual(values=natparks.pals("Saguaro",n=6,type="discrete"), name="Nest Attempt")+
  geom_bar(color="black") +
  # ggpubr::rotate()+
  labs(x = "Total number of nesting attempts") +
  labs(y= "Number of hens")+
  theme_classic(base_size=30)+
  theme(text=element_text(family="serif"))+
  theme(text = element_text(size = 30))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title=element_text(size= 30))+
  theme(legend.position = "none")

# ggsave(plot = numberofnestsperhen, filename = "./Figures/numberofnestsperhen.jpg", dpi = 350, width = 10, height = 8)
  

# see how many nests are successful or failed---- 
# there are 84 successful >=19 days
attemptsandnests2successfulnests <- attemptsandnests2withdistkm %>% filter(totalnesttime>=19)
attemptsandnests2successfulnests$outcome <- "success"
# there are 60 failed nests attempts <=18 days
attemptsandnests2failednests <- attemptsandnests2withdistkm %>% filter(totalnesttime<=18)
attemptsandnests2failednests$outcome <- "failed"

attemptsandnests2succfailnests <- full_join(attemptsandnests2successfulnests, attemptsandnests2failednests)
attemptsandnests2succfailnests$outcome <- as.factor(attemptsandnests2succfailnests$outcome)

# ggplot of failed and successful nests 
ggplot(attemptsandnests2succfailnests, aes(outcome, stat="count")) +
  geom_bar(aes(fill = outcome))+
  theme_classic()+
  labs(x = "Nest outcome") +
  labs(y= "Number of nests")+
  scale_fill_manual(values=natparks.pals("KingsCanyon",n=2,type="discrete"))+
  theme(text=element_text(family="serif"))+
  theme(text = element_text(size = 20))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme(axis.text = element_text(size = 20))

# make logistic regression model to see if latitude or date predicts whether a successful nest or not----
attemptsandnests2succfailnests$fate <- attemptsandnests2succfailnests$outcome
attemptsandnests2succfailnests$outcome <- attemptsandnests2succfailnests$fate
str(attemptsandnests2succfailnests$fate)
attemptsandnests2succfailnests$outcome <- ifelse(attemptsandnests2succfailnests$fate=="success", "1", "0")
str(attemptsandnests2succfailnests)
attemptsandnests2succfailnests$outcome <- as.numeric(attemptsandnests2succfailnests$outcome)

# convert attempt_start to Julian date  
attemptsandnests2succfailnests$julian <- format(attemptsandnests2succfailnests$attempt_start, "%j")
attemptsandnests2succfailnests$julian <- as.numeric(attemptsandnests2succfailnests$julian)

# glm model shows that as latitude increases probability of surviving increases

latitudeglmer <- glm(outcome~lat, data=attemptsandnests2succfailnests, family=binomial)
summary(latitudeglmer)

dateglmer <- glm(outcome~julian, data=attemptsandnests2succfailnests, family=binomial)
summary(dateglmer)

# perform model selection
# make list of models
latanddate <- list(latitudeglmer,dateglmer)
#
# name models
latanddatenames <- c("latitudeglmer","dateglmer")
#
# run AIC on models
aictab(latanddate, modnames = latanddatenames, second.ord = TRUE)

# coolors.co to get custom color palletes for plots, it gives you the color code numbers
# make plot of logistic model predictions----

library(ggpubr)
ggplot(attemptsandnests2succfailnests, aes(x=julian, y=outcome)) + 
  stat_smooth(method="glm", level=0.95,method.args = list(family=binomial), color=natparks.pals("Glacier",1))+
  theme_classic()+
  stat_regline_equation(aes(label =  paste(..eq.label.., sep = "~~~~")), size=6, family="serif")+
  labs(x = expression(italic(z))) +
  labs(x = "Ordinal date") +
  labs(y= "Probability of successful nest")+
  theme(text=element_text(family="serif"))+
  theme(text = element_text(size = 14))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme(axis.title = element_text(size = 15))+
  scale_x_continuous(breaks=c(0,50,100,150,200),labels=c("0\n(Jan. 1st)","50\n(Feb. 20th)", "100\n(April 10th)", "150\n(May 30th)", "200\n(July 19th)"))


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
agesforplot$age <- as.factor(agesforplot$age)

ggplot(data=agesforplot,aes(x=distkm, y=age, color=age)) +
  geom_point(size=2)+
  scale_color_manual(values=natparks.pals("KingsCanyon", 3), name="Nest Attempt")+
  labs(x = "Distance Moved from Previous Nest Attempt (km)") +
  labs(y= "Hen Age")+
  theme_classic()+
  theme(text=element_text(family="serif"))+
  theme(axis.text = element_text(size = 12))+
  theme(axis.title=element_text(size=15))

# calculate mean movement length by age----

# second year birds, 388.84 km
sy <- agesforplot %>% filter(age=="Second Year")
mean(sy$distkm,na.rm=TRUE)

#Calculating 95% confidence intervals 
sample.meanavg<-mean(sy$distkm,na.rm=TRUE)
nrow(sy)
samplen.navg<-71
samplen.sdavg<-sd(sy$distkm,na.rm=TRUE)
samplen.seavg <- samplen.sdavg/sqrt(samplen.navg)

alphanavg = 0.05
degrees.freedomnavg = samplen.navg - 1
t.scorenavg = qt(p=alphanavg/2, df=degrees.freedomnavg,lower.tail=F)
print(t.scorenavg)
margin.errornavg <- t.scorenavg * samplen.seavg

lower.boundnavg <- sample.meanavg - margin.errornavg
upper.boundnavg <- sample.meanavg + margin.errornavg
print(c(lower.boundnavg, upper.boundnavg))

# After Second Year Birds, 619.196 km 
asy <- agesforplot %>% filter(age=="After Second Year")
mean(asy$distkm,na.rm=TRUE)

#Calculating 95% confidence intervals 
sample.meanavg<-mean(asy$distkm,na.rm=TRUE)
nrow(asy)
samplen.navg<-44
samplen.sdavg<-sd(asy$distkm,na.rm=TRUE)
samplen.seavg <- samplen.sdavg/sqrt(samplen.navg)

alphanavg = 0.05
degrees.freedomnavg = samplen.navg - 1
t.scorenavg = qt(p=alphanavg/2, df=degrees.freedomnavg,lower.tail=F)
print(t.scorenavg)
margin.errornavg <- t.scorenavg * samplen.seavg

lower.boundnavg <- sample.meanavg - margin.errornavg
upper.boundnavg <- sample.meanavg + margin.errornavg
print(c(lower.boundnavg, upper.boundnavg))

# After Hatch Year Birds, 629.56 km 
ahy <- agesforplot %>% filter(age=="After Hatch Year")
mean(ahy$distkm,na.rm=TRUE)

# run ANOVA to see if different age hens move different differences between nest attempts, it is only slightly significant between second year and ahy
age.aov <- aov(distkm ~ age, data = agesforplot %>% filter(age=="After Second Year"|age=="Second Year"))
summary(age.aov)
TukeyHSD(age.aov)

# run ANOVA to see if birds move different differences by attempt, first attempt different from attempts 2,3,4
agesforplot$attempt <- as.factor(agesforplot$attempt)
agesforplott <- agesforplot %>% filter(attempt=="1"|attempt=="2"|attempt=="3")
attempt.aov <- aov(distkm ~ attempt, data = agesforplott)
summary(attempt.aov)
TukeyHSD(attempt.aov)

# calculate mean distance moved by attempt, after 1st attempt on average moved 799.05 meters----
# there are 6th attempts but no 7th so there is no average distance moved 
onedist <- dfmerge2 %>% filter(attempt=="1") %>%  filter(distkm != '')%>% droplevels()
twodist <- dfmerge2 %>% filter(attempt=="2")%>%  filter(distkm != '')%>% droplevels()
threedist <- dfmerge2 %>% filter(attempt=="3")%>%  filter(distkm != '')%>% droplevels()
fourdist <- dfmerge2 %>% filter(attempt=="4")%>%  filter(distkm != '')%>% droplevels()
fivedist <- dfmerge2 %>% filter(attempt=="5")%>%  filter(distkm != '')%>% droplevels()

error3 <- qt(0.975,df=length(onedist$distkm)-1)*sd(onedist$distkm)/sqrt(length(onedist$distkm))
error4 <- qt(0.975,df=length(twodist$distkm)-1)*sd(twodist$distkm)/sqrt(length(twodist$distkm))
error5 <- qt(0.975,df=length(threedist$distkm)-1)*sd(threedist$distkm)/sqrt(length(threedist$distkm))
error6 <- qt(0.975,df=length(fourdist$distkm)-1)*sd(fourdist$distkm)/sqrt(length(fourdist$distkm))
error7 <- qt(0.975,df=length(fivedist$distkm)-1)*sd(fivedist$distkm)/sqrt(length(fivedist$distkm))

mean(onedist$distkm, na.rm=TRUE)
mean(onedist$distkm, na.rm=TRUE)+error3
mean(onedist$distkm, na.rm=TRUE)-error3
mean(twodist$distkm, na.rm=TRUE)
mean(twodist$distkm, na.rm=TRUE)+error4
mean(twodist$distkm, na.rm=TRUE)-error4
mean(threedist$distkm, na.rm=TRUE)
mean(threedist$distkm, na.rm=TRUE)+error5
mean(threedist$distkm, na.rm=TRUE)-error5
mean(fourdist$distkm, na.rm=TRUE)
mean(fivedist$distkm, na.rm=TRUE)


# ggplot of number of hens by age----
agesforplot$attempt <- as.factor(agesforplot$attempt)
agesforplot$age <- as.factor(agesforplot$age)

ggplot(agesforplot, aes(age, stat="count")) + 
  geom_bar(aes(fill = age))+
  theme_classic()+
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

# merge into one dataframe all of the counts of attempts, there are 154 hens we have age info for 
countsbyage <- rbind(numbersy,numberasy,numberahy)

# see the average number of nests attempts, 2.82 nest attempts for all ages, however remember some birds had no age----
avgnestattempts <- mean(countsbyage$n,na.rm=TRUE)

#Calculating 95% confidence intervals 
sample.meanavg<-mean(countsbyage$n)
nrow(countsbyage)
samplen.navg<-50
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
  theme_classic()+
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
# second year birds 3.23 nests
symean <- attemptsandages %>% filter(age=="Second Year") 
mean(symean$n, na.rm=TRUE)

# asy birds 3.14 nests
asymean <- attemptsandages %>% filter(age=="After Second Year") 
mean(asymean$n, na.rm=TRUE)

# ahy birds 1.86 nests 
ahymean <- attemptsandages %>% filter(age=="After Hatch Year") 
mean(ahymean$n, na.rm=TRUE)

# see how many hens have more than one nest attempt in the entire dataset, there are 42 of 52 hens disregarding whether they have an age or not (~80.77%)----
numberattemptsbybird <- attemptsandnests2withdistkm  %>% group_by(burst) %>% summarise(n = n())
numberattemptsgreaterthan1 <- numberattemptsbybird %>% filter(n>1) %>% unique
percentofhensmorethan1 <- (42/52)*100

# run ANOVA to see if the distance traveled by a hen in total determined whether they had fewer or greater nesting attempts as per Scott. M, suggestion----
# arrange data in order
henarranged <- febtojunehendata %>%
  arrange(burst,date)

# get first and last locations for each bird id 
henfirstlast <- henarranged %>% group_by(burst) %>% slice(c(1,n()))
henfirstlast <- data.frame(henfirstlast)

# Pull out just long-lat columns from original data
library(sf)
# Transform to NAD83(HARN)/UTM 19N (EPSG = 3749)
firstlast2022 <- st_as_sf(henfirstlast, coords = c("long", "lat"))
## amwo.wgs.84 is dataframe with coordinates in WGS84
## st_as_sf converts the dataframe to a spatial object
st_crs(firstlast2022) <- 4326
## specify the projection of the spatial object with the EPSG code (not proj.4 string)
firstlast202219n <- st_transform(firstlast2022, "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs")
## Final formatting (b/c spatial object stores coordinates different than regular dataframe)
firstlast202219n <- firstlast202219n %>% 
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                                  y = sf::st_coordinates(.)[,2])%>%
  st_drop_geometry() %>%
  glimpse()

str(firstlast202219n)
plot(firstlast202219n$x, firstlast202219n$y)

# Format date for adehabitat ----
firstlast202219n$timestamp <- as.POSIXct(firstlast202219n$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Generate ltraj objects ----
# Generate trajectory
trajamwofirstlast <- as.ltraj(xy = firstlast202219n[,c("x","y")], date = firstlast202219n$timestamp, id = firstlast202219n$burst, burst = firstlast202219n$burst)
# Convert to dataframe so easier to work with
trajamwo_dffirstlast <- ld(trajamwofirstlast)
#   mutate(individual = as.factor(id)) %>% 
#   select(-id)

# total number of hens in entire dataset that we have a first and last GPS location for is 55 hens
totalnumberofhensindatasetfirstlast <- unique(trajamwo_dffirstlast$burst)

# make a km column and drop nas so that each hen has one distance rather than the last location having a na
trajamwo_dffirstlast$distkm <- trajamwo_dffirstlast$dist/1000
trajamwo_dffirstlastdropna <- trajamwo_dffirstlast %>% filter(!is.na(distkm))
plot(trajamwo_dffirstlastdropna$distkm)''

# get mean distance travelled from wintering to spring breeding area----
mean(trajamwo_dffirstlastdropna$distkm)

#Calculating 95% confidence intervals 
sample.meanavg<-mean(trajamwo_dffirstlastdropna$distkm)
nrow(trajamwo_dffirstlastdropna)
samplen.navg<-55
samplen.sdavg<-sd(trajamwo_dffirstlastdropna$distkm)
samplen.seavg <- samplen.sdavg/sqrt(samplen.navg)

alphanavg = 0.05
degrees.freedomnavg = samplen.navg - 1
t.scorenavg = qt(p=alphanavg/2, df=degrees.freedomnavg,lower.tail=F)
print(t.scorenavg)
margin.errornavg <- t.scorenavg * samplen.seavg

lower.boundnavg <- sample.meanavg - margin.errornavg
upper.boundnavg <- sample.meanavg + margin.errornavg
print(c(lower.boundnavg, upper.boundnavg))

# I need to combine the total distance travelled during the breeding period 1 Jan-1 Sept with the dataframe that has the total number of nesting attempts for each hen (numberattemptsbybird) some of the hens in the trajamwo_dffirstlastdropna are not in the numberattemptsbybirds because a few birds did not nest so I'm turning the nas after the full_join in the n column to 0 because those birds did not nest

distmovednumberattempts <- full_join(trajamwo_dffirstlastdropna, numberattemptsbybird)
distmovednumberattempts <- distmovednumberattempts %>% mutate(n = ifelse(is.na(n), 0, n))

# run ANOVA to see if total distance traveled influenced how many time the hens nested, it did not (p=0.359)----
disttravellednumbnests.aov <- aov(n~distkm, data=distmovednumberattempts)
summary(disttravellednumbnests.aov)


# Convert to UTM projection
# EPSG:102008 North America Albers Equal Area Conic
# +proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 


# Project in UTM 19N ----
# UTM zone 19N = EPSG:26919
# PROJ.4 = "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# WGS 84 version
# str(combinednests24)

# # Pull out just long-lat columns from original data
# datacoord24 <- cbind(combinednests24$long, combinednests24$lat)
# # Project long-lat coordinates in UTM
# datacoordMetric24 <- data.frame(project(datacoord24, "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
# # Rename UTM x,y coordinate columns to whatever you want (I did x, y ... you could do easting, northing, whatever ya want)
# colnames(datacoordMetric24) <- c("x","y")
# # Tack on UTM coordinate columns to original dataframe
# amwo202224 <- cbind(combinednests24, datacoordMetric24)
# str(amwo202224)
# 
# plot(amwo202224$long, amwo202224$lat)
# plot(amwo202224$x, amwo202224$y)
# 
# # Format date for adehabitat ----
# str(amwo202224)
# amwo202224$timestamp <- as.POSIXct(amwo202224$attempt_end, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# 
# # Generate ltraj objects ----
# # Generate trajectory
# trajamwo24 <- as.ltraj(xy = amwo202224[,c("x","y")], date = amwo202224$timestamp, id = amwo202224$burst, burst = amwo202224$burst)
# # Convert to dataframe so easier to work with
# trajamwo_df24 <- ld(trajamwo24)
# #   mutate(individual = as.factor(id)) %>% 
# #   select(-id)
# 
# # make a km column
# trajamwo_df24$distkm <- trajamwo_df24$dist/1000
# 
# # calculate mean movement length, 379 km
# mean(trajamwo_df24$distkm,na.rm=TRUE)
# 
# # see how many nests moved greater than 50km
# kmgreat5024 <- trajamwo_df24 %>% filter(distkm>50)
# 
# # what percent nests greater than 50km from last (32%)
# percgreaterthan5024 <- (146/450)*100
# 
# # see how many birds made movements greater than 50km (91 of 177) (51%)
# idsgreat5024 <- unique(kmgreat5024$id)
# idsgreat50whole24 <- unique(trajamwo_df24$id)
# percbirdsgreat5024 <- (91/177)*100
# 
# # see how many nests moved greater than 100km (30%)
# kmgreat10024 <- trajamwo_df24 %>% filter(distkm>100)
# # what percent nests more than 100km from last
# percgreaterthan10024 <- (136/450)*100
# 
# # see how many birds made movements greater than 100km (89 of 1177) (50%)
# idsgreat10024 <- unique(kmgreat10024$id)
# idsgreat100whole24 <- unique(trajamwo_df24$id)
# percbirdsgreat10024 <- (89/177)*100
# 
# # write csv
# write.csv(trajamwo_df24, "./Data/nestmovements24.csv", row.names = FALSE)
# 
# # Visualize distribution of step lengths ----
# ggplot(trajamwo_df24, aes(x = log(distkm))) +
#   geom_histogram() +
#   # scale_x_continuous(limits = c(0, 40)) +
#   # geom_density(alpha = 0.3, color = "#FF6666", fill = "#FF6666") +
#   labs(x = "Step length (km)") +
#   theme_bw()

# precent of each mating system figure
percbreeedingsystems <- read.csv("./Data/matingystemsfigure.csv")

ggplot(percbreeedingsystems, aes(x="", y=perc, fill=system))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  theme_void()+
  # geom_text(aes(label = paste0(system)), position = position_stack(vjust=0.5), angle=65) +
  labs(x = NULL, y = NULL, fill = NULL)+
  scale_fill_manual(values=natparks.pals("CraterLake", 4), name="Mating System")+
  theme(text=element_text(family="serif"))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text=element_text(size=16))+
  theme(legend.position = "left")



# make ggmap of renests by bird id----

# Pull out just long-lat columns from original data
  library(sf)
# Transform to NAD83(HARN)/UTM 19N (EPSG = 3749)
  amwomove <- st_as_sf(attemptandnests2withdistkmm, coords = c("long", "lat"))
## amwo.wgs.84 is dataframe with coordinates in WGS84
  ## st_as_sf converts the dataframe to a spatial object
st_crs(amwomove) <- 4326
  ## Final formatting (b/c spatial object stores coordinates different than regular dataframe)
amwomove19n <- amwomove %>%
    dplyr::mutate(x = sf::st_coordinates(.)[,1],
                  y = sf::st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    glimpse()

# make data ordered sequentially 
amwomove19n$burst <- as.factor(amwomove19n$burst)
ordered <- amwomove19n %>% arrange(burst,attempt_start)
str(amwomove19n)

# get data in ordered format because we are plotting movement tracks
ordered <- amwomove19n %>%
  group_by(burst) %>%
  mutate(xend = lead(x), yend = lead(y))

ordered2 <- amwomove19n %>% 
  group_by(burst) %>% 
  filter(distkm<=5)

# make actual ggmap
 
# download free map because get_map function requires google API key which costs money
us <- c(left = -95, bottom = 30, right = -67, top = 49)
mapofus <- ggmap(get_stamenmap(us, zoom=6, maptype="toner-lite"))

#Set up bounding box
height <- max(ordered$y) - min(ordered$y)
width <- max(ordered$x) - min(ordered$x)

sac_borders <- c(bottom  = min(ordered$y)  - 0.1 * height, 
                 top     = max(ordered$y)  + 0.1 * height,
                 left    = min(ordered$x) - 0.1 * width,
                 right   = max(ordered$x) + 0.1 * width)


                
nestmovements <- mapofus+
  geom_segment(data = ordered, aes(x=x, y=y,xend = xend, yend = yend, group=burst), arrow=arrow(length=unit(4, "mm")), lwd=1.1, color="navy")+
  # geom_point(data=ordered2, aes(x=x, y=y), color="darkgreen", size=5)+
  geom_point(data=ordered, aes(x=x, y=y), color="orange", size=2.50)+
  ylab(element_text("Latitude (decimal degrees)")) +
  xlab(element_text("Longitude (decimal degrees)"))+
  ggsn::scalebar(x.min = -70, x.max = -95, 
                 y.min = 33, y.max = 49, 
                 dist = 300, dist_unit="km",transform = TRUE, 
                 model = "WGS84", height = 0.02, 
                 st.dist = 0.05, location="bottomright")+
  ggspatial::annotation_north_arrow( height = unit(1.4, "cm"), width = unit(0.9, "cm"), pad_y = unit(0.7, "cm"), location="topleft")+
   theme_classic(base_size=30)+
   theme(text=element_text(family="serif"))+
   theme(plot.title = element_text(hjust = 0.5))+
   # theme(text = element_text(size = 12))+
   theme(axis.title=element_text(size=30))+
  geom_hline(yintercept=40, linetype="dashed", 
             color = "black", size=1.1)

# ggsave(plot = nestmovements, filename = "./Figures/nestmovements.jpg", dpi = 400, width = 12, height = 10)

# arrange plots for publication
durationandrenestplot <- plot_grid(numberofnestsperhen,nestdurationplot,renestplot,distmovedbyattemptplot,labels = c('A', 'B', 'C', 'D'), ncol=2, label_fontfamily = "serif", face="bold", label_size = 30)
# ggsave(plot = durationandrenestplot, filename = "./Figures/durationandrenestplot.jpg", dpi = 400, width = 20, height = 20)

# arrange plots for publication
weekandlatplot <- plot_grid(weekofnestingseasonplot+ theme(legend.position = "none"),latitudeplot+ theme(legend.position = "none"),align="hv",labels = c('A', 'B'), label_fontfamily = "serif", label_size = 40)
print(weekandlatplot)
# ggsave(plot = weekandlatplot, filename = "./Figures/weekandlatplot.jpg", dpi = 400, width = 35, height = 20)

