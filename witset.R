# Witset MR
#QA and analysis

# Author: Kristen Peck
# Created: fall 2021, updated Fall/winter 2022

library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(recapr)

# load exported tables for Dates, SK, CO, CH, ST
# note that these are the unaltered tables exported as-is from 
# Witset_Fish_Tagging_Data_V2_MASTER.accdb

# Each species table has the same headings so I just rbind them here and 
# connect with the dates table with a left_join


witset.dates.raw <- read_excel("Tag_Data_Sample_Dates.xlsx")
witsetSK.raw <- read_excel("Tag_Data_Sockeye.xlsx")
witsetCO.raw <- read_excel("Tag_Data_Coho.xlsx")
witsetCH.raw <- read_excel("Tag_Data_Chinook.xlsx")
witsetST.raw <- read_excel("Tag_Data_Steelhead.xlsx")

witset.raw <- rbind (witsetSK.raw,witsetCO.raw,witsetCH.raw,
                     witsetST.raw) %>% 
  left_join(witset.dates.raw, by="Sample_Id")



#### QA/QC ####

#species records missing dates:
witset.raw[which(is.na(witset.raw$Sample_Date)),]
witset.raw[which(is.na(witset.raw$Sample_Id)),]
# fixed orphan records

# recode and tidy up tag colour 

unique(witset.raw$`Recaptured Color`) #fixed some of the really weird coho records
unique(witset.raw$AppliedColor)

witset <- witset.raw %>% 
  mutate(tag.col=recode(AppliedColor,Orange="o",yellow="y",White="w",
                        Yellow="y",Green="g",`Light Green`="lt.g",
                        `Light Orange`="lt.o",Pink="p",`Lime Green`="lt.g",
                        Blue="b",Red="r")) %>% 
  mutate(recap.col=recode(`Recaptured Color`,Orange="o",yellow="y",
                          White="w",Yellow="y",Green="g",
                          `Light Green`="lt.g",`Light Orange`="lt.o",
                          Pink="p",`Lime Green`="lt.g",Blue="b",Red="r")) %>%
  mutate(year = year(Sample_Date),
         new.tag = ifelse(is.na(tag.col)&is.na(AppliedTagNumber),NA,
                          paste0(tag.col,"-",AppliedTagNumber))) %>% 
  mutate(recap.tag = ifelse(is.na(recap.col)&is.na(`Recaptured number`),NA,
                            paste0(recap.col,"-",`Recaptured number`)))
str(witset)
unique(witset$tag.col)
witset[which(witset$tag.col %in% "Grey"),c("Sample_Date", "Counter", "Species")]

#how many new records per year?
print.data.frame(witset %>% 
  group_by(year, Species) %>% 
  summarize(total = length(Species)))



#QA by year####

yr.select <- 2022

#how many fish recapped at campground?
witset %>% 
  filter(year %in% yr.select, Location_Code %in% "Campground") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  group_by(Species) %>%
  summarize(length(Species))
  

#how many of those fish were marked at the canyon?
newtags.canyon <- witset %>% 
  filter(year %in% yr.select, Location_Code %in% "Canyon") %>% 
  summarize(uniq.tags=unique(AppliedTagNumber))
witset %>% 
  filter(year %in% yr.select, Location_Code %in% "Campground") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  filter(`Recaptured number` %in% newtags.canyon$uniq.tags) %>% 
  group_by(Species) %>% 
  summarize(length(Species))
#in 2021, 116 SK were recapped at campground, 40 of which were 
#   tagged at the canyon (33%)
#in 2022, 153 fish recapped at campground (126 CO,24 SK,3 ST), 
# 11 SK (46%) and 46 CO (37%) of which were tagged at the canyon



#what is the ave/longest time between tagging and recapture? TBD



#are there any duplicate new tag numbers within yr select? 
#   Does colour need to be attached? Yes - done with new.tag column

tmp <- witset %>% 
  filter(!is.na(new.tag), year %in% yr.select) 
(tmp2 <- tmp[duplicated(tmp$new.tag),] %>% 
  select(Sample_Date, Location_Code, Species, Counter, new.tag) %>% 
  arrange(Location_Code,Sample_Date))
#2020 SK: y-92871:y-92875 cannot be fixed
#2020 CO: B-1159 and G-62323 to G-62327 not fixable 
#2021 SK: y-4901 duplicate not fixable, the rest fixed
#2021 CO: B-53760 duplicate not fixable, the rest fixed
#2022: were 19 dupes, resolved most except two CO with missing tag number



#how many recaptures without a new tag record? Filter out.
(new.recaps <- witset %>% 
    filter(year %in% yr.select) %>% 
    filter(!is.na(recap.tag), 
           !(recap.tag %in% new.tag) ) ) %>% 
  select(Sample_Date, Location_Code, Species, Counter, recap.tag) %>% 
  arrange(Location_Code, Sample_Date)

#there are 14 unresolvable recaps without tag applied in 2020
#there are 15 recaptures without a matching tag number in 2021
#canyon data not entered for SK on July 19th 2021- DONE
#need to fix database structure for A2 tags (so they don't get put in recaps)

#2020 CO: G-62576, B-45917, G-62502
#there are 10 recaptures without a matching tag number in 2021 CO
# most fixed, but B-53949 is an A2 (to fix)
# Could not solve remaining orphan recaps at campground in 2022 (4)
# Try for canyon orphans...


#how many new tag numbers with no colour?
witset %>% 
  filter(!is.na(AppliedTagNumber),is.na(AppliedColor), year %in% yr.select) %>% 
  select(Sample_Date, Counter, new.tag, Species, Location_Code)
# 1 tag with no colour, just a number in 2020 - unfixable
#77 missing colour of tag, just 1 in 2021-fixed, 
#had three (2 CO, 1ST)-all fixed

#how many tag colours with no tag number?
witset %>% 
  filter(!is.na(AppliedColor),is.na(AppliedTagNumber), year %in% yr.select) %>% 
  select(Sample_Date, Counter, new.tag, Species, Location_Code)
# 1 tag with no number, just a colour in 2020
#160 tags with no number, just a colour in 2021
# 4 tags with no number in 2022, could only fix two


#how many recap tag colours with no tag number?
witset %>% 
  filter(!is.na(`Recaptured Color`),is.na(`Recaptured number`), year %in% yr.select) %>% 
  select(Sample_Date, Counter, recap.tag, Species, Location_Code)
#none in 2020
#160 tags with no number, just a colour; 
# 3 in 2021, all b/c recap tag number doesn't exist
# 1 in 2022, unfixable

#how many recap tag numbers with no colour?
witset %>% 
  filter(!is.na(`Recaptured number`),is.na(`Recaptured Color`), year %in% yr.select) %>% 
  select(Sample_Date, Counter, recap.tag)
#none in 2020
#none in 2021
#none in 2022

#check if the tag status is recorded incorrectly
(no.tag.number <- witset %>% 
    filter(TagStatus %in% c("A","A2"),
           is.na(AppliedTagNumber)&is.na(AppliedColor))) %>% 
  filter(year %in% yr.select) %>% 
  select(Counter)
#49 cases where tag status was recorded as a new tag but no tag number 
# or colour. Just 1 in 2021- fixed. None in 2020, 2022



# ###
#### SOCKEYE ####
# ###

nanikaswim <- read_excel("NanikaSnorkel.xlsx") %>% 
  select(year=Year, nanika.counted=`total sockeye counted`,
         nanika.tags=`total tags observed`)
str(nanikaswim)


SKtotals <- witset %>%
    filter(Species %in% "SK") %>% 
    group_by(year) %>% 
    summarize(totalSK = length(year),
              harvested.by.crew=length(which(Harvested %in% TRUE)),
              newtags = length(which(TagStatus %in% c("A","A2"))),
              newtagcol = length(which(!is.na(AppliedColor))),
              recaps.witset = length(which(TagStatus %in% c("AR","R"))))%>% 
    left_join(nanikaswim)
SKtotals

SKbylocation <- witset %>% 
  filter(Species %in% "SK") %>% 
  group_by(Location_Code, year) %>% 
  summarize(totalcaught = length(Location_Code), 
            harvested=length(which(Harvested %in% TRUE)),
            released= length(which(TagStatus %in% c("A")))) 
SKbylocation



  
#### SK Witset MR ####

#get closed LP estimate (with Chapman mod) from fish tagged at
#the campground then recaptured at the canyon. Assume no tags lost
# (i.e. no fallback below study area or deaths between camp and canyon)
#year.select <- 2022

markedcampground <- witset %>% 
  #filter(year %in% year.select) %>% 
  filter(Species %in% "SK") %>% 
  filter(Location_Code %in% "Campground") %>% 
  filter(TagStatus %in% c("A","A2")) 
#length(unique(markedcampground$new.tag))

#markedcampground[anyDuplicated(markedcampground$new.tag),"new.tag"]
#562 fish marked at campground in 2021 


# # of new tags put out in the canyon
newtagscanyon <- witset %>% 
  filter(Species %in% "SK") %>% 
  #filter(year %in% year.select) %>% 
  filter(Location_Code %in% "Canyon") %>% 
  #filter(TagStatus %in% c("A","A2")) %>% 
  filter(!is.na(new.tag))
#length(unique(newtagscanyon$new.tag))

#newtagscanyon[anyDuplicated(newtagscanyon$new.tag),"new.tag"]
#1788 fish marked at canyon in 2021
# one duplicate new tag put out: yellow 4901 on 27 July 2021. Already noted in db


# filter out new canyon tags from recaps at canyon
canyontagrecaps <- witset %>% 
  filter(Species %in% "SK") %>% 
  #filter(year %in% year.select) %>% 
  filter(Location_Code %in% "Canyon") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  filter(!(recap.tag %in% unique(newtagscanyon$new.tag)))
#49 fish recapped at canyon from campground in 2021

#total canyon catch EXCLUDING tags put out at canyon 
canyoncatch <- witset %>% 
  filter(Species %in% "SK") %>% 
  #filter(year %in% year.select) %>% 
  filter(Location_Code %in% "Canyon") %>% 
  filter(!(recap.tag %in% unique(newtagscanyon$new.tag)))
#2452 SK captured at canyon. Should exclude other portions of catch?
  

#LP estimate for campground -> canyon

#marked in campground
(m <- markedcampground %>% 
  group_by(year) %>% 
  summarize(marked = length(Sample_Date)))

#recapped at canyon
(r <- canyontagrecaps %>% 
    group_by(year) %>% 
    summarize(recapped = length(Sample_Date)))

#total unique fish caught at canyon
(c <- canyoncatch %>% 
    group_by(year) %>% 
    summarize(total.catch = length(Sample_Date)))


LP <- m %>% 
  left_join(r) %>% 
  left_join(c) %>% 
  mutate(LP = (marked)*(total.catch)/(recapped)) %>% 
  mutate(Chap = NChapman(marked,total.catch,recapped)) %>% 
  mutate(vChap = vChapman(marked,total.catch,recapped)) %>% 
  mutate(seChap = seChapman(marked,total.catch,recapped)) %>% 
  mutate(CI95Chap = seChap*1.965)
LP
#write_csv(LP, file = "Chapman.estSK2012-2022.csv")

ggplot(data=LP)+
  geom_line(aes(x=year, y=Chap))+
  geom_point(aes(x=year, y=Chap))+
  geom_errorbar(aes(x=year, ymax=Chap+CI95Chap, ymin=Chap-CI95Chap),width=0.1)+
  scale_x_continuous(breaks = seq(min(LP$year),max(LP$year),1))+
  labs(title = "Sockeye In-Canyon Estimate", y="Pooled LP estimate (95%CI)")


# check CPUE (based on a day as effort) for canyon and campground:

(witsetSKrecent <- witset %>%
  filter(Species %in% "SK") %>% 
  filter(year %in% c(2018:2022)) %>% 
  group_by(Sample_Date, year) %>% 
  summarize(n.SK = length(unique(Counter))) %>% 
    mutate(fake.date = as_date(yday(Sample_Date), 
                               origin = ymd("2022-01-01")))) 

totalcaughtSK <- SKbylocation %>% 
  group_by(year) %>% 
  summarize(totalcaught = sum(totalcaught),
            totalharvested = sum(harvested),
            totalreleased = sum(released)) %>% 
  filter(year %in% c(2018:2022))

plot.SK.daily <- ggplot(witsetSKrecent)+
  geom_line(aes(x=as_date(fake.date), y=n.SK))+
  geom_text(data = totalcaughtSK, aes(x=ymd("2022-09-10"), y=500,
                                      label=paste0("total caught=",
                                                   totalcaught)),
            size=2.5)+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d")+
  facet_wrap(~year)+
  labs(title = "Daily catch of sockeye Campground+Canyon (2018-2022)", y="daily catch of sockeye",
       x="date")+
  theme(axis.text.x = element_text(angle=45, hjust=1))
plot.SK.daily

#ggsave(plot = plot.SK.daily, filename = "dailySKcatch2018-2022.png",
#       height = 4, width = 6,device = "png")





# # # # # # # # #
###### COHO #####
# # # # # # # # #

# look at total harvested, tagged, and recaptured by year
(COtotals <- witset %>% 
  filter(Species %in% "CO") %>% 
  group_by(year) %>% 
  summarize(totalcaught = length(year),
            harvested.by.crew=length(which(Harvested %in% TRUE)),
            newtags = length(which(TagStatus %in% c("A","A2"))),
            recaps.witset = length(which(TagStatus %in% c("AR","R")))))


(CObylocation <- witset %>% 
  filter(Species %in% "CO") %>% 
  group_by(Location_Code, year) %>% 
  summarize(totalcaught = length(Location_Code), 
            harvested=length(which(Harvested %in% TRUE)),
            #released = length(which(TagStatus %in% c("A"))),
            newtags = length(which(TagStatus %in% c("A","A2"))),
            recaps.witset = length(which(TagStatus %in% c("AR","R")))) %>% 
  arrange(year,Location_Code))

# coho timing
(COfirstday <- witset %>% 
  filter(Species %in% "CO") %>% 
  mutate(cumulCO = 1:length(Counter), prop.daily = cumulCO/length(Species)*100) %>% 
  group_by(Location_Code, year) %>% 
  summarize(first.caught = as_date(Sample_Date[1]), first.50 = as_date(Sample_Date[which.min(abs(50-cumulCO))]),
            first.one.percent = as_date(Sample_Date[which.min(abs(1-prop.daily))])) %>% 
  arrange(year,Location_Code))




# CPUE #
#look at daily catch of Coho

(witsetCOrecent <- witset %>% 
    filter(Species %in% "CO") %>% 
    filter( year %in% c(2018:2022)) %>% 
    group_by(Sample_Date, year) %>% 
    summarize(n.CO = length(unique(Counter))) %>% 
    mutate(fake.date = as_date(yday(Sample_Date), 
                               origin = ymd("2022-01-01")))) 

totalcaughtCO <- CObylocation %>% 
  group_by(year) %>% 
  summarize(totalcaught = sum(totalcaught),
            totalharvested = sum(harvested),
            totalnewtags = sum(newtags)) %>% 
  filter(year %in% c(2018:2022))

plot.CO.daily <- ggplot(witsetCOrecent)+
  geom_line(aes(x=as_date(fake.date), y=n.CO))+
  geom_text(data = totalcaughtCO, aes(x=ymd("2022-10-01"), y=300,
                                      label=paste0("total caught=",totalcaught)),
            size=2.5)+
  facet_wrap(~year)+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d")+
  labs(title = "Daily catch of coho at Campground+Canyon (2018-2022)", y="daily catch of coho",
       x="date")+
  theme(axis.text.x = element_text(angle=45, hjust=1))
plot.CO.daily

#ggsave(plot = plot.CO.daily, filename = "dailyCOcatch2018-2022.png",
#       height = 4, width = 6,device = "png")



#### Toboggan ####

# Quick LP estimate

tobog <- read_excel("TobogganFenceData_MASTER-copy1-Feb-2023.xlsx",
           sheet = "IndividualFish")
str(tobog)

tobog %>% 
  filter(species %in% "co") %>% 
  group_by(year) %>% 
  summarize(totalcaught = length(species),
            hatchery.origin = length(which(mark_gender %in% c("af","am","aj"))),
            wild.origin = length(which(mark_gender %in% c("wf","wm","wj"))),
            witset.tag = length(which(!is.na(recap_tag_number))))

# prelim.summary.CO <- read_excel("moricetown sockeye tagging estimates_v3.xlsx",
# sheet = "COKP")
# 
# LP.CO <- prelim.summary.CO %>% 
#   mutate(ChapmanMR = NChapman(esttags,Toboggan.catch,
#                               Toboggan.recaps)) %>% 
#   mutate(ChapmanVar = vChapman(esttags,Toboggan.catch,
#                                Toboggan.recaps)) %>% 
#   mutate(ChapmanSE = seChapman(esttags,Toboggan.catch,
#                                Toboggan.recaps)) %>% 
#   mutate(Chapman95CI = ChapmanSE*1.965)
# names(prelim.summary.CO)

#old datasheet:
#now line up with Toboggan Creek recaps
tobog.tags.raw <- read_csv("Toboggan.tagrecoveries.2018-2021.csv",
                              na = "NA")

tobog.tags <- tobog.tags.raw %>% 
  filter(!is.na(tag)) %>% 
  mutate(year = year(Date),tob.tag = tag, tob.date = Date) %>% 
  select(year, tob.date, tob.tag)

tag.match1 <- tobog.tags %>%
  left_join(witsetCO, by= c("year","tob.tag" = "AppliedTagNumber")) %>% 
  select(tob.date,tob.tag, Sample_Date, Location_Code, new.tag)
tag.match2 <- tobog.tags %>%
  left_join(witsetCO, by= c("year","tob.tag" = "Recaptured number")) %>% 
  select(tob.date,tob.tag, Sample_Date, Location_Code, recap.tag)

tag.match1 %>% 
  filter(is.na(new.tag))
#18 recovered tags at toboggan between 2018 and 2021 have no match at witset

recap.col <- tag.match2 %>% 
  filter(!is.na(recap.tag)) %>% 
  select(Sample_Date.recap=Sample_Date,tob.tag,recap.tag)
  

tag.match <- tag.match1 %>% 
  left_join(recap.col) %>% 
  mutate(year = year(tob.date),
         canyon.to.fence = as_date(tob.date)-as_date(Sample_Date))
  
#time between tagging and recovery at Toboggan
(ave.canyon.to.fence <- tag.match %>% 
  filter(!is.na(Sample_Date)) %>% 
  select(year,tob.tag, canyon.to.fence) %>% 
  group_by(year) %>% 
  summarize(ave.canyon.to.fence = round(mean(canyon.to.fence),0), 
            n=length(canyon.to.fence)))

tag.match %>% 
  filter(is.na(new.tag) & is.na(recap.tag))
#17 tags found in neither the initial cap or the recap

ggplot()+
  geom_histogram(data = tag.match,aes(x=canyon.to.fence), binwidth=5)+
  geom_vline(data= ave.canyon.to.fence,aes(xintercept = ave.canyon.to.fence))+
  facet_wrap(~year)


witsetCH <- read_excel("QueryCH.xlsx") %>% 
  mutate(year = year(Sample_Date),
         tag.col = substr(AppliedColor,1,1),
         new.tag = ifelse(is.na(tag.col),NA,
                          paste0(tag.col,"-",AppliedTagNumber))) %>% 
  mutate(recap.col = substr(`Recaptured Color`,1,1),
         recap.tag = ifelse(is.na(recap.col),NA,
                            paste0(recap.col,"-",`Recaptured number`)))
str(witsetCH)

# ch.col <- witsetCH %>% 
#   filter(!is.na(AppliedColor)) %>% 
#   group_by(year, AppliedColor, Species) %>% 
#   summarize(n=length(unique(Counter)), 
#             min.applied.tag.num = min(AppliedTagNumber, na.rm=T), 
#             max.applied.tag.num = max(AppliedTagNumber, na.rm=T))
# write.csv(ch.col, "ch.col.csv", row.names = F)






# OLD REQUESTS/ANALYSIS ####

# witsetSK.raw2022 <- witset.raw %>% 
#   filter(Species %in% "SK", year(Sample_Date) %in% "2022")
# witsetSK.raw2022 <- witset.raw %>% 
#   filter(Species %in% "SK", year(Sample_Date) %in% "2022")
# 
# #request by WJ for daily harvested by species in 2022:
# SK.harvest.2022 <- witsetSK.raw2022 %>% 
#   filter(Harvested %in% T) %>% 
#   group_by(date = as_date(Sample_Date)) %>% 
#   summarize(daily.SK.harvest = length(Counter))
# write_csv(SK.harvest.2022, file = "SK.harvest.2022.csv")
#  
# 
# witsetCO.raw2022 <- read_excel("QueryCO2022.xlsx") %>% 
#   mutate(year = year(Sample_Date)) %>% 
#   filter(year %in% 2022) %>% 
#   select(-year)
# 
# witsetCH.raw2022 <- read_excel("QueryCH2022.xlsx") %>% 
#   mutate(year = year(Sample_Date)) %>% 
#   filter(year %in% 2022) %>% 
#   select(-year)
# CH.harvest.2022 <- witsetCH.raw2022 %>% 
#   filter(Harvested %in% T) %>% 
#   group_by(date = as_date(Sample_Date)) %>% 
#   summarize(daily.CH.harvest = length(Counter))
# 
# 
# witsetST.raw2022 <- read_excel("QueryST2022.xlsx") %>% 
#   mutate(year = year(Sample_Date)) %>% 
#   filter(year %in% 2022) %>% 
#   select(-year)
# ST.harvest.2022 <- witsetST.raw2022 %>% 
#   filter(Harvested %in% T) %>% 
#   group_by(date = as_date(Sample_Date)) %>% 
#   summarize(daily.ST.harvest = length(Counter))
# 
# CO.harvest.2022 <- witsetCO.raw2022 %>% 
#   filter(Harvested %in% T) %>% 
#   group_by(date = as_date(Sample_Date)) %>% 
#   summarize(daily.CO.harvest = length(Counter)) %>% 
#   full_join(SK.harvest.2022) %>% 
#   full_join(CH.harvest.2022) %>% 
#   full_join(ST.harvest.2022) %>% 
#   arrange(date)
# write_csv(CO.harvest.2022, file = "Witset.harvest.2022.csv",na = "")






# sk.col <- witsetSK %>% 
#   filter(!is.na(AppliedColor)) %>% 
#   group_by(year, AppliedColor, Species) %>% 
#   summarize(n=length(unique(Counter)), 
#             min.applied.tag.num = min(AppliedTagNumber, na.rm=T), 
#             max.applied.tag.num = max(AppliedTagNumber, na.rm=T))
# write.csv(sk.col, "sk.col.csv", row.names = F)


