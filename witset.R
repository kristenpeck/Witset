# Witset MR
#QA and analysis

# Author: Kristen Peck
# Created: fall 2021, updated Fall/winter 2022/23

library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(recapr)

# load exported tables for Dates, SK, CO, CH, and ST
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

str(witset.raw)

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
                        Yellow="y",Green="g",`Light Green`="g",
                        `Light Orange`="lt.o",Pink="p",`Lime Green`="g",
                        Blue="b",Red="r")) %>% 
  mutate(recap.col=recode(`Recaptured Color`,Orange="o",yellow="y",
                          White="w",Yellow="y",Green="g",
                          `Light Green`="g",`Light Orange`="lt.o",
                          Pink="p",`Lime Green`="g",Blue="b",Red="r")) %>%
  mutate(year = year(Sample_Date),
         new.tag = ifelse(is.na(tag.col)&is.na(AppliedTagNumber),NA,
                          paste0(tag.col,"-",AppliedTagNumber))) %>% 
  mutate(recap.tag = ifelse(is.na(recap.col)&is.na(`Recaptured number`),NA,
                            paste0(recap.col,"-",`Recaptured number`)))
str(witset)
unique(witset$tag.col)
witset[which(witset$tag.col %in% "Grey"),c("Sample_Date", "Counter", "Species")]

grep("66501",x = witset$AppliedTagNumber)



#how many new records per year?
print.data.frame(witset %>% 
  group_by(year, Species) %>% 
  summarize(total = length(Species)))



##### QA by year####

yr.select <- 2022

  ##### Fallback ####

#how many fish recapped at campground?
(total.camp.recaps <- witset %>% 
  filter(Location_Code %in% "Campground") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  group_by(year, Species) %>%
  summarize(recaps.camp = length(Species)))

#get list of unique tags applied at campground and canyon:
newtags.camp <- witset %>% 
  filter(Location_Code %in% "Campground") %>%
  group_by(year, Species) %>% 
  summarize(uniq.tags=na.omit(unique(AppliedTagNumber))) %>% #removed NAs
  mutate(uniq.yrtags = paste0(year,Species,uniq.tags)) 

newtags.canyon <- witset %>% 
  filter(Location_Code %in% "Canyon") %>%
  group_by(year, Species) %>% 
  summarize(uniq.tags=na.omit(unique(AppliedTagNumber))) %>% #removed NAs
  mutate(uniq.yrtags = paste0(year,Species,uniq.tags)) 

# how many new tags applied at campground?
total.newtags.camp <- newtags.camp %>% 
  group_by(year, Species) %>% 
  summarize(camp.applied = length(Species))

# how many new tags applied at canyon?
total.newtags.canyon <- newtags.canyon %>% 
  group_by(year, Species) %>% 
  summarize(canyon.applied = length(Species))

#how many of the recaps in camp were marked at the canyon 
# (i.e. fellback below canyon)?

#get list of unique fish tagged at canyon and recapped at campground:
fallback.fish <- witset %>% 
  filter(Location_Code %in% "Campground") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  mutate(uniq.yrtags = paste0(year,Species,`Recaptured number`)) %>% 
  filter(uniq.yrtags %in% newtags.canyon$uniq.yrtags) 

#how many total fallbacks?
fallback <- fallback.fish %>%   
  group_by(year, Species) %>% 
  summarize(canyon.fallback = length(Species))

#how many fallbacks were again recaptured at the canyon?
fallbacks.recap.canyon <- witset %>% 
  filter(Location_Code %in% "Canyon") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  mutate(uniq.yrtags = paste0(year,Species,`Recaptured number`)) %>% 
  filter(uniq.yrtags %in% fallback.fish$uniq.yrtags) %>% 
  group_by(year, Species) %>% 
  summarize(fallbacks.recap.canyon= length(Species))

#how many fish were marked at campground and recapped at canyon?
# This could be considered the expected percentage return for fallback fish
camptocanyon.recaps <- witset %>% 
  filter(Location_Code %in% "Canyon") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  mutate(uniq.yrtags = paste0(year,Species,`Recaptured number`)) %>% 
  filter(uniq.yrtags %in% newtags.camp$uniq.yrtags) %>% 
  group_by(year, Species) %>% 
  summarize(camptocanyon.recaps = length(Species))




#combine into table
table.fallback <- total.newtags.camp %>% 
  full_join(total.newtags.canyon) %>% 
  full_join(total.camp.recaps) %>% 
  full_join(fallback) %>% 
  mutate(percent.canyon.fallback = round(canyon.fallback/canyon.applied*100,1)) %>% 
  full_join(fallbacks.recap.canyon) %>% 
  mutate(percent.fallbacks.reascend = round(fallbacks.recap.canyon/canyon.fallback*100,1)) %>%
  full_join(camptocanyon.recaps) %>% 
  mutate(percent.camptocanyon = round(camptocanyon.recaps/camp.applied*100,1)) %>% 
  filter(Species %in% c("SK","CO","ST")) %>% 
  arrange(year, Species)


plot.fallback <- ggplot(table.fallback)+
  geom_line(aes(x=year, y=percent.canyon.fallback, col=Species), size= 1.5)+
  scale_x_continuous(breaks=seq(min(table.fallback$year),
                                max(table.fallback$year),1))+
  labs(x="Year",y="% Fallback from Canyon to Campground")
plot.fallback


plot.fallbackreascend <- ggplot(table.fallback)+
  geom_line(aes(x=year, y=percent.fallbacks.reascend, col=Species), size=1.5)+
  geom_point(aes(x=year, y=percent.fallbacks.reascend, col=Species, size=fallbacks.recap.canyon))+
  geom_line(aes(x=year, y=percent.camptocanyon, col=Species), linetype="dashed")+
  scale_x_continuous(breaks=seq(min(table.fallback$year),
                                max(table.fallback$year),1))+
  labs(x="Year", y="% Fallback recapped at Canyon \nCompared to % of Camp recapped at Canyon",
       size="# fallbacks\nrecapped at canyon")
plot.fallbackreascend




##### Repeat recaps ####

#how many fish recaptured repetitively?
repeat.captures <- witset %>% 
  filter(!is.na(`Recaptured number`), Species %in% c("SK","CO","ST")) %>% 
  mutate(uniq.yrtags = paste0(year,Species,`Recaptured number`)) %>% 
  group_by(year, Species,uniq.yrtags, Location_Code) %>% 
   summarize(number.recaps = length(uniq.yrtags)) %>% 
   filter(number.recaps >1) %>% 
  arrange(year,Species,desc(number.recaps))
  
table(repeat.captures[,c("year", "Species", "number.recaps")])

plot.camp.recaps <- ggplot(repeat.captures[which(repeat.captures$Location_Code %in% "Campground"),c("year", "Species", "number.recaps")])+
  geom_histogram(aes(x=as.factor(number.recaps), fill=Species), stat="count")+
  facet_wrap(~year)+
  labs(title = "Campground recaps", x= "# of times recaptured", y= "# individuals")
plot.camp.recaps

plot.canyon.recaps <- ggplot(repeat.captures[which(repeat.captures$Location_Code %in% "Canyon"),c("year", "Species", "number.recaps")])+
  geom_histogram(aes(x=as.factor(number.recaps), fill=Species), stat="count")+
  facet_wrap(~year)+
  labs(title = "Canyon recaps", x= "# of times recaptured", y= "# individuals")
plot.canyon.recaps
  
#in 2021, 116 SK were recapped at campground, 40 of which were 
#   tagged at the canyon (33%)
#in 2022, 153 fish recapped at campground (126 CO,24 SK,3 ST), 
# 11 SK (46%) and 46 CO (37%) of which were tagged at the canyon







#what is the ave/longest time between tagging and recapture? TBD
# Carl looked at this with the BTSPAS groupings


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
# Resolved CO canyon orphans in 2022 


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
#### Nanika data ####
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



# # # # # # # # # # # #
#### SK Estimates ####
# # # # # # # # # # # #

#get closed LP estimate (with Chapman mod) from fish tagged at
#the campground then recaptured at the canyon. Assume no tags lost
# (i.e. no fallback below study area)
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





# # # # # # # # # # # # # 
#### COHO  Estimates ####
# # # # # # # # # # # # # 

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



#### Toboggan data ####

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
tobog.tags <- tobog %>% 
  filter(!is.na(recap_tag_number))

#old datasheet:
#now line up with Toboggan Creek recaps
# tobog.tags.raw <- read_csv("Toboggan.tagrecoveries.2018-2021.csv",
#                               na = "NA")
# tobog.tags <- tobog.tags.raw %>% 
#   filter(!is.na(tag)) %>% 
#   mutate(year = year(Date),tob.tag = tag, tob.date = Date) %>% 
#   select(year, tob.date, tob.tag)

witsetCO <- witset %>% 
  filter(Species %in% "CO")

tag.match1 <- tobog.tags %>%
  left_join(witsetCO, by= c("year","recap_tag_number" = "AppliedTagNumber")) %>% 
  select(date,recap_tag_number, Sample_Date, Location_Code, new.tag)
tag.match2 <- tobog.tags %>%
  left_join(witsetCO, by= c("year","recap_tag_number" = "Recaptured number")) %>% 
  select(date,recap_tag_number, Sample_Date, Location_Code, recap.tag)

tag.match1 %>% 
  filter(is.na(new.tag)) %>% 
  arrange(desc(date))
#18 recovered tags at toboggan between 2018 and 2021 have no match at witset
#update: back to 2014, there are now 77 tags from Toboggan that are not in Witset DB
# fixed 10 of these so far, 67 remain
tag.match1 %>% 
  filter(is.na(new.tag)) %>% 
  group_by(year(date)) %>% 
  summarize(orphans = length(recap_tag_number))


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


#### REPORTING ####
# From other script- integrate
#### effort and catch summary table ####

effort.table <- allsalmon %>% 
  group_by(year, Location_Code) %>% 
  summarize(first=min(date), last=max(date), ndays=length(unique(date)),
            days.between = yday(last)-yday(first), totCO = length(which(Species %in% "CO")),
            totSK = length(which(Species %in% "SK")), 
            totCH = length(which(Species %in% "CH"))) %>% 
  mutate(first.fake.date = as_date(yday(first), origin=ymd("2021-01-01")),
         last.fake.date = as_date(yday(last), origin=ymd("2021-01-01")))

(salmon.table <- allsalmon %>% 
    group_by(year, Location_Code, Species) %>% 
    summarize(`Total Caught` = length(Species)) %>% 
    pivot_wider(names_from = Location_Code,values_from =`Total Caught` ))


table1 <- effort.table %>% 
  select(Year=year, Location = Location_Code, First= first, 
         Last=last, `Total Days Fished`=ndays,SK=totSK, CH=totCH, CO=totCO) %>% 
  gt(groupname_col = NULL) %>% 
  fmt_date(columns = c(First,Last), date_style = 9) 
table1

#gtsave(table1, filename="tabletiming.rtf")


#trends in catch per day

plot.COperday.loess <- ggplot(data = effort.table)+
  geom_point(aes(x=year, y=totCO/ndays, col=Location_Code),  size=2)+
  geom_smooth(aes(x=year, y=totCO/ndays, col=Location_Code),method = "loess",se = F)+
  labs(x= "", y="Coho per day", title="A. Coho")+
  scale_x_continuous(breaks = seq(2012, 2021, 1))+
  theme(legend.position = "bottom")
plot.COperday.loess

#statistics - trend in catch/day?

effort.table[effort.table$year >=2017,]

(sum.COglm <- summary(glm(data = effort.table, formula=totCO ~ year+Location_Code,
                          family = "poisson")))
(sum.SKglm <- summary(glm(data = effort.table, formula=totSK ~ year+Location_Code,
                          family = "poisson")))
summary(glm(data = effort.table[effort.table$year >=2017,], formula=totCH ~ year+Location_Code,
            family = "poisson"))
#summary table for coho GLM:
table.COglm <- sum.COglm$coeff %>% 
  gt(groupname_col = NULL) 
gtsave(table.COglm, filename="table.COglm.rtf")
#summary table for sockeye GLM:
table.SKglm <- sum.SKglm$coeff %>% 
  gt(groupname_col = NULL) 
gtsave(table.SKglm, filename="table.SKglm.rtf")


plot.COperday.lm <- ggplot(data = effort.table)+
  geom_point(aes(x=year, y=totCO/ndays, col=Location_Code),  size=2)+
  geom_smooth(aes(x=year, y=totCO/ndays, col=Location_Code),method = "lm",se = F)+
  labs(x= "", y="Coho per day", title="A. Coho")+
  scale_x_continuous(breaks = seq(2012, 2021, 1))+
  theme(legend.position = "none")
plot.COperday.lm



plot.SKperday <- ggplot(data = effort.table)+
  geom_point(aes(x=year, y=totSK/ndays, col=Location_Code),  size=2)+
  geom_smooth(aes(x=year, y=totSK/ndays, col=Location_Code),method = "loess",se = F)+
  labs(x= "", y="Sockeye per day", col="Location", title="B. Sockeye")+
  scale_x_continuous(breaks = seq(2012, 2021, 1))+
  theme(legend.position = "none")
plot.SKperday

plot.CHperday <- ggplot(data = effort.table)+
  geom_point(aes(x=year, y=totCH/ndays, col=Location_Code),  size=2)+
  #geom_smooth(aes(x=year, y=totCH/ndays, col=Location_Code),method = "loess", se=F)+
  labs(y="Chinook per day", col="Location", title="C. Chinook")+
  scale_x_continuous(breaks = seq(2012, 2021, 1))+
  theme(legend.position = "bottom")
plot.CHperday

plot.catchperday <- arrangeGrob(plot.COperday, plot.SKperday, plot.CHperday)
plot(plot.catchperday)

#ggsave(plot=plot.catchperday , 
#       filename = "catchperday.png", device = "png", width = 6.5, height=6)


# plot of seasonal timing and catch

plot.timing <- ggplot(data=allsalmon)+
  geom_histogram(aes(x=fake.date, fill=Species),binwidth = 2)+
  # geom_text(data=effort.table.camp, aes(label=paste("a.",ndays, "days"), x=ymd("2021-10-01"),
  #                                      y=1550), size=2.25)+
  #  geom_text(data=effort.table.can, aes(label=paste("b.",ndays, "days"), x=ymd("2021-10-01"),
  #                                    y=1300),size=2.25, col="purple")+
  facet_wrap(~year)+
  labs(x="",y="# salmon")
plot.timing

#ggsave(plot = plot.timing, filename = "timing2012-2021.png", width = 6.5, height=4,
#        device = "png")

#### size diff harvested and tagged fish ####

(sizes <- allsalmon %>%
   filter(ForkLength <120) %>% 
   mutate(TagStatus = ifelse(TagStatus %in% c("A","AR","A2", "R"), "Yes","No")) %>% 
   mutate(tag.harvest = paste0(TagStatus,".",Harvested)) %>% 
   #mutate(tag.harvest = ifelse(TagStatus %in% "No" & Harvested %in% "True", "harvested",
   #                             ifelse(TagStatus %in% "No" & Harvested %in% "False", "not harvested",
   #                                   ifelse(TagStatus %in% "Yes", "tagged", NA)))) %>% 
   #group_by(year, Species, Location_Code, Harvested, TagStatus) %>% 
   #summarize(ave.FL = mean(ForkLength, na.rm=T), min.FL=min(ForkLength, na.rm=T),
   #          max.FL=max(ForkLength, na.rm=T)) %>% 
   filter(tag.harvest %in% c("No.FALSE", "No.TRUE", "Yes.FALSE")) %>% 
   mutate(Tag.Harvest = ifelse(tag.harvest %in% "No.FALSE", "Released",
                               ifelse(tag.harvest %in% "No.TRUE", "Harvested",
                                      ifelse(tag.harvest %in% "Yes.FALSE", "Tagged",NA)))) %>% 
   arrange(Species, year,Location_Code, Harvested))

sizes.CO.df <- sizes %>%
  filter(Species %in% "CO")

ggplot(sizes.CO.df)+
  geom_histogram(aes(x=ForkLength, fill=Tag.Harvest), binwidth=5, col="black")+
  facet_wrap(~year)

plot.forklengthCO <- ggplot(sizes.CO.df)+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  c(0.75,0.15),
        axis.text.y = element_blank())+
  labs(title = "A. Coho", fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))
plot.forklengthCO

ggsave(plot=plot.forklengthCO, 
       filename = "plot.forklengthCO.png", device = "png", width = 12, height=9.4)



sizes.SK.df <- sizes %>%
  filter(Species %in% "SK")

ggplot(sizes.SK.df)+
  geom_histogram(aes(x=ForkLength, fill=Tag.Harvest), binwidth=5,  
                 position = "dodge")+
  facet_wrap(~year)

plot.forklengthSK <- ggplot(sizes.SK.df)+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest),varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  c(0.75,0.15),
        axis.text.y = element_blank())+
  labs(title = "B. Sockeye",  fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))
plot.forklengthSK

ggsave(plot=plot.forklengthSK, 
       filename = "plot.forklengthSK.png", device = "png", width = 12, height=9.4)


sizes.CH.df <- sizes %>%
  filter(Species %in% "CH")

ggplot(sizes.CH.df)+
  geom_histogram(aes(x=ForkLength, fill=Harvested), binwidth=2)+
  facet_wrap(~year)

plot.forklengthCH <- ggplot(sizes.CH.df)+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest))+
  facet_wrap(~year)+
  theme(legend.position = "bottom",
        axis.text.y = element_blank())+
  labs(title = "C. Chinook", x="Fork Length (cm)")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))
plot.forklengthCH

ggsave(plot=plot.forklengthCH, 
       filename = "plot.forklengthCH.png", device = "png", width = 12, height=9.4)


plot.forklength <- arrangeGrob(plot.forklengthCO, plot.forklengthSK, 
                               plot.forklengthCH)
plot(plot.forklength)


#ggsave(plot=plot.forklength, 
#       filename = "plot.forklength.png", device = "png", width = 6.5, height=9.4)


#### environmental data - flow ####

#download_hydat()

bulkley.flow.raw <- hy_daily_flows(station_number = "08EE005") %>% 
  mutate(year = year(Date),yday = yday(Date), 
         fake.date=as_date(yday, origin = ymd("2021-01-01")))
unique(bulkley.flow.raw$year)
#this station seems to have been non-operational before 2008 so we can't really 
# do much in the way of comparison by decade

# ave.bulkley.flow <- bulkley.flow.raw %>% 
#   filter(year %in% 1991:2021) %>% 
#   mutate(decade = ifelse(year %in% 1991:2000, "1991:2000",
#                          ifelse(year %in% 2001:2010, "2001:2010", "2011:2021"))) %>% 
#   group_by(decade, fake.date) %>% 
#   summarize(ave.daily = mean(Value, na.rm=T), sd.daily = sd(Value, na.rm=T)) %>% 
#   mutate(lower.sd = ifelse(ave.daily-sd.daily<0, 0, ave.daily-sd.daily),
#          upper.sd = ave.daily+sd.daily)
# ave.bulkley.flow


ave.bulkley.flow <- bulkley.flow.raw %>%
  filter(year %in% 2008:2021) %>%
  group_by(fake.date) %>%
  summarize(ave.daily = mean(Value, na.rm=T), sd.daily = sd(Value, na.rm=T)) %>%
  mutate(lower.sd = ifelse(ave.daily-sd.daily<0, 0, ave.daily-sd.daily),
         upper.sd = ave.daily+sd.daily)
ave.bulkley.flow

ggplot()+
  geom_ribbon(data=ave.bulkley.flow, 
              aes(x=fake.date, ymin=lower.sd, ymax=upper.sd),alpha=.5)

bulkley.flow <- bulkley.flow.raw %>% 
  filter(Date %in% ymd("2012-01-01"):ymd("2021-12-31")) %>% 
  filter(fake.date >= ymd("2021-07-01")& fake.date < ymd("2021-11-01"))


plot.bulkley.flow <- ggplot(bulkley.flow)+
  geom_line(aes(x=fake.date, y=Value, col=as_factor(year)), size=1.5)+
  labs(x="Date", y="Discharge (m^3/s, station 08EE005)", col="")+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d")
plot.bulkley.flow

#ggsave(plot= plot.bulkley.flow, filename = "plot.bulkley.flow.png", 
#       height = 4, width = 6.5, device = "png")

plot.bulkley.start <- ggplot()+
  geom_ribbon(data=ave.bulkley.flow, 
              aes(x=fake.date, ymin=lower.sd, ymax=upper.sd),alpha=.5)+
  geom_line(data = bulkley.flow, aes(x=fake.date, y=Value), size=1)+
  geom_vline(data = effort.table[effort.table$year <2023,], 
             aes(xintercept = first.fake.date,col=Location_Code, linetype = Location_Code), 
             size=1.5,alpha=.5)+
  geom_vline(data = effort.table[effort.table$year <2023,], 
             aes(xintercept = last.fake.date,col=Location_Code, linetype = Location_Code), 
             size=1.5,alpha=.5)+
  geom_hline(data = bulkley.flow, aes(yintercept = 310), size=.5, col="black")+
  facet_wrap(~ year)+
  labs(x="Date", y="Discharge (m^3/s, station 08EE005)", col="Location",
       linetype="Location")+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d", 
               limits = c(ymd("2021-07-01"),ymd("2021-10-31")))+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = c(0.75, 0))
plot.bulkley.start

ggsave(plot= plot.bulkley.start, filename = "plot.flow.startend.png", 
       height = 4, width = 6.5, device = "png")

# caught per day by flow

plot.timing.flow <- ggplot()+
  geom_histogram(data=allsalmon, aes(x=fake.date, fill=Species),binwidth = 1)+
  geom_line(data = bulkley.flow, aes(x=fake.date, y=Value), alpha = 0.25, size=1)+
  facet_wrap(~year)+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d", 
               limits = c(ymd("2021-07-02"),ymd("2021-10-20")))+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = c(0.75, 0.1),
        legend.direction = "horizontal")+
  labs(x="",y="# salmon")
plot.timing.flow

ggsave(plot=plot.timing.flow, filename="plot.timing.flow.png", height = 6, 
       width=6.5, device = "png")


range(allsalmon$fake.date)

#investigate constant sample proportion
# any harvested tagged fish?
# ave time to recapture fish between witset sites and toboggan?










# # OLD REQUESTS/ANALYSIS ####
# 
# 
# health <- witset.raw %>% 
#   mutate(year = year(Sample_Date)) %>% 
#   group_by(Species, year) %>% 
#   summarize(scale.loss = length(which(`Scale loss` %in% TRUE)),
#             bite.marks = length(which(`bite marks` %in% TRUE)),
#             net.marks = length(which(`Net marks` %in% TRUE)),
#             bleeding.gills = length(which(`Bleeding gills` %in% TRUE)),
#             cyst= length(which(Cyst %in% TRUE)),
#             torn.tails = length(which(`Torn tail` %in% TRUE)),
#             torn.fin = length(which(`Torn fin` %in% TRUE)),
#             fungus = length(which(Fungus %in% TRUE)),
#             sea.lice = length(which(`sea lice` %in% TRUE)),
#             tot.species = length(Species)) %>% 
#   arrange(year, Species)
# 
# 
# 
# #write_csv(health, "health.csv")
# 
# ggsave(plot=ggplot(health)+
#          geom_line(aes(x=year, y=sea.lice/tot.species*100, col= Species))+
#          scale_x_continuous(breaks = seq(min(health$year),max(health$year),1))+
#          labs(x="year", y=paste("% sea lice")),filename = "sea.lice.png",width=6,
#        height=4) 
# 
# ggsave(plot=ggplot(health)+
#          geom_line(aes(x=year, y=scale.loss/tot.species*100, col= Species))+
#          scale_x_continuous(breaks = seq(min(health$year),max(health$year),1))+
#          labs(x="year", y="% scale loss"),filename = "scale.loss.png",width=6,
#        height=4) 
# 
# ggsave(plot=ggplot(health)+
#          geom_line(aes(x=year, y=bite.marks/tot.species*100, col= Species))+
#          scale_x_continuous(breaks = seq(min(health$year),max(health$year),1))+
#          labs(x="year", y="% bite marks"),filename = "bite.marks.png",width=6,
#        height=4) 
# 
# ggsave(plot=ggplot(health)+
#          geom_line(aes(x=year, y=net.marks/tot.species*100, col= Species))+
#          scale_x_continuous(breaks = seq(min(health$year),max(health$year),1))+
#          labs(x="year", y="% net marks"),filename = "net.marks.png",width=6,
#        height=4) 
# 
# 
# ggsave(plot = ggplot(health)+
#          geom_line(aes(x=year, y=torn.tails/tot.species*100, col= Species))+
#          scale_x_continuous(breaks = seq(min(health$year),max(health$year),1))+
#          labs(x="year", y="% torn tail"),filename = "torn.tail.png",width=6,
#        height=4) 



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


