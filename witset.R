# Witset MR
#QA and analysis

# Author: Kristen Peck
# Created: fall 2021, updated Fall/winter 2022/23

library(tidyverse) #; citation("tidyverse")
library(readxl) #; citation("readxl")
library(lubridate) #; citation("lubridate")
library(recapr)
library(gridExtra) #; citation("gridExtra")
#citation("knitr")

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
                            paste0(recap.col,"-",`Recaptured number`))) %>% 
  mutate(tag.yr.sp = ifelse(!is.na(AppliedTagNumber), paste0(AppliedTagNumber,".",year,Species),
                            ifelse(!is.na(`Recaptured number`),paste0(`Recaptured number`,".",year,Species),NA)))
#Note that applied tag is in tag.yr.sp if AR, not recap tag

str(witset)

unique(witset$tag.col)
witset[which(witset$tag.col %in% "Grey"),c("Sample_Date", "Counter", "Species")]

grep("66501",x = witset$AppliedTagNumber)



#how many new records per year?
print.data.frame(witset %>% 
  group_by(year, Species) %>% 
  summarize(total = length(Species)))


##### QA by year####

yr.select <- 2018:2022



# make a summary table

table.summary <- witset %>% 
  group_by(year, Location_Code) %>% 
  summarize(first=format(as.Date(first(Sample_Date)), format = "%d-%b"),
            last=format(as.Date(last(Sample_Date)), format = "%d-%b"),
            n.days = length(unique(Sample_Date)),
            totSK = length(which(Species %in% "SK")),
            totCO = length(which(Species %in% "CO")),
            totCH = length(which(Species %in% "CH")),
            totST = length(which(Species %in% "ST"))) 
table.summary







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
# (i.e. fallback below canyon)?

#get list of unique fish tagged at canyon and recapped at campground:
fallback.fish <- witset %>% 
  filter(Location_Code %in% "Campground") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  mutate(uniq.yrtags = paste0(year,Species,`Recaptured number`)) %>% 
  filter(uniq.yrtags %in% newtags.canyon$uniq.yrtags) 


#how many total fallbacks?
(fallback <- fallback.fish %>%   
  group_by(year, Species) %>% 
  summarize(canyon.fallback = length(Species)))

#how many fallbacks were again recaptured at the canyon?
(fallbacks.recap.canyon <- witset %>% 
  filter(Location_Code %in% "Canyon") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  mutate(uniq.yrtags = paste0(year,Species,`Recaptured number`)) %>% 
  filter(uniq.yrtags %in% fallback.fish$uniq.yrtags) %>% 
  group_by(year, Species) %>% 
  summarize(fallbacks.recap.canyon= length(Species)))

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
(table.fallback <- total.newtags.camp %>% 
  full_join(total.newtags.canyon) %>% 
  full_join(total.camp.recaps) %>% 
  full_join(fallback) %>% 
  mutate(percent.canyon.fallback = round(canyon.fallback/canyon.applied*100,1)) %>% 
  full_join(fallbacks.recap.canyon) %>% 
  mutate(percent.fallbacks.reascend = round(fallbacks.recap.canyon/canyon.fallback*100,1)) %>%
  full_join(camptocanyon.recaps) %>% 
  mutate(percent.camptocanyon = round(camptocanyon.recaps/camp.applied*100,1)) %>% 
  filter(Species %in% c("SK","CO"),year %in% yr.select) %>% 
  # select(year, Species, canyon.fallback, percent.canyon.fallback, 
  #        percent.fallbacks.reascend, percent.camptocanyon) %>% 
  arrange(year, Species))



plot.fallback <- ggplot(table.fallback)+
  geom_point(aes(x=year, y=percent.canyon.fallback, col=Species, 
                 size = canyon.fallback), alpha = 0.5)+
  geom_line(aes(x=year, y=percent.canyon.fallback, col=Species), size= 1.5)+
  scale_y_continuous(breaks=seq(floor(min(table.fallback$percent.canyon.fallback)),
                               max(table.fallback$percent.canyon.fallback),1))+
  scale_x_continuous(breaks=seq(min(table.fallback$year),
                                max(table.fallback$year),1))+
  labs(x="Year",y="% Fallback from Canyon to Campground", 
       size = "# recaptured \nfallback")
plot.fallback


plot.fallbackreascend <- ggplot(table.fallback)+
  geom_line(aes(x=year, y=percent.fallbacks.reascend, col=Species), size=1)+
  geom_point(aes(x=year, y=percent.fallbacks.reascend, col=Species, 
                 size=fallbacks.recap.canyon, alpha = 0.5))+
  geom_line(aes(x=year, y=percent.camptocanyon, col=Species), linetype="dashed")+
  scale_y_continuous(breaks=seq(0,max(table.fallback$percent.camptocanyon, na.rm=T),1))+
  scale_x_continuous(breaks=seq(min(table.fallback$year),
                                max(table.fallback$year),1))+
  labs(x="Year", y="% Fallback recapped at Canyon \nCompared to % of Camp recapped at Canyon",
       size="# fallbacks\nrecapped \nat canyon")
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
  filter(!is.na(new.tag), year %in% yr.select) %>% 
  mutate(tag.sp.year = paste0(new.tag,Species,year))
(tmp2 <- tmp[duplicated(tmp$tag.sp.year),] %>% 
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
#had three (2 CO, 1ST)-all fixed. Just 5 between CH and ST

#how many tag colours with no tag number?
(missing.applied.tagnum <- witset %>% 
  filter(!is.na(AppliedColor),is.na(AppliedTagNumber), year %in% yr.select) %>% 
  select(year, Sample_Date, Counter, new.tag, Species, Location_Code) %>% 
  group_by(year, Species) %>% 
  summarize(no.applied.tag = length(Species)))

# 1 tag with no number, just a colour in 2020
#160 tags with no number, just a colour in 2021
# 4 tags with no number in 2022, could only fix two


#how many recap tag colours with no tag number?
(missing.recap.tagnum <- witset %>% 
  filter(!is.na(`Recaptured Color`),is.na(`Recaptured number`), year %in% yr.select) %>% 
  select(year, Sample_Date, Counter, recap.tag, Species, Location_Code) %>% 
  group_by(year, Species) %>% 
  summarize(no.recap.tag = length(Species)))

#none in 2020
#160 tags with no number, just a colour; 
# 3 in 2021, all b/c recap tag number doesn't exist
# 1 in 2022, unfixable

#how many recap tag numbers with no colour?
witset %>% 
  filter(!is.na(`Recaptured number`),is.na(`Recaptured Color`), year %in% yr.select) %>% 
  select(year ,Sample_Date, Counter, recap.tag)
#none in 2020
#none in 2021
#none in 2022

#check if the tag status is recorded incorrectly
#looking for 1. Tagstatus A, A2, AR does not have col or # in applied columns
#     2. TagStatus AR, R does not have col or # in recap cols
#     3. TagStatus A, AR, A2 with harvested checked.

no.tag.number <- witset %>% 
    filter(year %in% yr.select) %>% 
    filter(Species %in% c("CO","SK","ST")) %>% 
    mutate(bad.tagstatus.applied = ifelse(TagStatus %in% c("A","A2","AR")&
           is.na(AppliedTagNumber)&is.na(AppliedColor), "bad",NA)) %>% 
    mutate(bad.tagstatus.recap = ifelse(TagStatus %in% c("R","AR")&
            is.na(`Recaptured number`)&is.na(`Recaptured Color`), "bad",NA)) %>% 
    mutate(bad.tagstatus.harvest = ifelse(TagStatus %in% c("A","A2","AR")&
          Harvested %in% TRUE, "bad",NA)) %>% 
    select(year,Sample_Date, Counter,Species,TagStatus,
           bad.tagstatus.applied,bad.tagstatus.recap,
           bad.tagstatus.harvest) %>% 
    group_by(year, Species) %>% 
    summarize(bad.tagstatus.applied=length(which(!is.na(bad.tagstatus.applied))),
              bad.tagstatus.recap=length(which(!is.na(bad.tagstatus.recap))),
              bad.tagstatus.harvest=length(which(!is.na(bad.tagstatus.harvest))))
no.tag.number
#49 cases where tag status was recorded as A, A2, or AR but no tag number 
# or colour. Just 1 in 2021- fixed. None in 2020, 2022 for CO, SK
# Most issues are with Recap tag number missing. 


# orphan recap tags (that were never applied)
uniq.applied.tag <- witset %>% 
  filter(year %in% yr.select) %>% 
  filter(!is.na(tag.yr.sp) & TagStatus %in% c("A","A2","AR")) %>%
  group_by(year) %>% 
  summarize(uniq.tag = unique(tag.yr.sp)) %>% 
  mutate(TagStatus = "A")

uniq.recap.tag <- witset %>% 
  filter(year %in% yr.select) %>% 
  filter(!is.na(tag.yr.sp) & TagStatus %in% c("R")) %>% 
  group_by(year, Species) %>% 
  summarize(uniq.tag = unique(tag.yr.sp)) %>% 
  mutate(TagStatus = "R") %>% 
  left_join(uniq.applied.tag, by=c("year","uniq.tag")) %>% 
  filter(is.na(TagStatus.y))
uniq.recap.tag

#total orphan tags:
tot.recap.orphans <- uniq.recap.tag %>% 
  filter(Species %in% c("CO","SK","ST")) %>% 
  group_by(year, Species) %>% 
  summarize(orphan.recaps = length(Species))
tot.recap.orphans



# Problems Remaining Summary:

problems.remaining <- data.frame(year = yr.select, 
                                 Species = rep(c("CO","SK","ST"),length(yr.select))) %>% 
  left_join(missing.applied.tagnum, by = c("year","Species")) %>% 
  left_join(missing.recap.tagnum, by = c("year","Species")) %>% 
  left_join(tot.recap.orphans, by = c("year","Species")) %>% 
  left_join(no.tag.number, by = c("year","Species")) %>% 
  arrange(year, Species)
problems.remaining




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

## LP with Chapman modifier for SK:
LP <- m %>% 
  left_join(r) %>% 
  left_join(c) %>% 
  mutate(LP = (marked)*(total.catch)/(recapped)) %>% #CS had a note about having to add the recapped fish to the numerator?
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
  filter(year %in% yr.select) %>% 
  group_by(Sample_Date, year) %>% 
  summarize(n.SK = length(unique(Counter))) %>% 
    mutate(fake.date = as_date(yday(Sample_Date), 
                               origin = ymd("2022-01-01")))) 

totalcaughtSK <- SKbylocation %>% 
  group_by(year) %>% 
  summarize(totalcaught = sum(totalcaught),
            totalharvested = sum(harvested),
            totalreleased = sum(released)) %>% 
  filter(year %in% yr.select)

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
    filter( year %in% yr.select) %>% 
    group_by(Sample_Date, year) %>% 
    summarize(n.CO = length(unique(Counter))) %>% 
    mutate(fake.date = as_date(yday(Sample_Date), 
                               origin = ymd("2022-01-01")))) 

totalcaughtCO <- CObylocation %>% 
  group_by(year) %>% 
  summarize(totalcaught = sum(totalcaught),
            totalharvested = sum(harvested),
            totalnewtags = sum(newtags)) %>% 
  filter(year %in% yr.select)

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

#old datasheet:
#now line up with Toboggan Creek recaps
# tobog.tags.raw <- read_csv("Toboggan.tagrecoveries.2018-2021.csv",
#                               na = "NA")
# tobog.tags <- tobog.tags.raw %>% 
#   filter(!is.na(tag)) %>% 
#   mutate(year = year(Date),tob.tag = tag, tob.date = Date) %>% 
#   select(year, tob.date, tob.tag)


tobog <- read_excel("TobogganFenceData_MASTER-copy1-Mar-2023.xlsx",
           sheet = "IndividualFish", col_types = c("guess","guess","guess","guess",
                                                   "guess","guess","guess","guess",
                                                   "guess","guess","guess","text",
                                                   "guess","guess","guess","guess",
                                                   "guess","guess","guess","guess",
                                                   "guess","guess","guess","guess")) %>% 
  filter(species %in% "co") %>% 
  mutate(FL = as.numeric(fork_length_mm)/10) %>% 
  mutate(recap_tag_colour = tolower(recap_tag_colour)) %>% 
  mutate(unknown.witset.tag = ifelse(recap_tag_number %in% c("lost"),"un",NA)) %>% 
  mutate(witset.tagloss = ifelse(recap_tag_number %in% c("lost-tag loss"),"tagloss",NA)) %>% 
  mutate(origin = ifelse(recap_tag_colour %in% c("blue","green"), "witset",
                         ifelse(recap_tag_number %in% c("lost","lost-tag loss"), "witset",NA))) %>% 
  mutate(recap_tag_number = as.numeric(recap_tag_number))

str(tobog)

(table.tobog <- tobog %>% 
  filter(year %in% yr.select) %>% 
  group_by(year) %>% 
  summarize(totalcaught = length(species),
            witset.tagged = length(which(origin %in% "witset")),
            total.unk.tags = length(which(!is.na(unknown.witset.tag))),
            min.tagloss = length(which(!is.na(witset.tagloss))),
            hatchery.origin = length(which(mark_gender %in% c("af","am","aj"))),
            wild.origin = length(which(mark_gender %in% c("wf","wm","wj"))) ))


# check FL of the Toboggan recaps and the rest of the fish - not many measured...
# so use the FLs from Witset

ggplot(tobog)+
  geom_histogram(aes(x=FL, fill=!is.na(recap_tag_number)))+
  facet_wrap(~year)

tobog.tags <- tobog %>% 
  filter(!is.na(recap_tag_number))

witsetCO <- witset %>% 
  filter(Species %in% "CO") %>% 
  filter(year %in% yr.select) %>% 
  mutate(tag.number = ifelse(!is.na(`Recaptured number`),`Recaptured number`,
                             ifelse(!is.na(AppliedTagNumber),AppliedTagNumber,NA)))


tag.match1 <- tobog.tags %>%
  left_join(witsetCO, by= c("year","recap_tag_number" = "tag.number")) %>% 
  select(year, date, recap_tag_number, TagStatus, Sample_Date, species,
         Location_Code, ForkLength) %>% 
  filter(year %in% yr.select)

#orphaned Toboggan recaps:
tag.match1 %>% 
  filter(is.na(recap_tag_number)) %>% 
  group_by(year, species) %>% 
  summarize(length(species))



#18 recovered tags at toboggan between 2018 and 2021 have no match at witset
#update: back to 2014, there are now 77 tags from Toboggan that are not in Witset DB
# fixed back to 2018 of these so far, 58 remain from 2016 and further backward


# Are campground and canyon tagged fish equally likely to be recaptured at Toboggan?

newtags.camp.canyon <- rbind(total.newtags.camp %>% 
                               mutate(Location_Code = "Campground",newtags.applied=camp.applied) %>% 
                               filter(Species %in% "CO"),
                             total.newtags.canyon %>% 
                               mutate(Location_Code = "Canyon",newtags.applied=canyon.applied) %>% 
                               filter(Species %in% "CO")) %>% 
  select(year,Species, Location_Code, newtags.applied)

recap.origin.tobog <- tag.match1 %>% 
  filter(TagStatus %in% "A") %>% 
  group_by(year, Location_Code) %>% 
  summarize(number.from.origin = length(Location_Code)) %>% 
  left_join(newtags.camp.canyon, by = c("year","Location_Code")) %>% 
  mutate(percent.recap = round(number.from.origin/newtags.applied*100,2))
recap.origin.tobog


#### Dirty estimates by location ####
#

#assume Campground OR Canyon is only location where MR taking place. 
# this is generating population estimates based on assumption that fish 
# are milling around where they are tagged, whereas all other analyses assume
# that fish are moving immediately, directionally, upstream to the next location

uniq.tags.CO.camp <- witset %>% 
  filter(Species %in% "CO", year %in% yr.select, 
         Location_Code %in% "Campground",TagStatus %in% c("A","A2","AR")) %>%
  group_by(year, Location_Code) %>% 
  summarize(AppliedTagNumber = unique(AppliedTagNumber, na.rm=T)) %>% 
  mutate(mark.origin = "Camp")

uniq.tags.CO.cany <- witset %>% 
  filter(Species %in% "CO", year %in% yr.select, 
         Location_Code %in% "Canyon",TagStatus %in% c("A","A2","AR")) %>%
  group_by(year, Location_Code) %>% 
  summarize(AppliedTagNumber = unique(AppliedTagNumber, na.rm=T)) %>% 
  mutate(mark.origin = "Canyon")

(COCamp <- witset %>% 
    filter(Species %in% "CO", year %in% yr.select) %>%
    left_join(uniq.tags.CO.camp, by=c("year","Location_Code","AppliedTagNumber")) %>% 
    left_join(uniq.tags.CO.cany, by=c("year","Location_Code","AppliedTagNumber")) %>%
    filter(Location_Code %in% "Campground", is.na(mark.origin.y)) %>% 
    group_by(Location_Code,Species,year) %>% 
    summarize(totalcaught = length(Location_Code), 
              harvested=length(which(Harvested %in% TRUE)),
              #released = length(which(TagStatus %in% c("A"))),
              newtags = length(which(TagStatus %in% c("A","A2"))),
              recaps.camp = length(which(TagStatus %in% c("AR","R")))) %>% 
    mutate(Chap = NChapman(newtags,totalcaught,recaps.camp)) %>% 
    mutate(seChap = seChapman(newtags,totalcaught,recaps.camp)) %>% 
    mutate(CI95Chap = seChap*1.965) %>% 
    arrange(year,Location_Code))


(COCanyon <- witset %>% 
    filter(Species %in% "CO", year %in% yr.select) %>%
    left_join(uniq.tags.CO.camp, by=c("year","Location_Code","AppliedTagNumber")) %>% 
    left_join(uniq.tags.CO.cany, by=c("year","Location_Code","AppliedTagNumber")) %>%
    filter(Location_Code %in% "Canyon", is.na(mark.origin.x)) %>% 
    group_by(Location_Code,Species,year) %>% 
    summarize(totalcaught = length(Location_Code), 
              harvested=length(which(Harvested %in% TRUE)),
              #released = length(which(TagStatus %in% c("A"))),
              newtags = length(which(TagStatus %in% c("A","A2"))),
              recaps.cany = length(which(TagStatus %in% c("AR","R")))) %>% 
    mutate(Chap = NChapman(newtags,totalcaught,recaps.cany)) %>% 
    mutate(seChap = seChapman(newtags,totalcaught,recaps.cany)) %>% 
    mutate(CI95Chap = seChap*1.965) %>% 
    arrange(year,Location_Code))

COCamp
COCanyon

ggplot(rbind(COCamp,COCanyon))+
  geom_point(aes(x=year, y=Chap, col=Location_Code, size=totalcaught))+
  geom_errorbar(aes(x=year,ymin=Chap-CI95Chap,ymax=Chap+CI95Chap, col=Location_Code), width=0.2)+
  geom_line(aes(x=year, y=Chap, col=Location_Code))
# I'm not really sure what to make of this... so shelved for now.




#generate estimates using only campground OR canyon and toboggan
#This might help point out if there is differential survival/tag loss from
# one location or the other?

dirty.est.tobog <- recap.origin.tobog  %>%  
  left_join(table.tobog) %>% 
  select(year, Location_Code, Species, number.from.origin,newtags.applied,
         totalcaught) %>% 
  mutate(Chap = NChapman(newtags.applied,totalcaught,number.from.origin)) %>% 
  mutate(seChap = seChapman(newtags.applied,totalcaught,number.from.origin)) %>% 
  mutate(CI95Chap = seChap*1.96) %>% 
  arrange(year, Location_Code)
dirty.est.tobog

ggplot(dirty.est.tobog)+
  geom_point(aes(x=year, y=Chap, col=Location_Code))+
  geom_errorbar(aes(x=year, ymax=Chap+CI95Chap,ymin=Chap-CI95Chap, 
                    col=Location_Code), width=0.1)+
  geom_line(aes(x=year, y=Chap, col=Location_Code))+
  labs(y="Chapman estimate from different tagging \nLocations to Toboggan +/- 95CI")






plot.recap.origin.tobog <- ggplot(recap.origin.tobog)+
  geom_line(aes(x=year, y=percent.recap, col=Location_Code))+
  geom_point(aes(x=year, y=percent.recap, col=Location_Code, 
                 size=newtags.applied),alpha=.5)+
  scale_y_continuous(breaks = seq(floor(min(recap.origin.tobog$percent.recap)),
                                max(recap.origin.tobog$percent.recap),1))+
  labs(y="% of applied tags recaptured at Toboggan",col="Location",
       size = "# tags applied")
plot.recap.origin.tobog


# #time between tagging and recovery at Toboggan
# (ave.canyon.to.fence <- tag.match %>%
#   filter(!is.na(Sample_Date)) %>%
#   select(year,recap_tag_number, canyon.to.fence) %>%
#   group_by(year) %>%
#   summarize(ave.canyon.to.fence = round(mean(canyon.to.fence),0),
#             n=length(canyon.to.fence)))
# 
#  tag.match %>% 
#    filter(is.na(new.tag) & is.na(recap.tag))
#   #17 tags found in neither the initial cap or the recap
# 
# ggplot()+
#   geom_histogram(data = tag.match,aes(x=canyon.to.fence), binwidth=5)+
#   geom_vline(data= ave.canyon.to.fence,aes(xintercept = ave.canyon.to.fence))+
#   facet_wrap(~year)


#difference between those with tag status A, A2 and AR vs a number in AppliedTag
witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  filter(TagStatus %in% c("A","A2","AR")) %>% 
  nrow()

witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  filter(!is.na(AppliedTagNumber)) %>% 
  nrow()

witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  filter(TagStatus %in% c("A","A2","AR")& is.na(AppliedTagNumber)) %>% 
  select(Sample_Date,Location_Code,Counter, Species, TagStatus,AppliedColor,
         AppliedTagNumber)

# about 7 entries total > 2018. Reduced to 2




#difference between those with tag status R and AR vs a number in RecapTagNumber
witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  filter(TagStatus %in% c("R","AR")) %>% 
  nrow()

witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  filter(!is.na(`Recaptured Color`)) %>% 
  nrow()

witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  filter(!is.na(`Recaptured number`)) %>% 
  nrow()

witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  filter(!is.na(`Recaptured Color`) & is.na(`Recaptured number`)) %>% 
  select(Sample_Date,Location_Code,Counter, Species, TagStatus,`Recaptured Color`,
         `Recaptured number`)
# 10 fish where the tag number was not recorded but there is a tag colour.Fixed 5
# For these we could generate likely tags...?
# about 62 entries total, fixed some so now 46. 
# Legitimate to forget these though and just go with number,
# since most of them were batch-marked only when they should not have been and recorded
# inconsistently. This would mask some actual recaps but we are most interested in 
# tagged fish because they were for sure tagged and recaptured. Should mention this in 
# the methods though... In future, can use this R rate for tag loss rate, 
# and just use the recap tag number for the MR analysis



#### Capture Histories ####

#KP's try at capture histories...:
#Applied tags
applied.witset <- witset %>% 
  filter(!is.na(AppliedTagNumber)) %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK", "ST")) %>% 
  select(year, Sample_Date, Location_Code, Species,TagStatus,
         tag.col=AppliedColor, tag.num = AppliedTagNumber,
         ForkLength, Sex)

#how many with status A, A2, AR but excluding because no number?
witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  filter(TagStatus %in% c("A","A2","AR")& is.na(AppliedTagNumber)) %>% 
  select(Sample_Date,Location_Code,Counter, Species, TagStatus,AppliedColor,
         AppliedTagNumber) %>% 
  nrow()


recap.witset <- witset %>% 
  filter(!is.na(`Recaptured number`)) %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK", "ST")) %>% 
  select(year, Sample_Date, Location_Code, Species,TagStatus,
          tag.col=`Recaptured Color`, tag.num = `Recaptured number`,
          ForkLength, Sex)



#how many excluded because have no number?
witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  filter(TagStatus %in% c("R","AR") & is.na(`Recaptured number`)) %>% 
  select(Sample_Date,Location_Code,Counter, Species, TagStatus,`Recaptured Color`,
         `Recaptured number`) %>% 
  nrow()

# how many double-counted AR fish that would be fixed if we were using individual capture histories?
witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  filter(TagStatus %in% c("AR")) %>% 
  nrow()

#stack these so 1 row = 1 fish:

tags.all.witset <- rbind(applied.witset, recap.witset) %>% 
  mutate(tag.yr.sp = paste0(tag.num, year, Species),
         ISOweek = isoweek(Sample_Date)) %>% 
  arrange(year,Species, tag.num)


uniq.tags <- data.frame(tag.yr.sp = unique(tags.all.witset$tag.yr.sp)) %>% 
  mutate(ID = 1:length(unique(tags.all.witset$tag.yr.sp)))
#abandoned for the moment, since Carl's method works after some small adjustments




#### REPORTING ####
# From other script- integrate
##### effort and catch summary table ####
yr.select 

effort.table <- witset %>% 
  filter(year %in% yr.select) %>% 
  group_by(year, Location_Code) %>% 
  summarize(first=min(Sample_Date), last=max(Sample_Date), 
            ndays=length(unique(Sample_Date)),
            duration = yday(last)-yday(first), 
            totCO = length(which(Species %in% "CO")),
            totSK = length(which(Species %in% "SK")), 
            totCH = length(which(Species %in% "CH"))) %>% 
  mutate(first.fake.date = as_date(yday(first), origin=ymd("2022-01-01")),
         last.fake.date = as_date(yday(last), origin=ymd("2022-01-01")))

(salmon.table <- witset %>% 
    filter(year %in% yr.select) %>% 
    group_by(year, Location_Code, Species) %>% 
    summarize(`Total Caught` = length(Species)) %>% 
    pivot_wider(names_from = Location_Code,values_from =`Total Caught` ))

daily.catch <- witset %>% 
  filter(year %in% yr.select) %>% 
  group_by(year, Sample_Date, Species, Location_Code) %>% 
  summarize(dailyCO = length(which(Species %in% "CO")),
            dailySK = length(which(Species %in% "SK")), 
            dailyCH = length(which(Species %in% "CH"))) 




#overall trends in catch per day

plot.COperday.loess <- ggplot(data = effort.table)+
  geom_point(aes(x=year, y=totCO/ndays, col=Location_Code, size=ndays), alpha=0.50)+
  geom_smooth(aes(x=year, y=totCO/ndays, col=Location_Code),method = "loess",se = F)+
  labs(x= "", y="Ave. Coho per day", title="A. Coho", col="", size="#days fished")+
  scale_x_continuous(breaks = yr.select)+
  theme(legend.position = "none")
plot.COperday.loess

plot.SKperday.loess <- ggplot(data = effort.table)+
  geom_point(aes(x=year, y=totSK/ndays, col=Location_Code, size=ndays), alpha=0.50)+
  geom_smooth(aes(x=year, y=totSK/ndays, col=Location_Code),method = "loess",se = F)+
  labs(x= "", y="Ave. Sockeye per day", title="B. Sockeye", col="", size="#days fished")+
  scale_x_continuous(breaks = yr.select)+
  theme(legend.position = "none")
plot.SKperday.loess

plot.CHperday.loess <- ggplot(data = effort.table)+
  geom_point(aes(x=year, y=totCH/ndays, col=Location_Code, size=ndays), alpha=0.50)+
  geom_smooth(aes(x=year, y=totCH/ndays, col=Location_Code),method = "loess", se=F)+
  labs(y="Ave. Chinook per day", col="Location", title="C. Chinook",col="", size="#days fished")+
  scale_x_continuous(breaks = yr.select)+
  theme(legend.position = "bottom")
plot.CHperday.loess

plot.catchperday <- arrangeGrob(plot.COperday.loess, plot.SKperday.loess, 
                                plot.CHperday.loess)
plot(plot.catchperday)

#statistics - trend in catch/day?

(sum.COglm <- summary(glm(data = effort.table, formula=totCO ~ year+Location_Code,
                          family = "poisson")))
(sum.SKglm <- summary(glm(data = effort.table, formula=totSK ~ year+Location_Code,
                          family = "poisson")))
summary(glm(data = effort.table[effort.table$year >=2017,], formula=totCH ~ year+Location_Code,
            family = "poisson"))






##### FL diff harvested, released and tagged fish ####

(sizes <- witset %>%
   filter(year %in% yr.select) %>% 
   filter(ForkLength %in% c(20:120 )) %>% 
   filter(TagStatus != "R") %>% #so that decisions are not masked by recaps
   mutate(TagStatus = ifelse(TagStatus %in% c("A","AR","A2","R"), "Yes","No")) %>% 
   mutate(tag.harvest = paste0(TagStatus,".",Harvested)) %>% 
   #mutate(tag.harvest = ifelse(TagStatus %in% "No" & Harvested %in% "True", "harvested",
   #                             ifelse(TagStatus %in% "No" & Harvested %in% "False", "not harvested",
   #                                   ifelse(TagStatus %in% "Yes", "tagged", NA)))) %>% 
   #group_by(year, Species, Location_Code, Harvested, TagStatus) %>% 
   #summarize(ave.FL = mean(ForkLength, na.rm=T), min.FL=min(ForkLength, na.rm=T),
   #          max.FL=max(ForkLength, na.rm=T)) %>% 
   mutate(Tag.Harvest = ifelse(tag.harvest %in% "No.FALSE", "Released",
                               ifelse(tag.harvest %in% "No.TRUE", "Harvested",
                                      ifelse(tag.harvest %in% "Yes.FALSE", "Tagged",NA)))) %>% 
   arrange(Species, year,Location_Code, Harvested))


plot.forklengthallsp <- ggplot(sizes)+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~Species)+
  theme(legend.position =  "bottom",
        axis.text.y = element_blank())+
  labs(fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))
plot.forklengthallsp


sizes.CO.df <- sizes %>%
  filter(Species %in% "CO") 

ggplot(sizes.CO.df)+
  geom_histogram(aes(x=ForkLength, fill=Tag.Harvest), binwidth=5, col="black")+
  facet_wrap(~year+Location_Code)

#compare sizes for fish recaptured at Toboggan with tagged fish
sizes.CO.tagged <- sizes.CO.df %>% 
  filter(Tag.Harvest %in% "Tagged") %>% 
  mutate(tag.number = ifelse(!is.na(`Recaptured number`),`Recaptured number`,
                             ifelse(!is.na(AppliedTagNumber),AppliedTagNumber,NA))) %>% 
  left_join(tobog.tags, by = c("year", "tag.number" = "recap_tag_number")) %>% 
  filter(year %in% yr.select)


plot.forklengthCOtobog.camp <- ggplot(sizes.CO.tagged[sizes.CO.tagged$Location_Code %in% "Campground",])+
  geom_boxplot(aes(x=ForkLength, fill=!is.na(order)), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  c(0.85,0.25),
        axis.text.y = element_blank())+
  labs(fill = "Campground fish \nRecaptured at Toboggan")
plot.forklengthCOtobog.camp

plot.forklengthCOtobog.canyon <- ggplot(sizes.CO.tagged[sizes.CO.tagged$Location_Code %in% "Canyon",])+
  geom_boxplot(aes(x=ForkLength, fill=!is.na(order)), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  c(0.85,0.25),
        axis.text.y = element_blank())+
  labs(fill = "Canyon fish \nRecaptured at Toboggan")
plot.forklengthCOtobog.canyon

plot.forklengthCOtobog <- arrangeGrob(plot.forklengthCOtobog.camp,
                                      plot.forklengthCOtobog.canyon)
plot(plot.forklengthCOtobog)

#FL just in Witset by year
plot.forklengthCOyr <- ggplot(sizes.CO.df)+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  c(0.75,0.25),
        axis.text.y = element_blank())+
  labs(title = "A. Coho", fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))
plot.forklengthCOyr



plot.forklengthCO.camp <- ggplot(sizes.CO.df[which(sizes.CO.df$Location_Code %in% "Campground"),])+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  c(0.75,0.25),
        axis.text.y = element_blank())+
  labs(title = "A. Coho Campground", fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))+
  scale_x_continuous(limits = c(20,90))
plot.forklengthCO.camp

plot.forklengthCO.canyon <- ggplot(sizes.CO.df[which(sizes.CO.df$Location_Code %in% "Canyon"),])+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  c(0.75,0.25),
        axis.text.y = element_blank())+
  labs(title = "B. Coho Canyon", fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))+
  scale_x_continuous(limits = c(20,90))
plot.forklengthCO.canyon

plot.forklengthCO.location <- arrangeGrob(plot.forklengthCO.camp,
                                          plot.forklengthCO.canyon)
plot(plot.forklengthCO.location)

# ggsave(plot=plot.forklengthCO, 
#        filename = "plot.forklengthCO.png", device = "png", width = 12, height=9.4)

sizes.SK.df <- sizes %>%
  filter(Species %in% "SK")

ggplot(sizes.SK.df)+
  geom_histogram(aes(x=ForkLength, fill=Tag.Harvest), binwidth=5,  
                 position = "stack")+
  facet_wrap(~year)

plot.forklengthSK <- ggplot(sizes.SK.df)+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest),col="black", varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  c(0.75,0.25),
        axis.text.y = element_blank())+
  labs(title = "B. Sockeye",  fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))
plot.forklengthSK


plot.forklengthSK.camp <- ggplot(sizes.SK.df[which(sizes.SK.df$Location_Code %in% "Campground"),])+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  c(0.75,0.25),
        axis.text.y = element_blank())+
  labs(title = "A. Sockeye Campground", fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))+
  scale_x_continuous(limits = c(20,80))
plot.forklengthSK.camp

plot.forklengthSK.canyon <- ggplot(sizes.SK.df[which(sizes.SK.df$Location_Code %in% "Canyon"),])+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  c(0.75,0.25),
        axis.text.y = element_blank())+
  labs(title = "B. Sockeye Canyon", fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))+
  scale_x_continuous(limits = c(20,80))
plot.forklengthSK.canyon

plot.forklengthSK.location <- arrangeGrob(plot.forklengthSK.camp,
                                          plot.forklengthSK.canyon)
plot(plot.forklengthSK.location)


# ggsave(plot=plot.forklengthSK, 
#        filename = "plot.forklengthSK.png", device = "png", width = 12, height=9.4)


sizes.CH.df <- sizes %>%
  filter(Species %in% "CH")

ggplot(sizes.CH.df)+
  geom_histogram(aes(x=ForkLength, fill=Harvested),col="black", binwidth=2)+
  facet_wrap(~year)

plot.forklengthCH <- ggplot(sizes.CH.df)+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest))+
  facet_wrap(~year)+
  theme(legend.position = "bottom",
        axis.text.y = element_blank())+
  labs(title = "C. Chinook", x="Fork Length (cm)")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))
plot.forklengthCH

# ggsave(plot=plot.forklengthCH, 
#        filename = "plot.forklengthCH.png", device = "png", width = 12, height=9.4)


# plot.forklength <- arrangeGrob(plot.forklengthCO, plot.forklengthSK)
# plot(plot.forklength)


#ggsave(plot=plot.forklength, 
#       filename = "plot.forklength.png", device = "png", width = 6.5, height=9.4)


#### environmental data - flow ####
library(tidyhydat)
#download_hydat()

bulkley.flow.realtime <- read_csv("08EE005_QR_20230308T2306.csv", skip = 10,
                                  col_names = c("Date","Parameter","Value",
                                                "Approval","Qualifier")) %>% 
  mutate(datetime = dmy_hm(Date), Date = dmy(substr(Date,1,10)), Parameter = "Flow", 
         STATION_NUMBER = "08EE005",Symbol=NA) %>%
  group_by(STATION_NUMBER, Date, Parameter, Value, Symbol) %>% 
  summarize(Value = mean(Value, na.rm=T))



bulkley.flow.raw <- hy_daily_flows(station_number = "08EE005") %>% 
  rbind(bulkley.flow.realtime)%>% 
  mutate(year = year(Date),yday = yday(Date), 
         fake.date=as_date(yday, origin = ymd("2022-01-01"))) 

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
  filter(year %in% yr.select) %>%
  group_by(fake.date) %>%
  summarize(ave.daily = mean(Value, na.rm=T), sd.daily = sd(Value, na.rm=T)) %>%
  mutate(lower.sd = ifelse(ave.daily-sd.daily<0, 0, ave.daily-sd.daily),
         upper.sd = ave.daily+sd.daily)
ave.bulkley.flow

ggplot()+
  geom_ribbon(data=ave.bulkley.flow, 
              aes(x=fake.date, ymin=lower.sd, ymax=upper.sd),alpha=.5)

bulkley.flow <- bulkley.flow.raw %>% 
  filter(Date %in% ymd("2018-01-01"):ymd("2022-12-31")) %>% 
  filter(fake.date >= ymd("2022-07-01")& fake.date < ymd("2022-11-01"))


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
  geom_vline(data = effort.table, 
             aes(xintercept = first.fake.date, col=Location_Code, 
                 linetype = Location_Code), 
             size=1.5,alpha=.5)+
  geom_vline(data = effort.table, 
             aes(xintercept = last.fake.date,col=Location_Code, 
                 linetype = Location_Code), 
             size=1.5,alpha=.5)+
  geom_hline(data = bulkley.flow, aes(yintercept = 310), size=.5, col="black")+
  facet_wrap(~ year)+
  labs(x="Date", y="Discharge (m^3/s, station 08EE005)", col="Location",
       linetype="Location")+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d", 
               limits = c(ymd("2022-07-01"),ymd("2022-10-31")))+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = c(0.75, 0.25))
plot.bulkley.start

# ggsave(plot= plot.bulkley.start, filename = "plot.flow.startend.png", 
#        height = 4, width = 6.5, device = "png")

# caught per day by flow

witset <- witset %>% 
  mutate(fake.date = as_date(yday(Sample_Date), origin=ymd("2022-01-01")))


plot.timing.flow <- ggplot(witset[which(witset$year %in% yr.select),])+
  geom_histogram(aes(x=fake.date, fill=Species),binwidth = 1)+
  geom_line(data = bulkley.flow, aes(x=fake.date, y=Value), alpha = 0.25, size=1)+
  facet_wrap(~year)+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d", 
               limits = c(ymd("2022-07-02"),ymd("2022-10-20")))+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "bottom",
        legend.direction = "horizontal")+
  labs(x="",y="# fish caught")
plot.timing.flow

# ggsave(plot=plot.timing.flow, filename="plot.timing.flow.png", height = 6, 
#        width=6.5, device = "png")





# # OLD REQUESTS/ANALYSIS ####
# 
# 
health <- witset.raw %>%
  mutate(year = year(Sample_Date)) %>%
  group_by(Species, year) %>%
  summarize(scale.loss = length(which(`Scale loss` %in% TRUE)),
            bite.marks = length(which(`bite marks` %in% TRUE)),
            net.marks = length(which(`Net marks` %in% TRUE)),
            bleeding.gills = length(which(`Bleeding gills` %in% TRUE)),
            cyst= length(which(Cyst %in% TRUE)),
            torn.tails = length(which(`Torn tail` %in% TRUE)),
            torn.fin = length(which(`Torn fin` %in% TRUE)),
            fungus = length(which(Fungus %in% TRUE)),
            sea.lice = length(which(`sea lice` %in% TRUE)),
            tot.species = length(Species)) %>%
  arrange(year, Species)
names(health)

health.yr <- health %>% 
  group_by(year) %>% 
  summarize(prop.sea.lice = sum(sea.lice)/sum(tot.species), 
            prop.scale.loss = sum(scale.loss)/sum(tot.species),
            prop.net.marks = sum(net.marks)/sum(tot.species)) %>% 
  pivot_longer(!year, names_to = "health.note", values_to = "prop")

ggplot(health.yr)+
  geom_line(aes(x=year, y=prop, col=health.note))+
  scale_x_continuous(breaks = seq(min(health$year),max(health$year),1))
  

#write_csv(health, "health.csv")

# ggsave(plot=ggplot(health)+
#          geom_line(aes(x=year, y=sea.lice/tot.species*100,col= Species))+
#          scale_x_continuous(breaks = seq(min(health$year),max(health$year),1))+
#          labs(x="year", y=paste("% sea lice")),filename = "sea.lice.png",width=6,
#       height=4)
# 
# 
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


