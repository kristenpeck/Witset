# Witset MR
#QA and pre-analysis

# Author: Kristen Peck
# Created: fall 2021, updated Fall/winter 2022/23
# revisited Feb 2025 for final report

library(tidyverse) #; citation("tidyverse")
library(readxl) #; citation("readxl")
library(lubridate) #; citation("lubridate")
library(recapr)
library(gridExtra) #; citation("gridExtra")
#citation("knitr")

# load exported tables for Dates, SK, CO, CH, ST, and (new in 2023) the seine effort
# note that these are the unaltered tables exported as-is from 
# Witset_Fish_Tagging_Data_V3_MASTER.accdb and later years

# Each species table has the same headings so I just rbind them here and 
# connect with the dates table with a left_join

#2023 has a new database so I left those relationships alone and load them separately
#the 2024 database includes 2023 data so is filtered for 2024 only


# Load Carl's simple petersen function:
source("SimplePetersen.R")


### Load Witset Data ####

#from db v3: 2012 to 2022

nms <- names(read_excel("./data/Tag_Data_Coho.xlsx", n_max = 0))
ct <- ifelse(grepl("^AppliedColor2", nms)|grepl("^Recaptured.Color2", nms), "text", 
             ifelse(grepl("^AppliedTagNumber2", nms)|grepl("^Recaptured.number2", nms), 
                    "numeric","guess"))


witset.raw.upto22 <- rbind(read_excel("Tag_Data_Sockeye.xlsx",
                                      .name_repair="universal",col_types = ct),
                          read_excel("Tag_Data_Coho.xlsx",
                                     .name_repair="universal",col_types = ct),
                          read_excel("Tag_Data_Chinook.xlsx",
                                     .name_repair="universal",col_types = ct),
                          read_excel("Tag_Data_Steelhead.xlsx",
                                     .name_repair="universal",col_types = ct)) %>% 
  left_join(read_excel("Tag_Data_Sample_Dates.xlsx",.name_repair="universal"), by="Sample_Id")

# nms.eff <- names(read_excel("Sampling_effort.xlsx", n_max = 0))
# ct.eff <- ifelse(grepl("^start_time", nms.eff), "datetime", 
#              ifelse(grepl("^end_time", nms.eff), "datetime","guess"))

samplingeffort22 <- read_excel("Sampling_effort.xlsx",.name_repair="universal") %>% 
  left_join(read_excel("Tag_Data_Sample_Dates.xlsx",.name_repair="universal"), 
            by=c("sample_ID"="Sample_Id"))



# from db v3: 2023 

witset.raw.upto23 <- rbind(read_excel("Tag_Data_Sockeye2023.xlsx",
                                      .name_repair="universal",col_types = ct),
                           read_excel("Tag_Data_Coho2023.xlsx",
                                      .name_repair="universal",col_types = ct),
                           read_excel("Tag_Data_Chinook2023.xlsx",
                                      .name_repair="universal",col_types = ct),
                           read_excel("Tag_Data_Steelhead2023.xlsx",
                                      .name_repair="universal",col_types = ct)) %>% 
  left_join(read_excel("Tag_Data_Sample_Dates2023.xlsx",.name_repair="universal"), by="Sample_Id")

samplingeffort23 <- read_excel("Sampling_effort2023.xlsx",.name_repair="universal") %>% 
  left_join(read_excel("Tag_Data_Sample_Dates2023.xlsx",.name_repair="universal"), by=c("sample_ID"="Sample_Id"))




#do not yet have raw data for QAQC:
witset.raw.upto24 <- rbind(read_excel("Tag_Data_Sockeye2024.xlsx",
                                      .name_repair="universal",col_types = ct),
                           read_excel("Tag_Data_Coho2024.xlsx",
                                      .name_repair="universal",col_types = ct),
                           read_excel("Tag_Data_Chinook2024.xlsx",
                                      .name_repair="universal",col_types = ct),
                           read_excel("Tag_Data_Steelhead2024.xlsx",
                                      .name_repair="universal",col_types = ct))%>%
  left_join(read_excel("Tag_Data_Sample_Dates2024.xlsx",.name_repair="universal"),
            by="Sample_Id") %>%
  filter(sample_year %in% 2024)
  


samplingeffort24 <- read_excel("Sampling_effort2024.xlsx",.name_repair="universal") %>% 
  left_join(read_excel("Tag_Data_Sample_Dates2024.xlsx",.name_repair="universal"), 
            by=c("sample_ID"="Sample_Id")) %>% 
  filter(sample_year %in% 2024)


samplingeffort <- rbind(samplingeffort22,samplingeffort23, samplingeffort24) %>% 
  mutate(start_time = ymd_hms(paste(substr(Sample_Date, 1,10),substr(start_time, 12,19)))) %>% 
  mutate(end_time = ymd_hms(paste(substr(Sample_Date, 1,10),substr(end_time, 12,19)))) %>% 
  mutate(duration.hrs = difftime(end_time,start_time,units = "hours"))


witset.raw <- rbind(witset.raw.upto22, witset.raw.upto23, witset.raw.upto24) 


#clean up and pair down dataset

witset <- witset.raw %>% 
  mutate(Sample_Date = as_date(Sample_Date)) %>% 
  mutate(tag.col=recode(AppliedColor,Orange="o",yellow="y",White="w",
                        Yellow="y",Green="g",`Light Green`="g",
                        `Light Orange`="lt.o",Pink="p",`Lime Green`="g",
                        Blue="b",Red="r")) %>% 
  mutate(recap.col=recode(Recaptured.Color,Orange="o",yellow="y",
                          White="w",Yellow="y",Green="g",
                          `Light Green`="g",`Light Orange`="lt.o",
                          Pink="p",`Lime Green`="g",Blue="b",Red="r")) %>%
  mutate(year = year(Sample_Date),
         new.tag = ifelse(is.na(tag.col)&is.na(AppliedTagNumber),NA,
                          paste0(tag.col,"-",AppliedTagNumber))) %>% #combines col letter and #
  mutate(recap.tag = ifelse(is.na(recap.col)&is.na(Recaptured.number),NA,
                            paste0(recap.col,"-",Recaptured.number))) %>% 
  mutate(tag.yr.sp = ifelse(!is.na(AppliedTagNumber), paste0(AppliedTagNumber,".",year,Species),
                            ifelse(!is.na(Recaptured.number),paste0(Recaptured.number,".",year,Species),NA))) %>% 
  #the following from Carl's script:
  mutate(AppliedTagNumberPresent = !is.na(AppliedTagNumber), #uses only those fish with tag number
       RecapturedTagNumberPresent=!is.na(Recaptured.number)) %>% 
  mutate(VentralClip = NULL, AdiposeClip = NULL) %>% 
  mutate(Year.Species = paste0(year,".",Species)) %>% 
  mutate(AppliedColor = tolower(AppliedColor),
         RecapturedColor = tolower(Recaptured.Color)) %>% 
  mutate(myTagColor = RecapturedColor, 
         myTagNumber = Recaptured.number) %>% #starts with recap # in this spot so if there is an applied tag it will overwrite
  mutate(myTagColor = ifelse(!is.na(AppliedColor),AppliedColor,myTagColor)) %>% 
  mutate(myTagNumber = ifelse(!is.na(AppliedColor),AppliedTagNumber,myTagNumber)) %>% 
  mutate(ISOweek = isoweek(Sample_Date)) %>% 
  mutate(SYT = paste(Species, year, myTagNumber, sep=".")) 
#Note that applied tag is in tag.yr.sp, not recap tag in cases of AR


rm(list = c("samplingeffort22","samplingeffort23","samplingeffort24", "ct","nms",
            "witset.raw.upto22", "witset.raw.upto23", "witset.raw.upto24"))

#### Years select ####

yr.select <- 2018:2024 # will change this to c(2015:2016, 2018:2024) once DB goes thru 2015,2016 data


#### CPUE Seine ####

str(samplingeffort)

effort.camp.byday <- samplingeffort %>% 
  #filter(sample_year %in% yr.select) %>% 
  group_by(Sample_Date,sample_year, Location_Code) %>% 
  summarize(num.sets = length(set_num), 
            total.set.time = sum(duration.hrs, na.rm=T),
            mn.time.per.set = mean(duration.hrs, na.rm=T), 
            tot.SK = sum(sockeye_cnt),tot.CO = sum(coho_cnt),
            tot.CH = sum(chinook_cnt),tot.ST = sum(steelhead_cnt),
            tot.PK = sum(pinks_cnt),
            CPUEbyset.SK = tot.SK/num.sets,
            CPUEbyset.CO = tot.CO/num.sets,
            CPUEbyhr.SK = tot.SK/as.numeric(total.set.time),
            CPUEbyhr.CO = tot.CO/as.numeric(total.set.time),
            CPUEbyset.PK = tot.PK/num.sets,
            CPUEbyhr.PK = tot.PK/as.numeric(total.set.time)) %>% 
  filter(!is.na(mn.time.per.set))

# ggplot(effort.camp.byday)+
#   geom_line(aes(x=yday(Sample_Date),y=CPUEbyset.SK), col="black")+
#   geom_line(aes(x=yday(Sample_Date),y=CPUEbyhr.SK),col="red")+
#   facet_wrap(~sample_year)+
#   labs(x="julian day", y="CPUE", title="SK")
# 
# ggplot(effort.camp.byday)+
#   geom_line(aes(x=yday(Sample_Date),y=CPUEbyset.CO), col="black")+
#   geom_line(aes(x=yday(Sample_Date),y=CPUEbyhr.CO),col="blue")+
#   facet_wrap(~sample_year)+
#   labs(x="julian day", y="CPUE", title="CO")
# 
# ggplot(effort.camp.byday)+
#   geom_line(aes(x=yday(Sample_Date),y=CPUEbyset.PK), col="black")+
#   geom_line(aes(x=yday(Sample_Date),y=CPUEbyhr.PK),col="blue")+
#   facet_wrap(~sample_year)+
#   labs(x="julian day", y="CPUE", title="PK")


effort.camp.byyr <- effort.camp.byday%>% 
  group_by(sample_year, Location_Code) %>% 
  summarize(num.days.fished = length(unique(Sample_Date)),
            total.sets = sum(num.sets),
            total.hrs = sum(total.set.time),
            ave.daily.sets = mean(num.sets), min.daily.sets=min(num.sets),
            max.daily.sets = max(num.sets),ave.time.per.set=mean(mn.time.per.set,na.rm=T),
            tot.SK = sum(tot.SK),tot.CO = sum(tot.CO),tot.CH = sum(tot.CH, na.rm=T),
            tot.ST = sum(tot.ST),
            CPUEbyset.SK = mean(CPUEbyset.SK),CPUEbyset.CO = mean(CPUEbyset.CO),
            CPUEbyhr.SK = mean(CPUEbyhr.SK, na.rm=T),CPUEbyhr.CO = mean(CPUEbyhr.CO, na.rm=T))
effort.camp.byyr

# ggplot(effort.camp.byyr)+
#   geom_line(aes(x=sample_year,y=CPUEbyset.SK), col="black")+
#   geom_line(aes(x=sample_year,y=CPUEbyhr.SK), col="red")+
#   geom_line(aes(x=sample_year,y=CPUEbyset.CO), col="blue")+
#   geom_line(aes(x=sample_year,y=CPUEbyhr.CO), col="purple")+
#   labs(y="CPUE", title = "CO by set (blue), CO by hr (purple),
#        SK by set (black), SK by hr (red)")+
#   scale_x_continuous(breaks=seq(min(effort.camp.byyr$sample_year),
#                                 max(effort.camp.byyr$sample_year),1))




#### CPUE Dipnet ####

# look at number of unique fishman initials through the day

fishermen <- witset %>% 
  filter(year %in% yr.select,Location_Code %in% "Canyon", !is.na(Fisherman)) %>% 
  group_by(Sample_Date, year) %>% 
  summarize(num.fisher = length(unique(Fisherman)))

fishermen %>% 
  group_by(year) %>% 
  summarize(ave.num.fisher = mean(num.fisher))

# ggplot(fishermen)+
#   geom_bar(aes(x=yday(Sample_Date), y=num.fisher), stat="identity")+
#   facet_wrap(~year)
#looks pretty constant so not that helpful for effort


#### General Summary ####

#how many new records per year in tagging data?
print.data.frame(witset %>% 
                   group_by(year, Species) %>% 
                   summarize(total = length(Species)))

# make a summary table of last several years

table.summary <- witset %>% 
  #filter(year %in% yr.select) %>% 
  group_by(year, Location_Code) %>% 
  summarize(first=format(as.Date(min(Sample_Date)), format = "%d-%b"),
            last=format(as.Date(max(Sample_Date)), format = "%d-%b"),
            n.days = length(unique(Sample_Date)),
            totSK = length(which(Species %in% "SK")),
            totCO = length(which(Species %in% "CO")),
            totCH = length(which(Species %in% "CH")),
            totST = length(which(Species %in% "ST"))) 
table.summary


tag.table.summary <- witset %>% 
  #filter(year %in% yr.select) %>% 
  filter(!is.na(new.tag)) %>% 
  group_by(year, Location_Code) %>% 
  summarize(first.day.in.season=format(as.Date(min(Sample_Date)), format = "%d-%b"),
            last.day.in.season=format(as.Date(max(Sample_Date)), format = "%d-%b"),
            n.days.in.season = length(unique(Sample_Date)),
            taggedSK = length(which(Species %in% "SK")),
            taggedCO = length(which(Species %in% "CO")),
            taggedCH = length(which(Species %in% "CH")),
            taggedST = length(which(Species %in% "ST"))) 
# tag.table.summary %>% 
#   write_csv("tag.table.summary.csv")

# Note that these numbers do not match up with the Seine effort tallies. Compare diffs

tmp <- effort.camp.byyr %>% 
  left_join(table.summary, by = c("sample_year"="year","Location_Code")) %>% 
  select(sample_year, Location_Code, num.days.fished, n.days,effortSK = tot.SK, 
         tagSK = totSK, effortCO = tot.CO, tagCO = totCO, 
         effortCH = tot.CH, tagCH = totCH, effortST = tot.ST, tagST = totST 
         )

# It is mostly the CH at the seine that the tag data was no good for. Sub in this value

table.summary.re <- table.summary %>% 
  mutate(totCH = ifelse(year %in% 2023 & Location_Code %in% "Campground",
                        as.integer(effort.camp.byyr[effort.camp.byyr$sample_year %in% 2023,"tot.CH"]),
                        totCH))


# how many of each species per day in recent year?

witset.daily <- witset %>% 
  filter(year %in% yr.select) %>% 
  group_by(year, Species, Sample_Date, Location_Code) %>%
  summarize(total.daily = n()) 

(witset.daily.stats <- witset %>% 
    filter(year %in% yr.select) %>% 
    group_by(year, Species, Sample_Date) %>% 
    summarize(total.daily = n()) %>% 
    group_by(year, Species) %>% 
    reframe(min.daily = min(total.daily),
            mean.daily = mean(total.daily),
            max.daily = max(total.daily),
            peak.day = Sample_Date[which(total.daily %in% max.daily)]) )


# table of TagStatus #

table.tagstatus <- witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  group_by(Species, year) %>% 
  summarize(tot.A = length(which(TagStatus %in% "A")),
            tot.A2 = length(which(TagStatus %in% "A2")),
            tot.AR = length(which(TagStatus %in% "AR")),
            tot.R = length(which(TagStatus %in% "R")),
            tot.NA = length(which(TagStatus %in% "NA")),
            #tot.NA2 = length(which(is.na(TagStatus))),
            tot.Harvested = length(which(Harvested %in% T)))
table.tagstatus


#### QA/QC each year ####

# to add: check number of records by day and by species to see if days were double-entered
# what is the ave/longest time between tagging and recapture? TBD
# Carl looked at this with the BTSPAS groupings

# for AR, are these included as new fish when tag replaced? Exclude entirely?



yr.select.QA <- 2024


#species records missing dates:

witset.raw %>% 
  filter(is.na(Sample_Date), sample_year %in% yr.select.QA)
# fixed orphan records

# # look for weird tag colours 
# unique(witset.raw$AppliedColor)
# unique(witset.raw$Recaptured.Color) #fixed some of the really weird coho records
# print.data.frame(witset.raw[which(witset.raw$Recaptured.Color %in% "Blue"),c("Sample_Date","Location_Code",
#                                                               "Species","Sex",
#                                                               "Recaptured.number")])
# 
#from Carl

# #any missing year
# xtabs(~Sample_Id+Species, data=witset[ is.na(witset$year),], exclude=NULL, na.action=na.pass)
# 
# xtabs(~year+TagStatus, data=witset[witset$Species=="CH",], exclude=NULL, na.action=na.pass)
# 
# # location of application or capture- should be "Campground" or "Canyon"
# xtabs(~Species+Location_Code, data=witset, exclude=NULL, na.action=na.pass)
# 
# # tag status - should be A, A2, AR, NA, R
# xtabs(~Species+TagStatus, data=witset, exclude=NULL, na.action=na.pass)
# 
# # year
# xtabs(~Species+year, data=witset, exclude=NULL, na.action=na.pass)
# 
# xtabs(~TagStatus+year+Species, data=witset, exclude=NULL, na.action=na.pass)
# 
# xtabs(~TagStatus+Location_Code, data=witset, exclude=NULL, na.action=na.pass)
# xtabs(~TagStatus+AppliedCaudalPunch+Species, data=witset, exclude=NULL, na.action=na.pass)
# 
# xtabs(~AppliedCaudalPunch+year+Species, data=witset, exclude=NULL, na.action=na.pass)
# 
# xtabs(~AppliedCaudalPunch+year, data=witset, exclude=NULL, na.action=na.pass)
# 
# # Look like RemovedColor and RemovedTagNumber are never used
# xtabs(~RemovedTagNumber, data=witset, exclude=NULL, na.action=na.pass)
# xtabs(~RemovedTagColor,  data=witset, exclude=NULL, na.action=na.pass)
# witset$RemovedTagNumber <- NULL
# witset$RemovedTagColor  <- NULL
# 
# # check the double tagged fish.
# select <- witset$TagStatus=="A2"
# sum(select)
# temp <- witset[select, c("year","Sample_Date","Location_Code","Species","TagStatus","AppliedColor","AppliedTagNumber","AppliedTagNumber2","Recaptured.Color","Recaptured.number","Comments.x")]
# temp <- temp[ order(temp$Species, temp$Sample_Date),]
# temp
# 
# # check for applied and recapture number field both non-NA
# xtabs(~is.na(AppliedTagNumber)+is.na(Recaptured.number), data=witset, exclude=NULL, na.action=na.pass)
# # Lots of records with AppliedTagNumber and RecapturedTagNumber are both NA - these must be captures of 
# # fish that are not tagged.
# # A few records where both fields are given
# select <- !is.na(witset$AppliedTagNumber) & !is.na(witset$Recaptured.number)
# sum(select)
# (temp <- witset[select,])
# #write.csv(temp, file="both-applied-recap-numbers.csv", row.names=FALSE)
# 
# 
# ## Tag status of NA is typically missing because fish is harvested but not always
# ## If not harvested, with a tag status of NA and a tag number present, we change to NA
# witset$TagStatus[ witset$TagStatus == "NA"] <- NA
# xtabs(~TagStatus + Harvested, data=witset, exclude=NULL, na.action=na.pass)


# ## sometimes tag status is missing (or set to NA), not harvested, and a tag number recorded
# ## e.g. ST 58023
# select <- is.na(witset$TagStatus) & !witset$Harvested & !is.na(witset$AppliedTagNumber)
# sum(select)
# temp <- witset[select, c("year","Sample_Date","Location_Code","Species","TagStatus","AppliedColor","AppliedTagNumber","Recaptured.Color","Recaptured.number","Comments.x")]
# witset$TagStatus[select]<- "A"
# witset$QA_Comments.x[select ] <- paste0(witset$QA_Comments.x[select], ";Changed tag status to A")

# ## CH uses punches to record marking information
# xtabs(~Species+AppliedCaudalPunch, data=witset, exclude=NULL, na.action=na.pass)
# xtabs(~TagStatus+AppliedCaudalPunch, data=witset, exclude=NULL, na.action=na.pass)
# xtabs(~Species+AppliedCaudalPunch+year, data=witset[is.na(witset$TagStatus),], exclude=NULL, na.action=na.pass)




# look for and correct duplicate new tags within each species:
tmp <- witset %>% 
  filter(!is.na(new.tag), year %in% yr.select.QA) %>% 
  mutate(tag.sp.year = paste0(new.tag,Species,year)) %>% 
  filter(Species %in% c("CO","SK"))
(tmp2 <- tmp[duplicated(tmp$tag.sp.year),] %>% 
  select(Sample_Date, Location_Code, Species, Counter, new.tag) %>% 
  arrange(Location_Code,Sample_Date))
#2018 SK: did not do yet
#2018 CO: could not fix g-65567 and 65556
#2019 CO: 8 records, could not fix canyon records 
#2020 SK: y-92871:y-92875 cannot be fixed
#2020 CO: B-1159 and G-62323 to G-62327 not fixable 
#2021 SK: y-4901 duplicate not fixable, the rest fixed
#2021 CO: B-53760 duplicate not fixable, the rest fixed
#2022: were 19 dupes, resolved most except two CO with missing tag number
#2023: were 95 dupes, 9 CO not easily resolved: B-7032,7305,7274,7281*,7898,8119,8251,52747,52751 
#2024: were 32 dupes, 2 SK not easily resolved


#tool for searching tag #s 
# witset[grep("66501",x = witset$AppliedTagNumber),]

#difference between those with tag status A, A2 and AR vs a number in AppliedTag
witset %>% 
  filter(year %in% yr.select.QA, Species %in% c("CO","SK")) %>% 
  filter(TagStatus %in% c("A","A2","AR")) %>% 
  nrow()

witset %>% 
  filter(year %in% yr.select.QA, Species %in% c("CO","SK")) %>% 
  filter(!is.na(AppliedTagNumber)) %>% 
  nrow()

witset %>% 
  filter(year %in% yr.select.QA, Species %in% c("CO","SK")) %>% 
  filter(TagStatus %in% c("A","A2","AR")& is.na(AppliedTagNumber)) %>% 
  select(Sample_Date,Location_Code,Counter, Species, TagStatus,AppliedColor,
         AppliedTagNumber)

#none in 2018, 2019
# about 7 entries total > 2018. Reduced to 2
# 1 more in 2023
# 3 in 2024 - fixed


#difference between those with tag status R and AR vs 
# a number in RecapTagNumber and Color
witset %>% 
  filter(year %in% yr.select.QA, Species %in% c("CO","SK")) %>% 
  filter(TagStatus %in% c("R","AR")) %>% 
  nrow() #more tag status R/AR than recap field filled in for ~66 fish, 7 more in 2023

witset %>% 
  filter(year %in% yr.select.QA, Species %in% c("CO","SK")) %>% 
  filter(!is.na(Recaptured.Color)) %>% 
  nrow()

witset %>% 
  filter(year %in% yr.select.QA, Species %in% c("CO","SK")) %>% 
  filter(!is.na(Recaptured.number)) %>% 
  nrow()

witset %>% 
  filter(year %in% yr.select.QA, Species %in% c("CO","SK")) %>% 
  filter(!is.na(Recaptured.Color) & is.na(Recaptured.number)) %>% 
  select(Sample_Date,Location_Code,Counter, Species, TagStatus,Recaptured.Color,
         Recaptured.number)
# 10 fish where the tag number was not recorded but there is a tag colour.Fixed 5
# about 62 entries total, fixed some so now 46. 
# Legitimate to forget these though and just go with number,
# since most of them were batch-marked only when they should not have been and recorded
# inconsistently. This would mask some actual recaps but we are most interested in 
# tagged fish because they were for sure tagged and recaptured. Should mention this in 
# the methods though... In future, can use this R rate for tag loss rate, 
# and just use the recap tag number for the MR analysis
# in 2023 there was one SK missing tag number with tag color
# in 2024 they have many recap tag col missing, one not quickly resolvable 



# orphan recap tags (that were never applied) - this version using tag status 
uniq.applied.tag <- witset %>% 
  filter(year %in% yr.select) %>% 
  filter(!is.na(tag.yr.sp) & TagStatus %in% c("A","A2","AR")) %>%
  group_by(year) %>% 
  reframe(uniq.tag = unique(tag.yr.sp)) %>% 
  mutate(TagStatus = "A")

uniq.recap.tag <- witset %>% 
  filter(year %in% yr.select.QA) %>% 
  filter(!is.na(tag.yr.sp) & TagStatus %in% c("R")) %>% 
  group_by(year, Species) %>% 
  reframe(uniq.tag = unique(tag.yr.sp)) %>% 
  mutate(TagStatus = "R") %>% 
  left_join(uniq.applied.tag, by=c("year","uniq.tag")) %>% 
  filter(is.na(TagStatus.y))
uniq.recap.tag


#total orphan tags:
tot.recap.orphans <- uniq.recap.tag %>% 
  filter(Species %in% c("CO", "SK")) %>% 
  group_by(year, Species) %>% 
  summarize(orphan.recaps = length(Species))
tot.recap.orphans



#how many recaptures without a new tag record? This one using color-number combos
(new.recaps <- witset %>% 
    filter(year %in% yr.select,Species %in% c("CO","SK")) %>% 
    filter(!is.na(recap.tag), 
           !(recap.tag %in% new.tag) )  %>% 
  select(Sample_Date, Location_Code, Species, Counter, recap.tag, Sex, ForkLength) %>% 
  arrange(Species, Location_Code, Sample_Date))

# 2018: unsolved orphans: 59193,48052,6525,6506,65557
# 2019: all resolved
#there are 14 unresolvable recaps without tag applied in 2020
#there are 15 recaptures without a matching tag number in 2021
#canyon data not entered for SK on July 19th 2021- DONE
#need to fix database structure for A2 tags (so they don't get put in recaps)-DONE

#2020 CO: G-62576, B-45917, G-62502
#there are 10 recaptures without a matching tag number in 2021 CO
# most fixed, but B-53949 is an A2 (to fix)
# Could not solve remaining orphan recaps at campground in 2022 (4)
# Resolved CO canyon orphans in 2022 

#2023 31 unresolveables (some may be SK misID'd as CO or vice versa?:
#2023 SK: 4198, 9205 unresolvable,530,4528,4765,4040,4173,4194,4166
#2023 CO: 2164,6562,5446,4409,4468,4457,44100,4497,62228,3423,2354,424,3919

#2024 CO: 6 tags not resolvable, 4 from likely missing pages
#2024 SK: 4 tags not resolvable, 1 likely from missing page.


#how many new tag numbers with no colour?
witset %>% 
  filter(!is.na(AppliedTagNumber),is.na(AppliedColor), year %in% yr.select.QA,
         Species %in% c("CO","SK")) %>% 
  select(Sample_Date, Counter, new.tag, Species, Location_Code)
# 1 tag with no colour, just a number in 2020 - unfixable
#77 missing colour of tag, just 1 in 2021-fixed, 
#had three (2 CO, 1ST)-all fixed. Just 5 between CH and ST
# 2023 none
# 2024 none

#how many new tag colours with no tag number?
(missing.applied.tagnum <- witset %>% 
  filter(!is.na(AppliedColor),is.na(AppliedTagNumber), year %in% yr.select,Species %in% c("CO","SK")) %>% 
  select(year, Sample_Date, Counter, new.tag, Species, Location_Code) %>% 
  group_by(year, Species) %>% 
  summarize(no.applied.tag = length(Species)))

# 2 tags with no number in 2018
# 1 tag with no number, just a colour in 2020
#160 tags with no number, just a colour in 2021
# 4 tags with no number in 2022, could only fix 2
# 3 tags with no number in 2023, could fix 2, 1 SK remaining
# 5 SK tags in 2024 all fixed

#how many recap tag colours with no tag number?
(missing.recap.tagnum <- witset %>% 
  filter(!is.na(Recaptured.Color),is.na(Recaptured.number), year %in% yr.select,Species %in% c("CO","SK")) %>% 
  select(year, Sample_Date, Counter, recap.tag, Species, Location_Code) %>% 
  group_by(year, Species) %>% 
  summarize(no.recap.tag = length(Species)))

#none in 2020
#160 tags with no number, just a colour; 
# 3 in 2021, all b/c recap tag number doesn't exist
# 1 in 2022, unfixable
# 6 in 2023, fixed 4, 2 remaining
# 1 SK in 2024, fixed

#how many recap tag numbers with no colour?
witset %>% 
  filter(!is.na(Recaptured.number),is.na(Recaptured.Color), year %in% yr.select.QA,Species %in% c("CO","SK")) %>% 
  select(year ,Sample_Date, Species, Counter, recap.tag)
#none in 2018
#one in 2019
#none in 2020
#none in 2021
#none in 2022
#5 CO in 2023. I could add blue here but the tag numbers don't make sense either
# 13 in 2024, all but one fixed


#Check if the tag status is recorded incorrectly:
#looking for 1. Tagstatus A, A2, AR does not have col or # in applied columns
#     2. TagStatus AR, R does not have col or # in recap cols
#     3. TagStatus A, AR, A2 with harvested checked.

no.tag.number <- witset %>% 
    filter(year %in% yr.select.QA) %>% 
    filter(Species %in% c("CO","SK")) %>% 
    mutate(bad.tagstatus.applied = ifelse(TagStatus %in% c("A","A2","AR")&
           is.na(AppliedTagNumber)&is.na(AppliedColor), "bad",NA)) %>% 
    mutate(bad.tagstatus.recap = ifelse(TagStatus %in% c("R","AR")&
            is.na(Recaptured.number)&is.na(Recaptured.Color), "bad",NA)) %>% 
    mutate(bad.tagstatus.harvest = ifelse(TagStatus %in% c("A","A2","AR")&
          Harvested %in% TRUE, "bad",NA)) %>% 
    select(year,Sample_Date, Counter,Species,TagStatus,ForkLength,
           bad.tagstatus.applied,bad.tagstatus.recap,
           bad.tagstatus.harvest) %>% 
  filter(!is.na(bad.tagstatus.recap)) %>% 
    group_by(year, Species) %>% 
    summarize(bad.tagstatus.applied=length(which(!is.na(bad.tagstatus.applied))),
              bad.tagstatus.recap=length(which(!is.na(bad.tagstatus.recap))),
              bad.tagstatus.harvest=length(which(!is.na(bad.tagstatus.harvest))))
no.tag.number
#49 cases where tag status was recorded as A, A2, or AR but no tag number 
# or colour. Just 1 in 2021- fixed. None in 2020, 2022 for CO, SK
# Most issues are with Recap tag number missing. 
#2023 some of these unresolved for SK - one a fumble fish, another a tag loss that wasn't retagged
#   CO - 3 lost tags, 2 re-applied (ARs) and 1 not
#2019 last 5 not resolveable
#2024 







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
  reframe(uniq.tags=na.omit(unique(AppliedTagNumber))) %>% #removed NAs
  mutate(uniq.yrtags = paste0(year,Species,uniq.tags)) 

newtags.canyon <- witset %>% 
  filter(Location_Code %in% "Canyon") %>%
  group_by(year, Species) %>% 
  reframe(uniq.tags=na.omit(unique(AppliedTagNumber))) %>% #removed NAs
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
  mutate(uniq.yrtags = paste0(year,Species,Recaptured.number)) %>% 
  filter(uniq.yrtags %in% newtags.canyon$uniq.yrtags) 


#how many total fallbacks?
(fallback <- fallback.fish %>%   
    group_by(year, Species) %>% 
    summarize(canyon.fallback = length(Species)))

#how many fallbacks were again recaptured at the canyon?
(fallbacks.recap.canyon <- witset %>% 
    filter(Location_Code %in% "Canyon") %>% 
    filter(TagStatus %in% c("AR","R")) %>% 
    mutate(uniq.yrtags = paste0(year,Species,Recaptured.number)) %>% 
    filter(uniq.yrtags %in% fallback.fish$uniq.yrtags) %>% 
    group_by(year, Species) %>% 
    summarize(fallbacks.recap.canyon= length(Species)))

#how many fish were marked at campground and recapped at canyon?
# This could be considered the expected percentage return for fallback fish
camptocanyon.recaps <- witset %>% 
  filter(Location_Code %in% "Canyon") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  mutate(uniq.yrtags = paste0(year,Species,Recaptured.number)) %>% 
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
  geom_line(aes(x=year, y=percent.canyon.fallback, col=Species), linewidth= 1.5)+
  scale_y_continuous(breaks=seq(floor(min(table.fallback$percent.canyon.fallback)),
                                max(table.fallback$percent.canyon.fallback),1))+
  scale_x_continuous(breaks=seq(min(table.fallback$year),
                                max(table.fallback$year),1))+
  labs(x="Year",y="% Fallback from Canyon to Campground", 
       size = "# recaptured \nfallback")
plot.fallback


plot.fallbackreascend <- ggplot(table.fallback)+
  geom_line(aes(x=year, y=percent.fallbacks.reascend, col=Species), linewidth=1)+
  geom_point(aes(x=year, y=percent.fallbacks.reascend, col=Species, 
                 size=fallbacks.recap.canyon),alpha = 0.5) +
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
  filter(!is.na(Recaptured.number), Species %in% c("SK","CO","ST")) %>% 
  mutate(uniq.yrtags = paste0(year,Species,Recaptured.number)) %>% 
  arrange(Sample_Date) %>% 
  group_by(year, Species,uniq.yrtags, Location_Code) %>% 
  summarize(number.recaps = length(uniq.yrtags), 
            date.first = first(Sample_Date), date.last=last(Sample_Date)) %>% 
  filter(number.recaps >1) %>% 
  arrange(year,Species,desc(number.recaps))

table(repeat.captures[,c("year", "Species", "number.recaps")])

repeat.capturesX <- witset %>% 
  filter(!is.na(Recaptured.number), Species %in% c("SK","CO"),
         year %in% yr.select) %>% 
  mutate(uniq.yrtags = paste0(year,Species,Recaptured.number)) %>% 
  arrange(Sample_Date) %>% 
  group_by(year, Species, uniq.yrtags, Location_Code) %>% 
  summarize(number.recaps = length(uniq.yrtags), Harvested = isTRUE(Harvested),
            date.first = first(Sample_Date), date.last=last(Sample_Date)) 
repeat.capturesX %>% 
  filter(Harvested %in% T) %>% 
  arrange(desc(year))


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

#### caudalpunch only fish ###

# how do these look in the data?
# These fish are generally smaller than average so are un-tagged, 
# have a TagStatus of NA
# and the field of ReCapturedCaudalPunch is filled in
# (which used to just be one field of CaudalPunch until the 2023 db upgrade)

witset %>% 
  filter(year %in% yr.select, Species %in% c("SK","CO")) %>% 
  filter(TagStatus %in% "NA" & !is.na(RecapturedCaudalPunch)) %>% 
  group_by(year, Species) %>% 
  summarize(n.caudalpunchonly = length(TagStatus), 
            mn.FL = mean(ForkLength, na.rm=T), sd.FL= sd(ForkLength, na.rm=T))

caudalpunchonly <- witset %>% 
  filter(year %in% yr.select, Species %in% c("SK","CO")) %>% 
  filter(TagStatus %in% "NA" & (!is.na(RecapturedCaudalPunch)|!is.na(AppliedCaudalPunch))) %>% 
  group_by(year, Species) %>% 
  summarize(n.NA = length(year), 
            mn.FL.caudalonly = mean(ForkLength, na.rm=T), 
            sd.FL.caudalonly= sd(ForkLength, na.rm=T))

witset %>% 
  filter(TagStatus %in% c("A","A2","AR")) %>% 
  group_by(year) %>% 
  summarize(n.A= length(TagStatus), mn.FL= mean(ForkLength, na.rm=T)) %>% 
  left_join(caudalpunchonly)

# ###
#### Nanika data ####
# ###

nanikaswim <- read_excel("NanikaSnorkel.xlsx",.name_repair = "universal") %>% 
  select(year=Year, nanika.counted=total.sockeye.counted,
         nanika.tags=total.tags.observed) %>% 
  mutate(Species = "SK")

unique(nanikaswim$year)

nanika.aerial.peak <- read_excel("moricetown sockeye tagging estimates_v3-copy-5-Feb-2025.xlsx",
                                 sheet="SKCompareEstimates", .name_repair = "universal")
str(nanika.aerial.peak)


#from CS scripts
n.counts <- nanikaswim %>% 
  filter(year %in% yr.select) #KP addition

head(n.counts)



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
            released.w.tag= length(which(TagStatus %in% c("A"))),
            recapped= length(which(TagStatus %in% c("AR","R")))) 
SKbylocation



# # # # # # # # # # # #
#### SK Estimates ####
# # # # # # # # # # # #

#get closed LP estimate (with Chapman mod) from fish tagged at
#the campground then recaptured at the canyon. Assume no tags lost
# (i.e. no fallback below study area)


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
  

#LP estimate for campground -> canyon for Sockeye

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
  mutate(CI95Chap = seChap*1.965) %>% 
  mutate(Chap = ifelse(recapped >= 20, Chap, NA)) #only include those years with a reasonable number of raps
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
    mutate(fake.date = as_date(yday(Sample_Date)-1, 
                               origin = ymd("2023-01-01")))) 

totalcaughtSK <- SKbylocation %>% 
  group_by(year) %>% 
  summarize(totalcaught = sum(totalcaught),
            totalharvested = sum(harvested),
            totalreleased = sum(released.w.tag)) %>% 
  filter(year %in% yr.select)

plot.SK.daily <- ggplot(witsetSKrecent)+
  geom_line(aes(x=as_date(fake.date), y=n.SK))+
  geom_text(data = totalcaughtSK, aes(x=ymd("2023-09-10"), y=500,
                                      label=paste0("total caught=",
                                                   totalcaught)))+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d")+
  facet_wrap(~year)+
  labs(title = "Daily catch of sockeye Campground+Canyon (2018-2024)", y="daily catch of sockeye",
       x="date")+
  theme(axis.text.x = element_text(angle=45, hjust=1))
plot.SK.daily

#ggsave(plot = plot.SK.daily, filename = "dailySKcatch2018-2023.png",
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
    mutate(fake.date = as_date(yday(Sample_Date)-1, 
                               origin = ymd("2023-01-01")))) 

totalcaughtCO <- CObylocation %>% 
  group_by(year) %>% 
  summarize(totalcaught = sum(totalcaught),
            totalharvested = sum(harvested),
            totalnewtags = sum(newtags)) %>% 
  filter(year %in% yr.select)

plot.CO.daily <- ggplot(witsetCOrecent)+
  geom_line(aes(x=as_date(fake.date), y=n.CO))+
  geom_text(data = totalcaughtCO, aes(x=ymd("2023-10-08"), y=350,
                                      label=paste0("total caught=\n",totalcaught)))+
  facet_wrap(~year)+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d")+
  labs(title = "Daily catch of coho at Campground+Canyon (2018-2023)", y="daily catch of coho",
       x="date")+
  theme(axis.text.x = element_text(angle=45, hjust=1))
plot.CO.daily

#ggsave(plot = plot.CO.daily, filename = "dailyCOcatch2018-2023.png",
#       height = 4, width = 6,device = "png")



#### Toboggan data ####



nms <- names(read_excel("TobogganFenceData_MASTER-copy26-Feb-2025.xlsx", 
                        sheet = "IndividualFish",n_max = 0))
ct <- ifelse(grepl("^poh_length_mm", nms)|grepl("^total", nms)|grepl("^recap_tag_number", nms), "numeric",
             ifelse(grepl("^recap_caudal_punch",nms),"text",
                    ifelse(grepl("^fumble_fish",nms),"logical","guess")))


tobog.raw <- read_excel("TobogganFenceData_MASTER-copy26-Feb-2025.xlsx",
                    sheet = "IndividualFish", col_types = ct) %>% 
  filter(species %in% "co") %>% 
  mutate(date = as_date(date), julian = yday(date), 
         appr.date = as_date(julian, origin = ymd("2025-01-01"))) %>% 
  mutate(species = toupper(species)) %>% 
  mutate(total = ifelse(is.na(total), 1, total)) %>% #could be corrected in database
  mutate(FL = as.numeric(fork_length_mm)/10) %>% 
  mutate(POH = as.numeric(poh_length_mm)/10) %>% 
  mutate(gender = case_when(mark_gender %in% c("wf","af")~"F",
                            mark_gender %in% c("wm","am","wj","aj")~"M")) %>% 
  mutate(recap_tag_colour = tolower(recap_tag_colour)) %>% 
  mutate(witset.taglost = ifelse(recap_caudal_punch %in% c("top","bottom","yes")&
                                   (is.na(recap_tag_colour)&is.na(recap_tag_number)),T,F)) %>% #could be added to tobog database as separate field
  mutate(total.hatchery = ifelse(mark_gender %in% c("af","am","aj"), total,NA)) %>% 
  mutate(total.wild = ifelse(mark_gender %in% c("wf","wm","wj"), total,NA)) %>% 
  #mutate(recap_tag_colour = ifelse(tag_origin_code %in% "drake",NA,recap_tag_colour)) %>% #figured out these were just re-used tags from Drake
  #mutate(recap_tag_number = ifelse(tag_origin_code %in% "drake",NA,recap_tag_number)) %>% 
  mutate(remove = ifelse(recap_tag_colour %in% "orange" & year %in% c(2022,2024),T,
                         ifelse(recap_tag_colour %in% "pink" & year %in% 2020,T,NA))) %>% 
  filter(is.na(remove))#this last filter to exclude rows of fish that were tagged, flushed and then were recaptured.
           


# What is a recap from toboggan?:
# added tag colours back to 2015 

tobog.raw %>% 
  group_by(year) %>% 
  summarize(total.tag.col = length(which(recap_tag_colour %in% c("blue","green","yellow"))),
            total.tag.num = length(which(!is.na(recap_tag_number))),
            total.fumbles = length(which(fumble_fish %in% T))) %>% 
  mutate(calc.fumbles = total.tag.col - total.tag.num)

tobog <- tobog.raw %>%
  mutate(origin = ifelse(recap_tag_colour %in% c("blue","green","yellow"), 1,NA)) %>%
  mutate(recap_tag_number = ifelse(origin %in% 1, as.numeric(recap_tag_number),NA)) %>%
  filter(recap_tag_colour %in% c("blue","green","yellow",NA))


str(tobog)

tobog.yearly.sum <- read_excel("Toboggan-YearSummaries-copy24-Dec-2024.xlsx",
                               sheet="TobogganFence")
str(tobog.yearly.sum)


### QAQC Toboggan Data ####

#to do: remove orange tag recaptures at the fence, since these had
# a tag applied and somehow got downstream to come up again. 
# So they should not be counted again. - DONE

yr.select.QA

# TBC tag recaps to remove from recap list
# also remove similar untagged component? And why is 2024 different?


tobog %>% 
  group_by(year) %>% 
  summarize(total.tbctagsapplied = length(which(!is.na(tob_tag))),
            recaps.tbctags = length(which(recap_tag_colour %in% c("orange","pink"))),
            #percent.tbctags.recapped = recaps.tbctags/total.tbctagsapplied*100,
            recaps.witset = length(which(!is.na(origin))),
            total.notrecaps = length(which(recap_tag_colour %in% c(NA))))
#TBC tag recaps should now be zero

# summary table of catch:
(table.tobog <- tobog %>% 
  filter(year %in% yr.select) %>% 
  group_by(year) %>% 
  summarize(totalcaught = sum(total),
            witset.tagged = sum(origin, na.rm=T),
            min.tagloss = sum(witset.taglost, na.rm=T),
            hatchery.origin = sum(total.hatchery, na.rm=T),
            wild.origin = sum(total.wild, na.rm=T) ))




# daily toboggan catch:

tobog.daily <- tobog %>% 
  filter(year %in% c(yr.select.QA)) %>% 
  group_by(year, date, appr.date) %>% 
  summarize(total.CO = sum(total),
            total.CO.newtobtag = length(which(!is.na(tob_tag))),
            total.CO.recaptobtag = length(which(recap_tag_colour %in% "orange"))) %>% #removed above.
  pivot_longer(cols=!c(year, date, appr.date), 
               names_to = "tag.status",
               values_to = "total")

ggplot(tobog.daily)+
  geom_bar(aes(x=date, y=total, fill=tag.status), stat="identity", position="dodge")+
  facet_wrap(~year)



# From CS


unique(tobog$recap_tag_colour)

t.data <- plyr::rename(tobog, c("year"="year",
                                 "date"="date",
                                 "species"="Species",
                                 "recap_tag_number"="RecapturedTagNumber")) %>% 
  filter(year %in% yr.select)

t.tagrecoveries <- t.data %>% 
  filter(!is.na(RecapturedTagNumber), #in this method, only those fish with tag numbers are recaps, excludes fumble fish
         Species %in% c("CO"),
         origin %in% 1) %>% 
  select(date,year,Species,RecapturedTagNumber)

xtabs(~Species+year, data=t.tagrecoveries, 
      exclude=NULL, na.action=na.pass)

# Extract total fish handled
t.markedunmarked.fish <- t.data %>% 
  filter(Species %in% c("CO")) %>% 
  select(c("date","year","Species","total","RecapturedTagNumber"))

t.markedunmarked <- plyr::ddply(t.markedunmarked.fish, c("date","year","Species"), 
                                    plyr::summarize,
                                    total.coho=sum(total,na.rm=T), #total coho captured by day
                                    total.marked=sum(!is.na(RecapturedTagNumber))) #total # of recovered tags in a day (excludes tag losses, or col only)

#### tag loss ####
#as of 2023 was more explicitly part of protocol for a subset of fish

(tag.loss <- tobog %>% 
   filter(year %in% 2023:2024) %>% 
   group_by(year) %>% 
   summarize(total.inspected = length(which(!is.na(recap_caudal_punch))),
             total.fish = sum(total),
             min.tag.lost = sum(witset.taglost),
             min.tag.loss.perc = min.tag.lost/total.inspected,
             expanded.tag.loss = min.tag.loss.perc*total.fish,
             witset.recaps = sum(origin, na.rm=T),
             tag.loss.perc = expanded.tag.loss/(witset.recaps+expanded.tag.loss)))
tag.loss$tag.loss.perc*100


# check FL of the Toboggan recaps and the FLs from Witset on measurements of the 
# same fish. Only really started being part of protocol in 2023/2024
(forklengths <- tobog %>% 
  filter(year %in% 2023:2024, !is.na(recap_tag_number)) %>% 
  select(recap_tag_number, mark_gender, FL, date) %>% 
  left_join(witset[witset$year %in% yr.select.QA,], 
            by = c("recap_tag_number"="AppliedTagNumber")) %>% 
  mutate(FLdiff = abs(FL-ForkLength)))
  
ggplot(forklengths)+
  geom_point(aes(x=ForkLength, y=FL, col=Location_Code))+
  geom_smooth(aes(x=ForkLength, y=FL), method = "lm")+
  geom_abline(intercept = 0, slope = 1)+
  labs(x="Witset FL (cm)", y= "Tobog FL (cm)", col="tag origin")

#TBC fish measurements consistently larger than Witset's. Which may make sense,
# given the development time between the two locations. Too bad everything is FL, not POH!

str(tobog)

ggplot(tobog)+
  geom_point(aes(x=FL, y=POH, col=gender))+
  geom_smooth(aes(x=FL, y=POH), method="lm")+
  labs(title="Toboggan FL to POH (cm)")


# are Witset recaps size representative of TBC run? 
ggplot(tobog[tobog$year %in% c(2023,2024),])+
  geom_histogram(aes(x=FL, fill=!is.na(recap_tag_number)))+
  facet_wrap(~c(year))+
  labs(fill = "Is a recap?")




# are there orphaned recap tags at TBC?
tobog.tags <- tobog %>% 
  filter(!is.na(recap_tag_number), year %in% yr.select.QA) %>% 
  select(order,date,recap_tag_colour,recap_tag_number) %>% 
  left_join(witset %>% 
              filter(Species %in% "CO") %>% 
              filter(year %in% yr.select.QA),
            by = c("recap_tag_number"="AppliedTagNumber")) 
(tobog.tags  %>% 
  filter(is.na(Species)) %>% 
    arrange(recap_tag_number))

#18 recovered tags at toboggan between 2018 and 2021 have no match at witset
#update: back to 2014, there are now 77 tags from Toboggan that are not in Witset DB
# fixed back to 2018 of these so far, 58 remain from 2016 and further backward
#2023: 10 tags that are not in witset db, fixed 1, the others come from missing data sheets
# at Witset
#2024: 14 outstanding tags not matched, all from likely missing pages at Witset




#### Filling Missing Witset Tags ####

# make up tag data for CO Sept 22 Canyon and Sep 27th Canyon in 2023,
# also missing applied tag series from July 4 - SK y-4166 to y-4198 in 2023:

# make up missing tag series from 2024:
# likely missing CO tag series: Aug 19: b-46230 to b-46250 (min), b-16801 to 16825 (min)
# and Sept 4: b-17401 to 17425
# and Sept 18: b-18586 to 18650
#full day missing Aug 19, 2024


#add missing applied tags
missing.Atags2023 <- data.frame(matrix(ncol = ncol(witset),
                                   nrow = length(c(52663:52705,52795:52829))))
names(missing.Atags2023) = names(witset)
witset.missing.tags2023 <-  missing.Atags2023 %>% 
                        mutate(Sample_Id = 9999, Species = "CO",
                         year = 2023, TagStatus = "A", AppliedColor= "Blue", 
                         AppliedCaudalPunch = "T",
                         AppliedTagNumber = c(52663:52705,52795:52829), 
                         Harvested = FALSE,tag.yr.sp = paste0(AppliedTagNumber,".",year,Species),
                         new.tag = paste0("b","-",AppliedTagNumber),
                         sample_year = 2023,Location_Code = "Canyon",
                         Sample_Date = as_date(ifelse(AppliedTagNumber<=52705,
                                              ymd("2023-09-22"),
                                              ymd("2023-09-27"))),
                         AppliedTagNumberPresent = T, Year.Species = paste0(year,Species),
                         myTagColor = AppliedColor, myTagNumber = AppliedTagNumber,
                         ISOweek = isoweek(Sample_Date),SYT = paste0(Species,year,myTagNumber))

missing.Atags2024 <- data.frame(matrix(ncol = ncol(witset),
                                   nrow = length(c(46230:46250,16801:16825,
                                                   17401:17425,18586:18650))))
names(missing.Atags2024) = names(witset)
witset.missing.tags2024 <-  missing.Atags2024 %>% 
  mutate(Sample_Id = 9999, Species = "CO",
         year = 2024, TagStatus = "A", AppliedColor= "Blue", 
         AppliedCaudalPunch = "B",
         AppliedTagNumber = c(46230:46250,16801:16825,17401:17425,18586:18650), 
         Harvested = FALSE,tag.yr.sp = paste0(AppliedTagNumber,".",year,Species),
         new.tag = paste0("b","-",AppliedTagNumber),
         sample_year = 2024,Location_Code = "Canyon",
         Sample_Date = as_date(ifelse(AppliedTagNumber %in% c(46230:46250,16801:16825),ymd("2024-08-19"),
                                      ifelse(AppliedTagNumber %in% c(17401:17425),ymd("2024-09-04"),
                                             ifelse(AppliedTagNumber %in% c(17401:17425),ymd("2024-09-18"), NA)))),
         AppliedTagNumberPresent = T, Year.Species = paste0(year,Species),
         myTagColor = AppliedColor, myTagNumber = AppliedTagNumber,
         ISOweek = isoweek(Sample_Date),SYT = paste0(Species,year,myTagNumber))

#combine: 
witset.missing.tagsCO <- rbind(witset.missing.tags2023,witset.missing.tags2024)


#add missing applied tags for SK 2023

missing.AtagsSK <- data.frame(matrix(ncol = ncol(witset),nrow = length(c(4166:4198))))
names(missing.AtagsSK) = names(witset)
witset.missing.tagsSK <-  missing.AtagsSK %>% 
  mutate(Sample_Id = 9999, Species = "SK",
         year = 2023, TagStatus = "A", AppliedColor= "Yellow", 
         AppliedCaudalPunch = "B",
         AppliedTagNumber = c(4166:4198), 
         Harvested = FALSE,tag.yr.sp = paste0(AppliedTagNumber,".",year,Species),
         new.tag = paste0("y","-",AppliedTagNumber),
         sample_year = 2023,Location_Code = "Campground",
         Sample_Date = as_date(ymd("2023-07-04")),
         AppliedTagNumberPresent = T, Year.Species = paste0(year,Species),
         myTagColor = AppliedColor, myTagNumber = AppliedTagNumber,
         ISOweek = isoweek(Sample_Date),SYT = paste0(Species,year,myTagNumber))

#Since these are whole pages lost, 
# calc ave time between tagging and recapture around this time
# CURRENTLY NOT USED, just adding known A tags for now

# x2023 <- witset %>% 
#   filter(Species %in% "CO", year %in% 2023, 
#          Location_Code %in% "Canyon",!is.na(recap.tag)) %>% 
#   select(Sample_Date_recap=Sample_Date,recap.tag) %>% 
#   left_join(witset[witset$year %in% 2023,c("Sample_Date","new.tag")], by=c("recap.tag"="new.tag")) %>% 
#   mutate(time.between = difftime(Sample_Date_recap,Sample_Date, units = "days")) %>% 
#   mutate(Sample_Date_recap = as_date(Sample_Date_recap)) %>% 
#   filter(Sample_Date_recap %in% seq(ymd("2023-09-15"),ymd("2023-10-15"),1)) %>% 
#   summarize(ave.time.between = mean(time.between, na.rm=T),
#             sd.time.between = sd(time.between,na.rm=T),
#             min.time.between = min(time.between,na.rm=T),
#             max.time.between = max(time.between,na.rm=T))
# x2023  
# 
# x2024 <- witset %>% 
#   filter(Species %in% "CO", year %in% 2024, 
#          Location_Code %in% "Campground",!is.na(recap.tag)) %>% 
#   select(Sample_Date_recap=Sample_Date,recap.tag) %>% 
#   left_join(witset[witset$year %in% 2024,c("Sample_Date","new.tag")], by=c("recap.tag"="new.tag")) %>% 
#   mutate(time.between = difftime(Sample_Date_recap,Sample_Date, units = "days")) %>% 
#   mutate(Sample_Date_recap = as_date(Sample_Date_recap)) %>% 
#   filter(Sample_Date_recap %in% seq(ymd("2024-08-19"),ymd("2024-09-18"),1)) %>% 
#   summarize(ave.time.between = mean(time.between, na.rm=T),
#             sd.time.between = sd(time.between,na.rm=T),
#             min.time.between = min(time.between,na.rm=T),
#             max.time.between = max(time.between,na.rm=T))
# x2024  
# 
# 
# 
# #calc ave tags recaptured/ fish released or harvested around this time
# total.tags.by.day23 <- witset %>% 
#   filter(Species %in% "CO", year %in% 2023, Location_Code %in% "Canyon" ) %>% 
#   mutate(Sample_Date = as_date(Sample_Date)) %>% 
#   group_by(Sample_Date,Location_Code) %>% 
#   summarize(tot.NAper.day = length(which(is.na(TagStatus)&Harvested %in% F)),
#             tot.Harvestper.day = length(which(Harvested %in% T)),
#             tot.Rper.day = length(which(TagStatus %in% c("R")))) 
# total.tags.by.day24 <- witset %>% 
#   filter(Species %in% "CO", year %in% 2024, Location_Code %in% "Campground" ) %>% 
#   mutate(Sample_Date = as_date(Sample_Date)) %>% 
#   group_by(Sample_Date,Location_Code) %>% 
#   summarize(tot.NAper.day = length(which(is.na(TagStatus)&Harvested %in% F)),
#             tot.Harvestper.day = length(which(Harvested %in% T)),
#             tot.Rper.day = length(which(TagStatus %in% c("R")))) 
# 
# 
# # use Sept 20+21st 2023 ave numbers to predict for Sept 22nd 2023
# total.tags.by.day22nd <- total.tags.by.day23 %>% 
#   filter(Sample_Date %in% seq(ymd("2023-09-20"),ymd("2023-09-21"),1)) %>% 
#   group_by() %>% 
#   summarize(tot.NAper.day = round(mean(tot.NAper.day),0),
#             tot.Harvestper.day = round(mean(tot.Harvestper.day),0),
#             tot.Rper.day = round(mean(tot.Rper.day),0)) %>% 
#   mutate(Sample_Date = ymd("2023-09-22"), 
#          start.sample = Sample_Date-x$ave.time.between-x$sd.time.between,
#          end.sample = Sample_Date)
# 
# total.tags.by.day22nd
# 
# #use Sept 26, and Oct 3rd for later part of Sept 27th 2023
# total.tags.by.day27th <- total.tags.by.day23 %>% 
#   filter(Sample_Date %in% seq(ymd("2023-09-26"),ymd("2023-10-03"),1)) %>% 
#   group_by() %>% 
#   summarize(tot.NAper.day = round(mean(tot.NAper.day),0),
#             tot.Harvestper.day = round(mean(tot.Harvestper.day),0),
#             tot.Rper.day = round(mean(tot.Rper.day),0)) %>% 
#   mutate(Sample_Date = ymd("2023-09-27"), 
#          start.sample = Sample_Date-x$ave.time.between-x$sd.time.between,
#          end.sample = Sample_Date)
# 
# total.tags.by.day27th
# 
# #use Aug 16 and 20th to predict for Aug 19th 2024
# total.tags.by.day19th <- total.tags.by.day24 %>% 
#   filter(Sample_Date %in% seq(ymd("2024-08-16"),ymd("2024-08-20"),1)) %>% 
#   group_by() %>% 
#   summarize(tot.NAper.day = round(mean(tot.NAper.day),0),
#             tot.Harvestper.day = round(mean(tot.Harvestper.day),0),
#             tot.Rper.day = round(mean(tot.Rper.day),0)) %>% 
#   mutate(Sample_Date = ymd("2024-08-19"), 
#          start.sample = Sample_Date-x$ave.time.between-x$sd.time.between,
#          end.sample = Sample_Date)
# 
# total.tags.by.day19th
# 
# 
# 
# #add a random sample of recap tags, using ave time between applied and recaps
# 
# #Sept 22nd:
# 
# sample.recaps22nd <- witset %>% 
#   filter(Species %in% "CO", 
#          Sample_Date %in% seq(total.tags.by.day22nd$start.sample,
#                               total.tags.by.day22nd$end.sample,1),
#          !is.na(AppliedTagNumber)) %>% 
#   select(AppliedTagNumber) 
# 
# sample.recaps22nd <- 
#   sample(sample.recaps22nd$AppliedTagNumber,total.tags.by.day22nd$tot.Rper.day)
# 
# missing.Rtags1 <- data.frame(matrix(ncol = ncol(witset),nrow = total.tags.by.day22nd$tot.Rper.day))
# names(missing.Rtags1) = names(witset)
# 
# witset.missing.tags <- rbind(witset.missing.tags,missing.Rtags1 %>% 
#                        mutate(Sample_Id = 9999, Species = "CO",
#                               year = 2023, TagStatus = "R", Recaptured.Color= "Blue", 
#                               RecapturedCaudalPunch = "T",
#                               Recaptured.number = sample.recaps22nd, 
#                               Harvested = FALSE,tag.yr.sp = paste0(Recaptured.number,".",year,Species),
#                               recap.tag = paste0("b","-",Recaptured.number),
#                               sample_year = 2023,Location_Code = "Canyon",
#                               Sample_Date = as_date(ymd("2023-09-22"))))
# 
# #Sept 27th:
# 
# sample.recaps27th <- witset %>% 
#   filter(Species %in% "CO", 
#          Sample_Date %in% seq(total.tags.by.day27th$start.sample,
#                               total.tags.by.day27th$end.sample,1),
#          !is.na(AppliedTagNumber)) %>% 
#   select(AppliedTagNumber) 
# 
# sample.recaps27th <- 
#   sample(sample.recaps27th$AppliedTagNumber,total.tags.by.day27th$tot.Rper.day)
# 
# missing.Rtags2 <- data.frame(matrix(ncol = ncol(witset),nrow = total.tags.by.day27th$tot.Rper.day))
# names(missing.Rtags2) = names(witset)
# 
# missing.NAtags2 <- data.frame(matrix(ncol = ncol(witset),nrow = total.tags.by.day27th$tot.NAper.day))
# names(missing.NAtags2) = names(witset)
# missing.NAtags2 <- missing.NAtags2 %>% 
#   mutate(Sample_Id = 9999, Species = "CO",
#          year = 2023, TagStatus = NA, 
#          Harvested = FALSE,tag.yr.sp = paste0(Recaptured.number,".",year,Species),
#          sample_year = 2023,Location_Code = "Canyon",
#          Sample_Date = as_date(ymd("2023-09-27")))
# 
# witset.missing.tags <- rbind(witset.missing.tags, missing.NAtags2,missing.Rtags2 %>% 
#                        mutate(Sample_Id = 9999, Species = "CO",
#                               year = 2023, TagStatus = "R", Recaptured.Color= "Blue", 
#                               RecapturedCaudalPunch = "T",
#                               Recaptured.number = sample.recaps27th, 
#                               Harvested = FALSE,tag.yr.sp = paste0(Recaptured.number,".",year,Species),
#                               recap.tag = paste0("b","-",Recaptured.number),
#                               sample_year = 2023,Location_Code = "Canyon",
#                               Sample_Date = as_date(ymd("2023-09-27"))))
# 
# # for Aug 19th 2024
# 
# sample.recaps19th <- witset %>% 
#   filter(Species %in% "CO", 
#          Sample_Date %in% seq(total.tags.by.day19th$start.sample,
#                               total.tags.by.day19th$end.sample,1),
#          !is.na(AppliedTagNumber)) %>% 
#   select(AppliedTagNumber) 
# 
# sample.recaps19th <- 
#   sample(sample.recaps19th$AppliedTagNumber,total.tags.by.day19th$tot.Rper.day)
# 
# missing.Rtags1 <- data.frame(matrix(ncol = ncol(witset),nrow = total.tags.by.day19th$tot.Rper.day))
# names(missing.Rtags1) = names(witset)
# 
# witset.missing.tags <- rbind(witset.missing.tags,missing.Rtags1 %>% 
#                                mutate(Sample_Id = 9999, Species = "CO",
#                                       year = 2024, TagStatus = "R", Recaptured.Color= "Blue", 
#                                       RecapturedCaudalPunch = "T",
#                                       Recaptured.number = sample.recaps19th, 
#                                       Harvested = FALSE,tag.yr.sp = paste0(Recaptured.number,".",year,Species),
#                                       recap.tag = paste0("b","-",Recaptured.number),
#                                       sample_year = 2024,Location_Code = "Canyon",
#                                       Sample_Date = as_date(ymd("2024-08-19"))))



# re-join with main witset db

witset <- rbind(witset, witset.missing.tagsCO,witset.missing.tagsSK)

# note that unless we set the seed we will have a different sample every time this is run


# re:check orphans at Toboggan to see if above added tags solves it:
tobog.tags <- tobog %>% 
  filter(!is.na(recap_tag_number), year %in% yr.select) %>% 
  select(order,date,recap_tag_colour,recap_tag_number,year) %>% 
  left_join(witset %>% 
              filter(Species %in% "CO") %>% 
              filter(year %in% yr.select),
            by = c("recap_tag_number"="AppliedTagNumber", "year")) 
(tobog.tags  %>% 
    filter(is.na(Species), year %in% yr.select.QA))

#should be 0 if all tags match




# witsetCO.QA <- witset %>% 
#   filter(Species %in% "CO") %>% 
#   filter(year %in% yr.select.QA) %>% 
#   mutate(tag.number = ifelse(!is.na(Recaptured.number),Recaptured.number,
#                              ifelse(!is.na(AppliedTagNumber),AppliedTagNumber,NA)))
# 
# witsetCO.tags <- witsetCO.QA %>% 
#   filter(!is.na(tag.number)) %>% 
#   group_by(year, Species) %>% 
#   reframe(uniq.tag.number = unique(tag.number))
# 
# 
# tag.match1 <- tobog.tags %>%
#   left_join(witsetCO, by= c("year","recap_tag_number" = "tag.number")) %>% 
#   select(year, date, recap_tag_number, TagStatus, Sample_Date, species,
#          Location_Code, ForkLength) %>% 
#   filter(year %in% yr.select)
# 
# 
# #orphaned Toboggan recaps:
# tag.match1 %>% 
#   filter(is.na(recap_tag_number)) %>% 
#   group_by(year, species) %>% 
#   summarize(length(species))
# 

# Problems Remaining Summary:

# orphan recap tags (that were never applied) - this version using tag status 
uniq.applied.tag <- witset %>% 
  filter(year %in% yr.select) %>% 
  filter(!is.na(tag.yr.sp) & TagStatus %in% c("A","A2","AR")) %>%
  group_by(year) %>% 
  reframe(uniq.tag = unique(tag.yr.sp)) %>% 
  mutate(TagStatus = "A")

uniq.recap.tag <- witset %>% 
  filter(year %in% yr.select) %>% 
  filter(!is.na(tag.yr.sp) & TagStatus %in% c("R")) %>% 
  group_by(year, Species) %>% 
  reframe(uniq.tag = unique(tag.yr.sp)) %>% 
  mutate(TagStatus = "R") %>% 
  left_join(uniq.applied.tag, by=c("year","uniq.tag")) %>% 
  filter(is.na(TagStatus.y))
uniq.recap.tag


#total orphan tags:
tot.recap.orphans <- uniq.recap.tag %>% 
  filter(Species %in% c("CO","SK")) %>% 
  group_by(year, Species) %>% 
  summarize(orphan.recaps = length(Species))
tot.recap.orphans



uniq.sp.yr <- witset %>% 
  filter(Species %in% c("SK","CO"), year %in% yr.select) %>% 
  mutate(sp.yr = paste0(year,Species)) 


problems.remaining <- data.frame(year = as.numeric(substr(unique(uniq.sp.yr$sp.yr),1,4)),
                                 Species = substr(unique(uniq.sp.yr$sp.yr),5,6)) %>% 
  full_join(missing.applied.tagnum, by = c("year","Species")) %>% 
  left_join(missing.recap.tagnum, by = c("year","Species")) %>% 
  left_join(tot.recap.orphans, by = c("year","Species")) %>% 
  left_join(no.tag.number, by = c("year","Species")) %>% 
  arrange(year, Species)
problems.remaining




#### CO Analysis ####


# Are campground and canyon tagged fish equally likely to be recaptured at Toboggan?

newtags.camp.canyon <- rbind(total.newtags.camp %>% 
                               mutate(Location_Code = "Campground",
                                      newtags.applied=camp.applied) %>% 
                               filter(Species %in% "CO"),
                             total.newtags.canyon %>% 
                               mutate(Location_Code = "Canyon",
                                      newtags.applied=canyon.applied) %>% 
                               filter(Species %in% "CO")) %>% 
  select(year,Species, Location_Code, newtags.applied)

recap.origin.tobog <- tobog.tags %>% 
  filter(TagStatus %in% c("A","A2","AR")) %>% 
  group_by(year, Location_Code) %>% 
  summarize(number.from.origin = length(Location_Code)) %>% 
  left_join(newtags.camp.canyon, by = c("year","Location_Code")) %>% 
  mutate(percent.recap = round(number.from.origin/newtags.applied*100,2))
recap.origin.tobog


#### Individual estimates by source location ####

#assume Campground OR Canyon is only location where MR taking place. 
# this is generating population estimates based on assumption that fish 
# are milling around where they are tagged, whereas all other analyses assume
# that fish are moving immediately, directionally, upstream to the next location

uniq.tags.CO.camp <- witset %>% 
  filter(Species %in% "CO", year %in% yr.select, 
         Location_Code %in% "Campground",TagStatus %in% c("A","A2","AR")) %>%
  group_by(year, Location_Code) %>% 
  reframe(AppliedTagNumber = unique(AppliedTagNumber, na.rm=T)) %>% 
  mutate(mark.origin = "Camp")

uniq.tags.CO.cany <- witset %>% 
  filter(Species %in% "CO", year %in% yr.select, 
         Location_Code %in% "Canyon",TagStatus %in% c("A","A2","AR")) %>%
  group_by(year, Location_Code) %>% 
  reframe(AppliedTagNumber = unique(AppliedTagNumber, na.rm=T)) %>% 
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
# this is just in-canyon, not including the toboggan recaps



#generate LP estimates using only campground OR canyon and toboggan
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

plot.LPby.origin.tobog <- ggplot(dirty.est.tobog)+
  geom_point(aes(x=year, y=Chap, col=Location_Code, size=newtags.applied), alpha=0.5)+
  geom_errorbar(aes(x=year, ymax=Chap+CI95Chap,ymin=Chap-CI95Chap, 
                    col=Location_Code), width=0.1)+
  geom_line(aes(x=year, y=Chap, col=Location_Code))+
  labs(y="Chapman estimate from different tagging \nLocations to Toboggan +/- 95CI")
plot.LPby.origin.tobog



#proportional recapture of applied tags from each origin. Why does this vary from year to year?
# Does this indicate pre-spawn mortality in some years? Likely changed with how long the fence was in

plot.recap.origin.tobog <- ggplot(recap.origin.tobog)+
  geom_line(aes(x=year, y=percent.recap, col=Location_Code))+
  geom_point(aes(x=year, y=percent.recap, col=Location_Code, 
                 size=newtags.applied),alpha=.5)+
  scale_y_continuous(breaks = seq(floor(min(recap.origin.tobog$percent.recap)),
                                round(max(recap.origin.tobog$percent.recap),1)))+
  labs(y="% of applied tags recaptured at Toboggan",col="Location",
       size = "# tags applied")
plot.recap.origin.tobog




#### Capture Histories ####

#KP's try at capture histories...:
#Applied tags
applied.witset <- witset %>% 
  filter(!is.na(AppliedTagNumber)) %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
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
  filter(!is.na(Recaptured.number)) %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK", "ST")) %>% 
  select(year, Sample_Date, Location_Code, Species,TagStatus,
          tag.col=Recaptured.Color, tag.num = Recaptured.number,
          ForkLength, Sex)

#how many recaps excluded because have no number?
witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK")) %>% 
  filter(TagStatus %in% c("R","AR") & is.na(Recaptured.number)) %>% 
  select(Sample_Date,Location_Code,Counter, Species, TagStatus,Recaptured.Color,
         Recaptured.number) %>% 
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



## ## ##
# From carl's script:
# witset <- witset %>% 
#   mutate(AppliedTagNumberPresent = !is.na(witset$AppliedTagNumber), #uses only those fish with tag number
#          RecapturedTagNumberPresent=!is.na(witset$Recaptured.number)) %>% 
#   mutate(VentralClip = NULL, AdiposeClip = NULL) %>% 
#   mutate(Year.Species = paste0(year,".",Species)) %>% 
#   mutate(AppliedColor = tolower(AppliedColor),
#          RecapturedColor = tolower(Recaptured.Color)) %>% 
#   mutate(myTagColor = Recaptured.Color, 
#          myTagNumber = Recaptured.number) %>% #starts with recap # in this spot so if there is an applied tag it will overwrite
#   mutate(ISOweek = isoweek(Sample_Date)) %>% 
#   mutate(SYT = paste(Species, year, myTagNumber, sep=".")) %>% 
#   filter(Species %in% c("CO","SK")) #filter for just these two sp




#look at whether there are recaps before applied tags
bad.recaps <- plyr::ddply(witset[witset$year %in% yr.select,], c("Species","year","myTagNumber"), function(x){
  # see if the fish is both released and recaptured
  if(x$Species[1] %in% c("CO","SK") & !is.na(x$myTagNumber[1])){
    #if(x$myTagNumber[1] == 36199)
    #browser()
    release_date <- NA
    recap_date   <- NA
    select <- x$TagStatus %in% c("A","A2","AR") & x$Location_Code=="Campground"
    if( any(select)) {
      release_date<- min(x$Sample_Date[select])
    }
    select <- x$TagStatus %in% c("R","A","AR")  & x$Location_Code=="Canyon"
    if( any(select)){
      recap_date <- min(x$Sample_Date[select])
    }
    if (!is.na(release_date) & !is.na(recap_date) & (release_date > recap_date)){
      x$release_date <- release_date
      x$recap_date   <- recap_date
      return(x)
    }
    return(NULL)
  }
  return(NULL)
})

bad.recaps %>% 
  arrange(desc(year)) #one tag in 2023 (b-6408) that cannot explain the recapture of


#KP re-assign fish with unidentifiable recapture (perhaps based on caudal punch) a TagStatus NA

witset %>% 
  filter(TagStatus %in% "R",is.na(witset$Recaptured.number)) %>% 
  select(year, TagStatus, Sample_Date, Time_of_Catch,Location_Code,
         Species, new.tag, recap.tag, RecapturedCaudalPunch) %>% 
  arrange(desc(year))

#there are a handful of these every year. Likely most are caudal punches only, not even tag losses
witset[which(witset$TagStatus %in% "R" &
               is.na(witset$Recaptured.number)), 
       "TagStatus"] <- "NA"


#currently this does not include any harvest of tagged fish, but it could.


#original with mods:
cap.hist <- plyr::ddply(witset[witset$year %in% yr.select & witset$Species %in% c("CO","SK"),], 
                        c("Species","year","myTagNumber"), function(x){
#cap.hist <- plyr::ddply(witset[1:100,], c("Species","year","myTagNumber"), function(x){
  #default values
  #browser()
  freq=0 
  hist=".."
  #live.hist = ".." started to include fate in here but left off for now
  w1 <- NA 
  w2 <- NA
  # if a tag number is present, then this is a single fish and away we go
  if(x$Species[1] %in% c("CO","SK") & !is.na(x$myTagNumber[1])){  
    freq <- 1
    hist <- "00"
    #live.hist = "00"
    w1 <- NA
    w2 <- NA
    # applied in campground "1_"
    select <- x$TagStatus %in% c("A","A2","AR")  & x$Location_Code=="Campground"
    if( any(select)) {
      substr(hist,1,1) <- '1'
      #substr(live.hist) <- 
      w1 <- min(x$ISOweek[select])
    }
    # applied or recaptured in canyon "_1"
    select <- x$TagStatus %in% c("R","A","A2","AR")  & x$Location_Code=="Canyon"
    if( any(select)){
      substr(hist,2,2) <- '1' 
      w2 <- min(x$ISOweek[select]) #the timing of the first capture (in cases of serial recaps)
    }
  } 
  #KP note that the below code gives capture histories to fish with no tag number @ canyon ("01"). 
  if(x$Species[1] %in% c("CO","SK") & is.na(x$myTagNumber[1])){  # batch marks or first time captures in Canyon
    # we will create individual capture histories for each fish so that stratification is a breeze later
    select <- x$TagStatus %in% c("NA","A","A2","AR") & x$Location_Code=="Canyon"
    x <- x[select,]
    freq <- 1
    hist <- rep("01", nrow(x))
    w1 <- NA  # not captured in the first stratum
    w2 <- x$ISOweek
  }
  data.frame(hist=hist, freq=freq, w1=w1, w2=w2)
}) 

#comparing row numbers for troubleshooting
# witset %>% 
#   filter(year %in% yr.select, Location_Code %in% "Campground", Species %in% c("CO","SK")) %>% 
#   summarize(tot.tagstatusA = length(which(TagStatus %in% c("A","A2"))),
#             tot.appliedcol = length(which(!is.na(AppliedColor))),
#             tot.appliednum = length(which(!is.na(AppliedTagNumber))))
# 
# witset %>% 
#   filter(year %in% yr.select, Location_Code %in% "Canyon", Species %in% c("CO","SK")) %>% 
#   summarize(tot.tagstatusANA = length(which(TagStatus %in% c("A","A2", "NA"))),
#             tot.appliedcol = length(which(!is.na(AppliedColor))),
#             tot.appliednum = length(which(!is.na(AppliedTagNumber))))

# Had some bugs re-running this script in 2025- the NAs were NOT in brackets. Seems to have corrected it


cap.hist$Species.Hist <- paste0(cap.hist$Species,'.', cap.hist$hist)

xtabs(~Species+hist, data=cap.hist, exclude=NULL, na.action=na.pass)
#these are misfits:
cap.hist[cap.hist$hist%in%"00",]


# make wide capture histories 
cap.hist.wide <- tidyr::pivot_wider(cap.hist,
                                    id_cols=c("Species","year"),
                                    names_from="hist",
                                    values_from="freq",
                                    values_fill=0,
                                    values_fn=sum) 
cap.hist.wide$n1 <- apply(cap.hist.wide[,c("10","11")],1,sum, na.rm=TRUE) 
cap.hist.wide$n2 <- apply(cap.hist.wide[,c("01","11")],1,sum, na.rm=TRUE)
cap.hist.wide$m2 <- cap.hist.wide$"11"

cap.hist.wide


all.summary <- plyr::adply(cap.hist.wide,1, function(x){
  est <- SimplePetersen( x$n1, x$m2, x$n2-x$m2)
  RSE=round(est$N.se/est$N.est,2) 
  data.frame(N.est=round(est$N.est), N.se=round(est$N.se), RSE=RSE)
}) %>% 
  select(c("Species","year","n1","n2","m2","N.est","N.se","RSE"))


#### Stratified Petersen ####

# Assess the marked fractions
mf.min.recap <- 20 # if there were not enough recaps (min 20)
equal.MF <- plyr::dlply(cap.hist, c("Species","year"), function(x){
  cat("Processing ", x$Species[1], " ", x$year[1], "\n")
  #if(x$Species[1]=="CO")browser()
  total.caps <- as.data.frame(xtabs(freq~w2, data=x, exclude=NULL, na.action=na.pass), stringsAsFactor=FALSE)
  total.caps <- total.caps[ !is.na(total.caps$w2),]
  total.caps <- plyr::rename(total.caps, c("Freq"="total.caps"))
  total.recaps <- as.data.frame(xtabs(freq~w2, data=x[!is.na(x$w1),], exclude=NULL, na.action=na.pass), stringsAsFactor=FALSE)
  total.recaps <- total.recaps[ !is.na(total.recaps$w2),]
  total.recaps <- plyr::rename(total.recaps, c("Freq"="total.recaps"))
  #browser()
  mf <- NULL
  chi.test <- NULL
  fish.test <- NULL
  if(nrow(total.caps)>1){
    mf <- merge(total.caps, total.recaps, by="w2")
    mf$w2 <- as.numeric(as.character(mf$w2))
    mf[ is.na(mf)] <- 0
    mf$mf <- mf$total.recaps / mf$total.caps
    if(sum(total.recaps$total.recaps, na.rm=TRUE) >= mf.min.recap){ 
      chi.test  <- chisq.test(cbind(mf$total.recaps, mf$total.caps-mf$total.recaps))
      fish.test <- fisher.test(cbind(mf$total.recaps, mf$total.caps-mf$total.recaps), simulate.p.value=TRUE)
    }
  }
  list(Species=x$Species[1],
       Year   =x$year[1],
       mf=mf, 
       chi.test=chi.test,
       fish.test=fish.test)
})


# Assess the recapture fractions
rf.min.recap <- 20 
equal.RF <- plyr::dlply(cap.hist, c("Species","year"), function(x){
  cat("Processing ", x$Species[1], " ", x$year[1], "\n")
  #if(x$Species[1]=="CO")browser()
  total.rel <- as.data.frame(xtabs(freq~w1, data=x, exclude=NULL, na.action=na.pass), stringsAsFactor=FALSE)
  total.rel <- total.rel[ !is.na(total.rel$w1),]
  total.rel <- plyr::rename(total.rel, c("Freq"="total.rel"))
  total.recaps <- as.data.frame(xtabs(freq~w1, data=x[!is.na(x$w2),], exclude=NULL, na.action=na.pass), stringsAsFactor=FALSE)
  total.recaps <- total.recaps[ !is.na(total.recaps$w1),]
  total.recaps <- plyr::rename(total.recaps, c("Freq"="total.recaps"))
  #browser()
  rf <- NULL
  chi.test <- NULL
  fish.test <- NULL
  if(nrow(total.rel)>1){
    rf <- merge(total.rel, total.recaps, by="w1")
    rf$w1 <- as.numeric(as.character(rf$w1))
    rf[ is.na(rf)] <- 0
    rf$rf <- rf$total.recaps / rf$total.rel
    if(sum(total.recaps$total.recaps, na.rm=TRUE) >= rf.min.recap){ 
      chi.test  <- chisq.test (cbind(rf$total.recaps, rf$total.rel-rf$total.recaps))
      fish.test <- fisher.test(cbind(rf$total.recaps, rf$total.rel-rf$total.recaps), simulate.p.value=TRUE)
    }
  }
  list(Species=x$Species[1],
       Year   =x$Year[1],
       rf=rf, 
       chi.test=chi.test,
       fish.test=fish.test)
  
})


#### TBC Capture Histories ####

# first limit witset to coho in years where TBC data is present
witset.red <- witset %>% 
  filter(Species %in% "CO",year  %in% yr.select)

# create records for the marked fish at T.Creek that we append to the witset.red
t.tagrecoveries$myTagNumber   <- t.tagrecoveries$RecapturedTagNumber
t.tagrecoveries$TagStatus     <- "R"
t.tagrecoveries$Location_Code <- "T.Creek"
t.tagrecoveries$Sample_Date   <- as_date(t.tagrecoveries$date)
t.tagrecoveries$ISOweek       <- lubridate::isoweek(t.tagrecoveries$Sample_Date)

t.witset <- plyr::rbind.fill( witset.red, t.tagrecoveries)
xtabs(~Location_Code+TagStatus, data=t.witset, exclude=NULL, na.action=na.pass)


t.cap.hist <- plyr::ddply(t.witset, c("Species","year","myTagNumber"), function(x){
  # if a tag number is present, then this is a single fish and away we go
  freq=0
  hist="..."
  w1 <- NA
  w2 <- NA
  w3 <- NA
  
  date1 <- as_date(NA)
  date2 <- as_date(NA)
  date3 <- as_date(NA)
  
  #if(x$Species[1] %in% c("SK","ST","CH"))stop("This species not supported for Toboggan Creek")
  if(!is.na(x$myTagNumber[1])){  # applied in campground or recaptured (from those released) on canyon or t.creek
    #browser()
    freq <- 1
    hist <- "000"
    select <- x$TagStatus %in% c("A","A2","AR") & x$Location_Code=="Campground"
    if( any(select)) {
      substr(hist,1,1) <- '1'
      w1 <- min(x$ISOweek[select])
      date1 <- as_date(mean(x$Sample_Date[select], na.rm=T)) #is this supposed to be mean? Change to First?
    }
    select <- x$TagStatus %in% c("R","A","A2","AR") & x$Location_Code=="Canyon" &
      x$Harvested %in% FALSE # I added this because third location is more influential to popn estimate. For co only 6 over years
    if( any(select)){
      substr(hist,2,2) <- '1' 
      w2 <- min(x$ISOweek[select])
      date2 <- as_date(mean(x$Sample_Date[select],na.rm=TRUE))
    }
    select <- x$TagStatus %in% c("R")      & x$Location_Code=="T.Creek"
    if( any(select)) {
      substr(hist,3,3) <- '1'
      w3 <- min(x$ISOweek[select])
      date3 <- as_date(mean(x$Sample_Date[select], na.rm=TRUE))
    }
  }
  data.frame(hist=hist, freq=freq, w1=w1, w2=w2, w3=w3,
             date1=date1, date2=date2, date3=date3)
})


# exclude histories that are missing (I think these were just auto-created, they are not real data...)
t.cap.hist <- t.cap.hist[ t.cap.hist$hist != "...",]

t.cap.hist %>% 
  filter(hist %in% "000")

# now to compute the total number of unmarked fish seen at location 2 and 3
# for location 3, we need to look at the t.markedunmarked 
# note that this object has been fixed for "total"

#unmarked at Witset Camp and Canyon
t.unmarked <- plyr::ddply(t.witset, c("Species","year","myTagNumber"), function(x){
  #campground:
  t.unmarked.1 <- 0
  #Canyon:
  select <- x$TagStatus %in% c(NA) & x$Location_Code=="Canyon" #should exclude harvested?
  t.unmarked.2 <- sum(select)
  data.frame(t.unmarked.1=t.unmarked.1,
             t.unmarked.2=t.unmarked.2)
})
t.unmarked <- plyr::ddply(t.unmarked, c("Species","year"), plyr::summarize,
                          t.unmarked.1 =sum(t.unmarked.1),
                          t.unmarked.2 =sum(t.unmarked.2))

#using tbc data now:
t.markedunmarked.sum <- plyr::ddply(t.markedunmarked, c("Species","year"), plyr::summarize,
                                    total.coho = sum(total.coho))
t.tagrecoveries.sum  <- plyr::ddply(t.tagrecoveries,  c("Species","year"), plyr::summarize,
                                    total.coho.marked = length(year))
t.markedunmarked.sum <- merge(t.markedunmarked.sum, t.tagrecoveries.sum)
t.markedunmarked.sum$t.unmarked.3 <- t.markedunmarked.sum$total.coho - t.markedunmarked.sum$total.coho.marked

t.unmarked <- merge(t.unmarked, t.markedunmarked.sum[,c("Species","year","t.unmarked.3")])
#*** NOTE: missing tag numbers were just removed like they didn't exist *** ###  

#compare to tobog data (troubleshooting):
# tobog %>% 
#   filter(year %in% yr.select, species %in% "CO", is.na(recap_tag_number)) %>% 
#   group_by(year) %>% 
#   summarize(tot.no.tagnum = sum(total))
# 
# t.unmarked



# assess the total marked removal from camp to canyon per yr

witset %>% 
  filter(year %in% yr.select, Species %in% c("CO","SK"), Location_Code %in% "Canyon") %>% 
  group_by(Species, year) %>% 
  summarize(tot.R = length(which(TagStatus %in% "R")),
            tot.A.AR.A2 = length(which(TagStatus %in% c("A","AR","A2"))),
            tot.R.harvested = length(which(TagStatus %in% "R" & Harvested %in% T)))

# likely 2023 SK tags were more reported by fisherman to crew rather than the morts being from the crew
# in that case we just remove them, yes?




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
   filter(ForkLength %in% c(20:120 )) %>% #get rid of outliers
   filter(TagStatus %in% c(NA,"A","NA","A2","AR")) %>% #so that decisions are not masked by recaps
   mutate(tagged = ifelse(TagStatus %in% c("A","A2","AR"), "Yes","No")) %>% 
   mutate(tag.harvest = paste0(tagged,".",Harvested)) %>% 
   mutate(Tag.Harvest = ifelse(tag.harvest %in% "No.FALSE", "Released",
                               ifelse(tag.harvest %in% "No.TRUE", "Harvested",
                                      ifelse(tag.harvest %in% "Yes.FALSE", "Tagged",NA)))) %>% 
   filter(!is.na(Tag.Harvest)) %>% #there were three NAs that were both tagged and harvested, just exclude these
   arrange(Species, year,Location_Code, Harvested))

unique(sizes$TagStatus)


plot.forklengthallsp <- ggplot(sizes)+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~Species)+
  theme(legend.position =  "bottom",
        axis.text.y = element_blank())+
  labs(fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))+
  scale_x_continuous(breaks=seq(min(sizes$ForkLength),
                                max(sizes$ForkLength),20))
plot.forklengthallsp

sizes.CO.df <- sizes %>%
  filter(Species %in% "CO") 

# ggplot(sizes.CO.df)+
#   geom_histogram(aes(x=ForkLength, fill=Tag.Harvest), binwidth=5, col="black")+
#   facet_wrap(~year+Location_Code)



#compare sizes for fish recaptured at Toboggan with tagged fish

#note that TBC only really started measuring FL consistently in 2023

witset %>% 
  filter(Species %in% "CO", year %in% 2023:2024, Location_Code %in% "Campground") %>% 
  group_by(year) %>% 
  summarize(total.measured=n(),
            min = min(ForkLength, na.rm=T)*10,
            max = max(ForkLength, na.rm=T)*10,
            mn = mean(ForkLength, na.rm=T)*10,
            perc.under.45 = length(which(ForkLength < 45))/n()*100)
witset %>% 
  filter(Species %in% "CO", year %in% 2023:2024, Location_Code %in% "Canyon") %>% 
  group_by(year) %>% 
  summarize(total.measured=n(),
            min = min(ForkLength, na.rm=T)*10,
            max = max(ForkLength, na.rm=T)*10,
            mn = mean(ForkLength, na.rm=T)*10,
            perc.under.45 = length(which(ForkLength < 45))/n()*100)
tobog %>% 
  filter(species %in% "CO", year %in% 2023:2024) %>% 
  group_by(year) %>% 
  summarize(total.measured = length(!is.na(fork_length_mm)),
            min= min(fork_length_mm, na.rm=T),
            max = max(fork_length_mm, na.rm=T),
            mn = mean(fork_length_mm, na.rm=T),
            perc.under.45 = length(which(fork_length_mm < 450))/total.measured*100)


plot.camp.FL.CO <- ggplot()+
  geom_histogram(data=witset[witset$year %in% 2023:2024 & 
                                    witset$Species %in% "CO"&
                                    witset$Location_Code %in% "Campground",],
                 aes(x=ForkLength*10, fill=as.factor(year)),binwidth = 25, position="stack")+
  scale_x_continuous(limits = c(250,850),breaks = seq(250,850,50))+
  theme(legend.position="none")+
  labs(title="camp CO")
plot.canyon.FL.CO <- ggplot()+
  geom_histogram(data=witset[witset$year %in% 2023:2024 & 
                                    witset$Species %in% "CO"&
                                    witset$Location_Code %in% "Canyon",],
                 aes(x=ForkLength*10, fill=as.factor(year)),binwidth = 25,position="stack")+
  scale_x_continuous(limits = c(250,850),breaks = seq(250,850,50))+
  theme(legend.position="none")+
  labs(title="canyon CO")
plot.TBC.FL.CO <- ggplot()+
  geom_histogram(data=tobog[tobog$species %in% "CO"&tobog$year %in%2023:2024,],
                 aes(x=fork_length_mm, fill=as.factor(year)), binwidth=25,position="stack")+
  scale_x_continuous(limits = c(250,850),breaks = seq(250,850,50))+
  theme(legend.position="bottom")+
  labs(title="TBC CO")
  
plot(arrangeGrob(plot.camp.FL.CO, plot.canyon.FL.CO, plot.TBC.FL.CO))

plot.camp.FL.CO.tagged <- ggplot()+
  geom_histogram(data=witset[witset$year %in% 2023:2024 & 
                               witset$Species %in% "CO"&
                               witset$Location_Code %in% "Campground"&
                               witset$TagStatus %in% c("A","A2"),],
                 aes(x=ForkLength*10, fill=as.factor(year)),binwidth = 25, position="stack")+
  scale_x_continuous(limits = c(250,850),breaks = seq(250,850,50))+
  theme(legend.position="none")+
  labs(title="camp CO")
plot.canyon.FL.CO.tagged <- ggplot()+
  geom_histogram(data=witset[witset$year %in% 2023:2024 & 
                               witset$Species %in% "CO"&
                               witset$Location_Code %in% "Canyon"&
                               witset$TagStatus %in% c("A","A2"),],
                 aes(x=ForkLength*10, fill=as.factor(year)),binwidth = 25,position="stack")+
  scale_x_continuous(limits = c(250,850),breaks = seq(250,850,50))+
  theme(legend.position="none")+
  labs(title="canyon CO")


plot(arrangeGrob(plot.camp.FL.CO.tagged, plot.canyon.FL.CO.tagged, plot.TBC.FL.CO))
#no huge differences that I can see. Maybe fewer big ones in 2024 @ canyon?


sizes.CO.tagged <- sizes.CO.df %>%
  filter(Tag.Harvest %in% "Tagged") %>%
  mutate(tag.number = ifelse(!is.na(Recaptured.number),Recaptured.number,
                             ifelse(!is.na(AppliedTagNumber),AppliedTagNumber,NA))) %>%
  left_join(t.cap.hist[!is.na(t.cap.hist$date3),] , by = c("year","Species", "tag.number" = "myTagNumber")) %>%
  filter(year %in% yr.select)
unique(sizes.CO.tagged$Location_Code)


plot.forklengthCOtobog.camp <- ggplot(sizes.CO.tagged[sizes.CO.tagged$Location_Code %in% "Campground",])+
  geom_boxplot(aes(x=ForkLength, fill=!is.na(date3)), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  "bottom",
        axis.text.y = element_blank())+
  labs(fill = "Campground fish \nRecaptured at Toboggan?")
plot.forklengthCOtobog.camp

plot.forklengthCOtobog.canyon <- ggplot(sizes.CO.tagged[sizes.CO.tagged$Location_Code %in% "Canyon",])+
  geom_boxplot(aes(x=ForkLength, fill=!is.na(date3)), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  "bottom",
        axis.text.y = element_blank())+
  labs(fill = "Canyon fish \nRecaptured at Toboggan?")
plot.forklengthCOtobog.canyon

plot.forklengthCOtobog <- arrangeGrob(plot.forklengthCOtobog.camp,
                                      plot.forklengthCOtobog.canyon)
plot(plot.forklengthCOtobog)


#FL just in Witset by year
plot.forklengthCOyr <- ggplot(sizes.CO.df)+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  "bottom",
        axis.text.y = element_blank())+
  labs(title = "A. Coho", fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))
plot.forklengthCOyr



plot.forklengthCO.camp <- ggplot(sizes.CO.df[which(sizes.CO.df$Location_Code %in% "Campground"),])+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  "bottom",
        axis.text.y = element_blank())+
  labs(title = "A. Coho Campground", fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))+
  scale_x_continuous(limits = c(20,90))
plot.forklengthCO.camp

plot.forklengthCO.canyon <- ggplot(sizes.CO.df[which(sizes.CO.df$Location_Code %in% "Canyon"),])+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position = "bottom",
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
  theme(legend.position =  "bottom",
        axis.text.y = element_blank())+
  labs(title = "B. Sockeye",  fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))
plot.forklengthSK


plot.forklengthSK.camp <- ggplot(sizes.SK.df[which(sizes.SK.df$Location_Code %in% "Campground"),])+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  "bottom",
        axis.text.y = element_blank())+
  labs(title = "A. Sockeye Campground", fill="")+
  scale_fill_discrete(breaks=c("Tagged","Released","Harvested"))+
  scale_x_continuous(limits = c(20,80))
plot.forklengthSK.camp

plot.forklengthSK.canyon <- ggplot(sizes.SK.df[which(sizes.SK.df$Location_Code %in% "Canyon"),])+
  geom_boxplot(aes(x=ForkLength, fill=Tag.Harvest), varwidth = T)+
  facet_wrap(~year)+
  theme(legend.position =  "bottom",
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


#### environmental data - flow and temp ####
library(tidyhydat)
#download_hydat()

## NEED TO DOWNLOAD AN UPDATED VERSION OF THIS CSV-done
bulkley.flow.realtime <- read_csv("08EE005_QR_20240117T2200.csv", skip = 10,
                                  col_names = c("Date","Parameter","Value",
                                                "Approval","Qualifier"),
                                  col_types = list("Date"= col_datetime(format=""))) %>% 
  mutate(datetime = Date, Date = as_date(datetime), Parameter = "Flow", 
         STATION_NUMBER = "08EE005",Symbol=NA) %>%
  group_by(STATION_NUMBER, Date, Parameter) %>% 
  summarize(Value = mean(Value, na.rm=T)) %>% 
  mutate(Symbol = NA)



bulkley.flow.raw <- hy_daily_flows(station_number = "08EE005") %>% 
  rbind(bulkley.flow.realtime)%>% 
  mutate(year = year(Date),yday = yday(Date), 
         fake.date=as_date(yday, origin = ymd("2023-01-01"))) 

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
  filter(Date %in% seq(ymd("2018-01-01"),ymd("2023-12-31"),1)) %>% 
  filter(fake.date >= ymd("2023-07-01")& fake.date < ymd("2023-11-01"))


plot.bulkley.flow <- ggplot(bulkley.flow)+
  geom_line(aes(x=fake.date, y=Value, col=as_factor(year)), linewidth=1.5)+
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
               limits = c(ymd("2023-07-01"),ymd("2023-10-31")))+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "bottom")
plot.bulkley.start

# ggsave(plot= plot.bulkley.start, filename = "plot.flow.startend.png", 
#        height = 4, width = 6.5, device = "png")

# caught per day by flow

witset <- witset %>% 
  mutate(fake.date = as_date(yday(Sample_Date), origin=ymd("2023-01-01")))

#darker line for hydrograph is above 300 cms, which is about 3.9 m at EC station

plot.timing.flow <- ggplot(witset[which(witset$year %in% yr.select),])+
  geom_histogram(aes(x=fake.date, fill=Species),binwidth = 1)+
  geom_line(data = bulkley.flow, aes(x=fake.date, y=Value), alpha = 0.25, size=1)+
  geom_line(data = bulkley.flow[bulkley.flow$Value >= 300,], aes(x=fake.date, y=Value), alpha = 0.75, size=1)+
  facet_wrap(~year)+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d", 
               limits = c(ymd("2023-07-02"),ymd("2023-10-20")))+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "bottom",
        legend.direction = "horizontal")+
  labs(x="",y="# fish caught")
plot.timing.flow

# ggsave(plot=plot.timing.flow, filename="plot.timing.flow.png", height = 6, 
#        width=6.5, device = "png")

##### temp #####

temp_08EC005.raw <- read_csv("08EE005_TW_20240328T1637.csv", 
                               skip = 9) %>% 
  mutate(Value = ifelse(`Value (°C)` %in% 99999, NA, `Value (°C)`)) %>% 
  mutate(day = substr(`Date (PST)`,1,2), month = substr(`Date (PST)`,4,5),
         year = substr(`Date (PST)`,7,10), 
         date = ymd(paste(year,"-",month,"-",day))) %>% 
  mutate(datetime = dmy_hm(`Date (PST)`)) %>% 
  mutate(yday = yday(date),
         fake.date = as_date(yday,origin = ymd("2024-01-01"))) %>% 
  filter(year %in% seq(2018,2023,1))
str(temp_08EC005.raw)

temp_08EC005.raw.over18 <- temp_08EC005.raw %>% 
  filter(year %in% seq(2018,2023,1)) %>%
  filter(Value >= 18)

temp_08EC005 <- temp_08EC005.raw %>% 
  filter(year %in% seq(2018,2023,1)) %>%
  #mutate(fake.date = recode(fake.date, seq(ymd("2023-06-15"),ymd("2023-07-22"),1),NA,Value)) %>% 
  group_by(date, fake.date, year) %>% 
  summarize(min.temp = min(Value, na.rm=T),
            mean.temp = mean(Value, na.rm=T),
            max.temp = max(Value, na.rm=T))

ggplot(temp_08EC005)+
  geom_point(aes(x=fake.date, y=min.temp), col="lightblue")+
  geom_point(aes(x=fake.date, y=max.temp), col="darkblue")+
  geom_hline(aes(yintercept = 18), linetype="dotted")+
  scale_x_date(limits = c(ymd("2024-07-22"),ymd("2024-10-31")),
               date_breaks = "1 week")+
  facet_wrap(~year)

plot.temps.2023 <- ggplot(temp_08EC005.raw)+
  geom_line(aes(x=datetime, y=Value), col="blue")+
  geom_point(data = temp_08EC005.raw.over18, aes(x=datetime, y=Value), col="red")+
  geom_hline(aes(yintercept = 18), linetype="dotted")+
  scale_y_continuous(limits = c(5,20), breaks = seq(0,20,1))+
  scale_x_datetime(limits = c(ymd_hm("2023-07-22 00:00"),ymd_hm("2023-09-30 00:00")),
                   date_breaks = "1 week", date_labels = "%b-%d")+
  theme(axis.text.x = element_text(hjust=1, angle=45))+
  labs(x="Date",y="Water Temperature (deg. C)")+
  facet_wrap(~year)
plot.temps.2023

# ggsave(plot= plot.temps.2023, filename = "plot.temps.2023.png", 
#        width = 8, height = 6)

#### Tyee-Witset-Toboggan Run timing ####

#tyee
tyee.CO <- readxl::read_excel("Skeena_Tyee_Indicies_56_23.xlsx", sheet="coho2018-2023") %>% 
  pivot_longer(-c("MONTH","DAY"),names_to="YEAR",values_to = "daily.index") %>% 
  mutate(yearF = as.factor(YEAR),
         year = as.numeric(YEAR),
         date=ymd(paste(YEAR,MONTH,DAY)), 
         yday = yday(date),
         fake.date=ymd(paste("2023",MONTH,DAY)),
         location = "tyee") %>%  
  arrange(date)

tyee.COsum <- tyee.CO %>% 
  group_by(yearF) %>% 
  summarise(total.index = sum(daily.index, na.rm=T)) %>% 
  ungroup()

tyee.CO.daily <- tyee.CO %>% 
  left_join(tyee.COsum, by="yearF") %>% 
  mutate(daily.prop = daily.index/total.index)

ggplot(tyee.CO.daily)+
  geom_line(aes(x=fake.date, y=daily.prop))+
  facet_wrap(~yearF)+
  scale_x_date(breaks="2 weeks",date_labels = format("%b-%d"))+
  theme(axis.text.x = element_text(angle=45, hjust=1))

plot.tyee.CO <- ggplot(tyee.COsum)+
  geom_point(aes(x=yearF, y=total.index))+
  geom_line(aes(x=yearF, y=total.index, group=1))+
  labs(x="Year",y="Tyee totalled daily index")
plot.tyee.CO

# toboggan

tobog.yrly <- tobog %>%
  filter(species %in% "CO", year %in% yr.select) %>%
  group_by(year) %>%
  summarize(total.count = sum(total))

tobog.daily <- tobog %>% 
  filter(species %in% "CO", year %in% yr.select) %>% 
  mutate(yday = yday(date)) %>% 
  group_by(year, date, yday) %>% 
  summarize(daily.count = sum(total)) %>%
  ungroup() %>% 
  left_join(tobog.yrly, by="year") %>% 
  mutate(location = "toboggan",daily.prop = daily.count/total.count)
tobog.daily

# witset

witset.yrly.CO <- witset %>% 
  filter(Species %in% "CO", year %in% yr.select) %>% 
  group_by(year, Location_Code) %>% 
  summarize(total.count = n())

witset.daily.CO <- witset %>% 
  filter(Species %in% "CO", year %in% yr.select) %>% 
  group_by(Sample_Date, year, Location_Code) %>% 
  summarize(daily.count = n()) %>% 
  mutate(location = "witset", date=Sample_Date, yday = yday(date)) %>% 
  left_join(witset.yrly.CO, by=c("year","Location_Code")) %>% 
  mutate(daily.prop = daily.count/total.count*-1)


#combine locations together:

daily.catch.CO <- tobog.daily %>% 
  plyr::rbind.fill(tyee.CO.daily) %>% 
  plyr::rbind.fill(witset.daily.CO)

ggplot(daily.catch.CO)+
  geom_line(aes(x=yday, y=daily.prop, col=location))+
  scale_x_continuous(breaks=seq(plyr::round_any(min(daily.catch.CO$yday),10),
                                max(daily.catch.CO$yday),20))+
  facet_wrap(~year, scales="free_y")
ggplot(daily.catch.CO)+
  geom_bar(aes(x=yday, y=daily.prop, fill=location), stat = "identity")+
  scale_x_continuous(breaks=seq(plyr::round_any(min(daily.catch.CO$yday),10),
                                max(daily.catch.CO$yday),20))+
  facet_wrap(~year)


# # OLD REQUESTS/ANALYSIS ####
# 
# 
health <- witset.raw %>%
  mutate(year = year(Sample_Date)) %>%
  group_by(Species, year) %>%
  summarize(scale.loss = length(which(Scale.loss %in% TRUE)),
            bite.marks = length(which(bite.marks %in% TRUE)),
            net.marks = length(which(Net.marks %in% TRUE)),
            bleeding.gills = length(which(Bleeding.gills %in% TRUE)),
            cyst= length(which(Cyst %in% TRUE)),
            torn.tails = length(which(Torn.tail %in% TRUE)),
            torn.fin = length(which(Torn.fin %in% TRUE)),
            fungus = length(which(Fungus %in% TRUE)),
            sea.lice = length(which(sea.lice %in% TRUE)),
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

#PSR- timing of run in year

yr.select

theme_witset <- function(base_size = 18) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # L'ensemble de la figure
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      # Zone où se situe le graphique
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Les axes
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text.x = element_text(size = rel(0.80), face = "bold",
                                 angle=60, hjust = 1, vjust=1),
      axis.text.y = element_text(size = rel(0.80), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      # La légende
      legend.position = c(.9, .9),
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}

annual.totals.CO <- witset.daily.CO %>%
  group_by(year) %>%
  summarize(total.annual.catch = sum(daily.count))

annual.totals.SK <- witset.daily %>%
  filter(Species %in% "SK") %>% 
  group_by(year) %>%
  summarize(total.annual.catch = sum(total.daily))

plot.SK.timing <- ggplot(witset.daily[witset.daily$Species %in% "SK",])+
  # geom_line(aes(x=as_date(yday(Sample_Date)-1,origin=ymd("2024-01-01")), y=total.daily),
  #           linewidth = 1.25)+
  geom_bar(aes(x=as_date(yday(Sample_Date)-1,origin=ymd("2024-01-01")), y=total.daily),
           stat="identity")+
  geom_text(data = annual.totals.SK,
            aes(x=ymd("2024-09-15"),y=500,
                label = paste0("total catch = ",total.annual.catch)),
            fontface = "bold")+
  facet_wrap(~year)+
  scale_x_date(date_breaks = "2 weeks",date_labels = "%b-%d")+
  theme_witset()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="Date",y="Daily Catch of Sockeye",title="Daily Catch of Sockeye Campground+Canyon")
plot.SK.timing
# ggsave(plot = plot.SK.timing, filename = "plot.SK.timing19-24.png",device = "png",
#        width = 10, height = 5)

plot.CO.timing <- ggplot(witset.daily[witset.daily$Species %in% "CO",])+
  # geom_line(aes(x=as_date(yday(Sample_Date)-1,origin=ymd("2024-01-01")), y=total.daily),
  #           linewidth = 1.25)+
  geom_bar(aes(x=as_date(yday(Sample_Date)-1,origin=ymd("2024-01-01")), y=total.daily),
            stat="identity")+
  geom_text(data = annual.totals.CO,
            aes(x=ymd("2024-08-05"),y=300,
                label = paste0("total             \ncatch = ",total.annual.catch)),
            fontface = "bold")+
  facet_wrap(~year)+
  scale_x_date(date_breaks = "2 weeks",date_labels = "%b-%d")+
  theme_witset()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="Date",y="Daily Catch of Coho",title="Daily Catch of Coho Campground+Canyon")
plot.CO.timing
# ggsave(plot = plot.CO.timing, filename = "plot.CO.timing19-24.png",device = "png",
#        width = 10, height = 5)

# 
# ggplot(witset.daily[witset.daily$Species %in% "CH",])+
#   geom_line(aes(x=as_date(yday(Sample_Date)-1,origin=ymd("2023-01-01")), y=total.daily))+
#   facet_wrap(~year)+
#   scale_x_date(date_breaks = "2 weeks",date_labels = "%b-%d")+
#   theme(axis.text.x = element_text(angle=45, hjust=1))+
#   labs(x="Date",y="Daily catch of Sockeye",title="Daily catch of chinook Compground+Canyon")
# 
