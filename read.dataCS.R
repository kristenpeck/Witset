# read in the tagging database for all of the species

library(readxl)
library(tidyverse)

#workbook <- file.path("Witset MR","Witset_Fish_Tagging_Data_V2.xlsx")
#workbook <- file.path("Witset MR","Witset_Fish_Tagging_Data_V2_copy12-Jan-23.xlsx")

# I created the csv files based on the data files by hand
# Tobaggon tag recoveries (mostly coho)
#t.tagrecoveries  <- file.path("Witset MR","Toboggan.tagrecoveries.2018-2021.csv")

#t.markedunmarked <- file.path("Witset MR","Toboggan.total-marked.coho.2018-2021.csv")


#t.workbook <- file.path("Witset MR","TobogganFenceData_copy12-Jan-23.xlsx")

# Nanika snorkel data for sockeye
#n.counts.file <- file.path("Witset MR","NanikaSnorkel.xlsx")

####################################################################
####################################################################
####################################################################
####################################################################

# Tagging on campground and recaptured/recovered in canyon


# Chinook
# ch.tagged <- readxl::read_excel(workbook,
#                                 sheet="Tag_Data_Chinook",
#                                 .name_repair="universal")


nms <- names(read_excel("Tag_Data_Sockeye.xlsx", n_max = 0))
ct <- ifelse(grepl("^AppliedColor2", nms), "text", 
             ifelse(grepl("^AppliedTagNumber2", nms), "numeric","guess"))


witset.raw.upto22 <- rbind(read_excel("Tag_Data_Sockeye.xlsx",col_types = ct,
                                      .name_repair="universal"),
                           read_excel("Tag_Data_Coho.xlsx",col_types = ct,
                                      .name_repair="universal"),
                           read_excel("Tag_Data_Chinook.xlsx",col_types = ct,
                                      .name_repair="universal"),
                           read_excel("Tag_Data_Steelhead.xlsx",col_types = ct,
                                      .name_repair="universal")) 

# from db v3: 2023 

witset.raw.upto23 <- rbind(read_excel("Tag_Data_Sockeye2023.xlsx",col_types = ct,
                                      .name_repair="universal"),
                           read_excel("Tag_Data_Coho2023.xlsx",col_types = ct,
                                      .name_repair="universal"),
                           read_excel("Tag_Data_Chinook2023.xlsx",col_types = ct,
                                      .name_repair="universal"),
                           read_excel("Tag_Data_Steelhead2023.xlsx",col_types = ct,
                                      .name_repair="universal")) 

all.fish <- rbind(witset.raw.upto22, witset.raw.upto23)

# #KP changed to read from excel export:
# ch.tagged <- readxl::read_excel("Tag_Data_Chinook.xlsx",.name_repair="universal")
# 
# # Coho
# # co.tagged <- readxl::read_excel(workbook,
# #                                 sheet="Tag_Data_Coho",
# #                                 .name_repair="universal")
# #KP changed to read from excel export:
# co.tagged <- readxl::read_excel("Tag_Data_Coho.xlsx",.name_repair="universal")
# 
# # Sockeye
# # so.tagged <- readxl::read_excel(workbook,
# #                                 sheet="Tag_Data_Sockeye",
# #                                 .name_repair="universal")
# #KP changed to read from excel export:
# so.tagged <- readxl::read_excel("Tag_Data_Sockeye.xlsx",.name_repair="universal")
# 
# # steelhead
# # st.tagged <- readxl::read_excel(workbook,
# #                                 sheet="Tag_Data_Steelhead",
# #                                 .name_repair="universal")
# #KP changed to read from excel export:
# st.tagged <- readxl::read_excel("Tag_Data_Steelhead.xlsx",.name_repair="universal")
# 

# all.fish <- rbind(ch.tagged, co.tagged, so.tagged, st.tagged)



# change the field names for recaptures
all.fish <- plyr::rename(all.fish, c("Recaptured.number"="RecapturedTagNumber",
                                     "Recaptured.Color" ="RecapturedColor"))


# merge sample dates with fish data
# sample.dates <- readxl::read_excel(workbook,
#                                 sheet="Tag_Data_Sample_Dates",
#                                 .name_repair="universal")
#KP changed to read from excel export:
sample.dates <- rbind(read_excel("Tag_Data_Sample_Dates.xlsx",
                                   .name_repair="universal"),
                              read_excel("Tag_Data_Sample_Dates2023.xlsx",
                                         .name_repair="universal"))


sample.dates$Sample_Date <- as.Date(sample.dates$Sample_Date)
sample.dates$Year        <- lubridate::year(sample.dates$Sample_Date)

# check that sample ids match
setdiff(sample.dates$Sample_Id, all.fish$Sample_Id)
setdiff(all.fish$Sample_Id, sample.dates$Sample_Id)

# missing some sample dates
all.fish <- merge(all.fish, sample.dates, by="Sample_Id", all.x=TRUE)  %>% 
  filter(Year >= 2018)
  

xtabs(~Sample_Id+Species, data=all.fish[ is.na(all.fish$Year),], exclude=NULL, na.action=na.pass)

xtabs(~Year+TagStatus, data=all.fish[all.fish$Species=="CH",], exclude=NULL, na.action=na.pass)

# location of application or capture- should be "Campground" or "Canyon"
xtabs(~Species+Location_Code, data=all.fish, exclude=NULL, na.action=na.pass)

# tag status - should be A, A2, AR, NA, R
xtabs(~Species+TagStatus, data=all.fish, exclude=NULL, na.action=na.pass)
all.fish$TagStatus <- toupper(all.fish$TagStatus)
xtabs(~Species+TagStatus, data=all.fish, exclude=NULL, na.action=na.pass)


# year
xtabs(~Species+Year, data=all.fish, exclude=NULL, na.action=na.pass)

xtabs(~TagStatus+Year+Species, data=all.fish, exclude=NULL, na.action=na.pass)

xtabs(~TagStatus+Location_Code, data=all.fish, exclude=NULL, na.action=na.pass)
xtabs(~TagStatus+AppliedCaudalPunch+Species, data=all.fish, exclude=NULL, na.action=na.pass)

xtabs(~AppliedCaudalPunch+Year+Species, data=all.fish, exclude=NULL, na.action=na.pass)

xtabs(~AppliedCaudalPunch+Year, data=all.fish, exclude=NULL, na.action=na.pass)


## Fish tag info is recorded in the AppliedColor/AppliedTagNumber, RemovedColor/RemovedTagNumber, and Recaptured color/Recapture number fields
## We will create a new column for TagColor/TagNumber
## Usually these columns are exclusive, i.e. no values in both but we check

# Look like RemovedColor and RemovedTagNumber are never used
xtabs(~RemovedTagNumber, data=all.fish, exclude=NULL, na.action=na.pass)
xtabs(~RemovedTagColor,  data=all.fish, exclude=NULL, na.action=na.pass)
all.fish$RemovedTagNumber <- NULL
all.fish$RemovedTagColor  <- NULL

# check the double tagged fish.
select <- all.fish$TagStatus=="A2"
sum(select)
temp <- all.fish[select, c("Year","Sample_Date","Location_Code","Species","TagStatus","AppliedColor","AppliedTagNumber","AppliedTagNumber2","RecapturedColor","RecapturedTagNumber","Comments.x")]
temp <- temp[ order(temp$Species, temp$Sample_Date),]
temp






# check for applied and recapture number field both non-NA
xtabs(~is.na(AppliedTagNumber)+is.na(RecapturedTagNumber), data=all.fish, exclude=NULL, na.action=na.pass)
# Lots of records with AppliedTagNumber and RecapturedTagNumber are both NA - these must be captures of 
# fish that are not tagged.
# A few records where both fields are given
select <- !is.na(all.fish$AppliedTagNumber) & !is.na(all.fish$RecapturedTagNumber)
sum(select)
temp <- all.fish[select,]
#write.csv(temp, file="both-applied-recap-numbers.csv", row.names=FALSE)


all.fish[select, c("Year","Sample_Date","Location_Code","Species","TagStatus","AppliedColor","AppliedTagNumber","RecapturedColor","RecapturedTagNumber","Comments.x")]

temp <- all.fish[ !is.na(all.fish$AppliedTagNumber) & !is.na(all.fish$Recapture.number),]



## Tag status of NA is typically missing because fish is harvested but not always
## If not harvested, with a tag status of NA and a tag number present, we change to NA
all.fish$TagStatus[ all.fish$TagStatus == "NA"] <- NA
xtabs(~TagStatus + Harvested, data=all.fish, exclude=NULL, na.action=na.pass)




## sometimes tag status is missing (or set to NA), not harvested, and a tag number recorded
## e.g. ST 58023
select <- is.na(all.fish$TagStatus) & !all.fish$Harvested & !is.na(all.fish$AppliedTagNumber)
sum(select)
all.fish[select, c("Year","Sample_Date","Location_Code","Species","TagStatus","AppliedColor","AppliedTagNumber","RecapturedColor","RecapturedTagNumber","Comments.x")]
all.fish$TagStatus[select]<- "A"
all.fish$MyComment[select ] <- paste0(all.fish$MyComment[select], ";Changed tag status to A")

## CH uses punches to record marking information
xtabs(~Species+CaudalPunch, data=all.fish, exclude=NULL, na.action=na.pass)
xtabs(~TagStatus+CaudalPunch, data=all.fish, exclude=NULL, na.action=na.pass)
xtabs(~Species+CaudalPunch, data=all.fish[is.na(all.fish$TagStatus),], exclude=NULL, na.action=na.pass)


rm(ch.tagged, co.tagged, so.tagged, st.tagged)


####################################################################
####################################################################
####################################################################
####################################################################

# # This was a hand created file used previously
# t.tagrecoveries <- read.csv("Toboggan.tagrecoveries.2018-2021.csv", 
#                             header=TRUE, as.is=TRUE, strip.white=TRUE)
# 
# 
# t.tagrecoveries$Date <- as.Date(t.tagrecoveries$Date)
# t.tagrecoveries$Year <- lubridate::year(t.tagrecoveries$Date)
# t.tagrecoveries <- plyr::rename(t.tagrecoveries, c("tag"="RecapturedTagNumber"))
# t.tagrecoveries$Species <- "CO"  # need to check this
# head(t.tagrecoveries)
# xtabs(~Year, data=t.tagrecoveries, exclude=NULL, na.action=na.pass)
# 
# 
# 
# # This file has total CO captured (excluding wild and hatchery jacks) and # of marks recaptured
# # The actual marks recaptured are in the previous file
# t.markedunmarked      <- read.csv("Toboggan.total-marked.coho.2018-2021.csv", 
#                                   header=TRUE, as.is=TRUE, strip.white=TRUE)
# t.markedunmarked$Date <- as.Date(t.markedunmarked$Date)
# t.markedunmarked$Year <- lubridate::year(t.markedunmarked$Date)
# t.markedunmarked$Species <- "CO"
# head(t.markedunmarked)
# 
# 
# # The cross tabulations jive (hurrah)
# xtabs(total.marked~Year, data=t.markedunmarked, exclude=NULL, na.action=na.pass)
# xtabs(~Year, data=t.tagrecoveries, exclude=NULL, na.action=na.pass)

# Now the data appears to be in a workbook that I can massage as needed
t.data <- readxl::read_excel("TobogganFenceData_MASTER-copy29-Mar-2023.xlsx",
                             sheet='IndividualFish',
                             .name_repair="universal") %>% 
  filter(year >= 2018)
t.data$date <- as.Date(t.data$date)
t.data$species <- toupper(t.data$species)
t.data <- plyr::rename(t.data, c("year"="Year",
                                 "date"="Date",
                                 "species"="Species",
                                 "recap_tag_number"="RecapturedTagNumber"))

t.tagrecoveries.new <- t.data[!is.na(t.data$RecapturedTagNumber) & 
                                t.data$Species %in% c("CO"),c("Date","Year","Species","RecapturedTagNumber")]

# do some basic comparisions
# xtabs(~Species+Year, data=t.tagrecoveries,     exclude=NULL, na.action=na.pass)
xtabs(~Species+Year, data=t.tagrecoveries.new, 
      exclude=NULL, na.action=na.pass)

# Extract total fish handled
t.markedunmarked.new.fish <- t.data[t.data$Species %in% c("CO"),
                                    c("Date","Year","Species","RecapturedTagNumber")]
head(t.markedunmarked.new.fish)
t.markedunmarked.new <- plyr::ddply(t.markedunmarked.new.fish, c("Date","Year","Species"), 
                                    plyr::summarize,
                                    total.coho=length(Date),
                                    total.marked=sum(!is.na(RecapturedTagNumber)))



####################################################################
####################################################################
####################################################################
####################################################################

# Nanika snorkel data for sockeye
# This has total count and number of tagged fish seen

n.counts <- readxl::read_excel("NanikaSnorkel.xlsx", sheet="Sheet1",
                               .name_repair = "universal") %>% 
  mutate(Species = "SK") %>% 
  filter(Year >= 2018) #KP addition

head(n.counts)

