#########################################################################
#												                                                #
#         Climate Change Mental Health: CDC Data Management  		        #
#												                                                #
#########################################################################

# Author: Kate Munson 
# 03/01/2021

library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(stringr)
library(zoo)

options(stringsAsFactors=FALSE)
options(scipen=999)
options(dplyr.summarise.inform = FALSE)

#########################################################################
# PART 1: Preliminary data management
#########################################################################

var <- "50367"     ## Change var to user ID

in_path <- paste0("C:/Users/",var,"/ICF/EPA Climate and Mental Health - 03_Analysis Data and Manuscript/modeling/ccmh-data/raw/population/")
out_path <- paste0("C:/Users/",var,"/ICF/EPA Climate and Mental Health - 03_Analysis Data and Manuscript/modeling/ccmh-data/processed/")

## Read in ICLUS v2 population data from EPA

temp_iclus_cty_tot <- read.csv(paste0(in_path,"/Interpolated_ICLUS_v2_CIRA2.0.csv"))

names(temp_iclus_cty_tot) <- gsub("X", "", names(temp_iclus_cty_tot), fixed = TRUE)

temp_iclus_cty_tot <- temp_iclus_cty_tot[,-c(5,6,7,8)] # Select data 2010 and onward

temp_iclus_cty_tot <- temp_iclus_cty_tot[!is.na(temp_iclus_cty_tot$GEOID10),]

# Create year column

iclus_cty_tot <- reshape::melt(temp_iclus_cty_tot, id.vars = c("GEOID10", "ICLUSGEOID","COUNAME","STNAME"), variable.name = "Year")

names(iclus_cty_tot) <- c("fips","iclusgeoid","county","state","year","popn")

# iclus_cty_tot$year <- as.numeric(iclus_cty_tot$year)

## Read in ICLUS v2 population data by age group and gender from EPA

temp_iclus_agesex_5yr <- read.csv(paste0(in_path,"/ICLUSv2_processed.csv"),
                                  colClasses = c(rep("integer",2),rep("character",2),rep("NULL",2),"factor","numeric"))

## For QA, output Loving, TX popn values that include zeroes
# temp_iclus_agesex_5yr_LovingTX <- temp_iclus_agesex_5yr[temp_iclus_agesex_5yr$Column == 48&temp_iclus_agesex_5yr$Row == 301,]
# write.csv(temp_iclus_agesex_5yr_LovingTX, paste0(out_path,"QA/ICLUSv2_Popn_AgeSex_LovingTX.csv"),row.names = FALSE)

temp_iclus_agesex_5yr$age_gender <- with(temp_iclus_agesex_5yr,paste0(AgeRange,"_",Gender))

temp_iclus_agesex_5yr$fips <- with(temp_iclus_agesex_5yr,paste0(Column,str_pad(as.character(Row),width=3,pad="0")))

temp_iclus_agesex_5yr <- temp_iclus_agesex_5yr[,c("fips","Year","age_gender","Population")]

# Create age_gender columns

iclus_agesex_5yr <- reshape2::dcast(temp_iclus_agesex_5yr,fips+Year ~ age_gender, value.var="Population")

names(iclus_agesex_5yr)[2] <- "year"

# Merge agesex and tot DFs together

temp_iclus_cty_agesex_tot <- merge(iclus_cty_tot,iclus_agesex_5yr,by=c("fips","year"),all.x=TRUE)

## Interpolate between data years

# Note: No 2100 data for 5-yr ages, so unable to interp 2096+
temp_iclus_cty_agesex_tot_2095 <- temp_iclus_cty_agesex_tot[!(temp_iclus_cty_agesex_tot$year==2096|
                                                            temp_iclus_cty_agesex_tot$year==2097|
                                                            temp_iclus_cty_agesex_tot$year==2098|
                                                            temp_iclus_cty_agesex_tot$year==2099|
                                                            temp_iclus_cty_agesex_tot$year==2100),]

temp_iclus_cty_agesex_interp_2095 <- as.data.frame(na.approx(temp_iclus_cty_agesex_tot_2095[,c(7:44)]))

iclus_cty_agesex_interp_2095 <- cbind(temp_iclus_cty_agesex_tot_2095[,c(1:6)],temp_iclus_cty_agesex_interp_2095)

## Calculate sex-, age- population percentages for each county, every 5 years

iclus_cty_agesex_popperc_2095 <- iclus_cty_agesex_interp_2095

iclus_cty_agesex_popperc_2095[c(7:44)] <- iclus_cty_agesex_popperc_2095[c(7:44)]/rowSums(iclus_cty_agesex_popperc_2095[,c(7:44)])

# Repeat popn percentages for the year 2095 all the way through 2100

iclus_cty_agesex_popperc_2096_2100 <- merge(temp_iclus_cty_agesex_tot[(temp_iclus_cty_agesex_tot$year==2096|
                                                                temp_iclus_cty_agesex_tot$year==2097|
                                                                temp_iclus_cty_agesex_tot$year==2098|
                                                                temp_iclus_cty_agesex_tot$year==2099|
                                                                temp_iclus_cty_agesex_tot$year==2100),c(1:6)],
                                             iclus_cty_agesex_popperc_2095[iclus_cty_agesex_popperc_2095$year==2095,-c(2,6)],
                                             by=c("fips","iclusgeoid","county","state"),all.x=TRUE)

## Apply population percentages to interpolated county total population for all years through 2021

temp_iclus_cty_agesex <- rbind(iclus_cty_agesex_popperc_2095,iclus_cty_agesex_popperc_2096_2100)

temp_iclus_cty_agesex$popn <- as.numeric(temp_iclus_cty_agesex$popn)

temp_iclus_cty_agesex[c(7:44)] <- temp_iclus_cty_agesex[c(7:44)]*temp_iclus_cty_agesex$popn

## Sum by 10-year age group to match baseline suicide incidence rate age groups

temp_iclus_cty_agesex_f <- temp_iclus_cty_agesex[,grepl(c("fips|year|iclusgeoid|county|state|popn|FEMALE"), names(temp_iclus_cty_agesex))] 
names(temp_iclus_cty_agesex_f) <- gsub("_FEMALE", "", names(temp_iclus_cty_agesex_f), fixed = TRUE)

iclus_cty_agesex_f <- reshape::melt(temp_iclus_cty_agesex_f, 
                                    id.vars = c("fips","year","iclusgeoid","county","state","popn"), variable.name = "agegrp")
names(iclus_cty_agesex_f) <- c("fips","year","iclusgeoid","county","state","county_popn","agegrp","sex_age_popn")
iclus_cty_agesex_f$sex <- "FEMALE"

temp_iclus_cty_agesex_m <- temp_iclus_cty_agesex[,!grepl(c("FEMALE"), names(temp_iclus_cty_agesex))] 
names(temp_iclus_cty_agesex_m) <- gsub("_MALE", "", names(temp_iclus_cty_agesex_m), fixed = TRUE)

iclus_cty_agesex_m <- reshape::melt(temp_iclus_cty_agesex_m, 
                                    id.vars = c("fips","year","iclusgeoid","county","state","popn"), variable.name = "agegrp")
names(iclus_cty_agesex_m) <- c("fips","year","iclusgeoid","county","state","county_popn","agegrp","sex_age_popn")
iclus_cty_agesex_m$sex <- "MALE"

iclus_cty_agesex <- rbind(iclus_cty_agesex_f,iclus_cty_agesex_m)

# Remove ages <5, since no suicide incidence
iclus_cty_agesex <- as.data.frame(iclus_cty_agesex[!(iclus_cty_agesex$agegrp=="0TO0"|iclus_cty_agesex$agegrp=="1TO4"),])

# Calculate total population in ten year age groups per county-year-sex
iclus_cty_agesex$agegrp_rev <- with(iclus_cty_agesex, 
                                    ifelse(agegrp=="5TO9"|agegrp=="10TO14","5-14 years",
                                           ifelse(agegrp=="15TO19"|agegrp=="20TO24","15-24 years",
                                                  ifelse(agegrp=="25TO29"|agegrp=="30TO34","25-34 years",
                                                         ifelse(agegrp=="35TO39"|agegrp=="40TO44","35-44 years",
                                                                ifelse(agegrp=="45TO49"|agegrp=="50TO54","45-54 years",
                                                                       ifelse(agegrp=="55TO59"|agegrp=="60TO64","55-64 years",
                                                                              ifelse(agegrp=="65TO69"|agegrp=="70TO74","65-74 years",
                                                                                     ifelse(agegrp=="75TO79"|agegrp=="80TO84","75-84 years","85+ years")))))))))


iclus_cty_agesex_tenyear <- iclus_cty_agesex %>% 
  select(c("fips","year","iclusgeoid","county","state","sex","agegrp_rev","sex_age_popn")) %>%
  group_by(fips,year,iclusgeoid,county,state,sex,agegrp_rev) %>%
  summarise_all(sum)

# Change agegrp_rev to agegrp for consistency with incidence dataset
names(iclus_cty_agesex_tenyear)[7] <- "agegrp"

length(unique(iclus_cty_agesex_tenyear$fips)) # 3109

iclus_cty_agesex_tenyear$fips <- str_pad(as.character(iclus_cty_agesex_tenyear$fips),width=5,pad="0")

## Write to file

# Shannon county (FIPS 46113) changed to Oglala Lakota county (FIPS 46102) in 2015
iclus_cty_agesex_tenyear$fips <- with(iclus_cty_agesex_tenyear,ifelse(fips=="46113","46102",fips))
# iclus_cty_agesex_tenyear$iclusgeoid <- with(iclus_cty_agesex_tenyear,ifelse(iclusgeoid==46113,46102,iclusgeoid))

write.csv(iclus_cty_agesex_tenyear, paste0(out_path,"ICLUSv2_InterpCtyPopn_AgeSex_2010-2100.csv"),row.names = FALSE)

## QA: Compare FIPS from baseline climate files to FIPS in ICLUS data

clim_base <- read.csv("C:/Users/50367/ICF/EPA Climate and Mental Health - 03_Analysis Data and Manuscript/modeling/ccmh-data/processed/climate/Baseline/EPA Health_Precip_mm_LivnehBaseline_weighted by county FIPS population.csv")


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


clim_base$fips <- substrRight(clim_base$County.FIPS,5)

length(unique(clim_base$fips)) # 3108

iclus_cty_agesex_tenyear_missingclim <- iclus_cty_agesex_tenyear[!(iclus_cty_agesex_tenyear$fips %in% clim_base$fips),]

unique(iclus_cty_agesex_tenyear_missingclim$fips) # "51515"


clim_base_missingiclus <- clim_base[!(clim_base$fips %in% iclus_cty_agesex_tenyear$fips),]

unique(clim_base_missingiclus$fips) # 0


## QA: Compare to previous version that had 2015-2100 data

# iclus_cty_agesex_tenyear_2015 <- read.csv(paste0(out_path,"/ICLUSv2_InterpCtyPopn_AgeSex_2015-2100.csv"))
# iclus_cty_agesex_tenyear_compare <- merge(iclus_cty_agesex_tenyear,iclus_cty_agesex_tenyear_2015,
#                                           by=c("fips","year","iclusgeoid","county","state","sex","agegrp"),all=TRUE)
# 
# iclus_cty_agesex_tenyear_compare$PopCheck <- with(iclus_cty_agesex_tenyear_compare,sex_age_popn.y-sex_age_popn.x)
# 
# max(iclus_cty_agesex_tenyear_compare$PopCheck[!is.na(iclus_cty_agesex_tenyear_compare$PopCheck)])
# # [1] 0.5
# min(iclus_cty_agesex_tenyear_compare$PopCheck[!is.na(iclus_cty_agesex_tenyear_compare$PopCheck)])
# # [1] -0.4993937
# QA results - all OK

## QA note:
# Loving TX (FIPS = 48301) has zero population for several 5-year age groups in the ICLUS v2 data provided by EPA
# View(temp_iclus_agesex_5yr[temp_iclus_agesex_5yr$fips==48301,])