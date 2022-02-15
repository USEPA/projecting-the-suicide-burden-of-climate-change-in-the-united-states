#########################################################################
#												                                                #
#         Climate Change Mental Health: CDC Data Management  		        #
#												                                                #
#########################################################################

# Authors: Kate Munson and Madison Howell
# 02/03/2021
# Last Update: 10/04/2021

library(dplyr)
library(tidyr)
library(reshape2)
# library(purrr)
library(stringr)

options(stringsAsFactors=FALSE)
options(scipen=999)
options(dplyr.summarise.inform = FALSE)

# Functions
left <- function(text, n) {
  substr(text, 1, n)
}

right <- function(text, n) {
  substr(text, nchar(text) - (n - 1), nchar(text))
}

var <- "50367"     ## Change var to user ID


in_path <-
  paste0(
    "C:/Users/",
    var,
    "/ICF/EPA Climate and Mental Health - 03_Analysis Data and Manuscript/modeling/ccmh-data/raw/incidence/"
  )

out_path <-
  paste0(
    "C:/Users/",
    var,
    "/ICF/EPA Climate and Mental Health - 03_Analysis Data and Manuscript/modeling/ccmh-data/processed/"
  )

fips_path <-
  paste0(
    "C:/Users/",
    var,
    "/ICF/EPA Climate and Mental Health - 03_Analysis Data and Manuscript/modeling/ccmh-data/raw/population/"
  )

#########################################################################
# PART 1: Evaluate urbanization categories for state and county incidence
#########################################################################

## Read in state- and county-level urbanization incidence data ("_URBANIZ" .txt files)

COUNTY <- read.delim(paste0(in_path,"Underlying Cause of Death, 1999-2019_COUNTY_URBANIZ.txt"))

STATE <- read.delim(paste0(in_path,"Underlying Cause of Death, 1999-2019_STATE_URBANIZ.txt"))

## Evaluate county-level urbanization data 
# Create dataframe of unique counties and 2013 urbanization values

df.COUNTY <- filter(COUNTY,!is.na(County.Code)) %>%
  select(c("County.Code","X2013.Urbanization","X2013.Urbanization.Code"))

#df of unique values of the df.County
df.COUNTY_UNIQUE <- unique(df.COUNTY)
# 2109 obs

length(df.COUNTY_UNIQUE$County.Code) # 2109
length(unique(df.COUNTY_UNIQUE$County.Code)) # 2109 - Good, shows no overlap between county-urbanization categories

# Categorization of x2013.Urbanization by distinct County
df.COUNTY_UNIQUE %>%
  group_by(X2013.Urbanization,X2013.Urbanization.Code) %>%
  summarise(distinct_County = n_distinct(County.Code))

# # A tibble: 6 x 2
# X2013.Urbanization      distinct_County
# <chr>                             <int>
#   1 Large Central Metro                  68
# 2 Large Fringe Metro                  337
# 3 Medium Metro                        339
# 4 Micropolitan (Nonmetro)             552
# 5 NonCore (Nonmetro)                  527
# 6 Small Metro                         286

# Identify unique counties by distinct_x2013.Urbanization
county_distincturb <- df.COUNTY_UNIQUE %>%
  group_by(County.Code) %>%
  summarise(distinct_X2013.Urbanization = n_distinct(X2013.Urbanization))

# Count if distinct_x2013.Urbanization occurs more than once per county
sum(df.COUNTY_UNIQUE$distinct_x2013.Urbanization > 1, na.rm = TRUE)
## [1] 0
# No counties have multiple 2013 urbanization designations

## Confirm that the state-level data has the same urbanization designations as the county-level urbanization data 

#df of unique x.2013.Urbanization values -- COUNTY
df.COUNTY_2013.Urbanization <- unique(subset(County.Code, select=c(4)))
print(df.COUNTY_2013.Urbanization)

#             X2013.Urbanization
#  1                Medium Metro
#  9                 Small Metro
#  23         NonCore (Nonmetro)
#  25         Large Fringe Metro
#  49    Micropolitan (Nonmetro)
#  174       Large Central Metro
#  14589 

#df of unique x.2013.Urbanization values -- STATE
df.STATE <- filter(STATE,!is.na(State.Code))

df.STATE_2013.Urbanization <- unique(subset(df.STATE, select=c("X2013.Urbanization")))
print(df.STATE_2013.Urbanization)

# X2013.Urbanization
# 1      Large Central Metro
# 17      Large Fringe Metro
# 33            Medium Metro
# 50             Small Metro
# 66 Micropolitan (Nonmetro)
# 80      NonCore (Nonmetro)

## Confirmed - same urbanization categories

#########################################################################
# PART 5: Develop the annual baseline incidence dataset
#########################################################################

st_abbr_xwalk <-  read.csv(paste0(in_path,"State_Abbr_Xwalk.csv"))

temp_Urb_Codes <-  read.csv(paste0(in_path,"NCHSURCodes2013.csv"))

Urb_Codes <- temp_Urb_Codes %>% select(c("FIPS.code","X2013.code"))
names(Urb_Codes) <- c("fips","X2013_Urbanization_Code")

# National
temp_ucod_20yr_urb <- read.delim(paste0(in_path,"Underlying Cause of Death, 1999-2019_URB_NATL.txt"))
ucod_20yr_urb  <- filter(temp_ucod_20yr_urb,!is.na(X2013.Urbanization.Code)&!(Ten.Year.Age.Groups=="Not Stated")&!(Deaths=="Suppressed")) %>%
  select(c("X2013.Urbanization","Ten.Year.Age.Groups","Gender","Deaths","Population","Crude.Rate"))
names(ucod_20yr_urb ) <- c("X2013_Urbanization","Ten_Year_Age_Groups","Gender","Deaths_N","Popn_N","Crude_Rate_N")

# State
temp_ucod_20yr_state <- read.delim(paste0(in_path,"Underlying Cause of Death, 1999-2019_STATE.txt"))
ucod_20yr_state  <- filter(temp_ucod_20yr_state,!is.na(Deaths)&!(Ten.Year.Age.Groups=="Not Stated")) %>%
  select(c("State","Ten.Year.Age.Groups","Gender","Deaths","Population","Crude.Rate"))
names(ucod_20yr_state ) <- c("State","Ten_Year_Age_Groups","Gender","Deaths_S","Popn_S","Crude_Rate_S")

# County
temp_ucod_20yr_county <- read.delim(paste0(in_path,"Underlying Cause of Death, 1999-2019_COUNTY.txt"))
ucod_20yr_county  <- filter(temp_ucod_20yr_county,!is.na(Deaths)&!(Ten.Year.Age.Groups=="Not Stated")) %>%
  select(c("County","County.Code","Ten.Year.Age.Groups","Gender","Deaths","Population","Crude.Rate"))
names(ucod_20yr_county ) <- c("County","fips","Ten_Year_Age_Groups","Gender","Deaths_C","Popn_C","Crude_Rate_C")
ucod_20yr_county$State_Abbr <- with(ucod_20yr_county, right(ucod_20yr_county$County, 2))

# Update dataset to include all counties and all age/sex designations per county
temp_unique_fips <- read.csv(paste0(fips_path,"Interpolated_ICLUS_v2_CIRA2.0.csv"))
unique_fips <- distinct(temp_unique_fips[!(is.na(temp_unique_fips$GEOID10)),c("GEOID10","STNAME")])
names(unique_fips) <- c("fips","State")

unique_age <- as.data.frame(unique(ucod_20yr_county$Ten_Year_Age_Groups))
names(unique_age) <- "Ten_Year_Age_Groups"

unique_sex <- as.data.frame(unique(ucod_20yr_county$Gender))
names(unique_sex) <- "Gender"

unique_fips_age_sex <-
  merge(merge(merge(merge(unique_fips, unique_age), unique_sex),
        st_abbr_xwalk,
        by = c("State"),
        all.x = TRUE),
        Urb_Codes,all.x=TRUE)
        

ucod_20yr_county_rev <- merge(ucod_20yr_county,unique_fips_age_sex,by=c("fips","Ten_Year_Age_Groups","Gender","State_Abbr"),all.y=TRUE)

## Update ucod_20yr_county_rev to exclude AK and HI
ucod_20yr_county_state <- filter(ucod_20yr_county_rev,
                                 !(State_Abbr=="AK"|State_Abbr=="HI"))

## Observations: 55962     

## Since counties only have ONE urbanization category, create new DF that merges ucod_20yr_county_state 
# with unique urbanization info (df.COUNTY_UNIQUE)
names(df.COUNTY_UNIQUE) <- c("fips","X2013.Urbanization")

Urb_unique <- distinct(df.COUNTY_UNIQUE[,c("X2013.Urbanization","X2013.Urbanization.Code")])
names(Urb_unique) <- c("X2013_Urbanization","X2013_Urbanization_Code")

df.UCOD_COUNTY_STATE_Merge <- merge(Urb_unique, ucod_20yr_county_state,by="X2013_Urbanization_Code",all.y=TRUE)
## Observations: 55962

df.STATE <- df.STATE %>%
  rename(
    Deaths_State = Deaths,
    Population_State = Population,
    Crude.Rate_State = Crude.Rate
  )

unique_age_sex <- merge(unique_sex,unique_age)

names(df.STATE) <- gsub(".", "_", names(df.STATE), fixed = TRUE)

df.STATE_rev <- merge(df.STATE,unique_age_sex) # Gets rid of two rows of "not stated"


## Create new DF that merges the above (df.UCOD_COUNTY_STATE_Merge) DF with state-level urbanization data (df.STATE) 

# First, need to rename columns in df.UCOD_County_State_Merge to match with df.STATE to make sure merge is consistent
names(df.UCOD_COUNTY_STATE_Merge) <- gsub(".", "_", names(df.UCOD_COUNTY_STATE_Merge), fixed = TRUE)


# Merge
df.COUNTY_STATE_Merge <-
  merge(
    df.STATE_rev,
    df.UCOD_COUNTY_STATE_Merge,
    by = c(
      'State',
      'X2013_Urbanization',
      'X2013_Urbanization_Code',
      'Ten_Year_Age_Groups',
      'Gender'
    ),
    all.y = TRUE
  )


## Create column for Crude.Rate_Final and use ifelse statement to select the state-level data if county-level is 

## Merge with Census Region data: Underlying Cause of Death, 1999-2019_CR_URBANIZ.txt
CR_Underlying_COD <- read.delim(paste0(in_path,"Underlying Cause of Death, 1999-2019_CR_URBANIZ.txt"))

df.CR_Underlying_COD <- filter(CR_Underlying_COD,!is.na(X2013.Urbanization.Code))

df.CR_Underlying_COD <- df.CR_Underlying_COD %>%
  rename(
    Deaths_CR = Deaths,
    Population_CF = Population,
    Crude.Rate_CR = Crude.Rate
  )

## Read in crosswalk between Census Region and State: https://www2.census.gov/programs-surveys/popest/geographies/2014/state-geocodes-v2014.xls
CB_Region <-  read.csv(paste0(in_path,"Census-Bureau_Region-State_2015-05.csv"))

# Merge df.CR_Underlying_COD and CB_Region on Census.Region
df.Census_Region <- merge(df.CR_Underlying_COD, CB_Region)
df.Census_Region$Notes <- NULL

dim(df.Census_Region) # 5564 rows 

names(df.Census_Region) <- gsub(".", "_", names(df.Census_Region), fixed = TRUE)

df.Census_Region <- df.Census_Region %>% select(c('Census_Region','Census_Region_Code','State',
                                                  'X2013_Urbanization',
                                                  'X2013_Urbanization_Code',
                                                  'Gender',
                                                  'Ten_Year_Age_Groups',
                                                  'Deaths_CR','Population_CF','Crude_Rate_CR','Region'
                                                  ))

## Merge df.County_State_Merge with df.Census_Region
df.County_State_CR_Merge <- merge(
  df.COUNTY_STATE_Merge,
  df.Census_Region,
  by = c(
    'State',
    'X2013_Urbanization',
    'X2013_Urbanization_Code',
    'Gender',
    'Ten_Year_Age_Groups'
  ),
  all.x = TRUE
)

dim(df.County_State_CR_Merge) # 55962    24 - matches dimensions of df.COUNTY_STATE_Merge

df.County_State_CR_Urb_Merge <-
  merge(merge(
    df.County_State_CR_Merge,
    ucod_20yr_state,
    by = c('State', 'Ten_Year_Age_Groups', 'Gender'),
    all.x = TRUE
  ),
  ucod_20yr_urb,
  by = c('X2013_Urbanization', 'Ten_Year_Age_Groups', 'Gender'),
  all.x = TRUE)
  


# If no State_CR value per age/sex combo, select state value

df.County_State_CR_Urb_Merge$Crude_Rate_Final <- with(df.County_State_CR_Urb_Merge,
                                                      ifelse((is.na(Crude_Rate_C)|Crude_Rate_C=='Unreliable')
                                                             & (is.na(Crude_Rate_State)|Crude_Rate_State=='Unreliable')
                                                             & (is.na(Crude_Rate_CR)|Crude_Rate_CR=='Unreliable')
                                                             & (is.na(Crude_Rate_S)|Crude_Rate_S=='Unreliable'),
                                                             Crude_Rate_N,
                                                  ifelse((is.na(Crude_Rate_C)|Crude_Rate_C=='Unreliable')
                                                         & (is.na(Crude_Rate_State)|Crude_Rate_State=='Unreliable')
                                                         & (is.na(Crude_Rate_CR)|Crude_Rate_CR=='Unreliable'),
                                                         Crude_Rate_S,
                                                         ifelse((is.na(Crude_Rate_C)|Crude_Rate_C=='Unreliable')
                                                                & (is.na(Crude_Rate_State)|Crude_Rate_State=='Unreliable'),
                                                                Crude_Rate_CR,
                                                                ifelse((is.na(Crude_Rate_C)|Crude_Rate_C=='Unreliable'),
                                                                       Crude_Rate_State,Crude_Rate_C)))))

df.County_State_CR_Urb_Merge$Crude_Rate_Final_Decision <- with(df.County_State_CR_Urb_Merge,
                                                      ifelse((is.na(Crude_Rate_C)|Crude_Rate_C=='Unreliable')
                                                             & (is.na(Crude_Rate_State)|Crude_Rate_State=='Unreliable')
                                                             & (is.na(Crude_Rate_CR)|Crude_Rate_CR=='Unreliable')
                                                             & (is.na(Crude_Rate_S)|Crude_Rate_S=='Unreliable'),
                                                             "Crude_Rate_N",
                                                             ifelse((is.na(Crude_Rate_C)|Crude_Rate_C=='Unreliable')
                                                                    & (is.na(Crude_Rate_State)|Crude_Rate_State=='Unreliable')
                                                                    & (is.na(Crude_Rate_CR)|Crude_Rate_CR=='Unreliable'),
                                                                    "Crude_Rate_S",
                                                                    ifelse((is.na(Crude_Rate_C)|Crude_Rate_C=='Unreliable')
                                                                           & (is.na(Crude_Rate_State)|Crude_Rate_State=='Unreliable'),
                                                                           "Crude_Rate_CR",
                                                                           ifelse((is.na(Crude_Rate_C)|Crude_Rate_C=='Unreliable'),
                                                                                  "Crude_Rate_State","Crude_Rate_C")))))

nrow(df.County_State_CR_Urb_Merge[df.County_State_CR_Urb_Merge$Crude_Rate_Final_Decision == "Crude_Rate_N",]) # 50 - national URB estimate
nrow(df.County_State_CR_Urb_Merge[df.County_State_CR_Urb_Merge$Crude_Rate_Final_Decision == "Crude_Rate_S",]) # 105 - state estimate
nrow(df.County_State_CR_Urb_Merge[df.County_State_CR_Urb_Merge$Crude_Rate_Final_Decision == "Crude_Rate_CR",]) # 11540 - CR URB estimate
nrow(df.County_State_CR_Urb_Merge[df.County_State_CR_Urb_Merge$Crude_Rate_Final_Decision == "Crude_Rate_State",]) # 36111 - state URB estimate
nrow(df.County_State_CR_Urb_Merge[df.County_State_CR_Urb_Merge$Crude_Rate_Final_Decision == "Crude_Rate_C",]) # 8156 - county URB estimate


nrow(df.County_State_CR_Urb_Merge[!(is.na(df.County_State_CR_Urb_Merge$Crude_Rate_C)) &
                                    !(df.County_State_CR_Urb_Merge$Crude_Rate_C == "Unreliable"), ])

###############################################################################
# PART 6: Distribute the annual suicide rates by month using national data
###############################################################################

## Grab necessary columns from df.County_State_CR_Merge

ann_inc <- df.County_State_CR_Urb_Merge %>% 
  select(c("Census_Region","State","State_Abbr","fips","County","Popn_C","Gender","Ten_Year_Age_Groups","Crude_Rate_Final"))

ann_inc$Crude_Rate_Final <- as.numeric(ann_inc$Crude_Rate_Final)

# Note: census region tot popn within df.County_State_CR_Urb_Merge represents total CR popn per urbanization category

## National analysis
# Underlying Cause of Death, 1999-2019_MONTH_NATL.txt

temp_ucod_20yr_natl_mo_county <- read.delim(paste0(in_path,"Underlying Cause of Death, 1999-2019_MONTH_NATL.txt"),
                                            colClasses = c("NULL",rep("character",6),"numeric",rep("NULL",2)))
ucod_20yr_natl_mo_county  <- filter(temp_ucod_20yr_natl_mo_county,!is.na(Deaths)&!(Ten.Year.Age.Groups=="Not Stated")) %>%
  select(c("Month","Ten.Year.Age.Groups","Gender","Deaths"))
names(ucod_20yr_natl_mo_county) <- gsub(".", "_", names(ucod_20yr_natl_mo_county), fixed = TRUE)

ucod_20yr_natl_mo_county$MO <- with(ucod_20yr_natl_mo_county, left(ucod_20yr_natl_mo_county$Month, 3))
ucod_20yr_natl_mo_county$Year <- as.integer(with(ucod_20yr_natl_mo_county, right(ucod_20yr_natl_mo_county$Month, 4)))

dim(ucod_20yr_natl_mo_county) # 4536

## Grab national populations per sex and gender
# Underlying Cause of Death, 1999-2019_YEAR.txt
temp_ucod_20yr_natl_year <- read.delim(paste0(in_path,"Underlying Cause of Death, 1999-2019_YEAR.txt"))
ucod_20yr_natl_year  <- filter(temp_ucod_20yr_natl_year,!is.na(Year)&!(Ten.Year.Age.Groups=="Not Stated")) %>%
  select(c("Year","Ten.Year.Age.Groups","Gender","Population"))
names(ucod_20yr_natl_year) <- gsub(".", "_", names(ucod_20yr_natl_year), fixed = TRUE)

ucod_20yr_natl_mo_yr_county <- merge(ucod_20yr_natl_mo_county,ucod_20yr_natl_year,all.x=TRUE)

dim(ucod_20yr_natl_mo_yr_county) # 4536 - OK

ucod_20yr_natl_mo_yr_county$Population <- as.numeric(ucod_20yr_natl_mo_yr_county$Population)

# Check number of months listed per year-age-sex

mo_check_natl <- ucod_20yr_natl_mo_yr_county %>% select(c("Ten_Year_Age_Groups":"Year","MO")) %>%
  group_by(Ten_Year_Age_Groups,Gender,Year) %>%
  summarise(count = length(unique(MO)))

## QA Note: I attempted to subset the above CDC data to exclude AK and HI, but when I did this,
# month counts were < 12 for some years. Therefore, sticking with national distribution including AK and HI

# Confirmed - count = 12 months for all year-age-sex combinations

# QA: 
tot_popn_1999_2019 <- sum(ucod_20yr_natl_mo_yr_county$Population[ucod_20yr_natl_mo_yr_county$MO=="Jan"])
tot_deaths_1999_2019 <- sum(ucod_20yr_natl_mo_yr_county$Deaths) 
tot_deaths_1999_2019/tot_popn_1999_2019 # 0.0001318822 - very close to CDC report (0.0001233517); 
# differences likely due to absence of HI and AK in the ucod_20yr_natl_mo_yr_county DF

natl_monthly_distrib <- ucod_20yr_natl_mo_yr_county %>%
  select(c("Ten_Year_Age_Groups":"Gender","Deaths":"Population")) %>%
  group_by(Ten_Year_Age_Groups,Gender,MO) %>%
  summarise(mo_dist = sum(Deaths)/sum(Population))

natl_distrib <- ucod_20yr_natl_mo_yr_county %>%
  select(c("Ten_Year_Age_Groups":"Gender","Deaths":"Population")) %>%
  group_by(Ten_Year_Age_Groups,Gender) %>%
  summarise(
    popn_total_age_gender = sum(Population[MO=="Jan"]), # Doing this because total popn per year repeated for each month
    overall_dist = sum(Deaths)/sum(popn_total_age_gender))

# Perform QA calcs in excel

# write.csv(ucod_20yr_natl_mo_yr_county, paste0(out_path,"QA/QA_DistribCalcs.csv"),row.names = FALSE)
# write.csv(ann_inc, paste0(out_path,"QA/QA_ann_inc.csv"),row.names = FALSE)

## Distribute annual incidence values using monthly values

temp_mo_inc <- merge(merge(ann_inc,natl_monthly_distrib, by=c("Ten_Year_Age_Groups","Gender"),all.x=TRUE),
                natl_distrib, by=c("Ten_Year_Age_Groups","Gender"),all.x=TRUE)

dim(ann_inc) # 55962     
dim(temp_mo_inc) # Should be 12*dim(ann_inc) = 12*55962 = 671544 - OK

temp_mo_inc$Crude_Rate_Monthly <- with(temp_mo_inc,Crude_Rate_Final*(mo_dist/overall_dist))

# QA notes: 
# Overall rate is 0.00004048157 for females age 15-24
# CDC overall rate for females age 15-24 is 0.00004048157
# May need to caveat that our overall rate includes AK and HI

## QA the Crude_Rate_Monthly calcs
# Sum on Crude_Rate_Monthly per state-county-age-sex should match Crude_Rate_Final
mo_inc_QA <- temp_mo_inc %>% select(c("State","fips","Ten_Year_Age_Groups","Gender","Crude_Rate_Final","Crude_Rate_Monthly")) %>%
  group_by(State,fips,Ten_Year_Age_Groups,Gender,Crude_Rate_Final) %>%
  summarise_all(sum)

mo_inc_QA$QA_Check <- with(mo_inc_QA,Crude_Rate_Final-Crude_Rate_Monthly)
min(mo_inc_QA$QA_Check) # -0.00000000000002842171
max(mo_inc_QA$QA_Check) # 0.00000000000001421085
# Precision differences - OK

mo_inc_month_QA <- temp_mo_inc %>% select(c("State","fips","Ten_Year_Age_Groups","Gender","MO")) %>%
  group_by(State,fips,Ten_Year_Age_Groups,Gender) %>%
  summarise(count=length(unique(MO)))

# Confirmed - count = 12 months for all year-age-sex combinations

## Select columns of interest and write to file
mo_inc <- temp_mo_inc %>% select(c("State","State_Abbr","fips","Ten_Year_Age_Groups","Gender","MO","Crude_Rate_Monthly"))

names(mo_inc) <- c("state","state_Abbr","fips","agegrp","sex","month","inc_rate_monthly_per100000")

mo_inc$fips <- str_pad(as.character(mo_inc$fips),width=5,pad="0")

# Shannon county (FIPS 46113) changed to Oglala Lakota county (FIPS 46102) in 2015
mo_inc$fips <- with(mo_inc,ifelse(fips==46113,46102,fips))

write.csv(mo_inc, paste0(out_path,"mo_dist_suicide_rates_1999_2019.csv"),row.names = FALSE)

## QA

length(unique(mo_inc$fips)) # 3109
length(unique(unique_fips$fips)) # 3109 - OK

mo_inc_qa <- mo_inc %>% select(c("fips","agegrp","sex")) %>% 
  group_by(fips) %>% 
  summarise(
    agegrp_count = length(unique(agegrp)),
    sex_count = length(unique(sex))
  )

mo_inc_qa2 <- mo_inc %>% select(c("fips","agegrp","sex")) 
mo_inc_qa2$agegrp_sex <- with(mo_inc_qa2,paste0(agegrp,sex))

mo_inc_qa3 <- mo_inc_qa2 %>% select(c("fips","agegrp_sex")) %>% 
  group_by(fips) %>% 
  summarise(
    agegrp_sex_count = length(unique(agegrp_sex))
  )
