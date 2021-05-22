#--------------------------------------------------------------------------------------------#
#### Projections of regional labour market adjustment Australia from a VAR model          ####
#--------------------------------------------------------------------------------------------#

# This code allows the user to download and clean the latest ABS data for our paper
# "Projections of regional labour market adjustment Australia from a vector autogression model"
# Rebecca Colquhoun & Cedric Hodges & Adam Elderfield

#--------------------------------------------------------------------------------------------#
#### Contents                                                                             ####
#--------------------------------------------------------------------------------------------#

# 1. Preliminaries
# 2. Downloading [NB: can be skipped if you already have the data]
# 3. Cleaning
# 4. Smoothing

#--------------------------------------------------------------------------------------------#
#### 1. Preliminaries                                                                     ####
#--------------------------------------------------------------------------------------------#


# Make sure required packages are installed/loaded

rm(list = ls())
required_packages <-
  c(
    "readabs",
    "tidyr",
    "tidyverse"
  )

lapply(required_packages, require, character.only = TRUE)

rm(required_packages)


#--------------------------------------------------------------------------------------------#
#### 2. Download data  and aggregate                                                      ####
#--------------------------------------------------------------------------------------------#

# Aquire data from ABS
Detailed_labour_force <- read_abs(cat_no = "6291.0.55.001",tables = "16")

# Clean "series" column  - "> New South Wales ;  Employed total ;  Persons ;" split to "Region" "Variable" "Sex" 
LabData <- Detailed_labour_force %>% 
  separate(series, into = c("SA4","Variable","Sex"), sep = ";") %>% 
  mutate(SA4 = gsub(">","",.$SA4)) %>% 
  mutate(SA4 = trimws(SA4)) %>% 
  mutate(Variable = trimws(Variable)) %>%
  mutate(Sex = trimws(Sex)) 

# Create Civilian population by scaling up employment by the employment to population ratio

LabData <- LabData %>%
  select(date, SA4, Sex, value, Variable) %>% 
  spread(Variable,value) %>% 
  mutate(CivPop = `Employed total`/(`Employment to population ratio`/100)) %>%
  select(date, SA4, Sex, `Employed total`,`Labour force total`,
        `Unemployed total`,`CivPop`) %>% 
  gather(Variable, Value, -date, -Sex, -SA4 ) %>% 
  filter(Sex == "Persons")




sydregs     <-
  c(
    "Sydney - Baulkham Hills and Hawkesbury",
    "Sydney - Blacktown",
    "Sydney - City and Inner South",
    "Sydney - Eastern Suburbs",
    "Sydney - Inner South West",
    "Sydney - Inner West",
    "Sydney - North Sydney and Hornsby",
    "Sydney - Northern Beaches",
    "Sydney - Outer South West",
    "Sydney - Outer West and Blue Mountains",
    "Sydney - Parramatta",
    "Sydney - Ryde",
    "Sydney - South West",
    "Sydney - Sutherland"
  )

melbregs    <-
  c(
    "Melbourne - Inner",
    "Melbourne - Inner East",
    "Melbourne - Inner South",
    "Melbourne - North East",
    "Melbourne - North West",
    "Melbourne - Outer East",
    "Melbourne - South East",
    "Melbourne - West"
  )

brisregs    <-
  c(
    "Brisbane - East",
    "Brisbane - North",
    "Brisbane - South",
    "Brisbane - West",
    "Brisbane Inner City"
  )

adelregs    <-
  c("Adelaide - Central and Hills",
    "Adelaide - North",
    "Adelaide - South",
    "Adelaide - West")

perthregs   <-
  c(
    "Mandurah",
    "Perth - Inner",
    "Perth - North East",
    "Perth - North West",
    "Perth - South East",
    "Perth - South West"
  )

othregs     <-
  c(
    "No place of work SA4 (ACT)",
    "No place of work SA4 (NSW)",
    "No place of work SA4 (NTE)",
    "No place of work SA4 (QLD)",
    "No place of work SA4 (SAU)",
    "No place of work SA4 (TAS)",
    "No place of work SA4 (VIC)",
    "No place of work SA4 (WAU)"
  )


syddata   <-
  filter(LabData, SA4 %in% sydregs)    %>%
  group_by(date, Variable) %>%
  summarise(Value = sum(Value)) %>% 
  mutate(SA4 = "Sydney")

melbdata  <-
  filter(LabData, SA4 %in% melbregs)   %>%
  group_by(Date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Melbourne")

brisdata  <-
  filter(LabData, SA4 %in% brisregs)   %>%
  group_by(Date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Brisbane")

adeldata  <-
  filter(LabData, SA4 %in% adelregs)   %>%
  group_by(Date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Adelaide")

perthdata <-
  filter(LabData, SA4 %in% perthregs)  %>%
  group_by(Date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Perth")

otherdata <-
  filter(LabData, SA4 %in% othregs)    %>%
  group_by(Date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Other")

Ausdata <- LabData %>%
  group_by(Date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Australia")

LabData  <-
  LabData %>% 
  ungroup() %>%
  bind_rows(syddata) %>%  
  bind_rows(melbdata) %>%
  bind_rows(brisdata) %>%
  bind_rows(adeldata) %>%
  bind_rows(perthdata) %>%
  bind_rows(Ausdata) %>%
  arrange(SA4) %>%
  filter(!SA4 %in% sydregs) %>%
  filter(!SA4 %in% melbregs) %>%
  filter(!SA4 %in% brisregs) %>%
  filter(!SA4 %in% adelregs) %>%
  filter(!SA4 %in% perthregs)




rm(
  syddata,
  melbdata,
  brisdata,
  perthdata,
  otherdata,
  adeldata,
  Ausdata,
  adelregs,
  brisregs,
  melbregs,
  othregs,
  perthregs,
  sydregs
)




         