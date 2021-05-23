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
    "tidyverse",
    "ggrepel",
    "seasonal"
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
  group_by(date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Melbourne")

brisdata  <-
  filter(LabData, SA4 %in% brisregs)   %>%
  group_by(date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Brisbane")

adeldata  <-
  filter(LabData, SA4 %in% adelregs)   %>%
  group_by(date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Adelaide")

perthdata <-
  filter(LabData, SA4 %in% perthregs)  %>%
  group_by(date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Perth")

otherdata <-
  filter(LabData, SA4 %in% othregs)    %>%
  group_by(date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Other")

Ausdata <- LabData %>%
  group_by(date, Variable) %>%
  summarise(Value = sum(Value)) %>%
  mutate(SA4 = "Australia")


LabData_o  <-
  LabData %>% 
  select(!Sex) %>% 
  ungroup() %>%
  bind_rows(syddata) %>%  
  bind_rows(melbdata) %>%
  bind_rows(brisdata) %>%
  bind_rows(adeldata) %>%
  bind_rows(perthdata) %>%
  arrange(SA4) %>%
  filter(!SA4 %in% sydregs) %>%
  filter(!SA4 %in% melbregs) %>%
  filter(!SA4 %in% brisregs) %>%
  filter(!SA4 %in% adelregs) %>%
  filter(!SA4 %in% perthregs) %>% 
  
  filter(date <= "2019-12-01") %>% 
  
  spread(Variable, Value) %>% 
  mutate(Part_rate = 100*`Labour force total`/CivPop,
         Unemployment_rate = 100*`Unemployed total`/`Labour force total`) %>% 
  mutate(Date = date) %>% 
  select(!date) %>% 
  gather(Variable, Value, -SA4, -Date)


# Seasonal adjustment (note takes time)

LabData  <-
  LabData %>% 
  select(!Sex) %>% 
  ungroup() %>%
  bind_rows(syddata) %>%  
  bind_rows(melbdata) %>%
  bind_rows(brisdata) %>%
  bind_rows(adeldata) %>%
  bind_rows(perthdata) %>%
  arrange(SA4) %>%
  filter(!SA4 %in% sydregs) %>%
  filter(!SA4 %in% melbregs) %>%
  filter(!SA4 %in% brisregs) %>%
  filter(!SA4 %in% adelregs) %>%
  filter(!SA4 %in% perthregs) %>% 
  
  filter(date <= "2019-12-01") %>% 

  group_by(SA4,Variable) %>% 
  mutate(Value = final(seas(ts(Value, frequency = 12, start = c(1998,10), end = c(2019,12) ) ))) %>% 

  spread(Variable, Value) %>% 
  mutate(Part_rate = 100*`Labour force total`/CivPop,
         Unemployment_rate = 100*`Unemployed total`/`Labour force total`) %>% 
  mutate(Date = date) %>% 
  select(!date) %>% 
  gather(Variable, Value, -SA4, -Date)




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


#--------------------------------------------------------------------------------------------#
#### 3. Data visualisation                                                                ####
#--------------------------------------------------------------------------------------------#

studyregs    <-
  c(
    "Adelaide",
    "Townsville",
    "Bunbury",
    "Tasmania - West and North West"
  )

# Seperate out Australian employment growth 
AusLabEmp <- LabData %>%
  group_by(SA4, year(Date)) %>%
  filter(SA4 == "Australia") %>% 
  filter(Variable ==  "Employed total") %>% 
  summarise(val2 = mean(Value)) %>%
  mutate(val2 = val2 / lag(val2) * 100 - 100) 

# Seperate out Australian unemployment
AusLabUnemp <- LabData %>%
  spread(Variable, Value) %>%
  filter(SA4 == "Australia") %>%
  gather(key = "Variable",
         value = "Value",
         -`SA4`,-`Date`) %>%
  group_by(year(Date), SA4) %>%
  filter(Variable == "Unemployment_rate") %>%
  summarise(val2 = mean(Value))

# Seperate out Australian participation
AusLabPart <- LabData %>%
  spread(Variable, Value) %>%
   filter(SA4 == "Australia") %>%
  gather(key = "Variable",
         value = "Value",
         -`SA4`,-`Date`) %>%
  group_by(year(Date), SA4) %>%
  filter(Variable == "Part_rate") %>%
  summarise(val2 = mean(Value))

# Seperate out Australian population
AusLabPop <- LabData %>% 
  group_by(SA4, year(Date)) %>% 
  filter(Variable == "CivPop") %>%
  filter(SA4 == "Australia") %>%
  summarise(val2 = mean(Value)) %>%
  mutate(val2 = val2 / lag(val2) * 100 - 100) 


# Chart 4.a Emp growth rate 
LabData %>% 
  group_by(SA4, year(Date)) %>%
  filter(Variable == "Employed total") %>%
  filter(SA4 != "Australia") %>%
  summarise(val2 = mean(Value)) %>%
  mutate(val2 = val2 / lag(val2) * 100 - 100) %>%
  filter(`year(Date)` != c("1998")) %>%
  left_join(AusLabEmp, by = "year(Date)") %>%
  mutate(val3 = val2.x - val2.y) %>% 
  dplyr::select(c(1,2,6)) %>%
  ungroup() %>%
  mutate(decade = ifelse(`year(Date)` %in% 1998:2008, 1, 0)) %>% 
  group_by(SA4.x, decade) %>%
  dplyr::select(-c(`year(Date)`)) %>%
  summarise(val3 = mean(val3, na.rm = T)) %>%
  mutate(studyregion = ifelse(`SA4.x` %in% studyregs, 1, 0)) %>%
  spread(decade, val3) %>%
  ggplot(aes(x = `1`, y = `0`)) + 
  geom_point(aes(col = studyregion), size = 3) + 
  stat_smooth() + 
  geom_text_repel(aes(label = SA4.x, col = studyregion), size = 3) +
  theme_bw() +
  theme(legend.position = "none", axis.text = element_text(size = 10), axis.title = element_blank()) +
  ylim(-5, 5) + xlim(-5, 5) +
  geom_abline(intercept = 0, linetype = "dotted")

# Chart 4.b Unemp rate 
LabData %>%
  group_by(year(Date), SA4) %>% 
  filter(Variable == "Unemployment_rate") %>%
  summarise(val2 = mean(Value)) %>%
  left_join(AusLabUnemp, by = "year(Date)") %>% 
  mutate(val3 = val2.x - val2.y) %>% 
  dplyr::select(c(1,2,6)) %>%
  ungroup() %>% 
  mutate(decade = ifelse(`year(Date)` %in% 1998:2008, 1, 0)) %>%
  group_by(SA4.x, decade) %>%
  summarise(val3 = mean(val3)) %>% 
  spread(decade, val3) %>%
  mutate(studyregion = ifelse(`SA4.x` %in% studyregs, 1, 0)) %>%
  ggplot(aes(x = `1`, y = `0`)) +
  geom_point(aes(col = studyregion), size = 3) +
  stat_smooth() + 
  geom_text_repel(aes(label = SA4.x, col = studyregion), size = 3) + 
  theme_bw() + 
  theme(
    legend.position = "none", axis.text = element_text(size = 10), axis.title = element_blank()) +
  ylim(-3, 5) + xlim(-3, 5) +
  geom_abline(intercept = 0, linetype = "dotted")

# Chart 4.c Part rate 
LabData %>%
  group_by(year(Date), SA4) %>% 
  filter(Variable == "Part_rate") %>%
  summarise(val2 = mean(Value)) %>%
  left_join(AusLabPart, by = "year(Date)") %>%
  mutate(val3 = val2.x - val2.y) %>% 
  dplyr::select(c(1,2,6)) %>%
  ungroup() %>% 
  mutate(decade = ifelse(`year(Date)` %in% 1998:2008, 1, 0)) %>% 
  group_by(SA4.x, decade) %>%
  summarise(val3 = mean(val3)) %>%
  spread(decade, val3) %>%
  mutate(studyregion = ifelse(`SA4.x` %in% studyregs, 1, 0)) %>% 
  ggplot(aes(x = `1`, y = `0`)) +
  geom_point(aes(col = studyregion), size = 3) + 
  stat_smooth() +
  geom_text_repel(aes(label = SA4.x, col = studyregion), size = 3) +
  theme_bw() +
  theme(legend.position = "none", axis.text = element_text(size = 10), axis.title = element_blank()) +
  geom_abline(intercept = 0, linetype = "dotted")


# Chart 3.d Population growth rate 
LabData %>%
  group_by(SA4, year(Date)) %>% 
  filter(Variable == "CivPop") %>%
  filter(SA4 != "Australia") %>% 
  summarise(val2 = mean(Value)) %>% 
  mutate(val2 = val2 / lag(val2) * 100 - 100) %>%
  filter(`year(Date)` != c("1998")) %>%
  left_join(AusLabEmp, by = "year(Date)") %>%
  mutate(val3 = val2.x - val2.y) %>% 
  dplyr::select(c(1,2,6)) %>% 
  ungroup() %>%
  mutate(decade = ifelse(`year(Date)` %in% 1998:2008, 1, 0)) %>% 
  group_by(SA4.x, decade) %>% 
  dplyr::select(-c(`year(Date)`)) %>% 
  summarise(val3 = mean(val3, na.rm = T)) %>%
  mutate(studyregion = ifelse(`SA4.x` %in% studyregs, 1, 0)) %>%
  spread(decade, val3) %>%
  ggplot(aes(x = `1`, y = `0`)) +
  geom_point(aes(col = studyregion), size = 3) + 
  stat_smooth() +
  geom_text_repel(aes(label = SA4.x, col = studyregion), size = 3) +
  theme_bw() +
  theme(legend.position = "none", axis.text = element_text(size = 10), axis.title = element_blank()) +
  ylim(-2.5, 2) + xlim(-2.5, 2) + 
  geom_abline(intercept = 0, linetype = "dotted")



#--------------------------------------------------------------------------------------------#
#### 3. Variable creation and statistical tests                                             ###
#--------------------------------------------------------------------------------------------#

# Here we calculate the three main variables for analysis and determine their order of integration
# a)    delta_e = the first difference of the log of employment in reg_i less that of Australia
# b)    log_e   = the log of the ratio EMP/LFORCE in reg_i less that of Australia
# c)    log_p   = the log of the ratio of LFORCE/WAP in reg_i less that of Australia


LabData %>%
  group_by(SA4, lubridate::quarter(Date, with_year = TRUE)) %>%
  filter(Variable == "Employed total") %>%
  rename(quarter1 = "lubridate::quarter(Date, with_year = TRUE)") %>% 
  summarise(val2 = mean(Value)) %>%
  mutate(val2 = log(val2) - log(lag(val2))) %>%
  ungroup() %>%
  mutate(AU = rep(filter(., SA4 == "Australia")$val2, 74)) %>% # 56 is the number of unique regions
  mutate(delta_e = val2 - AU) %>% 
  dplyr::select(-val2,-AU) %>%
  filter(SA4 != "Australia" & !is.na(delta_e)) %>%
  filter(SA4 %in% studyregs) %>% 
  group_by(SA4) %>% 
  mutate(Date1 = seq(as.Date("1999-03-01"), as.Date("2019-12-01"), by = "quarter")) %>% 
  ggplot(aes(as.Date(Date1), delta_e)) +
  geom_line() +
  facet_wrap( ~ SA4, scales = "free") +
  ggtitle("delta_e")


delta_e <- LabData %>% 
  group_by(SA4, lubridate::quarter(Date, with_year = TRUE)) %>%
  filter(Variable == "Employed total") %>%
  rename(quarter1 = "lubridate::quarter(Date, with_year = TRUE)") %>% 
  summarise(val2 = mean(Value)) %>%
  mutate(val2 = log(val2) - log(lag(val2))) %>%
  ungroup() %>%
  mutate(AU = rep(filter(., SA4 == "Australia")$val2, 74)) %>% # 56 is the number of unique regions
  mutate(delta_e = val2 - AU) %>% 
  dplyr::select(-val2,-AU) %>%
  filter(SA4 != "Australia" & !is.na(delta_e)) %>%
  filter(SA4 %in% studyregs) %>% 
  group_by(SA4) %>% 
  mutate(Date = seq(as.Date("1999-03-01"), as.Date("2019-12-01"), by = "quarter")) # need a balanced amount of quarters in the range


delta_e_ur <- lapply(delta_e %>% 
                       dplyr::select(-`quarter1`) %>% 
                       spread(SA4,delta_e) %>% 
                       dplyr::select(-Date1), function(x){
                         
                         list(
                           
                           ADF =x %>% 
                             ts(start = c(1999,1), f =4) %>% 
                             ur.df(type = "drift", selectlags = "AIC"),
                           
                           DFGLS = x %>% 
                             ts(start = c(1999,1), f =4) %>% 
                             ur.ers(type ="DF-GLS",model = "const",lag.max = 4),
                           
                           KPSS = x %>% 
                             ts(start = c(1999,1), f =4) %>% 
                             ur.kpss(type ="mu",use.lag = 4)
                         )
                         
                         
                         
                         
                         
                       })


ur_test_delta_e <- delta_e_ur %>% 
  lapply(function(x){
    
    sapply(x, function(x){
      
      y <- tibble(`test stat` = NA,
                  `crit val-5%` =NA
      )
      
      
      y[["test stat"]] <-x@teststat[1]
      
      y[["crit val-5%"]] <- x@cval[1,2]
      
      
      
      return(y)
      
    } ) %>% 
      data.frame()
    
    
  }) %>%
  do.call("rbind",.) %>% 
  data.frame() %>% 
  mutate(reg1 = row.names(.)) %>% 
  mutate(stat_type = if_else(grepl("test stat",.$reg1),"test stat", "critical value 5%"),
         reg1 =gsub("\\..*","",.$reg1)) %>% 
  gather(test, value, -reg1, -stat_type) %>% 
  spread(stat_type, value) %>% 
  mutate(Hypothesis_test = if_else(abs(as.numeric(`test stat`)) > as.numeric(`critical value 5%`),
                                   "reject H0",
                                   "do not reject H0"))

delta_e_URregions <- ur_test_delta_e %>% 
  filter(reg1 %in% studyregs)



LabData %>% 
  spread(Variable, Value) %>%
  mutate(log_e = 100 * (`Employed total` / `Labour force total`)) %>%
  gather(key = "Variable", value = "Value",-`SA4`,-`Date`) %>%
  group_by(SA4, lubridate::quarter(Date, with_year = TRUE)) %>%
  filter(Variable == "log_e") %>%
  rename(quarter1 = "lubridate::quarter(Date, with_year = TRUE)") %>%
  summarise(val2 = mean(Value)) %>%
  group_by(SA4) %>% 
  ungroup()  %>% 
  mutate(AU = rep(filter(., SA4 == "Australia")$val2, 74)) %>% # 56 is the number of unique regions
  filter(SA4 %in% studyregs) %>% 
  mutate(log_e = log(val2 / AU)) %>%
  dplyr::select(-val2,-AU) %>%
  filter(SA4 != "Australia" & !is.na(log_e)) %>%
  group_by(SA4) %>% 
  mutate(Date = seq(as.Date("1998-12-01"), as.Date("2019-12-01"), by = "quarter")) %>% 
  ggplot(aes(Date, log_e)) + 
  geom_line() + 
  facet_wrap( ~ SA4, scales = "free") + 
  ggtitle("log_e")

log_e <- LabData %>% 
  spread(Variable, Value) %>%
  mutate(log_e = 100 * (`Employed total` / `Labour force total`)) %>%
  gather(key = "Variable", value = "Value",-`SA4`,-`Date`) %>%
  group_by(SA4, lubridate::quarter(Date, with_year = TRUE)) %>%
  filter(Variable == "log_e") %>%
  rename(quarter1 = "lubridate::quarter(Date, with_year = TRUE)") %>%
  summarise(val2 = mean(Value)) %>%
  group_by(SA4) %>% 
  ungroup()  %>% 
  mutate(AU = rep(filter(., SA4 == "Australia")$val2, 74)) %>% # 56 is the number of unique regions
  filter(SA4 %in% studyregs) %>% 
  mutate(log_e = log(val2 / AU)) %>%
  dplyr::select(-val2,-AU) %>%
  filter(SA4 != "Australia" & !is.na(log_e)) %>%
  group_by(SA4) %>% 
  mutate(Date = seq(as.Date("1998-12-01"), as.Date("2019-12-01"), by = "quarter"))


log_e_ur <- lapply(log_e %>% 
                     dplyr::select(-`quarter1`) %>% 
                     spread(SA4,log_e) %>% 
                     dplyr::select(-Date), function(x){
                       
                       list(
                         
                         ADF =x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.df(type = "drift", selectlags = "AIC"),
                         
                         DFGLS = x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.ers(type ="DF-GLS",model = "const",lag.max = 4),
                         
                         KPSS = x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.kpss(type ="mu",use.lag = 4)
                       )
                       
                       
                       
                       
                       
                     })


ur_test_log_e <- log_e_ur %>% 
  lapply(function(x){
    
    sapply(x, function(x){
      
      y <- tibble(`test stat` = NA,
                  `crit val-5%` =NA
      )
      
      
      y[["test stat"]] <-x@teststat[1]
      
      y[["crit val-5%"]] <- x@cval[1,2]
      
      
      
      return(y)
      
    } ) %>% 
      data.frame()
    
    
  }) %>%
  do.call("rbind",.) %>% 
  data.frame() %>% 
  mutate(reg1 = row.names(.)) %>% 
  mutate(stat_type = if_else(grepl("test stat",.$reg1),"test stat", "critical value 5%"),
         reg1 =gsub("\\..*","",.$reg1)) %>% 
  gather(test, value, -reg1, -stat_type) %>% 
  spread(stat_type, value) %>% 
  mutate(Hypothesis_test = if_else(abs(as.numeric(`test stat`)) > as.numeric(`critical value 5%`),
                                   "reject H0",
                                   "do not reject H0"))


logeURregions <- ur_test_log_e %>% 
  filter(reg1 %in% studyregs)

# West and North West non-statioanry based on KPSS test

# Chart 4.c log_p 

LabData   %>% 
  spread(Variable, Value) %>%
  mutate(log_p = 100 * (`Labour force total`/CivPop)) %>%
  gather(key = "Variable", value = "Value",-`SA4`,-`Date`) %>%
  group_by(SA4, lubridate::quarter(Date, with_year = TRUE)) %>%
  filter(Variable == "log_p") %>%
  rename(quarter1 = "lubridate::quarter(Date, with_year = TRUE)") %>%
  summarise(val2 = mean(Value))%>%
  ungroup() %>%
  mutate(AU = rep(filter(., SA4 == "Australia")$val2, 74)) %>% # 56 is the number of unique regions
  mutate(log_p = log(val2 / AU)) %>%
  dplyr::select(-val2,-AU) %>%
  filter(SA4 != "Australia" & !is.na(log_p)) %>%
  filter(SA4 %in% studyregs) %>% 
  group_by(SA4) %>% 
  mutate(Date = seq(as.Date("1998-10-01"), as.Date("2019-10-01"), by = "quarter")) %>% 
  ggplot(aes(Date, log_p)) + 
  geom_line() + 
  facet_wrap( ~ SA4, scales = "free") + ggtitle("log_p")

log_p <-  LabData %>% 
  spread(Variable, Value) %>%
  mutate(log_p = 100 * (`Labour force total`/CivPop)) %>%
  gather(key = "Variable", value = "Value",-`SA4`,-`Date`) %>%
  group_by(SA4, lubridate::quarter(Date, with_year = TRUE)) %>%
  filter(Variable == "log_p") %>%
  rename(quarter1 = "lubridate::quarter(Date, with_year = TRUE)") %>%
  summarise(val2 = mean(Value))%>%
  ungroup() %>%
  mutate(AU = rep(filter(., SA4 == "Australia")$val2, 74)) %>% # 56 is the number of unique regions
  mutate(log_p = log(val2 / AU)) %>%
  dplyr::select(-val2,-AU) %>%
  filter(SA4 != "Australia" & !is.na(log_p)) %>%
  filter(SA4 %in% studyregs) %>% 
  group_by(SA4) %>% 
  mutate(Date = seq(as.Date("1998-12-01"), as.Date("2019-12-01"), by = "quarter"))

log_p_ur <- lapply(log_p %>% 
                     dplyr::select(-`quarter1`) %>% 
                     spread(SA4,log_p) %>% 
                     dplyr::select(-Date), function(x){
                       
                       list(
                         
                         ADF =x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.df(type = "drift", selectlags = "AIC"),
                         
                         DFGLS = x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.ers(type ="DF-GLS",model = "const",lag.max = 4),
                         
                         KPSS = x %>% 
                           ts(start = c(1999,1), f =4) %>% 
                           ur.kpss(type ="mu", use.lag = 4)
                       )
                       
                       
                       
                       
                       
                     })


ur_test_log_p <- log_p_ur %>% 
  lapply(function(x){
    
    sapply(x, function(x){
      
      y <- tibble(`test stat` = NA,
                  `crit val-5%` =NA
      )
      
      
      y[["test stat"]] <-x@teststat[1]
      
      y[["crit val-5%"]] <- x@cval[1,2]
      
      
      
      return(y)
      
    } ) %>% 
      data.frame()
    
    
  }) %>%
  do.call("rbind",.) %>% 
  data.frame() %>% 
  mutate(reg1 = row.names(.)) %>% 
  mutate(stat_type = if_else(grepl("test stat",.$reg1),"test stat", "critical value 5%"),
         reg1 =gsub("\\..*","",.$reg1)) %>% 
  gather(test, value, -reg1, -stat_type) %>% 
  spread(stat_type, value) %>% 
  mutate(Hypothesis_test = if_else(abs(as.numeric(`test stat`)) > as.numeric(`critical value 5%`),
                                   "reject H0",
                                   "do not reject H0"))

logpURregions <- ur_test_log_p %>% 
  filter(reg1 %in% studyregs)

#--------------------------------------------------------------------------------------------#
#### 5. SVAR                                                                              ####
#--------------------------------------------------------------------------------------------#

Var.data <- left_join(delta_e,
                      log_e) %>% 
  left_join(log_p) %>% 
  dplyr::select(-quarter1) %>% 
  dplyr::select(Date,
                SA4,
                everything())


Bmat <- matrix(c(NA,0,0,NA,NA,0,NA,0,NA),ncol = 3, byrow = TRUE)


Varlist <- list()
for(i in seq_along(unique(Var.data$SA4))){
  
  
  Varlist[[paste(unique(Var.data$SA4)[i])]][["plot"]] <- Var.data %>% 
    filter(SA4 == unique(Var.data$SA4)[i] ) %>%
    gather(Variable, Value, -Date,-SA4) %>%
    rename(Variable = Variable) %>% 
    ggplot(aes(x = Date, y = Value)) + 
    geom_line(aes(colour = Variable)) +
    scale_colour_manual(values = c("Black","Red","Blue"))+
    #  theme_classic()+
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 20),
      legend.position = "bottom",
      axis.text = element_text(size = 20),
      axis.title = element_blank()
    )
  
  
  
  dat <- Var.data %>% 
    filter(SA4 == unique(Var.data$SA4)[i]) %>% 
    ungroup() %>% 
    dplyr::select(delta_e,log_e,log_p) %>% 
    ts(f = 4)
  
  Varlist[[paste(unique(Var.data$SA4)[i])]][["VAR"]] <- VAR(dat, type = "const", lag.max = 8, ic = "AIC")
  
  Varlist[[paste(unique(Var.data$SA4)[i])]][["summary and test"]]$summary <- Varlist[[paste(unique(Var.data$SA4)[i])]][["VAR"]] %>% summary()  
  
  Varlist[[paste(unique(Var.data$SA4)[i])]][["summary and test"]]$PortACtest <- Varlist[[paste(unique(Var.data$SA4)[i])]][["VAR"]] %>% serial.test()
  
  Varlist[[paste(unique(Var.data$SA4)[i])]][["summary and test"]]$JBnormtest <- Varlist[[paste(unique(Var.data$SA4)[i])]][["VAR"]] %>% normality.test()
  
  Varlist[[paste(unique(Var.data$SA4)[i])]][["summary and test"]]$roots <- Varlist[[paste(unique(Var.data$SA4)[i])]][["VAR"]] %>% roots(modulus = TRUE)
  
  
  if(max(Varlist[[paste(unique(Var.data$SA4)[i])]][["VAR"]][["summary and test"]][["roots"]]$roots) >= 1){
    
    Varlist[[paste(unique(Var.data$SA4)[i])]][["summary and test"]]$DynamicStab <-  "Not stable - check"
    
  }else{
    
    Varlist[[paste(unique(Var.data$SA4)[i])]][["summary and test"]]$DynamicStab <-  "VAR is stable"
    
    
  }
  
  Varlist[[paste(unique(Var.data$SA4)[i])]][["irf chol"]]$data <- Varlist[[paste(unique(Var.data$SA4)[i])]][["VAR"]] %>% irf(n.ahead = 20)
  
  Varlist[[paste(unique(Var.data$SA4)[i])]][["svar"]]<- Varlist[[paste(unique(Var.data$SA4)[i])]][["VAR"]] %>% SVAR(Bmat = Bmat)
  
  Varlist[[paste(unique(Var.data$SA4)[i])]][["svar irf"]]$data <- Varlist[[paste(unique(Var.data$SA4)[i])]][["VAR"]] %>% SVAR(Bmat = Bmat) %>% irf(n.ahead = 20)
  
  
}




         