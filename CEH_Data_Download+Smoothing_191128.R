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
    "zoo",
    "tidyr",
    "data.table", 
    "ggplot2",
    "readxl", 
    "Hmisc", 
    "zoo", 
    "lubridate", 
    "downloader", 
    "tempdisagg", 
    "tibble", 
    "rvest", 
    "WriteXLS", 
    "rsdmx", 
    "dplyr"
  )

lapply(required_packages, require, character.only = TRUE)

rm(required_packages)


#--------------------------------------------------------------------------------------------#
#### 2. Downloading                                                                       ####
#--------------------------------------------------------------------------------------------#

#  Downloading ABS CAT 6291.0.55.001 
ABS_6291_M <- "http://www.abs.gov.au/AUSSTATS/abs@.nsf/second+level+view?ReadForm&prodno=6291.0.55.001&viewtitle=Labour%20Force,%20Australia,%20Detailed%20-%20Electronic%20Delivery~Aug%202017~Previous~21/09/2017&&tabname=Past%20Future%20Issues&prodno=6291.0.55.001&issue=Aug%202017&num=&view=&"
download.file(ABS_6291_M, destfile = paste(ABS_html_path, "6291_0_55_001.html", sep = "/"))

# Read in html and scrape month and year of most recent release information
ABS_6291_M <-read_html(paste(ABS_html_path, "6291_0_55_001.html", sep = "/")) %>%  
  html_node(xpath='//*[@id="mainpane"]/div/ul[2]/li[1]/a') %>%  html_text

ABS_6291_M_Release <- as.character(ABS_6291_M)
month <- as.character(substr(ABS_6291_M_Release, 57,59))
year <- as.character(substr(ABS_6291_M_Release, 61, 64))
ABS_6291.0.55.001_Avail <- paste(month, year, sep=" ")

# Download latest release
ABS_6291_M <- (paste("http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/6291.0.55.001", month, "%20",year, "?OpenDocument", sep=""))
download.file(ABS_6291_M, destfile = paste(ABS_html_path, "6291_0_55_001.html", sep = "/"))

# Scrape all hyperlinks
ABS_6291_M <- read_html(paste(ABS_html_path, "6291_0_55_001.html", sep = "/"))  
ABS_6291_M <- as.data.frame(html_attr(html_nodes(ABS_6291_M,"a"), "href"))

# Keep only RM1
RM1 <- ABS_6291_M %>% filter(grepl("rm1.zip&6291.0.55.001&Data", ABS_6291_M[,1]))

# Unzip downloads
Emp_url <- paste("http://www.abs.gov.au", RM1[1,1], sep="")
download(Emp_url, dest=paste(ABS_input_data_path,"RM1.zip", sep = "/"), mode ="wb")
unzip(paste(ABS_input_data_path, "/RM1.zip", sep = ""), exdir = ABS_input_data_path)

#--------------------------------------------------------------------------------------------#
#### 3. Cleaning                                                                          ####
#--------------------------------------------------------------------------------------------#

# Set up where you want the data to be stored. NB: you need to have a folder called "ABS" and a sub-folder in "ABS" called "Metadata"

Data_path <- "C:/Jobs/000000_R/CEH"
ABS_input_data_path <- paste(Data_path, "/ABS", sep = "")
ABS_html_path <- paste(Data_path, "/ABS/Metadata", sep = "")

# Import Data and clean
RM1 <- read_excel(paste(ABS_input_data_path,"RM1.xlsx",sep = "/"), sheet = "Data 1")

# Keep relevant rows, cols
RM1 <- RM1[-c(1:3),1:8]

# Rename cols
colnames(RM1) <- c("Date","Sex", "Age", "SA4_NAME_2011", "FTEmp", "PTEmp", "UnEmp", "NILF")

# Fix date col
RM1$Date <- as.Date(as.numeric(RM1$Date), origin = "1899-12-30")

# Clean Ages
RM1$Age <- gsub(" years", "", RM1$Age)

# Clean SA4 names
RM1$SA4_CODE_2011 <- gsub("([0-9][0-9][0-9]).*", "\\1", RM1$SA4_NAME_2011)
RM1$SA4_NAME_2011 <- gsub("[0-9][0-9][0-9]", "", RM1$SA4_NAME_2011)
RM1$Sex <- as.factor(RM1$Sex)
RM1$Age <- as.factor(RM1$Age)
RM1$Sex <- trimws(RM1$Sex, which = c("both"))
RM1$Age <- trimws(RM1$Age, which = c("both"))
RM1$SA4_NAME_2011 <- trimws(RM1$SA4_NAME_2011, which = c("both"))

# Rename SA4s
RM1_2011 <- RM1 %>% 
  group_by(Date, SA4_CODE_2011) %>% 
  summarise(FTEmp= sum(as.numeric(FTEmp), na.rm =TRUE), 
            PTEmp= sum(as.numeric(PTEmp), na.rm =TRUE), 
            UnEmp= sum(as.numeric(UnEmp), na.rm =TRUE), 
            NILF= sum(as.numeric(NILF), na.rm =TRUE)) %>%
  rename(SA4 = SA4_CODE_2011)
   
  
# Convert dates
RM1_2011$Date <- as.Date(RM1_2011$Date) 
RM1$Date <- as.Date(RM1$Date) 

# Sort by time period
RM1_2011 <- RM1_2011 %>% 
  filter(Date <= max(RM1$Date))

# Renaming and aggregating
RM1_2011 <- RM1_2011 %>% 
  mutate(Emp = FTEmp + PTEmp,
         LForce = UnEmp +  Emp,
         CivPop = Emp + LForce)

# Re-arranging
RM1_2011 <- RM1_2011 %>% 
  gather(Variable, Value, - Date, -SA4)

#--------------------------------------------------------------------------------------------#
#### 4. Smoothing                                                                         ####
#--------------------------------------------------------------------------------------------#

##STL DECOMPOSITION

RM1_2011 <- RM1_2011 %>% 
group_by(SA4, Variable) %>% 
  mutate(Smoothed = forecast::trendcycle(stl(ts(Value, f = 12, start = c(1998,10)),
                             s.window = "per",
                             robust = TRUE)))

write.csv(RM1_2011, file = paste(Data_path, "/RM1_2011.csv", sep= ""))

          