library(tidyverse)
library(janitor)
library(magrittr)
library(RSQLite)
# load Michael Sinkinson's 13F filings data
df_mk <- read_csv('/Users/kevinchiou/Library/CloudStorage/GoogleDrive-jjchiou92@gmail.com/My Drive/Research/Database/Ownership/Data MS/scrape_parsed.csv')
head(df_mk)
df_mk %<>%
  mutate(rdate = ymd(rdate),
         fdate = ymd(fdate),
         ryear = year(rdate),
         fyear = year(fdate))
tabyl(df_mk, filetype)
tabyl(df_mk, ryear)

# 1. exclude file types that are not valid
# 2. exclude zero-holding record
df_mk2 <- df_mk %>% 
  filter(filetype == "13F-HR",
         shares > 0)
tabyl(df_mk2, ryear)
#==================================================
# Read CRSP from SQL
#==================================================
drv <- dbDriver("SQLite")
CRSP <- dbConnect(drv, "/Volumes/Sentra/Database/CRSP/CRSP.db")
dbListTables(CRSP)
dsenames <- dbReadTable(CRSP, 'dsenames') %>% 
  as_tibble() %>% 
  filter(shrcd %in% c(10,11,12)) %>% 
  select(permno, ncusip, namedt, nameendt) %>% 
  mutate(namedt = ymd(namedt),
         nameendt = ymd(nameendt))
head(dsenames)
tabyl(dsenames, shrcd)
tabyl(dsenames, exchcd)
crsp_mth <- dbReadTable(db_CRSP, 'monthly_stocks')
cik_name <- read.csv('/Users/kevinchiou/Library/CloudStorage/GoogleDrive-jjchiou92@gmail.com/My Drive/Research/Database/Ownership/Data MS/cikmap.csv')

df_ex1 <- df_mk %>% 
  filter(filetype == 'EX-1')
n_distinct(df_ex1$cik)
tabyl(df_ex1, ryear)

df_nta <- df_mk %>% 
  filter(filetype == '13F-NT/A')
n_distinct(df_nta$cik)
tabyl(df_nta, ryear)
