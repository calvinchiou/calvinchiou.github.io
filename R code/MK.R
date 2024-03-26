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
df_mk %<>% filter(ryear >= 2011)
# 1. exclude file types that are not valid
# 2. exclude zero-holding record
df_mk %<>% 
  filter(filetype == "13F-HR",
         shares > 0)
tabyl(df_mk, ryear)
head(df_mk)
# 46,695,557 obs


#==================================================
# Read CRSP from SQL
#==================================================
drv <- dbDriver("SQLite")
CRSP <- dbConnect(drv, "/Volumes/Sentra/Database/CRSP/CRSP.db")
dbListTables(CRSP)
# 1. dsenames
dsenames <- dbReadTable(CRSP, 'dsenames') %>% 
  as_tibble() %>% 
  filter(shrcd %in% c(10,11,12)) %>% 
  select(permno, ncusip, comnam, ticker, namedt, nameendt) %>% 
  mutate(namedt = ymd(namedt),
         nameendt = ymd(nameendt))
head(dsenames)
tabyl(dsenames, shrcd)
tabyl(dsenames, exchcd)
# 2. Monthly stocks
crsp_mth <- dbReadTable(CRSP, 'monthly_stocks') %>% 
  as_tibble() %>% 
  select(permno, sic=hsiccd, date, prc, vol, ret, retx, shrout, cfacshr) %>% 
  mutate(year = year(date),
         month = month(date))
head(crsp_mth)
# merge MK with ncusip
# Using the first available FDATE for a given CIK-RDATE avoids the staleness.
df_mk2 <- inner_join(df_mk, dsenames, by = c('cusip'='ncusip')) %>% 
  filter(fdate >= namedt, fdate <= nameendt) %>% 
  arrange(cik, cusip, rdate, fdate) %>% 
  group_by(cik, cusip, rdate) %>% 
  slice_head()
head(df_mk2)
rm(df_mk)
# merge with monthly stocks to get shares factor and price, return, etc
df_mk2 %<>%
  mutate(year = year(rdate),
         month = month(rdate)) %>% 
  select(cik, cusip, permno, shares, rdate, fdate, year, month) %>% 
  inner_join(crsp_mth, by = c('permno','year','month'))
head(df_mk2)
# there are probably several CIKs that report more than once in a given month; 
# for those CIKs we keep only the last report of the month
df_mk2 %<>%
  group_by(cik, permno, year, month) %>% 
  slice_tail()
# one last thing to do is to adjust shares held by the fund for stock splits, etc. The number
# of shares is reported as of FDATE but the effective holding date is RDATE. If a particular
# stock (PERMNO) had some event such as a stock split which affected the number of shares
# outstanding, the reported number of shares needs to be adjusted.
# 
# As an example, let's continue with fundno = 1099 (ROYCE HERITAGE FUND) reference above. Its
#  wficn is 105794, and its SEC's Edgar filings are here: http://goo.gl/t5mAi
# On RDATE=March 31 2010, the fund held permno = 76489 (LUFKIN INDUSTRIES), and the number
# of shares reported in s12type3 file is 43,400. However, if we dig up the actual SEC filing
# for that quarter (http://goo.gl/yTkHb), we'll see that the fund actually held only 21,700
#  shares which amounted to an investment of $1,717,555. Why the difference? It's because
# Lufkin Industries had a 2:1 split in June 2010, and the number of shares in s12type3 is
# already adjusted for the split. So we need to re-adjust it back before we compute dollar
# investments for every fund-month-stock observation
df_mk3 <- df_mk2 %>%
  mutate(year = year(fdate),
         month = month(fdate)) %>% 
  inner_join(crsp_mth, by = c('permno','year','month'), suffix = c('','_f')) %>% 
  mutate(shares_adj = shares*cfacshr_f/cfacshr,
         hld_amount = ifelse(shares_adj*abs(prc) > 0, shares_adj*abs(prc), shares*abs(prc)))
head(df_mk3)
# delete several duplicates to obtain the final holdings file we wanted
df_mk3 %<>%
  distinct(cik, permno, year, month, .keep_all = T) %>% 
  filter(!is.na(hld_amount), hld_amount > 0) %>% 
  select(-prc_f, -prc_r)
tr_s12[[i]] <- df
#==================================================
# Correct stock split problem
#==================================================
# Using the first available FDATE for a given FUNDNO-RDATE avoids the staleness.
df %<>% 
  group_by(fundno, cusip, rdate) %>% 
  slice_head()


cik_name <- read.csv('/Users/kevinchiou/Library/CloudStorage/GoogleDrive-jjchiou92@gmail.com/My Drive/Research/Database/Ownership/Data MS/cikmap.csv')

df_ex1 <- df_mk %>% 
  filter(filetype == 'EX-1')
n_distinct(df_ex1$cik)
tabyl(df_ex1, ryear)

df_nta <- df_mk %>% 
  filter(filetype == '13F-NT/A')
n_distinct(df_nta$cik)
tabyl(df_nta, ryear)
