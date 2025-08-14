library(tidyverse)

file_lst <- list.files("edgar_FilingsHTML/Form 10-K/320193/")
html_filing <- read_html(paste0("edgar_FilingsHTML/Form 10-K/320193/", file_lst[5]))

# Parsing Net Sales (2002)
tb_sales <- html_filing |> 
  html_elements(xpath = "/html/body/pre/text/div[2]/table") |> 
  html_table() %>%
  .[[1]]
tb_sales <- drop_all_na_columns(tb_sales)
names(tb_sales) <- paste0("X", c(1:12))
# cleaning
tb_sales2 <- tb_sales[,-1] |> 
  mutate(across(c(1:ncol(tb_sales)), function(x) as.numeric(str_remove_all(x, ","))))




# Statement of Operations

tbs <- html_filing |> 
  html_table()
tbs[99]
tb1 <- html_filing |> 
  html_elements(xpath = "/html/body/pre/text/table[48]") |> 
  html_table() %>% 
  .[[1]]
# cleaning
tb2 <- tb1[(-1):(-2),] |> 
  mutate(across(X2:X13, function(x) as.numeric(str_remove_all(x, ",")))) |> 
  filter(!is.na(X1), X1 != "")


# Function to drop all columns with only missing values
drop_all_na_columns <- function(df) {
  # Identify columns where all values are NA
  na_cols <- sapply(df, function(col) all(is.na(col)))
  
  # Drop those columns
  df_cleaned <- df[, !na_cols]
  
  return(df_cleaned)
}

tb_cleaned <- drop_all_na_columns(tb2) |> 
  drop_na()
yr <- 2015
names(tb_cleaned) <- c("item", paste0("value_",yr), paste0("value_",yr-1), paste0("value_",yr-2))

# Consolidated Balance Sheet
tb_bs <- html_filing |> 
  html_elements(xpath = "/html/body/pre/text/table[50]") |> 
  html_table() %>% 
  .[[1]]
# cleaning
tb_bs <- tb_bs[(-1):(-2),] |> 
  mutate(across(X2:X9, function(x) as.numeric(str_remove_all(x, ",")))) |> 
  filter(!is.na(X1), X1 != "")
tb_bs_cleaned <- drop_all_na_columns(tb_bs)

yr <- 2015
names(tb_bs_cleaned) <- c("item", paste0("value_",yr), paste0("value_",yr-1))
tb_bs_cleaned <- tb_bs_cleaned |> 
  filter(!is.na(value_2015) | !is.na(value_2014)) |> 
  mutate(item = str_to_lower(item),
         item = str_replace_all(item, " ", "_"))



