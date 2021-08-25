
library(tidyverse)
library(rvest)
setwd("~/RWork/Trading/BioPharmaCatalyst")
V <- View


# fda_calendar <- read_html("HTMLs/FDA Calendar of Biotech Stock Catalysts • BioPharmCatalyst.html")
fda_calendar <- read_html("HTMLs/20210823_FDA Calendar of Biotech Stock Catalysts • BioPharmCatalyst.html") 
fda_calendar <- read_html("HTMLs/20210824_FDA Calendar of Biotech Stock Catalysts • BioPharmCatalyst.html") 
fda_calendar <- read_html("HTMLs/20210825_FDA Calendar of Biotech Stock Catalysts • BioPharmCatalyst.html") 

headers <- fda_calendar %>% 
  html_element(xpath = '//*[@id="modern-screener-container"]/div/form/div[2]/div[2]/table/thead') %>% 
  html_table() %>% 
  names()

df_orig <- fda_calendar %>% 
  html_element(xpath = '//*[@id="modern-screener-container"]/div/form/div[2]/div[2]/table/tbody') %>% 
  html_table() %>%
  set_names(headers) 

tickers <- df_orig$Ticker %>% 
  str_split(pattern = "  ", simplify = T) %>%
  .[,1] %>%
  str_replace(pattern = "\n", replacement = "")

catalysts <- df_orig$Catalyst %>% str_split(pattern = "               ", simplify = T) 
dates_catalyst <- catalysts[,1] %>% str_replace("\n", "")
description_catalyst <- catalysts[,2]


df <- df_orig %>% 
  janitor::clean_names() %>% 
  mutate(ticker = tickers, 
         catalyst_date = dates_catalyst,
         description = description_catalyst) %>% 
  mutate(exact_catalyst_date = lubridate::mdy(catalyst_date)) %>% 
  relocate(ticker, stage, catalyst_date, exact_catalyst_date, last_updated, insider_holding = insider_holding_percent,
           estimated_primary_completion_date = est_epcd, description, drug) %>% 
  select(-price, -catalyst, -open, -previous_close) 


View(df)


df_previous <- df

anti_join(
  df %>% select(ticker, catalyst_date, last_updated),
  df_previous %>% select(ticker, catalyst_date, last_updated),
  by = c("ticker", "catalyst_date")
)



