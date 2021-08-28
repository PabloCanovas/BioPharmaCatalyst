
library(tidyverse)
library(rvest)
library(lubridate)
library(glue)

setwd("~/RWork/Trading/BioPharmaCatalyst")
source("Get_FDA_Calendar.R")
V <- View

date <- 20210825
date <- today()

df <- Get_FDA_Calendar(date)

# df %>% nrow()
# df %>% filter(!is.na(days_to_catalyst)) %>% nrow()

df %>% filter(last_updated == max(last_updated))

df %>% filter(days_to_catalyst <= 25, market_cap_mill <= 1000) 


## Compare with previous data ---------------------------------------

days_back <- 1

df_previous <- Get_FDA_Calendar(date %>% magrittr::add(days(-days_back)))

# Registros que han aparecido
anti_join(df, df_previous, by = c("ticker", "catalyst_date")) %>%
  select(ticker, stage, exact_catalyst_date, catalyst_date, 
         days_to_catalyst, last_updated, market_cap_mill) %>% 
  # filter(market_cap_mill <= 5000) %>%
  I() %>% 
  head(20)


# Registros que han desaparecido
anti_join(df_previous, df, by = c("ticker", "catalyst_date")) %>%
  select(ticker, stage, exact_catalyst_date, catalyst_date, 
         days_to_catalyst, last_updated, market_cap_mill) %>% 
  # filter(market_cap_mill <= 5000) %>%
  I() %>% 
  head(20)

 
## Historic calendar  -------------------------------------------


fda_hist <- Get_FDA_Calendar(date, "historical")



