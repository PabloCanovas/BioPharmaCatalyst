rm(list=ls())

#####################################
####### BIOPHARMA CATALYSTS #########
#####################################

## Primeras conclusiones:
# En los archivos históricos aparecen catalizadores a pasado (con catalizador previo a la fecha de la foto del historico)
# Tambien he visto un caso de desaparición de datos (CERC fue deslistada?)
# Los catalizadores no siempre ocurren en la fecha prevista: Ejemplo ASND adelantó un mes 25sept -> 25 agosto la PDUFA (resultado: aprobado) respecto a la fecha que aparecia en BPC.
# Por todo ello, tenemos gaps de precio los días de la noticia sin run-up que acompañe, pues en muchos casos la fecha del catalizador solo se sabe a posteriori. Al buscar run-ups en los archivos históricos encontraremos información que sabíamos en ese momento e información añadida a pasado.


library(tidyverse)
setwd("~/RWork/Trading/BioPharmaCatalyst")
V <- View


df_orig <- fst::read_fst("../AlgoTrading/Input/all_data_20210823.fst") %>% 
  as_tibble() %>% 
  mutate(market_cap = market_cap / 1e6) %>% 
  rename(ticker = symbol) %>% 
  distinct() 



### Historical FDA data ---------------------------------------------------------------

fda_hist <- read_html("HTMLs/20210826_Historical Biotech Catalyst Calendar • BioPharmCatalyst.html") 

headers <- fda_hist %>% 
  html_element(xpath = '//*[@id="modern-screener-container"]/div/form/div[2]/div[2]/table/thead') %>% 
  html_table() %>% 
  names()

hist_orig <- fda_hist %>% 
  html_element(xpath = '//*[@id="modern-screener-container"]/div/form/div[2]/div[2]/table/tbody') %>% 
  html_table() %>%
  set_names(headers) 


tickers <- hist_orig$Ticker %>% 
  str_split(pattern = "  ", simplify = T) %>%
  .[,1] %>%
  str_replace(pattern = "\n", replacement = "")

catalysts <- hist_orig$Catalyst %>% str_split(pattern = "               ", simplify = T) 
dates_catalyst <- catalysts[,1] %>% str_replace("\n", "")
description_catalyst <- catalysts[,2]

fda_hist <- hist_orig %>% 
  janitor::clean_names() %>% 
  mutate(ticker = tickers, 
         catalyst_date = lubridate::mdy(dates_catalyst),
         description = description_catalyst) %>% 
  relocate(ticker, stage, catalyst_date, description, drug) %>% 
  select(-catalyst) %>% 
  arrange(catalyst_date)

rm(catalysts, dates_catalyst, description_catalyst)

# fda_hist_previous <- fda_hist

## Compare with previous data ---------------------------------------

# Registros que han aparecido
anti_join(fda_hist, fda_hist_previous, by = c("ticker", "catalyst_date")) %>%
  head(20)

# Registros que han desaparecido
anti_join(fda_hist_previous, fda_hist, by = c("ticker", "catalyst_date")) %>%
  head(20)





# Cogemos los 90 días previos al catalizador y los 30 posteriores.
# De esta manera, siempre que dos catalizadores estén a menos de 120 días entre sí, 
# tendremos solape y por tanto días repetidos

df2 <- df_orig %>% 
  drop_na(close) %>% 
  # filter(ticker == "OCGN") %>% 
  select(-adjusted) %>% 
  inner_join(fda_hist, by = "ticker") %>% 
  mutate(days_to_catalyst = difftime(catalyst_date, date, units = "days") %>% as.integer()) %>% 
  relocate(ticker, date, catalyst_date, days_to_catalyst)

df2 %>% select(ticker, sector, industry) %>% distinct() %>% count(sector, industry, sort = T)

catalyst_days <- df2 %>%
  filter(days_to_catalyst == 0) %>% 
  select(ticker, catalyst_date, 
         catalyst_day_open = open,
         catalyst_day_high = high,
         catalyst_day_close = close)

df_to <- df2 %>% 
  filter(date <= catalyst_date) %>% 
  group_by(ticker, date) %>%
  filter(days_to_catalyst == min(days_to_catalyst)) %>%   #¿Permitimos solape o no?
  ungroup() %>%
  filter(days_to_catalyst %>% between(0,90)) 


df_from <- df2 %>% 
  filter(date >= catalyst_date) %>% 
  group_by(ticker, date) %>%
  filter(days_to_catalyst == max(days_to_catalyst)) %>%    #¿Permitimos solape o no?
  ungroup() %>%
  filter(days_to_catalyst %>% between(-30,0))


df <- bind_rows(df_to, df_from) %>% 
  left_join(catalyst_days, c("ticker", "catalyst_date")) %>% 
  # mutate(
  #   delta_open = round((catalyst_day_open - close)*100 / close, 1),
  #   delta_high = round((catalyst_day_high - close)*100 / close, 1),
  #   delta_close = round((catalyst_day_close - close)*100 / close, 1)
  # ) %>% 
  mutate(
    delta_open = round((close - catalyst_day_open)*100 / catalyst_day_open, 1),
    delta_high = round((close - catalyst_day_high)*100 / catalyst_day_high, 1),
    delta_close = round((close - catalyst_day_close)*100 / catalyst_day_close, 1)
  ) %>% 
  relocate(ticker, catalyst_date, date, days_to_catalyst,
           delta_open, delta_high, delta_close, 
           open, catalyst_day_open, 
           high, catalyst_day_high, 
           close, catalyst_day_close) %>% 
  arrange(ticker, catalyst_date, date, days_to_catalyst) %>%  
  group_by(date, ticker) %>%           # deduplicamos por volumen (algunos repetidos)
  filter(volume == max(volume)) %>%
  ungroup() %>% 
  distinct()                   # El days_to_catalyst = 0 se repite




### Exploring -------------
# 
# df_orig$ticker %>% n_distinct()
# fda_hist$ticker %>% n_distinct()
# 
# #### anti_join(X,Y) -> Los elementos que están en X pero no en Y
# 
# anti_join(df_orig, fda_hist, by = c("ticker")) %>% select(ticker) %>% n_distinct()
# 
# # Me faltan 183 farmas???
# anti_join(fda_hist, df_orig, by = c("ticker")) %>% select(ticker) %>% n_distinct()
# anti_join(fda_hist, df, by = c("ticker")) %>% select(ticker) %>% n_distinct()
# 
# # Pierdo 3 más de df_orig a df porque todos los catalizadores son anteriores al primero de mis datos de precio
# anti_join(
#   anti_join(fda_hist, df, by = c("ticker")) %>% select(ticker),
#   anti_join(fda_hist, df_orig, by = c("ticker")) %>% select(ticker), 
#   by = c("ticker")
# ) %>% 
#   distinct()


### -----------------------------------------


# Market_cap está en millones

df %>% 
  filter(ticker == "ITRM") %>% 
  # filter(stage == "Approved") %>% 
  # filter(market_cap <= 500) %>% 
  group_by(days_to_catalyst) %>% 
  summarise(
    across(contains("delta"), ~ mean(.x, na.rm = T))
    # across(contains("delta"), ~ sd(.x, na.rm = T))
  ) %>% 
    
    # mean_delta_open = mean(delta_close, na.rm = T),
    # mean_delta_ = mean(delta_close, na.rm = T),
    # mean_delta_close = mean(delta_close, na.rm = T),
    # sd_delta_close = sd(delta_close, na.rm = T)
    # ) %>% 
  pivot_longer(-days_to_catalyst, "var", "value") %>% 
  ggplot() + 
  geom_line(aes(days_to_catalyst, value, col = var)) + 
  # geom_line(aes(days_to_catalyst, delta_open, col = "red")) + 
  # geom_line(aes(days_to_catalyst, delta_high, col = "red")) + 
  # geom_line(aes(days_to_catalyst, mean_delta_close + sd_delta_close)) +
  # geom_line(aes(days_to_catalyst, mean_delta_close - sd_delta_close)) +
  NULL



df %>% 
  filter(ticker == "ITRM") %>% 
  group_by(catalyst_date, days_to_catalyst) %>% 
  summarise(
    across(contains("delta"), ~ mean(.x, na.rm = T))
  ) %>% 
  pivot_longer(-c(catalyst_date, days_to_catalyst), "var", "value") %>% 
  ggplot() + 
  geom_line(aes(days_to_catalyst, value, col = var)) + 
  facet_wrap(~ catalyst_date) + 
  NULL

# Pensar como quiero mostrar la info...






