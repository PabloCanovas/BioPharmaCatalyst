Get_FDA_Calendar <- function(date){
  
  if(!is.Date(date)) date <- date %>% ymd() 
  
  date <- date %>% format("%Y%m%d")
  
  fda_calendar <- read_html(glue("~/RWork/Trading/BioPharmaCatalyst/HTMLs/{date}_FDA Calendar of Biotech Stock Catalysts • BioPharmCatalyst.html")) 
  
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
           catalyst_date_raw = dates_catalyst,
           description = description_catalyst) %>% 
    mutate(
      catalyst_date = case_when(catalyst_date_raw %>% str_detect("deferred") ~ NA_character_,
                                catalyst_date_raw %>% str_detect("Decision") ~ NA_character_,
                                catalyst_date_raw %>% str_detect("delayed") ~ NA_character_,
                                catalyst_date_raw %>% str_detect("has passed") ~ NA_character_,
                                catalyst_date_raw %>% str_detect("not met") ~ NA_character_,
                                T ~ catalyst_date_raw),
      catalyst_date = catalyst_date %>% str_replace("Fall", "Early-4Q"),
      catalyst_date = catalyst_date %>% str_replace("Early", "1Q"),
      catalyst_date = catalyst_date %>% str_replace(" est", ""),
      catalyst_date = catalyst_date %>% str_replace("YE", "4Q"),
      catalyst_date = catalyst_date %>% str_replace("1H", "1Q"),
      catalyst_date = catalyst_date %>% str_replace("2H", "3Q"),
      catalyst_date = catalyst_date %>% str_replace("Q1", "1Q"),
      catalyst_date = catalyst_date %>% str_replace("Q2", "2Q"),
      catalyst_date = catalyst_date %>% str_replace("Q3", "3Q"),
      catalyst_date = catalyst_date %>% str_replace("Q4", "4Q"),
      catalyst_date_parsed = parse_date_time(catalyst_date, c("%b%y", "%q%Y"), quiet = T),
      catalyst_date_parsed2 = if_else(is.na(catalyst_date_parsed), 
                                      parse_date_time(catalyst_date, "mdy", quiet = T), 
                                      catalyst_date_parsed) %>% as.Date(),
      exact_catalyst_date = if_else(catalyst_date_raw %>% str_detect("/") & nchar(catalyst_date_raw) <= 10,
                                    mdy(catalyst_date_raw, quiet = T), as.Date(NA))
    ) %>% 
    select(-catalyst_date, -catalyst_date_parsed) %>% 
    rename(catalyst_date = catalyst_date_parsed2) %>% 
    mutate(
      last_updated = lubridate::mdy(last_updated, quiet = T),
      days_to_catalyst = difftime(exact_catalyst_date, today(), units = "days") %>% as.integer(), 
      previous_close = str_replace(previous_close, "[$]", "") %>% as.numeric(),
      market_cap_mill = market_cap %>% str_replace("m", "") %>% str_replace("b", "") %>% as.numeric(),
      market_cap_mill = if_else(market_cap %>% str_detect("b"), market_cap_mill*1000, market_cap_mill)) %>%
    relocate(ticker, stage, exact_catalyst_date, days_to_catalyst,
             catalyst_date, catalyst_date_raw,
             last_updated, close = previous_close, market_cap_mill,
             insider_holding = insider_holding_percent, 
             estimated_primary_completion_date = est_epcd, description, drug) %>%
    select(-price, -catalyst, -open, -market_cap) %>%
    arrange(days_to_catalyst, exact_catalyst_date, desc(last_updated))
  
  
  return(df)
}
