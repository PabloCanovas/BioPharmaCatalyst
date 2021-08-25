

### OLD  -----------------------------------------------------------

# Al leer el html directamente de la pagina no tenemos acceso al contenido premium, y solo sacamos 150 registros del calendario

fda_calendar <- read_html("https://www.biopharmcatalyst.com/calendars/fda-calendar")

fda_calendar %>% 
  html_element("body") %>%
  html_element("main") %>% 
  html_element("#modern-screener-container") %>% 
  html_element("screener")


df_orig <- jsonlite::fromJSON("data.json") %>% as_tibble()

# df_orig %>% head(1) %>% str

df <- df_orig %>% 
  unpack(c("stage", "companies", "cashflow"), names_repair = "unique") %>% 
  unpack(c("earnings", "prev_quarter_earnings"), names_sep = ".") %>%
  # select(-starts_with("id")) %>% 
  rename(ticker = ticker...17, 
         drug = name...2, 
         company = name...18,
         updated_at = updated_at...12, 
         updated_at_company_info = updated_at...33, 
         stage_label = label) %>% 
  select(
    ticker, catalyst_date, updated_at, stage_label, catalyst_date_text, 
    drug, indication, advisory_committee_date, note, estimated_primary_completion_date, press_link, 
    
    company, price, market_cap, average_daily_volume, relative_volume, price_to_book, updated_at_company_info,
    
    insiderholdingspct, net_cash, total_liabilities, cash_equivalents_and_short_term_investments, 
    operating_cash_flow, monthly_cash_burn, est_live_cash,  est_months_cash, enterpriseValue, 
    total_debt_to_equity, float, 
    
    earnings.eps, earnings.date, earnings.time, earnings.notes,
    earnings.period, 
    earnings.revenue, earnings.eps_prior, earnings.revenue_est,
    earnings.revenue_surprise, earnings.revenue_surprise_percent,
    
    prev_quarter_earnings.eps, prev_quarter_earnings.date, prev_quarter_earnings.time, 
    prev_quarter_earnings.notes, prev_quarter_earnings.period, 
    prev_quarter_earnings.revenue, prev_quarter_earnings.eps_prior, 
    prev_quarter_earnings.revenue_est, 
    prev_quarter_earnings.revenue_surprise, prev_quarter_earnings.revenue_surprise_percent
  )


# https://www.biopharmcatalyst.com/actions/bioPharm/sync/getJwtToken?CRAFT_CSRF_TOKEN=sHIZCmgqGKez~Tp54grOkzrtJFx_f7RP8j1hYYPm%7C67b792f5123b5a1ca88248d05a6ea5c13e6e4328


