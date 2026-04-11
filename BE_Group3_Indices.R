library(Rblpapi)
library(dplyr)
library(purrr) 
library(tidyr)

blpConnect()

index_tickers <- c("VIX Index", "V2X Index", "IVIUK Index", "VNKY Index", "VHSI Index", "INVIXN Index")

start_date <- as.Date("2015-01-01")
end_date <- as.Date("2026-03-31")

data_indices <- bdh(
  securities = index_tickers, 
  fields = "PX_LAST",
  start.date = start_date,
  end.date = end_date
)

df_indices_long <- bind_rows(data_indices, .id = "ticker") %>%
  arrange(date)

df_indices_wide <- bind_rows(data_indices, .id = "ticker") %>%
  mutate(ticker = factor(ticker, levels = index_tickers)) %>% 
  pivot_wider(names_from = ticker, values_from = PX_LAST) %>%
  arrange(date) %>%
  # Keep rows where the count of NAs is less than 3
  filter(rowSums(is.na(.)) < 3) %>%
  # Forward fill remaining NAs
  fill(-date, .direction = "down") %>%
  # Drop the first row
  drop_na()

save(df_indices_wide, file = "df_indices_wide.RData")
