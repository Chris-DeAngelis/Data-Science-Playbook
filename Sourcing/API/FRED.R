# This R script sources data from FRED - Federal Reserve Banks Data from St. Louis Fed
library(fredr) # Connect to FRED API
library(tidyverse) # Tidy and manipulate data
library(keyring) # Storing API credentials
library(lubridate) # Working with date/time formats

# Instructions on FREDR package
# https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html

# Instantiate API token for FRED
fredr_set_key(key_get(service="FRED",username="chris.deangelis@elkay.com",keyring="Elkay"))
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2021-12-31")

# Categories
# Employment, Exports, Industry, Private, Business, GDP, Services, Private Industries, Goods

# Build function for pulling state-level data for all states
# download_all_states <- function(state_code = state.abb[1], series_id = "UR", reverse_code = 0) {
#   if(reverse_code == 0){
#     detail <- fredr_series(str_c(state_code, series_id))
#     fredr(series_id = str_c(state_code, series_id),
#           #frequency = "q",
#           observation_start = as.Date("2000-01-01")) %>%
#       mutate(state = state_code,
#              `Type` = "State",
#              Title = str_to_title(detail$title),
#              Min_Obs = detail$observation_start,
#              Max_Obs = detail$observation_end,
#              Freq = detail$frequency_short,
#              Units = detail$units)  
#   } else if(reverse_code == 1){
#     detail <- fredr_series(str_c(state_code, series_id))
#     fredr(series_id = str_c(series_id, state_code),
#           #frequency = "q",
#           observation_start = as.Date("2000-01-01")) %>%
#       mutate(state = state_code,
#              `Type` = "State",
#              Title = str_to_title(detail$title),
#              Min_Obs = detail$observation_start,
#              Max_Obs = detail$observation_end,
#              Freq = detail$frequency_short,
#              Units = detail$units)
#   }
# 
# }
# state_list <- list()
# state_tickers <- c("NGSP","URN","STHPI","POP","NAN","HOWN","BPPRIV","OI96","WTOT","PCEG","CONSTNQGSP","SLIND","ECON","LEIHN") #"OPCI",""CONSN"
# state_reverse_tickers <- c("STTMINWGIL","ACTLISCOU","BUSAPPWNSA","NEWLISCOUMM")
# 
# for(i in 1:length(state_tickers)){
#   state_list[[i]] <- map_dfr(state.abb, download_all_states, series_id = state_tickers[i])
# }

# Building simplified function for accessing FRED
get_FRED <- function(series_id = "ILNQGSP",
                     start = start_date,
                     end = end_date,
                     frequency = "q",
                     units = "lin"){
  
  fredr(series_id = series_id,
        observation_start = start_date,
        observation_end = end_date,
        frequency = frequency, # quarterly
        units = units # change over previous value
  ) %>% return()
}

# Source GDP
GDP <- fredr(series_id = "GDP",
             observation_start = as.Date("2000-01-01"),
             observation_end = as.Date("2020-01-01"),
             frequency = "q", # quarterly
             units = "chg" # change over previous value
            )

#WINQGSP
#ILNQGSP

# fredr_series("PPIACO")
# Currencies
currency_tickers <- c("DEXCHUS","DEXBZUS","DEXTAUS","DEXMXUS","DEXUSEU")
currency_list <- list()
for(i in 1:length(currency_tickers)){
  detail <- fredr_series(currency_tickers[i])
  currency_list[[i]] <- get_FRED(currency_tickers[i], frequency = "d") %>%
                          mutate(`Type` = "Currency",
                                 Title = str_to_title(detail$title),
                                 Min_Obs = detail$observation_start,
                                 Max_Obs = detail$observation_end,
                                 Freq = detail$frequency_short,
                                 Units = detail$units)
                                        
}

# Commodities
commodity_tickers <- c("PPIACO","WPU057303","DCOILBRENTEU","WPU07110224","WPU10170502","PZINCUSDM","PCOPPUSDM","PALUMUSDM","DHHNGSP","PIORECRUSDM","WPU06220299",
                       "PNICKUSDM","DHHNGSP","PPIACO","GASDESLSW","PNGASUSUSDM")
#metric_change <- c("Acrylonitrile USA","Butadiene US","Chlorine","Iron Ore (62%) CFR Qingdao","Tin","Styrene","Styrene China","Zinc","Copper","Aluminum","Iron Ore","Nickel")
metric_change <- c("Global Price Of Aluminum","Global Price Of Nickel","Global Price Of Zinc","Global Price Of Copper","Global Price Of Iron Ore")

commodity_list <- list()
for(i in 1:length(commodity_tickers)){
  detail <- fredr_series(commodity_tickers[i])
  commodity_list[[i]] <- get_FRED(commodity_tickers[i], frequency = "m") %>%
                          mutate(`Type` = "Commodity",
                                 Title = str_to_title(detail$title),
                                 Min_Obs = detail$observation_start,
                                 Max_Obs = detail$observation_end,
                                 Freq = detail$frequency_short,
                                 Units = case_when(Title %in% metric_change ~ "U.S. Dollars per pound",
                                                   TRUE ~ detail$units),
                                 value = case_when(Title %in% metric_change ~ value / 2204.62262,
                                                   TRUE ~ value))
}

# Macroeconomic
macro_tickers <- c("PAYEMS","UNRATE","PCEC96","CPIAUCSL","DGS10","MORTGAGE30US","HOUST","CSUSHPINSA","ISRATIO","SP500","CPILFESL",
                   "MEDCPIM158SFRBCLE","STICKCPIM157SFRBATL","FEDFUNDS","HOUST","UMCSENT") #"GDP","GDPC1","NGMPEDCATUSMP","FYGFGDQ188S","GDPDEF","USHLTHSOCASSRQGSP"
macro_list <- list()
for(i in 1:length(macro_tickers)){
  detail <- fredr_series(macro_tickers[i])
  macro_list[[i]] <- get_FRED(macro_tickers[i], frequency = "m") %>%
                      mutate(`Type` = "Macroeconomic",
                             Title = str_to_title(detail$title),
                             Min_Obs = detail$observation_start,
                             Max_Obs = detail$observation_end,
                             Freq = detail$frequency_short,
                             Units = detail$units)
}
# GDP can only be done quarterly
macro_list[[i+1]] <- get_FRED(macro_tickers[i], frequency = "q") %>%
                      mutate(`Type` = "Macroeconomic",
                             Title = str_to_title(detail$title),
                             Min_Obs = detail$observation_start,
                             Max_Obs = detail$observation_end,
                             Freq = detail$frequency_short,
                             Units = detail$units)

# Aggregated
data <- do.call(rbind, c(commodity_list,currency_list,macro_list)) %>%
          mutate(Timestamp = realtime_end,
                 Link = paste("https://fred.stlouisfed.org/series/", series_id, sep = '')) %>%
          select(-c(4,5)) %>%
          dplyr::rename(Date = date,
                        Value = value)

daily <- data %>% 
          filter(Freq %in% c("D","W")) %>%
          mutate(Year = year(Date),
                 Month = month(Date)) %>%
          select(-Date) %>%
          group_by_all() %>%
          summarize_all(mean, na.rm = TRUE) %>%
          mutate(Date = as.Date(paste(Year, "-", Month, "-01", sep = '')),
                 Freq = "M") %>%
          ungroup() %>%
          select(-c(Year, Month))

final_data <- data %>%
                filter(!Freq %in% c("D","W")) %>%
                rbind(daily) %>%
                mutate(Name = case_when(Title == "Taiwan Dollars To U.s. Dollar Spot Exchange Rate" ~ "TWD/USD",
                                        Title == "U.s. Dollars To Euro Spot Exchange Rate" ~ "USD/EUR",
                                        Title == "Mexican Pesos To U.s. Dollar Spot Exchange Rate" ~ "MXN/USD",
                                        Title == "Chinese Yuan Renminbi To U.s. Dollar Spot Exchange Rate" ~ "CNY/USD",
                                        Title == "Brazilian Reals To U.s. Dollar Spot Exchange Rate" ~ "BRL/USD",
                                        Title == "Global Price Of Zinc" ~ "Zinc",
                                        Title == "Global Price Of Copper" ~ "Copper",
                                        Title == "Global Price Of Aluminum" ~ "Aluminum",
                                        Title == "Global Price Of Iron Ore" ~ "Iron Ore",
                                        Title == "Global Price Of Nickel" ~ "Nickel",
                                        Title == "30-Year Fixed Rate Mortgage Average In The United States" ~ "30 Year Mortgage Rate",
                                        TRUE ~ Title))

saveRDS(final_data, paste("./Commodities/Archive/FRED_", Sys.Date(), ".RDS", sep = ''))
#2204.62 lbs / metric ton

############################## Delete ##############################
# temp <- data.frame()
# mapping <- data.frame()
# for(i in 1:length(fix)){
#   detail <- fredr_series(fix[i])
#   temp <- c(detail$id,detail$observation_start,detail$observation_end,detail$frequency_short,detail$units, detail$title)
#   mapping <- rbind(mapping,temp) %>% as.data.frame()
# }
# names(mapping) <- c("series_id","Min_obs","observation_end","Freq","Units","Title")
# mapping$series_id <- na.locf(mapping$series_id)
# mapping$Title <- na.locf(mapping$Title)
# 
# hmm %>%
#   mutate(Freq = case_when(is.na(Freq.x) ~ Freq.y, TRUE ~ Freq.x),
#          Units = case_when(is.na(Units.x) ~ Units.y, TRUE ~ Units.x),
#          Title = case_when(is.na(Title.x) ~ Title.y, TRUE ~ Title.x),
#          Min_Obs = case_when(is.na(Min_Obs.x) ~ Min_Obs.y, TRUE ~ Min_Obs.x))
