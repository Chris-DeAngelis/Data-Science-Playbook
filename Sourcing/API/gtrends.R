# This R script sources data from Google Trends
library(gtrendsR) # Connect to Google Trends API
library(plyr) # Combining dataframes with different columns
library(tidyverse) # Tidy and manipulate data
library(lubridate) # Working with dates
library(zoo) # Working with dates
library(readxl) # Reading from Excel files

#https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf
#https://support.google.com/trends/answer/4365533?hl=en

# Setup geographic regions: US States
# states <- countries %>%
#             filter(country_code=="US" & !is.na(sub_code)) %>%
#             select(2) %>%
#             unique()
# states <- c(states[1:51,1])
# states <- append(states, "US")

# Setup time periods: Daily, derived from all time
begin <- as.Date("2015-01-01")
end <- floor_date(Sys.Date(), unit = "month") - 1
beg_periods <- seq(begin, end, by = "months")
end_periods <- ceiling_date(beg_periods, unit = "month") - 1

# The function wrap all the arguments of the gtrendR::trends function and return only the interest_over_time (you can change that)
googleTrendsData <- function (keywords = "elkay", geo = "US", time = "2019-01-01 2021-10-31") { 
  # Set the geographic region, time span, Google product,... 
  # for more information read the official documentation https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf 
  
  trends <- gtrendsR::gtrends(keywords, 
                              gprop = "web",#c("web", "news", "images", "froogle", "youtube"),
                              geo = geo,
                              time = time) %>%
                      return()
  
  #results <- trends$interest_over_time
}

# Source file for search items
gtrends_worksheet <- read_excel("C:/Users/chris.deangelis/Elkay/Competitive Intelligence - General/Google Trends/Google Trends Template.xlsx", sheet = "Search Parameters")
gtrends_list <- list()
for(i in 1:length(gtrends_worksheet$`Search Name`)){
  gtrends_list[[i]] <- gtrends_worksheet[i,]
}

############################################ All US ############################################
# Run "all-time" query to get monthly weights for each search
weight_list <- list()
temp_list <- list()
for(x in 1:length(gtrends_list)){
    keywords <- as.character(unlist(gtrends_list[[x]][3:7]))
    query <- googleTrendsData(keywords = keywords[!is.na(keywords)], geo = "US", time = "2015-01-01 2022-01-31")[[1]] #time = "all"
    if(!is.null(query)){
      temp_list[[x]] <- query %>%
                          select(1:4) %>%
                          #filter(hits != 0) %>%
                          mutate(Name = as.character(gtrends_list[[x]][1]),
                                 `Search Name` = as.character(gtrends_list[[x]][2]),
                                 Timestamp = Sys.Date()) %>%
                          dplyr::rename(weight = hits)
    }
    # Throttling API call
    #Sys.sleep(3)
    print(paste(x, " of ", length(gtrends_list), sep = ''))
}

# Tidy weight data
weights_df <- do.call(rbind, temp_list) %>%
                as.data.frame() %>%
                #select(-c(time, gprop, category)) %>%
                #dplyr::rename(weight = hits) %>%
                mutate(month = month(date),
                       year = year(date))

# Save weight query results
saveRDS(weights_df, "./Google Trends/Archive/weights_df.RDS")
  
# Iterate through search list for each keyword to get metrics (to be weighted) for creating daily results
result_list <- list()
periods_list <- list()
temp_list <- list()
for(x in 1:length(gtrends_list)){
  # Iterate through each state to get the weights for creating daily results
  for(y in 1:(length(periods)-1)){
    keywords <- as.character(unlist(gtrends_list[[x]][3:7]))
    query <- googleTrendsData(keywords = keywords[!is.na(keywords)], geo = "US", time = paste(periods[y], " ", as.Date(periods[y]) + months(1) - 1, sep = ''))[[1]]
    if(!is.null(query)){
      temp_list[[y]] <- query %>%
                          select(1:4) %>%
                          filter(hits != 0) %>%
                          mutate(Name = as.character(gtrends_list[[x]][1]),
                                 `Search Name` = as.character(gtrends_list[[x]][2]),
                                 Timestamp = Sys.Date())
      periods_list[[y]] <- temp_list[[y]]
      # Throttling API call
      #Sys.sleep(3)
    }
  }
  result_list[[x]] <- do.call(rbind, periods_list)
  print(paste(x, " of ", length(gtrends_list), sep = ''))
}

# Assign weights and add final mutations
results_df <- do.call(rbind, result_list) %>%
                as.data.frame() %>%
                mutate(weekend = case_when(weekdays(date) %in% c("Saturday", "Sunday") ~ "Weekend",
                                           TRUE ~ "Weekday"),
                       month = month(date),
                       year = year(date),
                       hits = as.numeric(hits)) %>%
                left_join(weights_df %>% select(year,month,keyword,weight), by = c("month", "year", "keyword")) %>%
                mutate(weight = as.numeric(weight)/100,
                       `Wtd. Hits` = weight * hits) %>%
                select(-c(month, year))
colnames(results_df) <- str_to_title(colnames(results_df))

# Save result query and tidy dataframe
saveRDS(result_list, "./Google Trends/Archive/result_list.RDS")
saveRDS(results_df, "./Google Trends/Archive/results_df.RDS")

############################################ States ############################################
# Iterate through search list for each keyword to get "weights" for creating daily results
weight_list <- list()
temp_list <- list()
for(x in 1:length(gtrends_list)){
  # Iterate through each state to get the weights for creating daily results
  for(y in 1:length(states)){
    keywords <- as.character(unlist(gtrends_list[[x]][3:7]))
    query <- googleTrendsData(keywords = keywords[!is.na(keywords)], geo = states[y], time = "all")[[1]]
    if(!is.null(query)){
      temp_list[[y]] <- query %>%
                          select(1:4) %>%
                          #filter(hits != 0) %>%
                          mutate(Name = as.character(gtrends_list[[x]][1]),
                                 `Search Name` = as.character(gtrends_list[[x]][2]),
                                 Timestamp = Sys.Date()) %>%
                          dplyr::rename(weight = hits)
    }
  }
  weight_list[[x]] <- do.call(rbind, temp_list)
}

# Iterate through search list for each keyword to get "weights" for creating daily results
result_list <- list()
periods_list <- list()
state_list <- list()
temp_list <- list()
for(x in 1:length(gtrends_list)){
  # Iterate through each state to get the weights for creating daily results
  for(y in 1:length(states)){
    # Iterate through each time period for each state and each search for creating daily results
    for(z in 1:(length(periods)-1)){
      keywords <- as.character(unlist(gtrends_list[[x]][3:7]))
      query <- googleTrendsData(keywords = keywords[!is.na(keywords)], geo = states[y], time = )[[1]]
      if(!is.null(query)){
        temp_list[[y]] <- query %>%
          select(1:4) %>%
          #filter(hits != 0) %>%
          mutate(Name = as.character(gtrends_list[[x]][1]),
                 `Search Name` = as.character(gtrends_list[[x]][2]),
                 Timestamp = Sys.Date())
      }
      periods_list[[z]] <- do.call(rbind, temp_list)
    }
    result_list[[x]] <- do.call(rbind, temp_list)
  }
  result_list[[x]] <- do.call(rbind, temp_list)
}

# googleTrendsData function is executed over the kwlist
# interest_over_time <- list()
# interest_by_region <- list()
# interest_by_dma <- list()
# interest_by_city <- list()
# related_topics <- list()
# related_queries <- list()
# state_interest_over_time <- list()
# state_interest_by_dma <- list()
# output_state_list <- list()
# output_state_dma_list <- list()
# final_state_list <- list()
# final_state_dma_list <- list()

# for(x in 1:length(gwords)){
#   for(y in 1:length(date_start_table)){
#     # download <- googleTrendsData(keywords=gwords[x],geo="US",time=paste(date_start_table[y], " ", date_end_table[y], sep = ''))
#     # interest_over_time[y] <- download[[1]] %>%
#     #                         filter(hits > 0)
#     # interest_by_region[y] <- download[[3]] %>% 
#     #                         mutate(time = time) %>%
#     #                         filter(hits > 0)
#     # interest_by_dma[y] <- download[[4]] %>% 
#     #                     mutate(time = time) %>%
#     #                     filter(hits > 0)
#     # interest_by_city[y] <- download[[5]] %>% mutate(time = time)
#     # related_topics[y] <- download[[6]] %>% mutate(time = time)
#     # related_queries[y] <- download[[7]]
#     
#     for(z in 1:length(states)){
#       state_download <- googleTrendsData(keywords=gwords[x],geo=states[z],time=paste(date_start_table[y], " ", date_end_table[y], sep = ''))
#       state_interest_over_time[z] <- state_download$interest_over_time
#       state_interest_by_dma[z] <- state_download$interest_by_dma
#     #output <- map_dfr(.x = list(keywords=gwords[x],geo=states[y],time="2019-01-01 2021-08-01"),
#     #                  .f = googleTrendsData) %>%
#     }
#     output_state_list[[y]] <- state_interest_over_time
#     output_state_dma_list[[y]] <- state_interest_by_dma
#   }
#   final_state_list[[x]] <- output_state_list
#   final_state_dma_list[[x]] <- output_state_dma_list
#   
#   
#   #output_list[[x]] <- do.call(rbind.data.frame,output_state_list)
# }
# 
# final_trends <- do.call(rbind.data.frame,output_list)

# output <- map_dfr(.x = gwords,
#                   .f = googleTrendsData) %>%
#             filter(hits>0)

############################################ Custom Search ############################################
# Create generic date table to spread costs out by month. Creating months and then rolling 3M, 6M, 1Y, 3Y, 5Y, All-Time
# date_start_table <- c(seq(month_begin %m-% months(36), month_begin, by="months"), # Daily
#                       month_begin %m-% months(3) + 1, # 3M
#                       month_begin %m-% months(6) + 1, # 6M
#                       month_begin %m-% months(12) + 1, # 1Y
#                       month_begin %m-% months(36) + 1, # 3Y
#                       month_begin %m-% months(60) + 1, # 5Y
#                       as.Date("2005-01-01")) # All-time
# date_end_table <- c(seq(month_end %m-% months(36), length = 36, by = "1 month") - 1, # Daily
#                     rep(as.Date(month_end), 1, 7)) # All other time periods: 3M, 6M...
# 
# date_interval <- c(rep("Daily",1,37),"3M", "6M", "1Y","3Y","5Y","All-Time")

# gwords <- c("elkay","elkay sinks","elkay crosstown quartz","elkay fireclay sinks","elkay drinking fountain","elkay stainless sinks","elkay perfect drain","elkay perfect faucet")
# #brands <- c("elkay","dayton","halsey taylor","revere","mrdirect","rene","sir faucet","aurora","polaris sinks","solera sinks")
# products <- c("elkay crosstown", "dart canyon", "elkay fireclay", "elkay quartz", "ezh20")
# brands <- c("elkay","dayton sink","halsey taylor","mrdirect", "dart canyon")
# competitors <- c("elkay sink", "kohler sink", "american standard sink", "franke sink", "kraus sink") # "karran sink", "just sink")

# All time Search
#1, 3, 4, 7
# product_search <- googleTrendsData(keywords = products, time = paste("2004-01-01 ", month_end, sep = ''))
# brand_search <- googleTrendsData(keywords = brands, time = paste("2004-01-01 ", month_end, sep = ''))
# competitor_search <- googleTrendsData(keywords = competitors, time = paste("2004-01-01 ", month_end, sep = ''))

# This function cleans up the Google Trends list and returns a single data frame
tidy_list <- function(my_list = product_search_monthly, type = "Product", period = "All-time", start_date = date_start_table){
  
  timeseries <- list()
  state_search <- list()
  city_search <- list()
  related <- list()
  
  for(i in length(my_list)){
    if(!is.null(my_list[[i]]$interest_over_time)){
      timeseries[[i]] <- my_list[[i]]$interest_over_time %>%
        select(1:3) %>%
        mutate(`Search Type` = type,
               `Search Period` = start_date[i],
               Timestamp = Sys.Date(),
               Weekend = case_when(weekdays(date, abbr = TRUE) %in% c("Sat", "Sun") ~ "Weekend",
                                   TRUE ~ "Weekday"))
    }

    if(!is.null(my_list[[i]]$interest_by_region)){
      state_search[[i]] <- my_list[[i]]$interest_by_region %>%
                            select(1:3) %>%
                            mutate(`Search Type` = type,
                                   `Search Period` = start_date[i],
                                   Timestamp = Sys.Date(),
                                   geo = location)
    }
    
    if(!is.null(my_list[[i]]$interest_by_dma)){
      city_search[[i]] <- my_list[[i]]$interest_by_dma %>%
                            select(1:3) %>%
                            mutate(`Search Type` = type,
                                   `Search Period` = start_date[i],
                                   Timestamp = Sys.Date(),
                                   geo = location) 
    }
    
    if(!is.null(my_list[[i]]$related_queries)){
      related[[i]] <- my_list[[i]]$related_queries %>%
                        select(1:3,5) %>%
                        mutate(`Search Type` = type,
                               `Search Period` = start_date[i],
                               Timestamp = Sys.Date())
    }
    
  }
  
  timeseries_df <- do.call(rbind, timeseries)
  state_search_df <- do.call(rbind, state_search)
  city_search_df <- do.call(rbind, city_search)
  related_df <- do.call(rbind, related)

  return(plyr::rbind.fill(timeseries_df, state_search_df, city_search_df, related_df))
}

# Pull in monthly data
# product_search_monthly <- list()
# brand_search_monthly <- list()
# competitor_search_monthly <- list()
# for(x in 1:length(date_start_table)){
#   product_search_monthly[[x]] <- googleTrendsData(keywords = products, time = paste(date_start_table[x], " ", date_end_table[x], sep = '')) %>%
#                                   tidy_list(type = "Product", period = date_start_table)
#   brand_search_monthly[[x]] <- googleTrendsData(keywords = brands, time = paste(date_start_table[x], " ", date_end_table[x], sep = ''))
#   competitor_search_monthly[[x]] <- googleTrendsData(keywords = competitors, time = paste(date_start_table[x], " ", date_end_table[x], sep = ''))
# }

# Pull in monthly data into list of lists
search_results <- list()
temp_search_results <- list()
for(x in 1:length(gtrends_list)){
  for(y in 1:length(date_start_table)){
    temp_search_results[[y]] <- googleTrendsData(keywords = paste(gtrends_list[[x]][3:7])[!is.na(gtrends_list[[x]][3:7])], time = ifelse(y == length(date_start_table), "all", paste(date_start_table[y], " ", date_end_table[y], sep = '')))# %>%
                                  #tidy_list(type = gtrends_list[[x]][2], period = date_start_table)
  }
  search_results[[x]] <- temp_search_results
}

# Unlist into tidy data frames
df_list <- list()
# for(x in 1:length(search_results)){
#     df_list[[x]] <- tidy_list(my_list = search_results[[x]], type = gtrends_list[[x]][2], period = interval)
# }

timeseries <- list()
state_search <- list()
city_search <- list()
related <- list()

temp_df <- data.frame()
timeseries <- data.frame()
country <- data.frame()
region <- data.frame()
dma <- data.frame()
city <- data.frame()
related_topics <- data.frame()
related_queries <- data.frame()

for(x in 1:length(search_results)){
  for(y in 1:length(date_start_table)){
    # Series Dataframe
    temp_df <- search_results[[x]][[y]]$interest_over_time %>%
                as.data.frame() %>%
                mutate(Name = gtrends_list[[x]]$Name,
                       `Search Name` = gtrends_list[[x]]$`Search Name`,
                       `Search Date` = paste(date_start_table[[y]], " - ", date_end_table[[y]], sep = '')
                       )
    timeseries <- rbind(timeseries, temp_df)
    
    # Country
    # temp_df <- search_results[[x]][[y]]$interest_by_country %>%
    #             as.data.frame() %>%
    #             mutate(Name = gtrends_list[[x]]$Name,
    #                    `Search Name` = gtrends_list[[x]]$`Search Name`,
    #                    `Search Date` = paste(date_start_table[[y]], " - ", date_end_table[[y]], sep = '')
    #             )
    # country <- rbind(country, temp_df)
    
    # Region
    temp_df <- search_results[[x]][[y]]$interest_by_region %>%
                as.data.frame() %>%
                mutate(Name = gtrends_list[[x]]$Name,
                       `Search Name` = gtrends_list[[x]]$`Search Name`,
                       `Search Date` = paste(date_start_table[[y]], " - ", date_end_table[[y]], sep = '')
                )
    region <- rbind(region, temp_df)
    
    # DMA
    temp_df <- search_results[[x]][[y]]$interest_by_dma %>%
                as.data.frame() %>%
                mutate(Name = gtrends_list[[x]]$Name,
                       `Search Name` = gtrends_list[[x]]$`Search Name`,
                       `Search Date` = paste(date_start_table[[y]], " - ", date_end_table[[y]], sep = '')
                )
    dma <- rbind(dma, temp_df)
    
    # City
    # temp_df <- search_results[[x]][[y]]$interest_by_city %>%
    #             as.data.frame() %>%
    #             mutate(Name = gtrends_list[[x]]$Name,
    #                    `Search Name` = gtrends_list[[x]]$`Search Name`,
    #                    `Search Date` = paste(date_start_table[[y]], " - ", date_end_table[[y]], sep = '')
    #             )
    # city <- rbind(city, temp_df)
    
+    # Related Topics
    # temp_df <- search_results[[x]][[y]]$related_topics %>%
    #             as.data.frame() %>%
    #             mutate(Name = gtrends_list[[x]]$Name,
    #                    `Search Name` = gtrends_list[[x]]$`Search Name`,
    #                    `Search Date` = paste(date_start_table[[y]], " - ", date_end_table[[y]], sep = '')
    #             )
    # related_topics <- rbind(related_topics, temp_df)
    
    # Related Queries
    # temp_df <- search_results[[x]][[y]]$related_queries %>%
    #             as.data.frame() %>%
    #             mutate(Name = gtrends_list[[x]]$Name,
    #                    `Search Name` = gtrends_list[[x]]$`Search Name`,
    #                    `Search Date` = paste(date_start_table[[y]], " - ", date_end_table[[y]], sep = '')
    #             )
    # related_queries <- rbind(related_queries, temp_df)
  }
}

# Producing tidy datasets
product_tidy <- tidy_list(product_search_monthly, type = "Product", period = "Monthly")
brand_tidy <- tidy_list(brand_search_monthly, type = "Brand", period = "Monthly")
competitor_tidy <- tidy_list(competitor_search_monthly, type = "Competitor", period = "Monthly")

final_trends <- plyr::rbind.fill(product_tidy, brand_tidy, competitor_tidy) %>%
                  filter(!is.na(hits) & !is.na(subject) & !is.na(related_queries) & !is.na(value)) %>% 
                  mutate(`Weekend or Weekday` = case_when(weekdays(Date) %in% c("Saturday", "Sunday") ~ "Weekend",
                                                          TRUE ~ "Weekday"))
############################################ Export ############################################
# Download the dataframe "output" as a .csv file 
write_csv(final_trends, paste(getwd(),"/Google Trends/final_trends.csv",sep=''))
saveRDS(final_trends, paste(getwd(),"/Google Trends/trends.RDS",sep=''))

test <- gtrends("elkay",
                gprop = "web",
                geo = "US",
                time = "all")
start <- seq(month_begin, as.Date(floor_date(Sys.Date())), by="months")

#seq(month_begin %m-% months(36), month_begin, by="months")
test2 <- gtrends("elkay",
                gprop = "web",
                geo = "US",
                time = c("all","2004-01-01 2004-01-31")
)


glist <- list()
for(i in 1:(length(periods)-1)){
  glist[[i]] <- gtrends("elkay",
                        gprop = "web",
                        geo = "US",
                        time = paste(periods[[i]], " ", periods[[i+1]]-1, sep = ''))[[1]]
}

############################################ Weekend Traffic ############################################

base_elkay <- googleTrendsData(keywords = "elkay", geo = "US", time = "all")[[1]]
base_elkay_sinks <- googleTrendsData(keywords = "elkay sinks", geo = "US", time = "all")[[1]]
base_dayton_sinks <- googleTrendsData(keywords = "dayton sinks", geo = "US", time = "all")[[1]]
base_elkay_drinking_fountain <- googleTrendsData(keywords = "elkay drinking fountain", geo = "US", time = "all")[[1]]
base_elkay_cooler <- googleTrendsData(keywords = "elkay cooler", geo = "US", time = "all")[[1]]
base_elkay_bfs <- googleTrendsData(keywords = "elkay bottle filling station", geo = "US", time = "all")[[1]]
base_elkay_faucet <- googleTrendsData(keywords = "elkay faucet", geo = "US", time = "all")[[1]]

daily_elkay_list <- list()
for(i in 1:length(periods)){
  daily_list[[i]] <- googleTrendsData(keywords = "elkay", geo = "US", time = paste(periods[i], " ", as.Date(periods[i]) + months(1) - 1, sep = ''))[[1]]
}
daily <- do.call(rbind, daily_list)

base <- base %>%
  mutate(month = month(date),
         year = year(date)) %>%
  dplyr::rename(weight = hits) 

df <- daily %>%
  select(1:2) %>%
  mutate(month = month(date),
         year = year(date)) %>%
  left_join(base, by = c("month", "year"))
df <- df %>% select(-c(month,year,date.y))
df$result <- df$hits * df$weight/100
df <- df %>% dplyr::rename(date = "date.x")

df <- df %>%
  mutate(weekend = case_when(weekdays(date) %in% c("Saturday","Sunday") ~ "Weekend",
                             TRUE ~ "Weekday"))

ggplot(final_df[final_df$date > as.Date("2018-12-31"),], aes(date, result)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  facet_wrap(. ~ weekend) +
  theme_minimal() +
  labs(x = "", y = "Search Popularity (100 = peak)")