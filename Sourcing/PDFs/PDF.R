library(tidyverse)
library(pdftools)

text <- #pdf_text("./3rd Party Research/HOME_DEPOT_LOWE_S_Category_by_Category_Growth_Rates_and_Retailer_Performance.pdf") %>% 
          pdf_text("C:/Users/chris.deangelis/Elkay/Competitive Intelligence - General/3rd Party Research/HOME_DEPOT_LOWE_S_Category_by_Category_Growth_Rates_and_Retailer_Performance.pdf") %>%
          strsplit(split = "\n")

text_trimmed <- lapply(text, str_trim)
names(text_trimmed) <- 1:length(text)

complete_df <- data.frame()
for(i in 1:length(text)){
  temp_df <- text_trimmed[[i]] %>%
              unlist() %>%
              as.data.frame() %>%
              dplyr::rename("Text" = 1) %>%
              filter(grepl("[[:digit:]]", Text) | grepl("increased", tolower(Text)) | grepl("decreased", tolower(Text)) |
                     grepl("above", tolower(Text)) |  grepl("below", tolower(Text)) |
                     grepl("average", tolower(Text)) | 
                     grepl("[[:digit:]]q", tolower(Text)) | grepl("q[[:digit:]]", tolower(Text)) | grepl("quarter", tolower(Text)) |
                     grepl("year", tolower(Text))) %>%
              mutate(Page = i,
                     Source = "Cleveland Research Company",
                     Timestamp = as.Date("2021-12-31"))
  complete_df <- rbind(complete_df, temp_df)
}




          
          

