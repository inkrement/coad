library(readr)
library(magrittr)
library(dplyr)

bids <- read_csv('./data/bids.csv', col_types = list(
  Time_Bid = col_character(),
  BidID = col_skip(),
  X1 = col_skip(),
  IP = col_skip(),
  Domain = col_skip(),
  AdslotID = col_skip(),
  URL = col_skip()
)) %>% mutate(
  Time_Bid = substr(Time_Bid, 1, 12)
) %>% write_csv('./data/bids_preprocessed.csv')

bids <- read_csv('./data/bids_preprocessed.csv', col_types = list(
  Time_Bid = col_datetime("%Y%m%d%H%M")
))

## remove unused rows
##TODO: empty userid?
cleanedbids <- bids %>% 
  filter(!is.na(UserID)) %>%  # we need the user id so delete all rows without one
  filter(!is.na(Time_Bid))  %>%  # same for bid time
  filter(!is.na(Payingprice)) %>%   # only bids that were displayed to the user
  arrange(Time_Bid) # order by date

## split set depending on advertiser
cleanedbids %>% filter(AdvertiserID != 2821) %>% select(-AdvertiserID) %>% write_feather('./data/ad_1.feather')
cleanedbids %>% filter(AdvertiserID != 2259) %>% select(-AdvertiserID) %>% write_feather('./data/ad_2.feather')
