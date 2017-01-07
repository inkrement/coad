library(readr)
library(feather)
library(magrittr)
library(dplyr)

bids <- read_csv('./data/bids.csv', col_types = list(
    Time_Bid = col_datetime("%Y%m%d%H%M00000"),
    BidID = col_skip(),
    X1 = col_skip(),
    IP = col_skip(),
    Domain = col_skip(),
    AdslotID = col_skip(),
    URL = col_skip()
))

## remove unused rows
##TODO: empty userid?
cleanedbids <- bids %>% 
  filter(!is.na(UserID)) %>%  # we need the user id so delete all rows without one
  filter(!is.na(Time_Bid))  %>%  # same for bid time
  filter(!is.na(Payingprice)) %>%   # only bids that were displayed to the user
  arrange(Time_Bid) # order by date

## TODO: split set depending on advertiser
cleanedbids %>% filter(AdvertiserID != 2821) %>% select(-AdvertiserID) %>% write_feather('ad_1.feather')
cleanedbids %>% filter(AdvertiserID != 2259) %>% select(-AdvertiserID) %>% write_feather('ad_2.feather')

## TODO: group by
adv1 <- read_feather('ad_1.feather') %>% group_by(UserID) %>% summarise(
  delivered_ads = n(),
  conversions = sum(conv),
  lag = if (max(conv) == 1) which.max(conv) else NA,
  
  Region = names(which.max(table(Region))),
  City = names(which.max(table(City))),
)

# Analysing how often users should see ads

