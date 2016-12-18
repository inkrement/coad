#devtools::install_github("tidyverse/readr")
library(readr)
library(feather)
library(magrittr)
library(dplyr)

columns <- c("character","character","character","character","character","character","numeric","numeric","numeric","character","character","character","numeric","numeric","character","character","numeric","character","numeric","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")

#bids <- read_csv('./data/bids.csv')
bids <- read.csv('./data/bids.csv', colClasses=columns, stringsAsFactors = FALSE)
#write_feather(bids, './bids.feather')
#bids <- read_feather('./bids.feather')

head(bids)

toString(Time_Bid[1])

Time_Bid[1]

20131022144300000

as.Date(Time_Bid, format="%d/%m/%Y")

#head(bids)
#str(bids)

#attach(bids)

without_missing <- bids[!is.na(bids$UserID),]
without_empty <- without_missing[withoutmissing$UserID != "",]

# just ads that where viewed 
viewed <- without_empty[!is.na(without_empty$Payingprice),] 

data <- viewed[sample(nrow(viewed), size=200000),]

#n_distinct
result <- group_by(data, UserID) %>% summarise(
  delivered_ads = n(),
  conversions = sum(conv)
)


# Analysing how often users should see ads

