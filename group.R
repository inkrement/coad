library(feather)

## group by
# There are multiple ways to aggregate different strings
# - select most occuring element: names(which.max(table(Region)))
# - select latest value: tail(Region, n=1)
# - or only select if it is always the same and if it is not set it NA

groupBids <- function (data) {
  result <- data %>% group_by(UserID) %>% summarise(
    delivered_ads = n(),
    conversion = sum(conv),
    lag = if (max(conv) == 1) which.max(conv) else NA,
    
    Browser = tail(Browser, n=1),
    Region = tail(Region, n=1),
    City = tail(City, n=1),
    Adslotwidth = if (length(unique(Adslotwidth)) == 1) first(Adslotwidth) else NA,
    Adslotheight = if (length(unique(Adslotheight)) == 1) first(Adslotheight) else NA
  )
  
  return(result)
}

read_feather('./data/ad_1.feather') %>% groupBids %>% write_feather('./data/ad1_grouped.feather')
read_feather('./data/ad_2.feather') %>% groupBids %>% write_feather('./data/ad2_grouped.feather')