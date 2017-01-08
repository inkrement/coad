library(feather)

test <- read_feather('./data/ad_1.feather')

## group by
# There are multiple ways to aggregate different strings
# - select most occuring element: names(which.max(table(Region)))
# - select latest value: tail(Region, n=1)
# - or only select if it is always the same and if it is not set it NA

somename <- function (data) {
  result <- data %>% group_by(UserID) %>% summarise(
    delivered_ads = n(),
    conversion = sum(conv),
    # lag 0 if not set? NA would not work
    lag = if (max(conv) == 1) which.max(conv) else 0,
    
    Browser = tail(Browser, n=1),
    Region = tail(Region, n=1),
    City = tail(City, n=1),
    Adslotwidth = if (length(unique(Adslotwidth)) == 1) first(Adslotwidth) else NA,
    Adslotheight = if (length(unique(Adslotheight)) == 1) first(Adslotheight) else NA
  )
  
  return(result)
}

adv1 <- read_feather('./data/ad_1.feather') %>% somename
adv2 <- read_feather('./data/ad_2.feather') %>% somename


# Analysing how often users should see ads
adv1_regression <- glm(conversions ~ lag + delivered_ads, data = adv1, family = "binomial"(link="probit"))
summary(adv1_regression)

adv2_regression <- glm(conversions ~ lag + delivered_ads, data = adv2, family = "binomial"(link="probit"))
summary(adv2_regression)
