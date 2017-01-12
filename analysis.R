library(feather)
library(data.table)
library(pglm)

# Analysing how often users should see ads
#res.adv1 <- glm(conversions ~ delivered_ads + lag, data = adv1, family=binomial())
#summary(res.adv1)
#b <- adv1[which(adv1$conv == 1 & adv1$lag != adv1$delivered_ads),]
#str(adv1)
#length(bids[which(adv1$conv == 1),])
#length(unique(bids$UserID))


bids <- read_feather('./data/ad_1.feather')

bids$Browser <- NULL
bids$Region <- NULL
bids$Adslotformat <- NULL
bids$CreativeID <- NULL
bids$Adslotvisibility <- NULL

bids <- data.table(bids)
bids <- bids[order(UserID,Time_Bid)]
bids <- setDT(bids)[, t := 1:.N, by=UserID]
bids <- as.data.frame(bids)

#  model = "pooling", method = "bfgs" , print.level = 3, R = 5
anb <- pglm(conv ~ t ,data=bids, index=c("UserID") ,family = binomial('logit'))

summary(anb)
