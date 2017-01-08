library(feather)

# Analysing how often users should see ads
res.adv1 <- glm(conversions ~ delivered_ads + lag, data = adv1, family=binomial())
summary(res.adv1)