#install.packages("feather")
library(feather)
#install.packages("data.table")
library(data.table)
#install.packages("pglm")
#library(pglm)
#library(bestglm)
library(rethinking)
#library(rstan)
library(dplyr)
#install.packages("tidyr")
library(tidyr)
library(plyr)

# Analysing how often users should see ads
#res.adv1 <- glm(conversions ~ delivered_ads + lag, data = adv1, family=binomial())
#summary(res.adv1)
#b <- adv1[which(adv1$conv == 1 & adv1$lag != adv1$delivered_ads),]
#str(adv1)
#length(bids[which(adv1$conv == 1),])
#length(unique(bids$UserID))
getwd()
bids <- read_feather('~/coad/data/ad_1.feather')
#which(bids$conv == 1)
#length(bids[,])
#length(bids[which(bids$conv==0),])

bids <- data.table(bids)
bids <- bids[order(bids$UserID,bids$Time_Bid),]
bids <- setDT(bids)[, t := 1:.N, by="UserID"]
bids <- as.data.frame(bids)

table(bids$conv)
table(bids$t)
bids$ID <- coerce_index(bids$UserID)
bids <- bids %>% group_by(ID) %>% mutate(obs_per_user = max(t))
bids <- bids %>% group_by(ID) %>% mutate(imp_per_user = sum(imp))
bids <- bids %>% group_by(ID) %>% mutate(conv_per_user = sum(conv))
bids <- as.data.frame(bids)
table(bids$imp_per_user)
table(bids$obs_per_user)
table(bids$conv_per_user)
#number of creative IDs
length(unique(bids$CreativeID))
#number of IDs
length(unique(bids$ID))
length(unique(bids$Region))
length(unique(bids$City))



#conversion rate
(nrow(bids[which(bids$conv==1),])/nrow(bids[which(bids$conv==0),]))

subs <- subset(bids[,c("ID","UserID","t","conv","obs_per_user","imp_per_user")], conv==1)
subs
table(subs$t)
#subs[,c("UserID","conv","t","obs_per_user")]
#subset(bids, UserID=="DAL9T45AuXk")
bids <- merge(bids,subs[,c("ID","t")], all.x = T, by = c("ID"))
bids$t.y1 <- ifelse(!is.na(bids$t.y), bids$t.y, bids$t.x)
table(bids$t.y1)
bids <- as.data.frame(bids)
bids <- plyr::rename(bids, c(t.y1="t"))
names(bids)
#aggregate data at ID level
bids.agg <- data.table(bids, key = c("ID"))
bids.agg <- bids.agg[, list(conv_all = sum(conv),imp_all = max(t),region=first(Region),city=first(City),click_all=sum(click)), by = key(bids.agg)]
head(bids.agg)
table(bids.agg$conv_all)
table(bids.agg$click_all)
table(bids.agg$imp_all)
1-nrow(bids.agg)/nrow(bids)
bids.agg <- as.data.frame(bids.agg)
#mean center impressions variable
bids.agg$cimp <- scale(bids.agg$imp_all, scale = FALSE)
bids.agg$cimp2 <-  bids.agg$cimp*bids.agg$cimp
#tail(bids.agg[,c("imp_all","obs_all")],1000)

#model with intercept only (quadratic approximation)
model.agg <- map(
  alist(
    conv_all ~ dbinom( 1 , p ) ,
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ) ,
  data=bids.agg )
precis(model.agg )

#probability of conversion (a-coefficient)
logistic(-9.03)
#true conversion rate
(nrow(bids[which(bids$conv==1),])/nrow(bids[which(bids$conv==0),]))

model <- glm(conv_all ~ 1,family=binomial(link='logit'),data=bids.agg)
summary(model)

model1 <- glm(conv_all ~ 1 + imp_all,family=binomial(link='logit'),data=bids.agg)
summary(model1)

model2 <- glm(conv_all ~ 1 + log(imp_all),family=binomial(link='logit'),data=bids.agg)
summary(model2)

model3 <- glm(conv_all ~ 1 + cimp + cimp2,family=binomial(link='logit'),data=bids.agg)
summary(model3)


#same model with mcmc (takes longer!)
#model.agg.mcmc <- map2stan(
#  alist(
#    conv_all ~ dbinom( 1 , p ) ,
#    logit(p) <- a + a[imp_all] ,
#    a ~ dnorm(0,10)
#  ) ,
#  data=bids.agg , warmup=250 , iter=500 , chains=2 , cores=3)
#precis(model.agg )

#model with intercept and number of inpressions
model.agg1 <- map(
  alist(
    conv_all ~ dbinom( 1 , p ) ,
    logit(p) <- a + beta*cimp,
    a ~ dnorm(0,10) ,
    beta ~ dnorm(0,10)
  ) ,
  data=bids.agg )
precis(model.agg1 )
#relative effect of frequency
#proportional increase of approx 48% for an additional exposure
exp(0.39)
#absolute effect
#baseline probability
logistic(-9.06)
#increasing by one impression
logistic(-9.06 + 0.39)

stats::AIC(model.agg, model.agg1)
stats::BIC(model.agg, model.agg1)
compare(model.agg, model.agg1)


#model with group scpecific intercepts (one incercept per unique number of impressions)
model.agg.group <- map2stan(
   alist(
     conv_all ~ dbinom( 1 , p ) ,
     logit(p) <- a + a_group[imp_all] ,
     a_group[imp_all]  ~ dnorm( 0 , sigma_group ),
     a ~ dnorm(0,10),
     sigma_group ~ dcauchy(0,1)
  ) ,
  data=bids.agg , warmup=250 , iter=500 , chains=2 , cores=3)
precis(model.agg.group,depth = 2)
options(scipen = 999)
logistic(-8.66 - 0.56)
logistic(-8.66 + 0.63)


#aggregate by ID and creative ID for model with creative specific intercept
bids$cID <- coerce_index(bids$CreativeID)
bids.agg <- data.table(bids, key = c("ID","cID"))
bids.agg <- bids.agg[, list(conv_all = sum(conv),obs_all = max(t),region=first(Region),city=first(City),imp_all=sum(imp),click_all=sum(click)), by = key(bids.agg)]
head(bids.agg)
table(bids.agg$conv_all)
table(bids.agg$click_all)
table(bids.agg$imp_all)
1-nrow(bids.agg)/nrow(bids)
bids.agg <- as.data.frame(bids.agg)
#mean center impressions variable
bids.agg$cimp <- scale(bids.agg$imp_all, scale = FALSE)

model.agg2 <- map2stan(
  alist(
    conv_all ~ dbinom( 1 , p ) ,
    logit(p) <- a + a_creative[cID] + beta*cimp,
    a_creative[cID]  ~ dnorm( 0 , sigma_creative ),
    a ~ dnorm(0,10),
    beta ~ dnorm(0,10),
    sigma_creative ~ dcauchy(0,1)
  ) ,
  data=bids.agg , warmup=250 , iter=500 , chains=2 , cores=3)






















class(bids)



str(Xy)

Xy <- within(bids, {
  Browser   <- NULL
  Region    <- NULL
  Adslotformat <- NULL
  CreativeID <- NULL
  Adslotvisibility <- NULL
  UserID <- NULL #factor(UserID)
  Time_Bid <- NULL
  imp <- NULL
  
  y    <- conv         # bwt into y
})
#, IC = "BICq"
bestglm(Xy, family = binomial)

bids$Browser <- NULL
bids$Region <- NULL
bids$Adslotformat <- NULL
bids$CreativeID <- NULL
bids$Adslotvisibility <- NULL



#  model = "pooling", method = "bfgs" , print.level = 3, R = 5
anb <- pglm(conv ~ t ,data=bids, index=c("UserID") ,family = binomial('logit'))

summary(anb)
