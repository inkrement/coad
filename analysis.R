graphics.off() # This closes all of R's graphics windows.
rm(list=ls())
library(feather)
library(data.table)
library(rethinking)
#library(rstan)
library(dplyr)
#install.packages("tidyr")
library(tidyr)
library(plyr)
#install.packages("lme4")
#library(lme4)
#install.packages("Zelig")
#library(Zelig)

# Analysing how often users should see ads
#res.adv1 <- glm(conversions ~ delivered_ads + lag, data = adv1, family=binomial())
#summary(res.adv1)
#b <- adv1[which(adv1$conv == 1 & adv1$lag != adv1$delivered_ads),]
#str(adv1)
#length(bids[which(adv1$conv == 1),])
#length(unique(bids$UserID))
#getwd()
bids1 <- read_feather('~/coad/data/ad_2.feather')
#which(bids$conv == 1)
#length(bids[,])
#length(bids[which(bids$conv==0),])

bids1 <- data.table(bids1)
bids <- unique(bids1, by = c("UserID", "Time_Bid"))
bids <- bids[order(bids$UserID,bids$Time_Bid),]
bids <- setDT(bids)[, t := 1:.N, by="UserID"]
bids <- as.data.frame(bids)
#head(bids[,c("UserID","Time_Bid","t")],500)

table(bids$conv)
table(bids$t)
hist(bids$t)
bids$ID <- coerce_index(bids$UserID)

#todo : immer 13?
bids <- bids %>% dplyr::group_by(ID) %>% dplyr::mutate(obs_per_user = max(t))
bids <- bids %>% dplyr::group_by(ID) %>% dplyr::mutate(imp_per_user = sum(imp))
bids <- bids %>% dplyr::group_by(ID) %>% dplyr::mutate(conv_per_user = sum(conv))
bids <- as.data.frame(bids)
table(bids$imp_per_user)
table(bids[bids$conv_per_user==1,]$imp_per_user)
table(bids$obs_per_user)
table(bids$conv_per_user)
#number of creative IDs
length(unique(bids$CreativeID))
#number of IDs
length(unique(bids$ID))
length(unique(bids$Region))
length(unique(bids$City))
bids <- as.data.frame(bids)
table(bids$obs_per_user)
names(bids)
#conversion rate
(nrow(bids[which(bids$conv==1),])/nrow(bids[which(bids$conv==0),]))*100

subs <- subset(bids[,c("ID","UserID","t","conv","obs_per_user","imp_per_user")], conv==1)
subs <- plyr::rename(subs, c(t="t1"))
#subs
table(subs$t1)
#subs[,c("UserID","conv","t1","obs_per_user")]
#subset(bids, UserID=="DAL9T45AuXk")

## like a join
bids <- merge(bids,subs[,c("ID","t1")], all.x = T, by = c("ID"))
bids$t.new <- ifelse(!is.na(bids$t1), bids$t1, bids$t)
table(bids$t.new)
table(bids$t)
bids <- as.data.frame(bids)
#bids.conv <- bids[bids$conv_per_user==1,]
#bids.conv[order(bids.conv$ID,bids.conv$Time_Bid),c("CreativeID","t","t.new","Time_Bid","conv","ID","imp_per_user","conv_per_user","obs_per_user")]
#bids[order(bids$ID,bids$Time_Bid),c("CreativeID","t","t.new","Time_Bid","conv","ID","imp_per_user","conv_per_user","obs_per_user")]
#alternative data set excluding all observations after a conversion took place
bids.clean <- subset(bids,t<=t.new)

#bids <- plyr::rename(bids, c(t.y1="t"))
#names(bids)
#aggregate data at ID level
bids.agg <- data.table(bids.clean, key = c("ID"))
bids.agg <- bids.agg[, list(conv_all = sum(conv),imp_all = max(t),region=first(Region),city=first(City),click_all=sum(click)), by = key(bids.agg)]
head(bids.agg)
#table(bids.agg$conv_all)
#table(bids.agg$click_all)
table(bids.agg$imp_all)
1-nrow(bids.agg)/nrow(bids)
bids.agg <- as.data.frame(bids.agg)
table(bids.agg[bids.agg$conv_all==1,]$imp_all)
table(bids.agg$imp_all)
#conversion rate by number of impressions
conv.n <- table(bids.agg[bids.agg$conv_all==1,]$imp_all)
n <- table(bids.agg$imp_all)[1:3]
conv.n/n
bids.agg$imp_all_cens <- ifelse(bids.agg$imp_all>5,6,bids.agg$imp_all) 
#mean center impressions variable
bids.agg$cimp <- scale(bids.agg$imp_all, scale = FALSE)
bids.agg$cimp2 <-  bids.agg$cimp*bids.agg$cimp
bids.agg$cimp_cens <- scale(bids.agg$imp_all_cens, scale = FALSE)
bids.agg$cimp_cens2 <-  bids.agg$cimp_cens*bids.agg$cimp_cens
#tail(bids.agg[,c("imp_all","obs_all")],1000)
bids.agg <- as.data.frame(bids.agg)

#intercept_only
model <- glm(conv_all ~ 1,family=binomial(link='logit'),data=bids.agg)
summary(model)
logistic(model$coefficients[1])
#true conversion rate
(nrow(bids[which(bids$conv==1),])/nrow(bids[which(bids$conv==0),]))

#intercept + impressions
model1 <- glm(conv_all ~ 1 + imp_all,family=binomial(link='logit'),data=bids.agg)
summary(model1)
model1b <- glm(conv_all ~ 1 + imp_all_cens,family=binomial(link='logit'),data=bids.agg)
summary(model1b)

#intercept + log impressions
model2 <- glm(conv_all ~ 1 + log(imp_all),family=binomial(link='logit'),data=bids.agg)
summary(model2)
model2b <- glm(conv_all ~ 1 + log(imp_all_cens),family=binomial(link='logit'),data=bids.agg)
summary(model2b)

#intercept + impressions + impressions2
model3 <- glm(conv_all ~ 1 + cimp + cimp2,family=binomial(link='logit'),data=bids.agg)
summary(model3)
model3b <- glm(conv_all ~ 1 + cimp_cens + cimp_cens2,family=binomial(link='logit'),data=bids.agg)
summary(model3b)

#hierarchical model
#model with group scpecific intercepts (one incercept per unique number of impressions)
model.agg.group <- map2stan(
  alist(
    conv_all ~ dbinom( 1 , p ) ,
    logit(p) <- a + a_group[imp_all_cens] ,
    a_group[imp_all_cens]  ~ dnorm( 0 , sigma_group ),
    a ~ dnorm(0,10),
    sigma_group ~ dcauchy(0,1)
  ) ,
  data=bids.agg , warmup=500 , iter=1000 , chains=1 , cores=3)
options(scipen = 999)
precis(model.agg.group,depth = 2)
logistic(-7.81-0.12)
logistic(-7.81+0.05)


#same model with poison distribution
model.agg.group1 <- map2stan(
  alist(
    conv_all ~ dpois( lambda ) ,
    log(lambda) <- a + a_group[imp_all_cens] ,
    a_group[imp_all_cens]  ~ dnorm( 0 , sigma_group ),
    a ~ dnorm(0,10),
    sigma_group ~ dcauchy(0,1)
  ) ,
  data=bids.agg , warmup=350 , iter=500 , chains=1 , cores=3)
precis(model.agg.group1,depth = 2)

#survival model
## Load survival package
#http://rstudio-pubs-static.s3.amazonaws.com/5896_8f0fed2ccbbd42489276e554a05af87e.html
library(survival)
table(bids.agg[bids.agg$conv_all==1,]$imp_all)
table(bids.agg$imp_all)
bids.agg$SurvObj <- with(bids.agg, Surv(imp_all_cens, conv_all == 1))
#bids.agg$SurvObj <- with(bids.agg, Surv(imp_all_cens, conv_all == 0))
surv.model <- survfit(SurvObj ~ 1, data = bids.agg)
## See survival estimates at given time (lots of outputs)
summary(surv.model)
## Plotting without any specification
plot(surv.model)
surv.model
head(bids.agg)


# todo
# rare events/zero inflation/mixture/survival
# account for time effects?
#z.out1 <- zelig(conv_all ~ 1 + log(imp_all), data=bids.agg, model = "relogit", tau = (nrow(bids[which(bids$conv==1),])/nrow(bids[which(bids$conv==0),]))*100)



#bayesian models

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

#same model with mcmc (takes longer!)
#model.agg.mcmc <- map2stan(
#  alist(
#    conv_all ~ dbinom( 1 , p ) ,
#    logit(p) <- a,
#    a ~ dnorm(0,10)
#  ) ,
#  data=bids.agg , warmup=250 , iter=500 , chains=2 , cores=3)
#precis(model.agg )

#model with intercept and number of inpressions
model.agg1 <- map(
  alist(
    conv_all ~ dbinom( 1 , p ) ,
    logit(p) <- a + beta*cimp,
    a ~ dnorm(0,100) ,
    beta ~ dnorm(0,1)
  ) ,
  data=bids.agg )
precis(model.agg1 )
#relative effect of frequency
exp(0.39)
#absolute effect
#baseline probability
logistic(-9.06)
#increasing by one impression
logistic(-9.06 + 0.39)

stats::AIC(model.agg, model.agg1)
stats::BIC(model.agg, model.agg1)
compare(model.agg, model.agg1)

#poison model
bids.agg$logimp_new <- log(bids.agg$imp_all)
bids.agg$clogimp_new <- scale(bids.agg$logimp_new, scale = FALSE)
bids.agg <- as.data.frame(bids.agg)

model.agg2 <- map(
  alist(
    conv_all ~ dpois( lambda ) ,
    log(lambda) <- a + beta*cimp_new,
    a ~ dnorm(0,10) ,
    beta ~ dnorm(0,10)
  ) ,
  data=bids.agg )
precis(model.agg1 )

model.agg1 <- map(
  alist(
    conv_all ~ dzipois(p , lambda ) ,
    logit(p) <- ap,
    log(lambda) <- al,
    ap ~ dnorm(0,1) ,
    al ~ dnorm(0,10)
  ) ,
  data=bids.agg )
precis(model.agg1 )





for(t in unique(bids.agg$imp_all_cens)) {
  bids.agg[paste("group",t,sep="")] <- ifelse(bids.agg$imp_all_cens==t,1,0)
}
m <- glmer(conv_all ~ 1 + (1 | group1) + (1 | group2) + (1 | group3) + (1 | group4), data = bids.agg, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m)
coefficients(m)





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
