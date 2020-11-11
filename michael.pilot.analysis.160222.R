#michael pre-test 

setwd("~/Google_Drive/Kal Higgins docs/Michael passage study/R analyses Michael")
mic <- read.csv(file="Michael_pilot_160222.csv")


####### EXCLUSIONS ######
#originally 253

#exclude attnck1: at 240
mic <- mic[ which(mic$attnchk1 == 2) , ]
#exclude seen prev michael description: at 234
mic <- mic[ which(mic$prevmic == 2) , ]
#exclude 'attnck2'survey: at 223
mic <- mic[ which(mic$attn1 == "survey") , ]


attach(mic)

# rmq
mic$loc <- (rmq_1 + rmq_3 + rmq_4 + rmq_5 + rmq_8 + rmq_16 + rmq_21 + rmq_25 + rmq_28 + rmq_29 + (7-rmq_13) + (7-rmq_24))/12
mic$ass <- ((7-rmq_2) + rmq_6 + rmq_7 + rmq_9 + (7-rmq_10) + rmq_11 + rmq_15 + rmq_19 + rmq_20 + rmq_22 + (7-rmq_27) + rmq_30)/12
mic$locomassess = mic$loc-mic$ass

detach(mic)

mic$micpass <- as.factor(mic$micpass) 
mic$gender <- as.factor(mic$gender)

# subsetting

micv1 <- subset(mic, micpass=="mic1")
micv2 <- subset(mic, micpass=="mic2")
micv3 <- subset(mic, micpass=="mic3")
micv4 <- subset(mic, micpass=="mic4")
micv5 <- subset(mic, micpass=="mic5")

#Kal measures with construal#

m <- aov(like7 ~ micpass, mic)
summary(m)
summaryBy(like7 ~ micpass, FUN = c(mean,sd), data=mic)
TukeyHSD(m)

m2 <-aov(recall_avg ~ like7*micpass, mic)
summary(m2)

summary(lm(recall_avg ~ likecont_1, data=mic))




ggplot(mic, aes(micpass, recall_avg)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=micpass), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=micpass), fun.y = mean, geom="point", colour="black", size=3) +
  theme(legend.position='none') +
  #facet_wrap ("like7") + 
  ggtitle("construal")

#summaryBy(like7 ~ micpass, FUN = c(mean,sd), data=mic)
#TukeyHSD(m2)

# DESCRIPTIVES

summary(mic$age)
sd(mic$age)
table(mic$gender)


##### CONTINUOUS RATINGS - MAIN EFFECTS ######

library(ggplot2)
theme_set(theme_bw(base_size = 14))
library(doBy)

likeavg <- (mic$like7 + mic$likecont_1 + mic$friends)/3

likeavg_mic2<- (micv2$like7 + micv2$likecont_1 + micv2$friends)/3

summary(likeavg ~ micpass, mic)

summary(likeavg_mic2 ~ micpass, micv2)


sd(micv2$likeavg)

table(mic$likeavg)

m <- aov(likeavg ~ micpass, mic)
summary(m)
summaryBy(likeavg ~ micpass, FUN = c(mean,sd), data=mic)
TukeyHSD(m)

ggplot(mic, aes(micpass, likeavg)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=micpass), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=micpass), fun.y = mean, geom="point", colour="black", size=3) +
  theme(legend.position='none') +
  ggtitle("Like composite")


# like7

m <- aov(like7 ~ micpass, mic)
summary(m)
summaryBy(like7 ~ micpass, FUN = c(mean,sd), data=mic)
TukeyHSD(m)

ggplot(mic, aes(micpass, like7)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=micpass), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=micpass), fun.y = mean, geom="point", colour="black", size=3) +
  theme(legend.position='none') +
  ggtitle("like7")

# likecont_1

m <- aov(likecont_1 ~ micpass, mic)
summary(m)
summaryBy(likecont_1 ~ micpass, FUN = c(mean,sd), data=mic)
TukeyHSD(m)

ggplot(mic, aes(micpass, likecont_1)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=micpass), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=micpass), fun.y = mean, geom="point", colour="black", size=3) +
  theme(legend.position='none') +
  ggtitle("likecont_1")

# friends

m <- aov(friends ~ micpass, mic)
summary(m)
summaryBy(friends ~ micpass, FUN = c(mean,sd), data=mic)
TukeyHSD(m)

ggplot(mic, aes(micpass, friends)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=micpass), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=micpass), fun.y = mean, geom="point", colour="black", size=3) +
  theme(legend.position='none') +
  ggtitle("friends")



##### GENDER INTERACTIONS ####

# like 7
m <- lm(like7 ~ gender, mic)
summary(m) #ns
m <- lm(like7 ~ micpass*gender, mic)
summary(m) #marginal 5*gender
m <- lm(like7 ~ gender, micv1)
summary(m) #ns
m <- lm(like7 ~ gender, micv2)
summary(m) #ns
m <- lm(like7 ~ gender, micv3)
summary(m) #ns
m <- lm(like7 ~ gender, micv4)
summary(m) #ns
m <- lm(like7 ~ gender, micv5)
summary(m) #marginal (female like more)

ggplot(micv5, aes(gender, like7)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=gender), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=gender), fun.y = mean, geom="point", colour="black", size=3) +
  theme(legend.position='none') +
  ggtitle("like7") 

ggplot(mic, aes(micpass, like7)) + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar') +
  geom_jitter(aes(shape=micpass), size=2, position = position_jitter(width = .2)) + 
  stat_summary(aes(group=micpass), fun.y = mean, geom="point", colour="black", size=3) +
  theme(legend.position='none') +
  ggtitle("like7") +
  facet_wrap('gender')

# likecont_1
m <- lm(likecont_1 ~ gender, mic)
summary(m) #ns
m <- lm(likecont_1 ~ micpass*gender, mic)
summary(m) #marginal 5*gender
m <- lm(likecont_1 ~ gender, micv1)
summary(m) #ns
m <- lm(likecont_1 ~ gender, micv2)
summary(m) #ns
m <- lm(likecont_1 ~ gender, micv3)
summary(m) #ns
m <- lm(likecont_1 ~ gender, micv4)
summary(m) #ns
m <- lm(likecont_1 ~ gender, micv5)
summary(m) #ns


# friends
m <- lm(friends ~ gender, mic)
summary(m) #ns
m <- lm(friends ~ micpass*gender, mic)
summary(m) #marginal 5*gender
m <- lm(friends ~ gender, micv1)
summary(m) #ns
m <- lm(friends ~ gender, micv2)
summary(m) #ns
m <- lm(friends ~ gender, micv3)
summary(m) #ns
m <- lm(friends ~ gender, micv4)
summary(m) #ns
m <- lm(friends ~ gender, micv5)
summary(m) #marginal


###### TRAITS - OLD ANALYSES ######

summary(micv1$honest1_12)
sd(micv1$honest1_1)
table(mic$honest1_1)

summary(micv1$cult1_12)
sd(micv1$cult1_1)
table(mic$cult1_1)

table(micv1$advent1_12)
table(micv1$indep) 
table(micv1$persist1)

table(micv2$honest2_12)
table(micv2$advent)
table(micv2$intel)
table(micv2$indep)
table(micv2$persist)

table(micv3$honest)
table(micv3$advent)
table(micv3$intel)
table(micv3$indep)
table(micv3$persist)

table(micv4$honest)
table(micv4$advent)
table(micv4$intel)
table(micv4$indep)
table(micv4$persist)

table(micv5$honest5)
table(micv5$advent)
table(micv5$intel)
table(micv5$indep)
table(micv5$persist)


#### AGE & GENDER (m=1) ####

table(micv1$gender)
table(micv1$likebinary, micv1$gender) # both genders biased (women dislike, men like)
table(micv2$likebinary, micv2$gender) # both genders biased
table(micv3$likebinary, micv3$gender) # both genders biased
table(micv4$likebinary, micv4$gender) # very slight M = 7:9 (like), W = 9:7 (dislike)

m <- lm(friends ~ age, data=micv4)
summary(m) #sig neg
m <- lm(friends ~ gender, data=micv4)
summary(m) #ns

m <- lm(likecont_1 ~ age, data=micv4)
summary(m) #ns
m <- lm(likecont_1 ~ gender, data=micv4)
summary(m) #ns


# like binary
fit <- glm(likebinary~age ,data=micv1,family=binomial())
fit <- glm(likebinary~gender ,data=micv1,family=binomial())


summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals


# MODE
m <- lm(friends ~ ass + loc, data=micv1)
summary(m) #ns
m <- lm(likecont_1 ~ ass + loc, data=micv1)
summary(m) #ns

fit <- glm(likebinary~ass+loc ,data=micv1,family=binomial())
summary(fit) # ns



