library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(ggthemes)
library(reportr)
library(knitr)
library(splines)

kki.demo <- read.csv("Data/KKI_demographicInfo.csv")
kki.social <- read.csv("Data/KKI_SocialResponsivenessScaleQuestionnaire.csv")
kki.movement <- read.csv("Data/KKI_movementAssessmentBatteryforChildren.csv")
kki.hand <- read.csv("Data\\KKI_handedness.csv")
wisc <- read.csv("Data/KKI_WechslerIntelligenceScaleforChildren.csv")
wisc4GAI <- read.csv("Data/wisc4GAI_lookup.csv")
wisc5GAI <- read.csv("Data/wisc5GAI_lookup.csv")
colnames(kki.demo)[1] <- "ID"
colnames(kki.social)[1] <- "ID"
colnames(kki.movement)[1] <- "ID"
colnames(kki.hand)[1] <- "ID"
colnames(wisc)[1] <- "ID"
colnames(wisc4GAI)[1] <- "ID"
colnames(wisc5GAI)[1] <- "ID"
wisc4 <- filter(wisc, WISC_VERSION == 4)
wisc5 <- filter(wisc, WISC_VERSION == 5)
wisc4 <- wisc4[-c(11:15)]
wisc5 <- wisc5[-c(5:10)]
wisc4 <- filter(wisc4, visit == 1)
wisc5 <- filter(wisc5, visit == 1)
#Calculate composite scores for both WISC versions
wisc4$score <- rowSums(wisc4[,5:10])
wisc5$score <- rowSums(wisc5[,5:9])
#Filter out NA scores
wisc4 <- na.omit(wisc4, cols = "score")
wisc5 <- na.omit(wisc5, cols = "score")
#Convert composite scores to GAI scores for WISC version 4
composite4 = rep(0,length(wisc4[,1]))
for (i in 1:length(wisc4[,1])) {
  composite4[i] <- wisc4GAI[wisc4[i,11]-5,2]
}
wisc4$GAI <- composite4
#Convert composite scores to GAI scores for WISC version 5
composite5 = rep(0,length(wisc5[,1]))
for (i in 1:length(wisc5[,1])) {
  composite5[i] <- wisc5GAI[wisc5[i,10]-4,2]
}
wisc5$GAI <- composite5
#Remove composite scores
wisc4 <- wisc4[-c(2:11)]
wisc5 <- wisc5[-c(2:10)]
#Combine WISC data
wisc.combo <- full_join(wisc4, wisc5, by = c('ID', 'GAI'))

kki <- merge(kki.demo,kki.social)
kki <- merge(kki,kki.movement)
kki <- merge(kki,wisc.combo)
kki <- merge(kki,kki.hand)
## 555
#Now have all the data in a huge data frame
#Removing N/A values. Mostly removing second visits.
kki <- subset(kki,!is.na(kki$SRS_TotalRawScore))
kki <- subset(kki, !is.na(kki$mABC_TotalStandardScore))
kki <- subset(kki,kki$mABC_AGE<14)
kki2 <- filter(kki,PrimaryDiagnosis != "None",SecondaryDiagnosis ==c("Yes", "No"))
multdia <- filter(kki, PrimaryDiagnosis != "None", SecondaryDiagnosis =="Yes")
singdia <- filter(kki, PrimaryDiagnosis != "None" , SecondaryDiagnosis == "No")
nodia <- filter(kki, PrimaryDiagnosis =="None" , SecondaryDiagnosis =="No")
kki.adhd <- subset(kki,PrimaryDiagnosis == "ADHD")
kki.adhd <- subset(kki.adhd,ADHD_Subtype != "Hyperactive/Impulsive")
kki.adhd1 <- subset(kki.adhd,kki.adhd$SRS_VERSION == 1)
kki.adhd2 <- subset(kki.adhd, kki.adhd$SRS_VERSION == 2)

SRS_1 <- which(kki$SRS_VERSION == 1)
kki.V1 <- kki[SRS_1,]
SRS_2 <- which(kki$SRS_VERSION == 2)
kki.V2 <- kki[SRS_2,]

mean.1 <- mean(kki.V1$SRS_TotalRawScore)
mean.2 <- mean(kki.V2$SRS_TotalRawScore)
var.1 <- var(kki.V1$SRS_TotalRawScore)
var.2 <- var(kki.V2$SRS_TotalRawScore)

kki.test1 <- (kki.V1$SRS_TotalRawScore-mean.1)/(sqrt(var.1))
kki.test2 <- (kki.V2$SRS_TotalRawScore-mean.2)/(sqrt(var.2))
hist(kki.V1$SRS_TotalRawScore)
hist(kki.V2$SRS_TotalRawScore)
hist(kki$SRS_TotalRawScore)
hist(sqrt(kki.V1$SRS_TotalRawScore))

kki.V2$Standard <- kki.V2$SRS_TotalRawScore - 13.28824
kki.V1$Standard <- kki.V1$SRS_TotalRawScore
kki.total <- rbind(kki.V1,kki.V2)
mean(kki.total$SRS_TotalRawScore)
var(kki.total$SRS_TotalRawScore)

## Trying the mean-mean 

y.sd <- sd(kki.V1$SRS_TotalRawScore)
x.sd <- sd(kki.V2$SRS_TotalRawScore)
y.bar <- mean(kki.V1$SRS_TotalRawScore)
x.bar <- mean(kki.V2$SRS_TotalRawScore)

Two_to_One <- function(score) {
  right.half = (y.bar - (y.sd/x.sd) * x.bar)
  left.half = y.sd/x.sd
  return (left.half * score + right.half)
}

test.standard <- Two_to_One(kki.V2$SRS_TotalRawScore)
mean(test.standard)
var(test.standard)

kki.V2$Standard <- Two_to_One(kki.V2$SRS_TotalRawScore)
kki.V1$Standard <- kki.V1$SRS_TotalRawScore
kki.total <- rbind(kki.V1,kki.V2)
test1 <- lm(Standard ~ mABC_TotalStandardScore,data = kki.total)
summary(test1)
kki.adhd <- subset(kki.total,kki.total$PrimaryDiagnosis == 'ADHD')
test.adhd <- lm(Standard ~ mABC_TotalStandardScore,data = kki.adhd)
kki.adhd$Standard.sqrt <- sqrt(kki.adhd$Standard)
test.adhd <- lm(Standard.sqrt ~ mABC_TotalStandardScore, data = kki.adhd)
summary(test.adhd)
