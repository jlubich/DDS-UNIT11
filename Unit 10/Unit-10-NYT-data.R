
#load first day dataset
fileLocation <- "http://stat.columbia.edu/~rachel/datasets/nyt1.csv"
nytData <- read.csv(url(fileLocation))
head(nytData)

#stratified Age groups
str(nytData)
nytData$ageGroup <- cut(nytData$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf))
levels(nytData$ageGroup) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

#Transform gender to a factor variable
nytData$Gender = factor(nytData$Gender, levels=c(1,0), labels = c("male", "female"))
head(nytData)

#plot ageGroup and impressions
ggplot(data=nytData, aes(x=ageGroup, y=Impressions, fill=ageGroup)) + geom_bar(stat="identity") + theme_bw()

#use Table(row, column) to compare age and gender for signedin
#only signedIn users have age and gender
table(nytData$ageGroup, nytData$Signed_In)
table(nytData$Gender, nytData$Signed_In)

#assign not signed in as NA
nytData$ageGroup[nytData$Signed_In == 0] = NA
nytData$Gender[nytData$Signed_In == 0] = NA
summary(nytData)

#plot again without NAs
ggplot(data=na.omit(nytData), aes(x=ageGroup, y=Impressions, fill=ageGroup)) + geom_bar(stat="identity") + theme_bw()

#(CTR = clicks/impressions) ; no CTR if there are no impressions
#conclusion <18 and 65+ have highest CTR
nytDataSubset <- subset(nytData, Impressions>0)

nytSummaryPerAgeGroup = na.omit(nytDataSubset) %>% 
                     group_by(ageGroup) %>% 
                     summarise(Impressions = sum(Impressions), Clicks = sum(Clicks))

ggplot(data=nytSummaryPerAgeGroup, aes(x=ageGroup, y=Clicks/Impressions, fill=ageGroup)) + geom_bar(stat="identity") + theme_bw()

#user segmentation:
#Impressions and no click
#Impressions and at least one click.
# count(filter(nytData, Impressions > 0 & Clicks == 0))
#415537
# count(filter(nytData, Impressions > 0 & Clicks > 0))
# 39838

nytData$UserSegment[nytData$Impressions > 0 & nytData$Clicks == 0] = "ImpsNoClick"
nytData$UserSegment[nytData$Impressions > 0 & nytData$Clicks > 0] = "ImpsAndClicks"
nytData$UserSegment = factor(nytData$UserSegment)
summary(nytData$UserSegment)

#get user segment data for the two click segments, omit NAs
nytUserSegmentData = na.omit(nytData) %>% 
                      group_by(ageGroup, UserSegment, Gender) %>% 
                      summarise(Impressions = sum(Impressions), Clicks = sum(Clicks))
head(nytUserSegmentData)

ggplot(data=subset(nytUserSegmentData,UserSegment == "ImpsAndClicks"), 
       aes(x=ageGroup, y=Clicks/Impressions, fill=Gender)) + 
      geom_bar(colour="black", stat="identity", position=position_dodge(), size=0.3) +
      scale_fill_hue(name="Gender of user segment") +
      xlab("Age category") + 
      ylab("CTR") + 
      ggtitle("CTR rates by age categories and gender for the ImpressionsAndClicks segment") + 
  theme_bw()