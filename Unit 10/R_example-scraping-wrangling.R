# Scraping-Wrangling example in R

rm(list=ls())
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("rvest")
# install.packages("tidyr")

library("rvest")   # Great for grabbing and parsing HTML
library("dplyr")   # Easy transformation of data.frames for summarization
library("tidyr")   # Nice way to arrange data
library("ggplot2") # Excellent for visuals

# Pulling from online
url <- 'http://espn.go.com/nfl/superbowl/history/winners'
webpage<- read_html(url)

# Identifying HTML nodes
sb_table<-html_nodes(webpage, 'table')
sb<-html_table(sb_table)

# Converting to Data Frame
a<-data.frame(sb)
names(a)<-a[2,]
df<-slice(a, 3:length(a$RESULT))

# Splitting Variables into Multiple
clean<-df %>%
  separate(RESULT, into=c("Team1", "Team2"), sep=", ") %>%
  # After splitting this way, I want the last 'word' from each column
  separate(Team1, into=c("Team1", "Team1_Score"),sep="[ ](?=[^ ]+$)") %>%
  separate(Team2, into=c("Team2", "Team2_Score"),sep="[ ](?=[^ ]+$)")
  
# Walking through the regular expression syntax
## [ ] - match a single character present in the set: here I chose a space.
## (?= ) - positive lookahead
## [^ ] - match a single character NOT present in the set: here I chose not a space
## * - matches between 0 and unlimited times, as many as possible
## $ - asserts position at the end of the line
### So, it says:
### 1) Look for a space.
### 2) Got it?  Now, right after that, look ahead for a not-space.
### 3) Is it the last one on the line? If so, split it!

# Create your Data Frame to Tidy
df<-data.frame(clean)
names(df) <- c("SuperBowl", "Date", "Site", "Team1", "Team1_Score", "Team2", "Team2_Score")

# Problem - Incorrect Classes
data.frame(sapply(df, class))

# Numeric Variables
df[,c("Team1_Score", "Team2_Score")]<-sapply(df[,c("Team1_Score", "Team2_Score")], as.numeric)

# Date Variable
df$Date<-as.Date(df$Date, format='%b. %d, %Y')

# Check Again
data.frame(sapply(df, class))

# Initial Count
df %>%
  count(Team1) %>%
  arrange(desc(n))

unique(df$Team1) # Watch out! May need to check if there are extras that need subsuming

# Creating a new variable
df["VictorySpread"]<- abs(df["Team1_Score"]-df["Team2_Score"])

# Assigning metrics from Summary
summary(df$VictorySpread)
FQ<-as.numeric(summary(df$VictorySpread)[2])
TQ<-as.numeric(summary(df$VictorySpread)[5])

# Creating a new variable to indicate competitiveness of the game
df["WasItClose"]<-"Standard" # Make everything standard at first, then overwrite!
df$WasItClose[which(df$VictorySpread<=FQ)] <- "Nail Biter" # Lower than first quartile might be extremely competitive
df$WasItClose[which(df$VictorySpread>=TQ)] <- "Blowout" # Higher than the third quartile might be hilariously uncompetitive

# Create a plot that differentiates between competitive games and non-competitive
ggplot(df, aes(Date, VictorySpread))+
  geom_bar(aes(fill=WasItClose), stat="identity")+
  ggtitle("Super Bowls - Degrees of Competitive") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Super Bowl Year") +
  ylab("Final Score Discrepancy")

# Create a Viewing List for the Five Most Recent Competitive Super Bowls
watchthese<-df %>%
  select(SuperBowl,VictorySpread,WasItClose, Date) %>% # Select These Variables Only
  filter(WasItClose=="Nail Biter") %>% # Only consider Nail Biters
  arrange(VictorySpread,desc(Date)) %>% # Arrange them by score and how recent they were
  slice(1:5) %>% # Pick the closest - in event of a tie, prioritize most recent
  select(-WasItClose, -VictorySpread) # Remove the Spoilers

data.frame(watchthese)