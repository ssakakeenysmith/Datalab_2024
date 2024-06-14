# load libraries
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(sentimentr)
library(lubridate)
#load survey data
survey <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1W9eGIihIHppys3LZe5FNbUuaIi_tfdscIq521lidRBU/edit?usp=sharing')
#Take a look at the first few rows of the data. What is the unit of observation?
#  
#Create a variable named date_time in your survey data. This should be based on the Timestamp variable. Use the mdy_hms variable to created a “date-time” object.
survey2<-survey %>% 
  mutate(date_time=mdy_hms(Timestamp))
#Create a visualization of the date_time variable
ggplot(data=survey2,aes(x=date_time))+
  geom_histogram()
#Create an object called sentiments by running the following
sentiments <- get_sentiments('bing')
#Explore the sentiments object. How many rows? How many columns? What is the unit of observation.
#There are 6786 rows, 2 columns, and the unit of observation is sentiment

#Create an object named words
words <- survey %>%
  dplyr::select(first_name,
                feeling_num,
                feeling) %>%
  unnest_tokens(word, feeling)

#Explore words. What is the unit of observation.
#Good question! No clue.

#Look up the help documentation for the function wordcloud2. What does it expect as the first argument of the function?
#I'm guessing it is data

#Create a dataframe named word_freq. This should be a dataframe which is conformant with the expectation of wordcloud2, showing how frequently each word appeared in our feelings.
word_freq<-words %>% 
  group_by(word) %>% 
  tally()
#Make a word cloud
wordcloud2(word_freq)
#Run the below to create an object named sw
sw <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/stopwords.csv')
#What is the sw object all about? Explore it a bit
#WORDS
#Remove from word_freq any rows in which the word appears in sw.
word_freq2<-word_freq %>% 
  filter( !word %in% sw$word )
#Make a new word cloud
wordcloud2(word_freq2)
#Make an object with the top 10 words used only. Name this object top10
top10<-word_freq2 %>% 
  arrange(desc(n)) %>% 
  head(word_freq2,n=10)
#Create a bar chart showing the number of times the top10 words were used
ggplot(data=top10,aes(x=word,y=n))+
  geom_col(fill='red')
#Run the below to join word_freq with sentiments
sentiment_freq<-left_join(word_freq,sentiments,by='word')
#Now explore the data. What is going on?
#The data is counting how many times a word was said and the associated feeling

#For the whole survey, were there more negative or positive sentiment words used
sentiment_freq %>% 
  group_by(sentiment) %>% 
  tally
#more positive than negative

#Create an object with the number of negative and positive words used for each person.
sentitment_name<-left_join(words,sentiment_freq,by='word')
sentiment_count<-sentitment_name %>% 
  group_by(first_name,sentiment) %>% 
  tally()

# In that object, create a new variabled named sentimentality, which is the number of positive words minus the number of negative words.
sentitment_count<-sentiment_count %>% 
  mutate(sentimentality=sum('positive'-'negative',na.rm=TRUE))



