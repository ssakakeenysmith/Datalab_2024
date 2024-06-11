#load libraries
library(tidyverse)
library(babynames)
#make object
bb_names<-babynames
#Create a histogram of the name Marie in 1982.
marie_1982<-babynames %>% 
  filter(name=='Marie') %>% 
  filter(year>=1982)
ggplot(data=marie_1982,
       aes(x=year,fill=sex))+
  geom_histogram()
# Create a line plot for proportion of the name Joe, colored by sex. Make the lines a bit thicker and more transparent.
joe<-babynames %>% 
  filter(name=='Joe')
ggplot(data=joe,aes(x=year,y=prop,colour = sex))+
  geom_line(alpha=.5,linewidth=2)+
  labs(x='Years',
       y='Proportion of the name Joe',
       title = 'Proportion of the name Joe over the Years, by sex')
#Create a bar chart of the 10 most popular female names in 2002
female_names_2002<-babynames %>% 
  filter(year==2002) %>% 
  filter(sex=='F') %>% 
  arrange(desc(n)) %>% 
  head(10)
ggplot(data=female_names_2002,aes(x=name,y=n))+
  geom_col()
#Make the bars transparent and filled with the color blue
ggplot(data=female_names_2002,aes(x=name,y=n))+
  geom_col(alpha=.5,fill='blue')
#Create a new data set called the_nineties that only contains years from the 1990s
the_nineties<-babynames %>% 
  filter(year>=1990&year<=1999)
#Save this dataset to your repository (use write_csv()).
write_csv(the_nineties,file='baby names in the 90s.cvs')
#Add, commit, and push your files to GitHub. Check GitHub and make sure that your code successfully pushed
#In RStudio pull from GitHub. Is it already up to date?
#yes
#Now that everything is up to date, make a visualisation of you and your team member’s names for a year of your choice
gabe_solomon_2003<-babynames %>% 
  filter(name%in%c('Gabriel','Solomon')) %>% 
  filter(year==2003) %>% 
  filter(sex=='M')
ggplot(data=gabe_solomon_2003,aes(x=name,y=n))+
  geom_col()
gabe_solomon<-babynames %>% 
  filter(name%in%c('Gabriel','Solomon'))
ggplot(data=gabe_solomon,aes(x=year,y=prop,color=name))+
  geom_line(alpha=.5,linewidth=2)
  