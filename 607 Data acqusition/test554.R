library(tidyverse)
library(ggplot2)
job_skill<-"https://raw.githubusercontent.com/stormwhale/data-mines/refs/heads/main/job_skills.csv"
df<-read.csv(job_skill, sep=',')
nrow(df) #12217

#create a function to extract the ID numbers from the job_link:
id<- function(df) {
  stringr::str_extract(df, "(\\d)+$")
}
#transform and rename job_link to job_ID:
df$job_link<-id(df$job_link)
names(df)[names(df)=='job_link'] <-"job_ID"
view(df)

#remove empty cells:
df<- na.omit(if_else(df$job_skills=="", NA, df))
nrow(df) #12212
view(df)



#separate the skills:
df_mod<- df %>%
  separate_wider_delim(job_skills, delim=',',
                       names_sep = '_',
                       too_few = 'align_start')

view(df_mod)

#Select only the first 20 skills listed:
df_mod_20<- df_mod %>%
  subset(select = c(1:21))

view(df_mod_20)

#pivot_longer to put all skills into one column:
df_mod_long<- df_mod_20 %>%
  pivot_longer(cols = c(2:21), #This is where we can adjust how many columns we want to include. Too many will cause loading issue.
               names_to = 'sets',
               values_to = 'Skills',
               values_drop_na = TRUE)
  
#remove 'sets' column:
df_mod_long<- df_mod_long%>%
  subset(select = c(job_ID, Skills))
  
#trimming the white spaces:
df_skills<-data.frame('Skills'=trimws(df_mod_long$Skills))

#Filter out the top 10 wanted skills:
top_ten<- df_skills %>%
  count(Skills) %>%
  slice_max(n, n= 10)

#plotting it in a histogram:
ggplot(top_ten, aes(x=reorder(Skills, n), y=n, fill=Skills)) +
  geom_bar(stat='identity') +
  labs(x='Skills', y="", title='Top 10 Data Science Job skills wanted') +
  theme(axis.text.x = element_text(angle = 45, hjust= 1))+
  

#Load Job posting table:#Load Job posting table:Skills
job_posting<- "https://raw.githubusercontent.com/stormwhale/data-mines/refs/heads/main/job_postings.csv"

df2<-read.csv(job_posting, header=TRUE, sep = ',')
head(df2)

#Transform the job_link to job_ID in job_posting table:
df2$job_link<- id(df2$job_link)
names(df2)[names(df2)=='job_link']<-'job_ID'
view(df2)

#Sort the ten top skills desired by associate level DS jobs:
#Match the job_ID from df_mod_20 to df2:
#First filter out all associate level jobs:
df2_ID_Lv<- df2 %>%
  filter(job_level=='Associate') %>%
  subset(select = c(job_ID, job_level))

df_asso_skill<- merge(df2_ID_Lv, df_mod_20, by='job_ID')

df_asso_skill_long<- df_asso_skill %>%
  pivot_longer(cols = c(3:22),
               names_to = 'sets',
               values_to = 'skills',
               values_drop_na = TRUE) %>%
  select(-'sets') %>%
  count(skills) %>%
  slice_max(n, n=10)
view(df_asso_skill_long)

#To generate the plot:
ggplot(df_asso_skill_long, aes(x=reorder(skills, n),y=n,fill=n)) +
  geom_bar(stat='identity') +
  coord_flip()+
  labs(x='Experties',
       y='Number of time appeared in job postings',
       title='Top 10 skills frequently desired by associate level data scientist')+
  theme(legend.position = 'none')
