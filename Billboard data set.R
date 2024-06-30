

# Are number one jams more cheerful, in general?

install.packages('tidytuesdayR')
library('tidytuesdayR')
library('tidyverse')

tuesdata <- tidytuesdayR::tt_load('2021-09-14')
billboard <- tuesdata$billboard
audio <- tuesdata$audio_features


# Or read in the data manually

billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')
glimpse(billboard)

# Number one jams
jams_billboard<-billboard %>%
  mutate(no1=(peak_position==1), week_id=mdy(week_id)) %>% 
  group_by(song_id) %>% 
  arrange(week_position, .by_group = TRUE) %>% 
  filter(!duplicated(song_id)) %>% 
  ungroup() %>% 
  select(song_id, no1)

view(jams_billboard)

glimpse(audio)
view(audio)

join_jams_audio <- jams_billboard %>% left_join(audio, by="song_id")
view(join_jams_audio)



t.test(valence~no1 , data = join_jams_audio)

# p-value = 0.01115 < 0.05 , There is a statisticall significant difference 
# between no.1 jams and non-no.1 jams.

# How large is the effect of being no.1 jams on the chearfulness?


df <- join_jams_audio %>% 
  group_by(no1) %>% 
  summarise(avg_val=mean(valence, na.rm = TRUE),
            var_val=var(valence, na.rm = TRUE))
df

pooled_sd <- sqrt(sum(df$var_val)/2)
cohens_d<- diff(df$avg_val)/pooled_sd
cohens_d

# cohens_d = 0.08310227
# Although the difference between no.1 jams and non.no.1 jams is 
# statistically significant, it is not important difference. 
# We would not expect a very large effect size of being no.1 jams on the cheerfulness.


