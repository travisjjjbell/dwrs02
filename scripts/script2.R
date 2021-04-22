library(tidyverse)


blp_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs02/master/data/blp-trials-short.txt")


# summarize ---------------------------------------------------------------

summarise(blp_df, avg = mean(rt, na.rm = T))
          
summarise(blp_df, 
          avg_rt = mean(rt, na.rm = T),
          sd_rt = sd(rt, na.rm = T),
          median_raw = median(rt.raw, na.rm = T),
          mad_raw = mad(rt.raw, na.rm = T)
)
          
summarise(blp_df, 
          avg_rt = mean(rt, na.rm = T),
          sd_rt = sd(rt, na.rm = T),
          median_raw = median(rt.raw, na.rm = T),
          mad_raw = mad(rt.raw, na.rm = T),
          range_rt = range(rt)
)

# count number of unique values in each variable
summarise(blp_df, across(everything(), n_distinct))

# calculate mean of last three variables
summarise(blp_df, across(rt:rt.raw, mean))

mean_xna <- function(x) mean(x, na.rm = T)

summarise(blp_df, across(rt:rt.raw, mean_xna))

summarise(blp_df, across(rt:rt.raw, ~mean(., na.rm = T)))

list(a = 12, b = 2, c = 3)
c(a = 12, b = 2, c = 3)

list(avg = ~mean(., na.rm = T),
     stdev = ~sd(., na.rm = T)
)

summarise(blp_df,
          across(rt:rt.raw,
                 list(avg = ~mean(., na.rm = T),
                      stdev = ~sd(., na.rm = T)
                 )
          )
)

# split apply combine
group_by(blp_df, lex) %>% 
  summarize(avg = mean(rt, na.rm = T))

summarize(group_by(blp_df, lex),
          avg = mean(rt, na.rm = T))

group_by(blp_df, lex, resp) %>% 
  summarize(avg = mean(rt, na.rm = T))

group_by(blp_df, lex) %>% 
  summarise(across(rt:rt.raw, ~mean(., na.rm = T)))

group_by(blp_df, participant) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


# Making messy stuff nice and tidy example --------------------------------

messy_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs02/master/data/example_1_messy.csv")
tidy_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs02/master/data/example_1_tidy.csv")

messy_df
tidy_df

messy_df %>% 
  mutate(delta = stimulus_left_number_of_circles - stimulus_right_number_of_circles,
         choice_left = choice == stimulus_left,
         more_left = delta > 0,
         accuracy = more_left == choice_left) %>% 
  select(subject = ID, age, delta, accuracy, rt = latency)

