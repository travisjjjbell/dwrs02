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
         accuracy = ifelse(more_left == choice_left, 'correct', 'incorrect')) %>% 
  select(subject = ID, age, delta, accuracy, rt = latency) %>% 
  mutate(delta = abs(delta),
         age_group = cut(age, breaks = seq(10, 80, by = 10)),
         age_group = str_remove_all(age_group, '[\\(\\]]'),
         age_group = str_replace(age_group, ',', '-'))


# combining data frames ---------------------------------------------------

Df_1 <- tibble(x = c(1, 2, 3),
               y = c(2, 7, 1),
               z = c(0, 2, 7))

Df_2 <- tibble(y = c(5, 7),
               z = c(6, 7),
               x = c(1, 2))

Df_3 <- tibble(a = c(5, 6, 1),
               b = c('a', 'b', 'c'),
               c = c(T, T, F))

Df_a <- tibble(x = c(1, 2, 3),
               y = c('a', 'b', 'c'))

Df_b <- tibble(x = c(2, 3, 4),
               z = c('d', 'e', 'f'))

Df_4 <- tibble(x = c(1, 2, 3),
               y = c(2, 7, 1),
               z = c(0, 2, 7))

Df_5 <- tibble(a = c(1, 1, 7),
               b = c(2, 3, 7),
               c = c('a', 'b', 'c'))

Df_6 <- tibble(x = c(1, 2, 3),
               y = c(4, 5, 6),
               z = c(7, 8, 9))

Df_7 <- tibble(y = c(6, 7),
               z = c(9, 10),
               x = c(3, 4))

bind_rows(Df_1, Df_2)

bind_rows(Df_2, Df_1)

bind_cols(Df_1, Df_3)

inner_join(Df_a, Df_b)
left_join(Df_a, Df_b)
right_join(Df_a, Df_b)
full_join(Df_a, Df_b)


stimuli <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs02/master/data/blp_stimuli.csv")

inner_join(blp_df, stimuli)

filter(stimuli, spell == 'staud')

all_equal(left_join(blp_df, stimuli), 
          inner_join(blp_df, stimuli))

right_join(blp_df, stimuli)

all_equal(right_join(blp_df, stimuli),
          full_join(blp_df, stimuli))

# wtf
inner_join(Df_4, Df_5)

inner_join(Df_4, Df_5, by = c('x' = 'a'))

inner_join(Df_4, rename(Df_5, x=a))

inner_join(Df_4, Df_5, by = c('x' = 'a', 'y' = 'b'))

inner_join(Df_4, rename(Df_5, x=a, y=b))

Df_5_alt <- rename(Df_5, x = a, y = b)

inner_join(Df_4, Df_5_alt, by = 'x')

subjects <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs02/master/data/example_1_subjects.csv")

left_join(tidy_df, subjects, by = c('subject' = 'ID'))

left_join(tidy_df, subjects, by = c('subject' = 'ID')) %>% 
  group_by(is_mobile) %>% 
  summarize(avg = mean(rt, na.rm = T),
            sd = sd(rt, na.rm = T))

x <- list(a = 10, b = 5, c = 3)
lapply(x, runif)
map(x, runif)

library(fs)
data_dfs <- dir_ls('data/exp_data') %>% map(read_csv)
class(data_dfs)
final_df <- bind_rows(data_dfs)
class(final_df)
names(data_dfs)
data_dfs[[1]]

final_df_2 <- dir_ls('data/exp_data') %>% map(read_csv) %>% bind_rows()
list.files(d.env, pattern='tif$', full.names=TRUE ) 