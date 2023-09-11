library(tidyverse)

blp_df <- read_csv('data/blp-trials-short.txt')


# Summarise ---------------------------------------------------------------

summarise(blp_df,
          mean(rt, na.rm = T)
) # result is a data frame (tibble)

summarise(blp_df,
          avg = mean(rt, na.rm = T)
) # rename the variable

summarise(blp_df,
          avg_rt = mean(rt, na.rm = T),
          sd_rt = sd(rt, na.rm = T),
          median_raw = median(rt.raw, na.rm = T),
          mad_raw = mad(rt.raw, na.rm = T)
          
)

summarise(blp_df,
          across(everything(), n_distinct)
) # count number of unique values in each variable

summarise(blp_df,
          across(rt:rt.raw, mean)
) # calculate mean of last three variables

mean_xna <- function(x) mean(x, na.rm = T) # Allows for not typing na.rm = T every time

summarise(blp_df,
          across(rt:rt.raw, mean_xna)
)          

summarise(blp_df,
          across(rt:rt.raw, ~mean(., na.rm = T))
) # another way if using a one-off function



list(avg = ~mean(., na.rm = T),
     stdev = ~sd(., na.r = T)
)# add this to the next line of code

summarise(blp_df,
          across(rt:rt.raw, 
                 list(avg = ~mean(., na.rm = T),
                      stdev = ~sd(., na.r = T)
                 )
          )
)  # the labels are put onto the name of the variables in the tibble        
          

# Group by (split, apply, combine) ----------------------------------------

group_by(blp_df,
         lex) # original data frame, broken into two (value of lex = 'N' or 'W')

group_by(blp_df, lex) %>% 
  summarise(avg = mean(rt, na.rm = T)
  )

group_by(blp_df, 
         lex,
         resp) %>% 
  summarise(avg = mean(rt, na.rm = T)
  )

group_by(blp_df,
         lex) %>% 
  summarise(across(rt:rt.raw, ~mean(., na.rm = T))
  )

group_by(blp_df, participant) %>% 
  summarise(n = n()
  ) # shows how many data points they have per person

group_by(blp_df, participant) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)
  )


# Making messy stuff nice and tidy example -------------------------------


messy_df <- read_csv('data/example_1_messy.csv')

tidy_df <-  read_csv('data/example_1_tidy.csv')

messy_df
tidy_df

messy_df %>% 
  mutate(delta = stimulus_left_number_of_circles - stimulus_right_number_of_circles,
         choice_left = choice == stimulus_left,
         more_left = delta > 0,
         accuracy = ifelse(choice_left == more_left, 'correct', 'incorrect')
         ) %>% 
  select(subject = ID, age, delta, accuracy, rt = latency
         ) %>% 
  mutate(delta = abs(delta),
         age_group = cut(age, breaks = seq(10, 80, by = 10)),
         age_group = str_remove_all(age_group, '[\\(\\]]'), # \\ means treat that symbol as a literal opening bracket, not as syntactically in regular expressions
                                                            # The square brackets indicate a set of symbols (using them syntactically)
         age_group = str_replace(age_group, ',', '-')
  ) 

# Combining data frames ---------------------------------------------------


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

Df_8 <- tibble(x = c(1, 2, 2, 3, 4, 5, 5, 6, 7, 8, 8))


Df_1
Df_2
bind_rows(Df_1, Df_2)

Df_3
bind_rows(Df_1, Df_3) #This works but not really bc it fills gaps with NA
bind_cols(Df_1, Df_3) #works better for these two data frames


Df_a
Df_b

inner_join(Df_a, Df_b) #matching rows based on values of common variables, value 1 and four don't get matched in the join
left_join(Df_a, Df_b)
right_join(Df_a, Df_b)
full_join(Df_a, Df_b) #also known as outer join, preserves all info

stimuli <- read_csv('data/blp_stimuli.csv')
stimuli


inner_join(blp_df, stimuli) #any row that occurs in stimuli, but not in blp_df will not be shown in new table

filter(stimuli, spell == 'staud')

all_equal(left_join(blp_df, stimuli),
          inner_join(blp_df, stimuli)
) # this equals true, meaning they are the same


right_join(blp_df, stimuli) %>% slice(50000:50010) # showing the NA's by selecting specific rows to show 

all_equal(right_join(blp_df, stimuli),
          inner_join(blp_df, stimuli)
) # Won't work, more rows in stimuli than in blp_df

all_equal(right_join(blp_df, stimuli),
          full_join(blp_df, stimuli)
) # Will work, presevres all rows

inner_join(Df_4, Df_5) # won't work because none of the variables are matching 

inner_join(Df_4, Df_5, by = c('x' = 'a') # changing the variable name to match data set variables
)

Df_5_alt <- rename(Df_5, x = a, y = b)

inner_join(Df_4, Df_5_alt, by = 'x') # to only match one variable, not both

subjects <- read_csv('data/example_1_subjects.csv')
subjects

left_join(tidy_df, subjects, by = c(
  'subject' = 'ID')
) %>%
  group_by(is_mobile) %>% 
  summarise(avg = mean(rt, na.rm = T),
            sd = sd(rt, na.rm = T)
  )



x <- list(a = 10, b = 5, c = 3)
lapply(x, runif)
map(x, runif) # purr version (tidyverse) of lapply function (base R), which is better than lapply
# Pass each variable from a list through a function (this case it's runif)

library(fs)
data_dfs <- dir_ls('data/exp_data') %>% 
  map(read_csv) #allows you to read multiple csv's at one time if they are within a folder 


class(data_dfs)
final_df <- bind_rows(data_dfs)
class(final_df)
names(data_dfs)
data_dfs[[1]] #to view the first df in the data_dfs list

data_dfs_2 <- dir_ls('data/exp_data') %>% 
  map(read_csv) %>% 
  bind_rows()

