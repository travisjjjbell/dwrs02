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
         age_group = cut(age, breaks = seq(10, 80, by = 10))
         )

