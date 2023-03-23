# Packages and data -------------------------------------------------------------------

library(tidyverse)
data_df <- read_csv('data/repeated_measured_b.csv')

library(readxl)
example_2_messy <- read_excel('data/example_2_messy.xlsx')

blp_df <- read_csv('data/blp-trials-short.txt') 

# see top 20 rows
print(blp_df, 
      n = 20)

# print all rows
print(blp_df, 
      n = Inf)

# pivots the table to see all of the columns, and more rows
glimpse(blp_df)

# dplyr verbs -------------------------------------------------------------

# * select
# * relocate
# * rename
# * slice
# * filter
# * mutate
# * transmute
# * arrange

# manipulating data frames ------------------------------------------------

# select (selects columns) specific variables
select(blp_df, 
       participant, lex, resp, rt)
# or else
select(blp_df, 
       participant, lex, resp, reaction_time = rt) # renaming rt at the same time
select(blp_df, 
       1, 2, 7)
select(blp_df, 
       lex:rt)
select(blp_df, 
       2:5) 
select(blp_df, 
       2:5, 
       rt.raw)
select(blp_df,
       starts_with('r'))
select(blp_df,
       ends_with('t'))
select(blp_df,
       contains('rt')) # 'rt' means 'response time' for this data set fyi

select(blp_df,
       matches('^rt')) # ^ means the start of the string
select(blp_df,
       matches('rt$')) # $ means the end of the string
select(blp_df,
       matches('^rt|rt$')
)

select(blp_df,
       matches('^rt|rt$'), 
       resp)

select(blp_df,
       -participant) # select everything BUT participant
select(blp_df,
       -(2:4)
)

# select numeric vairables
select(blp_df,
       where(is.numeric)
)
select(blp_df,
       where(is.character)
)


# writing functions
has_low_mean <- function(x){
  is.numeric(x) && mean(x, na.rm = TRUE) > 500 # is it numeric? if yes, is it greater than 2000?
} 

select(blp_df,
       where(has_low_mean)

)

select(blp_df,
       where(function(x){
         is.numeric(x) && mean(x, na.rm = TRUE) > 500
       }
       )
)

select(blp_df,
       where(~{
         is.numeric(.) && mean(., na.rm = TRUE) > 500
       }
       )
) # this does the same thing as the one above


# Relocate: change variable order -----------------------------------------

relocate(blp_df, rt) # moves rt (variable) the the front of the data frame

relocate(blp_df, starts_with('r'))

relocate(blp_df, rt.raw, .after = lex)

relocate(blp_df, rt.raw, .before = rt)

relocate(blp_df, rt, .after = last_col()) # moves the variable to the last column in the data frame

relocate(blp_df, where(is.numeric)) # numeric variables are at the front of df 


# Renaming variables ------------------------------------------------------

rename(blp_df, reaction_time = rt) # new name is equal to old name

rename(blp_df,
       reaction_time = rt, lexical = lex
)

blp_names <- names(blp_df)

rename_with(blp_df,
            ~str_replace(., 'rt', 'reaction_time')
) # this works, but not perfectly

rename_with(blp_df,
            ~str_replace(., 'rt', 'reaction_time'),
            matches('^rt|rt$')
) # this fixes it

rename_with(blp_df,
            ~str_replace(., 'rt', 'reaction_time'),
            c(rt, prev.rt)
)


toupper(blp_names) # all upper case
tolower(blp_names) # all lower case

rename_with(blp_df,
            toupper,
            where(is.numeric)
)


# Slice -------------------------------------------------------------------

# deals with rows (observations, the columns are the variables)

slice(blp_df,
      20:25)

slice(blp_df,
      333)

slice(blp_df,
      c(10, 25, 100)
)

slice(blp_df,
      c(10, 25, 100:105)
)

slice(blp_df,
      -c(10, 25, 100)
)

slice(blp_df,
      990:n()
) # n is the last row

slice(blp_df,
      (n()-5):n()
)


# Filter ------------------------------------------------------------------
# These are conjunctions:
filter(blp_df,
       lex == 'W'
)

filter(blp_df, 
       lex == "W",
       resp == 'W'
)

filter(blp_df,
       rt < 500
)

filter(blp_df,
       lex == 'W',
       resp == 'W',
       rt < 500
)

# These are disjunctions

filter(blp_df,
       lex == 'W' | resp == 'W' # This means 'or'
       
)

filter(blp_df,
       if_any(everything(),
              is.na) # Show all rows that contain at least one missing value (NA) in any variable
)

filter(blp_df,
       if_all(everything(), # show all rows that contain NA in all variables
              is.na) 
)

filter(blp_df,
       if_all(everything(), ~!is.na(.)) # does the same as na.omit
)

filter(blp_df,
       if_all(rt:rt.raw, ~. < 500) # The period is a place holder the each of the three variables
)

filter(blp_df,
       if_any(rt:rt.raw, ~. < 500)
)

filter(blp_df,
       if_any(rt:rt.raw, ~. < median(., na.rm = TRUE))
)


filter(blp_df,
       if_all(rt:rt.raw, ~. < median(., na.rm = TRUE))
)

filter(blp_df,
       if_all(where(is.numeric), ~. < median(., na.rm = TRUE))
)


# Mutate ------------------------------------------------------------------

mutate(blp_df,
       accuracy = lex == resp # create a new variable showing where lex is equal to the value of resp
) 

mutate(blp_df,
       accuracy = lex == resp,
       word_length = str_length(spell) # new variable showing the character length from variable spell
)

mutate(blp_df,
       accuracy = lex == resp,
       word_length = str_length(spell),
       fast_rt = rt < median(rt, na.rm = T) # new variable showing the fast reaction 
                                            # times when they are less than the median
)

mutate(blp_df,
       rt = round(rt/1000) # modify a current variable, change to seconds from milliseconds
)

mutate(blp_df,
       lex = as.factor(lex),
       spell = as.factor(spell),
       resp = as.factor(resp)
) # factors are categorical variables

mutate(blp_df,
       across(where(is.character), as.factor)
) # change multiple variables at once

mutate(blp_df,
       rt = (rt - mean(rt, na.rm = T)) / sd(rt, na.rm = T)
)

mutate(blp_df,
       rt = as.vector(scale(rt))
) # does the same as the code above

mutate(blp_df,
       across(rt:rt.raw, ~ as.vector(scale(.)))
)

# re-coding:
mutate(blp_df,
       lex = recode(lex, 'W' = 'word', 'N' = 'nonword')
)

mutate(blp_df,
       across(c(lex, resp), ~recode(., 'W' = 'word', 'N' = 'nonword'))
)

mutate(blp_df,
       rt_speed = ifelse(rt > 1000, 'slow', 'fast')
) # new variable, if this condition is true, return 'slow', if not, return 'fast'

mutate(blp_df,
       rt_speed = case_when(
       rt > 1000 ~ 'slow',
       rt < 500 ~ 'fast',
       TRUE ~ 'medium'
       )
) # three different classifications


# Transmute ---------------------------------------------------------------


transmute(blp_df,
       rt_speed = case_when(
         rt > 1000 ~ 'slow',
         rt < 500 ~ 'fast',
         TRUE ~ 'medium'
       )
) # only returns the mutated variables 


# Arrange -----------------------------------------------------------------

arrange(blp_df,
        rt
) # sorts df by rt in ascending order

arrange(blp_df,
        desc(rt),
) # same but descending order

arrange(blp_df,
        participant,
        rt
) # sort by participant first, then rt

arrange(blp_df,
        desc(participant),
        rt
)


# Pipelines ---------------------------------------------------------------

x <- c(1, 2, 3, 4, 5, 6)
log(x)
sqrt(log(x))
sum(sqrt(log(x)))
log(sum(sqrt(log(x))))# these are nested functions

x %>% log()
x %>% log() %>% sqrt() 
x %>% log() %>% sqrt() %>% log() # These do the same as above

blp_df_tmp <- mutate(blp_df,
       accuracy = lex == resp
)
blp_df_temp2 <- filter(blp_df_tmp, 
       accuracy == F
)
select(blp_df_temp2,
       participant,
       rt.raw,
       resp
) # created temporary data frames

mutate(blp_df,
       accuracy = lex == resp) %>% 
  filter(accuracy == F) %>% 
  select(participant,
         rt.raw,
         resp
  ) # using pipes instead of temporary data frames



