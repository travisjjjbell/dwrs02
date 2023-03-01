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


# manipulating data frames ------------------------------------------------

# select (selects columns) specific variables
select(blp_df, 
       participant, lex, resp, rt)
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
