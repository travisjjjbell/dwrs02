# Reading in data ---------------------------------------------------------

library(tidyverse)
data_df <- read_csv("~/Downloads/dwrs02-master/data/repeated_measured_b.csv")

library(readxl)
example_2_messy <- read_excel("~/Downloads/dwrs02-master/data/example_2_messy.xlsx")


data_df_2 <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs02/master/data/repeated_measured_b.csv")
data_df_2a <- read.csv("https://raw.githubusercontent.com/mark-andrews/dwrs02/master/data/repeated_measured_b.csv")


blp_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs02/master/data/blp-trials-short.txt")


# see top 20 rows
print(blp_df, n = 20)

# print all rows
print(blp_df, n = Inf)


glimpse(blp_df)

# select four of the variables
select(blp_df, participant, lex, resp, rt)

select(blp_df, 1, 2, 7)

select(blp_df, lex:rt)
select(blp_df, 2:5)
select(blp_df, 2:5, rt.raw)

select(blp_df, starts_with("r"))
select(blp_df, starts_with("rt"))
select(blp_df, ends_with('t'))
select(blp_df, contains('rt'))

select(blp_df, matches("^rt")) # string begins with rt
select(blp_df, matches("rt$")) # string ends with rt
select(blp_df, matches("^rt|rt$"))

select(blp_df, matches("^rt|rt$"), resp)

select(blp_df, -participant)
select(blp_df, -resp)
select(blp_df, -starts_with('r'))
select(blp_df, -(2:5))
