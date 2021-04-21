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

# dplyr verbs -------------------------------------------------------------

# * select
# * relocate
# * rename
# * slice
# * filter

# select four of the variables
select(blp_df, participant, lex, resp, rt)
# or else
select(blp_df, participant, lex, resp, reaction_time = rt)

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


# select numeric variables
select(blp_df, where(is.numeric))
select(blp_df, -where(is.numeric))
select(blp_df, where(is.character))

has_low_mean <- function(x){
  is.numeric(x) && (mean(x, na.rm = TRUE) > 500)
}

select(blp_df, where(has_low_mean))

select(blp_df, where(function(x){is.numeric(x) && (mean(x, na.rm = TRUE) > 500)}))
select(blp_df, where(~{is.numeric(.) && (mean(., na.rm = TRUE) > 500)}))

select(blp_df, rt, everything())


# Relocate: change variable order -----------------------------------------

relocate(blp_df, rt)

relocate(blp_df, starts_with('r'))

relocate(blp_df, rt.raw, .after = lex)
relocate(blp_df, rt.raw, .before = rt)

relocate(blp_df, rt, .after = last_col())

relocate(blp_df, where(is.numeric))


# renaming variables ------------------------------------------------------

rename(blp_df, reaction_time = rt)
rename(blp_df, reaction_time = rt, lexical = lex)

blp_names <- names(blp_df)

# this works, but not perfectly
rename_with(blp_df, ~str_replace(., 'rt', 'reaction_time'))

# this fixes it
rename_with(blp_df, 
            ~str_replace(., 'rt', 'reaction_time'), 
            matches('^rt|rt$'))

rename_with(blp_df, 
            ~str_replace(., 'rt', 'reaction_time'), 
            c(rt, prev.rt))

rename_with(blp_df, toupper)

rename_with(blp_df, toupper, where(is.numeric))


my_rename_f <- function(x){
  str_replace(x, 'rt', 'reaction_time')
}

my_rename_f(blp_names)

rename_with(blp_df, my_rename_f)
rename_with(blp_df, function(x){str_replace(x, 'rt', 'reaction_time')})
# purrr style lambdas 
rename_with(blp_df, ~str_replace(., 'rt', 'reaction_time'))

# slice -------------------------------------------------------------------

slice(blp_df, 20:25)
slice(blp_df, 333)
slice(blp_df, c(10, 25, 100))
slice(blp_df, c(10, 25, 100:105))
slice(blp_df, -c(10, 25, 100))
slice(blp_df, -1)

slice(blp_df, 1)
slice(blp_df, -1)

slice(blp_df, 990:n())
slice(blp_df, (n()-5):n())


# filter ------------------------------------------------------------------

filter(blp_df, lex == 'W')
filter(blp_df, lex == 'W', resp == 'W')
filter(blp_df, rt < 500)
filter(blp_df, lex == 'W', resp == 'W', rt < 500)
filter(blp_df, (lex == 'W') & (resp == 'W') & (rt < 500))
filter(blp_df, (lex == 'W') | (resp == 'W'))
