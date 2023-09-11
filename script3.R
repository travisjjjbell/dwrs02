# pivots ------------------------------------------------------------------

# going from a wide data structure (a lot of columns, fewer rows), to a long data structure (a lot of rows, fewer columns)

recall_df <- read_csv('data/repeated_measured_a.csv')

recall_df_long <- pivot_longer(recall_df, #the data frame
                               -Subject, #pivot all variables except for 'Subject'
                               names_to = 'condition', #add a new variable for the names of the variables that are being pivoted
                               values_to = 'recall' #add a new variable for the values associated with the names that are being pivoted
)

pivot_wider(recall_df_long,
            names_from = condition, #variables not in quotations because you are not renaming anything (using an actual variable)
            values_from = recall
)


recall_df_b <- read_csv('data/repeated_measured_b.csv')

pivot_longer(recall_df_b,
             -Subject,
             names_to = 'condition',
             values_to = 'recall'
) %>% 
  separate(condition, 
           into = c('cue',
                    'emotion')
  )


pivot_longer(recall_df_b,
             -Subject,
             names_to = c('cue',
                          'emotion'),
             names_sep = '_',
             values_to = 'recall'
)


pivot_longer(recall_df_b,
             -Subject,
             names_to = c('cue',
                          'emotion'),
             names_pattern = '(.*)_(.*)',
             values_to = 'recall'
)


pivot_longer(recall_df_b,
             -Subject,
             names_to = c('cue',
                          'emotion'),
             names_pattern = '(Cued|Free)_(Neg|Neu|Pos)',
             values_to = 'recall'
)






summarise(blp_df,
          across(rt:rt.raw,
                 list(avg = ~mean(., na.rm = T),
                      med = ~median(., na.rm = T),
                      stdev = ~sd(., na.rm = T)
                 )
          )
)


