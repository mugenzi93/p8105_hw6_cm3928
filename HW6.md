p8105\_hw6\_cm3928
================
Clement Mugenzi
11/20/2019

# Problem 1

``` r
weight_df = 
  read_csv("Data/birthweight.csv") %>% 
  janitor::clean_names() %>%
  rename(gender = babysex, momwt = delwt, momrace = mrace, dadrace = frace) %>% 
  mutate(
    gender = recode(gender, "1" = "Male", "2" = "Female"),
    dadrace = recode(dadrace, "1" = "White", "2" = "Black", "3" = "Asian",
                     "4" = "Puerto Rican", "8" = "Other", "9" = "Unknown"),
    momrace = recode(momrace, "1" = "White", "2" = "Black", "3" = "Asian",
                     "4" = "Puerto Rican", "8" = "Other"),
    malform = recode(malform, "0" = "Absent", "1" = "Present")) %>% view()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.
