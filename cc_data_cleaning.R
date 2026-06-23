############################################################################

# Analysis of Segregation in Cook County, 1970-2010

# by Michael Valentino Ochoa 

# data: scripts for going from raw data to cleaned dataset used in analysis 

############################################################################

# load libraries
library(haven)
library(tidyverse)
library(ggplot2) 
library(fixest)
library(modelsummary)
library(foreign)
library(stargazer)
library(dplyr)
library(purrr)
library(stargazer) 

# clear all objects from environment
rm(list=ls())

# load data (macbook)
cook.df <- read_dta('/Users/mvo/Desktop/cook_stat.dta')

# peek at data
head(cook.df)


#############################################

# kernel density estimates of race/ethnicity                    

#############################################

# construct variables for % of each group at tract level
cook <- cook.df %>%
  mutate(
    blackshare    = (totblk / tractpop)  * 100,
    hispshare     = (tothisp / tractpop) * 100,
    whiteshare    = (totwht  / tractpop) * 100,
    nonwhiteshare = (100 - whiteshare) 
  )

# double-check variable construction 
View(cook)


##########################################################################

# calculate fraction of tracts in each census where race/ethnicity >= 0.8 

##########################################################################

# construct race/ethnicity variables
cook <- cook.df %>% 
  mutate(
    blackshare = (totblk / tractpop),
    hispshare  = (tothisp / tractpop),
    whiteshare = (totwht / tractpop)
  )

# plot fraction of tracts where race/ethnicity >= 0.8 
frac_cook <- cook %>%
  group_by(year) %>%
  summarise(
    Frac_Black80 = mean(blackshare >= 0.8, na.rm = TRUE),
    Frac_Hisp80  = mean(hispshare  >= 0.8, na.rm = TRUE),
    Frac_White80 = mean(whiteshare >= 0.8, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols      = starts_with("Frac_"),
    names_to  = "Group",
    values_to = "Fraction"
  )

# display results as tibble
print(frac_cook)

###########################

# dissimilarity index (DI) 

###########################

# keep relevant variables
cook <- cook %>%
  select(year, tractpop, totwht, totblk, tothisp) %>%
  filter(tractpop > 0)

# dissimilarity function
dissimilarity_index <- function(g1, g2) 
  {
    0.5 * sum(abs((g1 / sum(g1, na.rm = TRUE)) -
                  (g2 / sum(g2, na.rm = TRUE))), na.rm = TRUE)
  }

# calculate DI by census decade 
di_by_year <- cook.df %>% 
  group_by(year) %>%
  summarise(
    DI_black_white = dissimilarity_index(totblk, totwht),
    DI_hisp_white  = dissimilarity_index(tothisp, totwht),
    DI_black_hisp  = dissimilarity_index(totblk, tothisp)
  )

# prepare DI by year
di_long <- di_by_year %>% 
  pivot_longer(cols = starts_with("DI"), names_to = "pair", values_to = "index")


######################################################################

# (neighborhood) tract composition x various socioeconomic indicators

######################################################################

# construct education share variables 
cook_edu <- cook.df %>%
  mutate(
    share_hs          = (educ12 / numpers25) * 100,
    share_college     = (educ15 / numpers25) * 100,
    share_ba          = (educ16 / numpers25) * 100
  )

  # double-check variable construction 
  View(cook_edu)

# construct per capita hh income variable and log(pchhinc) 
cook_pchhinc <- cook.df %>%
  mutate(
    pchhinc            = (agghhinc/numhh),
    log_pchhinc        = log(pchhinc)
  )

  # double-check variable construction 
  View(cook_pchhinc)

# prepare data for socioeconomic analysis
cook_ses <- cook.df %>%
  mutate(
    blackshare        = (totblk / tractpop),
    hispshare         = (tothisp / tractpop),
    whiteshare        = (totwht / tractpop),
    
    share_hs          = (educ12 / numpers25),
    share_college     = (educ15 / numpers25),
    share_ba          = (educ16 / numpers25),
    
    pchhinc           = (agghhinc/numhh),
    log_pchhinc       = log(pchhinc),
    
    unemployment      = unemrt,            
    welfare_rate      = welfpct, 
    poverty_rate      = poorpct  
    
  ) %>%
  filter(is.finite(log_pchhinc))


#########################################

# high school test scores in cook county 

#########################################

# load data
schools.df <- read_dta('/Users/mvo/Desktop/cook_schools_sat_2023.dta')

# peek at data
head(schools.df)

# display fraction of high schools that are low income as tibble
schools.df %>%
  summarise(
    mean_low   = mean(percent_lowinc, na.rm = TRUE),
    median_low = median(percent_lowinc, na.rm = TRUE),
    sd_low     = sd(percent_lowinc, na.rm = TRUE)
  )

# display fraction of racial/ethnic composition that is low-income as tibble
schools.df %>%
  summarise(
    avg_low_inc   = mean(percent_lowinc, na.rm = TRUE),
    avg_black     = mean(percent_black, na.rm = TRUE),
    avg_hispanic  = mean(percent_hisp, na.rm = TRUE),
    avg_asian     = mean(percent_asian, na.rm = TRUE),
    avg_white     = mean(percent_white, na.rm = TRUE),
  )

###############################################################

# high school test scores and more x Chicago vs. Suburban Cook

###############################################################

schools.df <- schools.df %>%
  mutate(region = ifelse(grepl("City of Chicago SD 299", 
                               district, ignore.case = TRUE),
                               "Chicago", "Suburban Cook"))

# display fraction of typical HS that is low income for Chicago vs. Suburban Cook
schools.df %>%
  group_by(region) %>%
  summarise(
    avg_low_inc   = mean(percent_lowinc, na.rm = TRUE),
    avg_black     = mean(percent_black, na.rm = TRUE),
    avg_hispanic  = mean(percent_hisp, na.rm = TRUE),
    avg_asian     = mean(percent_asian, na.rm = TRUE),
    avg_white     = mean(percent_white, na.rm = TRUE),
  )


###########################################

# high school test scores x housing prices               

###########################################

# load data 
prices <- read_dta('/Users/mvo/Desktop/prices_2022_2023.dta')
schools <- read_dta('/Users/mvo/Desktop/cook_schools_sat_2023.dta')

# data cleaning of prices - keep only relevant variables 
prices <- prices %>% 
  dplyr::select(submarket, mediansalesprice2022q32023q2) %>% 
  rename(median_price_2023 = mediansalesprice2022q32023q2)

# data cleaning of schools - keep only relevant variables 
schools <- schools %>% 
  dplyr::select(city, school_name, satmathaveragescore, satreadingaveragescore)

# remove Chicago submarket - use regex() for finer control of matching behaviour
suburban_prices <- prices %>% 
  filter(!str_detect(submarket, regex("^Chicago--|^City of Chicago$", ignore_case = TRUE))) %>%
  filter(!is.na(median_price_2023))

# double-check removal of Chicago submarket 
suburban_prices

##########################################
# select bottom 6 submarkets from suburbs
bottom6 <- suburban_prices %>%
  arrange(desc(median_price_2023)) %>%
  slice_tail(n = 6)

# double-check selection of bottom 6 submarkets 
print(bottom6)

# function - edit submarket names to show as communities 
split_munis <- function(x) 
{
  x %>%
    str_replace_all(" and ", "/") %>%
    str_split("/") %>%
    unlist() %>%
    str_trim()
}

# compute SAT mean of bottom 6 submarkets
resultsLow <- bottom6 %>%
  rowwise() %>%
  mutate(
    
    # note - each submarket may contain more than 1 community 
    communities       = map(submarket, split_munis),
    
    # filter school data for each community group
    data              = list(map(communities, ~ filter(schools, city %in% .x))),
    
    # compute stats
    n_high_schools    = list(map_int(data, ~ n_distinct(.x$school_name))),
    mean_sat_math     = list(map_dbl(data, ~ mean(.x$satmathaveragescore, na.rm = TRUE))),
    mean_sat_reading  = list(map_dbl(data, ~ mean(.x$satreadingaveragescore, na.rm = TRUE)))
  ) %>%
  
  # clean up for display of results
  mutate(
    median_price_2023 = round(median_price_2023),
    mean_sat_math     = list(round(mean_sat_math, 1)),
    mean_sat_reading  = list(round(mean_sat_reading, 1)),
    communities       = list(map_chr(communities, ~ paste(.x, collapse = ", ")))
  ) %>%
  
  dplyr::select(submarket, communities, n_high_schools, mean_sat_math, mean_sat_reading, 
                median_price_2023) 

# display SAT mean scores for bottom 6 submarkets in suburban cook county
View(resultsLow)

# #######################################
# #select top 6 submarkets from suburbs
# top6 <- suburban_prices %>%
#   arrange(desc(median_price_2023)) %>%
#   slice_head(n = 6)
# 
# print(top6)
# 
# resultsHigh <- top6 %>%
#   rowwise() %>%
#   mutate(
#     # each submarket may contain more than 1 municipality
#     communities       = map(submarket, split_munis),
# 
#     # filter school data for each community group
#     data              = list(map(communities, ~ filter(schools, city %in% .x))),
# 
#     # compute stats
#     n_high_schools    = list(map_int(data, ~ n_distinct(.x$school_name))),
#     mean_sat_math     = list(map_dbl(data, ~ mean(.x$satmathaveragescore, na.rm = TRUE))),
#     mean_sat_reading  = list(map_dbl(data, ~ mean(.x$satreadingaveragescore, na.rm = TRUE)))
#   ) %>%
# 
#   # clean up for display
#   mutate(
#     median_price_2023 = round(median_price_2023),
#     mean_sat_math     = list(round(mean_sat_math, 1)),
#     mean_sat_reading  = list(round(mean_sat_reading, 1)),
#     communities       = list(map_chr(communities, ~ paste(.x, collapse = ", ")))
#   ) %>%
# 
#   dplyr::select(submarket, communities, n_high_schools, mean_sat_math, mean_sat_reading,
#                 median_price_2023)
# 
# # display SAT mean scores for top 6 submarkets in suburban cook county
# View(resultsHigh)

# add City of Chicago averages to resultsLow
chicago_means <- schools %>%
  filter(str_detect(city, regex("^chicago$", ignore_case = TRUE))) %>%
  summarise(
    submarket         = "City of Chicago",
    communities       = list("Chicago"),
    n_high_schools    = list(n_distinct(school_name)),
    mean_sat_math     = list(round(mean(as.numeric(satmathaveragescore), na.rm = TRUE), 1)),
    mean_sat_reading  = list(round(mean(as.numeric(satreadingaveragescore), na.rm = TRUE), 1)),
    median_price_2023 = NA_real_
  )

final_table <- bind_rows(resultsLow, chicago_means)

# display SAT mean scores for top 6 submarkets in suburban cook county as tibble
view(final_table)

# generate professional looking table of tibble 
versus.df <- data.frame( 
  submarket = c("Hoffman Estates/Streamwood", "Melrose Park/Maywood", 
                "Oak Lawn/Blue Island", "Oak Forest/Country Club Hills", 
                "Chicago Heights/Park Forest", "Calumet City/Harvey", 
                "City of Chicago"), 
  mean_sat_math     = c(525, 383.2, 432.6, 432.5, 396.7, 397.9, 427.4), 
  mean_sat_reading  = c(518.1, 418.1, 444.2, 443.9, 421.9, 420.0, 443.1), 
  median_price_2023 = c(292000, 287500, 250000, 220000, 185000, 145000, NA) 
)


