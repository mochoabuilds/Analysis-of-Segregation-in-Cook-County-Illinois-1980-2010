####################################################
                                        
# Analysis of Segregation in Cook County, 1970-2010

# by Michael Valentino Ochoa        

####################################################

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

#############################################
                         
# kernel density estimates of race/ethnicity                    
                          
#############################################

# load data (macbook)
cook.df <- read_dta('/Users/mvo/Desktop/cook_stat.dta')

# peek at data
head(cook.df)

# construct variables for neighborhood-level % of each group at tract level
cook <- cook.df %>%
  mutate(
    blackshare    = (totblk / tractpop)  * 100,
    hispshare     = (tothisp / tractpop) * 100,
    whiteshare    = (totwht  / tractpop) * 100,
    nonwhiteshare = (100 - whiteshare) 
  )

  # double-check variable construction 
  View(cook)

# construct kernel density estimates for blackshare
ggplot(cook, aes(x = blackshare)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "Density of Blackshare",
           x = "Percent of Tract Population", 
           y = "Density")  +
  theme(plot.title = element_text(size = 14))

  # The kernel density estimate for blackshare skews to the right, suggesting 
  # most of Cook County's tracts have a limited African-American population 
  # share, with some areas having a majority share.
  
# construct kernel density estimates for whiteshare 
ggplot(cook, aes(x = whiteshare)) + 
  geom_density(fill = "orange", alpha = 0.4) + 
  labs(title = "Density of Whiteshare", 
           x = "Percent of Tract Population", 
           y = "Density") +
  theme(plot.title = element_text(size = 14))

  # The estimate for whiteshare presents as bimodal, implying some tracts are 
  # predominantly white, while other tracts have a modest share.

# construct kernel density estimates for nonwhiteshare
ggplot(cook, aes(x = nonwhiteshare)) +
  geom_density(fill = "darkgreen", alpha = 0.4) +
  labs(title = "Density of Non-whiteshare", 
           x = "Percent of Tract Population", 
           y = "Density") + 
  theme(plot.title = element_text(size = 14))
    
  # The estimate for nonwhiteshare inversely mirrors the whiteshare distribution, 
  # its bimodal shape emblematic of Cook County's racial segregation.

# construct kernel density estimates for all groups
ggplot(cook) +
  geom_density(aes(x = blackshare, color = "Black"), size = 1) +
  geom_density(aes(x = nonwhiteshare, color = "Nonwhite"), size = 1) +
  geom_density(aes(x = whiteshare, color = "White"), size = 1) +
  labs(title = "Density of Blackshare / Non-whiteshare / Whiteshare in Cook County", 
           x = "% of Tract Population", 
           y = "Density", color = "Group") +
  theme_minimal()

  # Concerns about market forces driving racial concentration may be ruled out 
  # by Shelly v. Kramer (1948), which rules restricted covenants in deeds 
  # unconstitutional, and the Fair Housing Act of 1968, which made discrimination 
  # in sale, rental and financing of housing on the basis of race, color, nationality,
  # religion and sex illegal.  Assuming the absence of racial discrimination, 
  # individuals' behavior, peer effects, background characteristics of neighborhood 
  # peers, wealth/income, neighborhood amenities altogether generate substantial 
  # segregation, and are the main drivers of segregation in Cook County.  
  # Other driving forces of segregation include cohort differences in family 
  # structure and unfolding historical events (e.g. pandemic, administration changes).

#################################################################################
# calculate fraction of tracts in each census where ethnicity/race >= 0.8 
cook <- cook.df %>% 
  mutate(
    blackshare = (totblk / tractpop),
    hispshare  = (tothisp / tractpop),
    whiteshare = (totwht / tractpop)
  )

  # plot fraction of tracts where blackshare / hispshare / whiteshare >= 0.8 
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

  # plot results by race/ethnicity over time
  ggplot(frac_cook, aes(x = year, y = Fraction, color = Group)) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Race/Ethnicity Share Greater Than 80%",
      subtitle = "Cook County, 1970-2010",
      x     = "Year",
      y     = "Fraction of Tracts",
      color = "Group"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

  
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

# plot DI by year over time
ggplot(di_long, aes(x = year, y = index, color = pair)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Dissimilarity Index in Cook County (1970-2010)",
    x     = "Year",
    y     = "Dissimilarity Index (0-1)",
    color = "Group Comparison"
  ) +
  theme_minimal()


#######################################################

# tract composition and other socioeconomic indicators

#######################################################

cook_edu <- cook.df %>%
  mutate(
    share_hs          = (educ12 / numpers25) * 100,
    share_college     = (educ15 / numpers25) * 100,
    share_ba          = (educ16 / numpers25) * 100
  )

  # double-check variable construction 
  View(cook_edu)

# construct per capita hh income variable for use in correlation below
cook_pchhinc <- cook.df %>%
  mutate(
    pchhinc            = (agghhinc/numhh),
  )

# construct log(pchhinc) for use in correlation below
cook_pchhinc <- cook.df %>%
  mutate(
    pchhinc            = (agghhinc/numhh),
    log_pchhinc        = log(pchhinc)
  )

  # double-check variable construction 
  View(cook_pchhinc)

# prepare data for racial/ethnic x socioeconomic status analysis
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

#####################################
# black share x log household income
ggplot(cook_ses, aes(x = blackshare, y = log_pchhinc)) +
  geom_point(alpha = 0.3, color = "orangered") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Black Share x Log Household Income",
        x = "Black Share (% of tract population)",
        y = "Log Average Household Income") +
  theme_minimal()

# hispanic share x log household income
ggplot(cook_ses, aes(x = hispshare, y = log_pchhinc)) +
  geom_point(alpha = 0.3, color = "purple") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Hispanic Share x Log Household Income",
        x = "Hispanic Share (% of tract population)", 
        y = "Log Average Household Income") +
  theme_minimal()
  
# white share x log household income
ggplot(cook_ses, aes(x = whiteshare, y = log_pchhinc)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "White Share x Household Income",
           x = "White Share (% of tract population)", 
           y = "Log Average Household Income") +
  theme_minimal()

##########################
# black share x education
ggplot(cook_ses, aes(x = blackshare, y = share_ba)) +
  geom_point(alpha = 0.3, color = "orangered") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Black Share x Educational Attainment",
        x = "Black Share (% of tract population)", 
        y = "BA/BS Degree or Higher") +
  theme_minimal()

# hisp share x education
ggplot(cook_ses, aes(x = hispshare, y = share_ba)) +
  geom_point(alpha = 0.3, color = "purple") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Hispanic Share x Educational Attainment",
        x = "Hispanic Share (% of tract population)", 
        y = "BA/BS Degree or Higher") +
  theme_minimal()

# white share x education 
ggplot(cook_ses, aes(x = whiteshare, y = share_ba)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "White Share x Educational Attainment",
    x = "White Share (proportion of tract population)", 
    y = "BA/BS Degree or Higher") +
  theme_minimal()


#########################################
#                                       
# high school test scores in cook county 
#                         
#########################################

# clear all objects from environment
rm(list=ls())

# load data
schools.df <- read_dta('/Users/mvo/Desktop/cook_schools_sat_2023.dta')

# peek at data
head(schools.df)

# display fraction of typical HS that is low income as tibble
schools.df %>%
  summarise(
    mean_low   = mean(percent_lowinc, na.rm = TRUE),
    median_low = median(percent_lowinc, na.rm = TRUE),
    sd_low     = sd(percent_lowinc, na.rm = TRUE)
  )

# plot low-income share of students across cook county schools
ggplot(schools.df, aes(x = percent_lowinc)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Share of Low-Income Students Across Cook County High Schools",
        x = "% of Low-Income Students",
        y = "Number of Schools") +
  theme_minimal()

# display fraction of racial/ethnic composition that is low-income as tibble
schools.df %>%
  summarise(
    avg_low_inc   = mean(percent_lowinc, na.rm = TRUE),
    avg_black     = mean(percent_black, na.rm = TRUE),
    avg_hispanic  = mean(percent_hisp, na.rm = TRUE),
    avg_asian     = mean(percent_asian, na.rm = TRUE),
    avg_white     = mean(percent_white, na.rm = TRUE),
  )

###########################
# black share x low-income
ggplot(schools.df, aes(x = percent_black, y = percent_lowinc)) +
  geom_point(alpha = 0.5, color = "orangered") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Cook County High Schools (2023)",
           x = "Black Share (%)", 
           y = "Low-Income Share (%)") +
  theme_minimal()

# hispanic share x low-income
ggplot(schools.df, aes(x = percent_hisp, y = percent_lowinc)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Cook County High Schools (2023)",
           x = "Hispanic Share (%)", 
           y = "Low-Income Share (%)") +
  theme_minimal()

# asian share x low-income
ggplot(schools.df, aes(x = percent_asian, y = percent_lowinc)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Cook County High Schools (2023)",
           x = "Asian Share (%)", 
           y = "Low-Income Share (%)") +
  theme_minimal()

# white share x low-income
ggplot(schools.df, aes(x = percent_white, y = percent_lowinc)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Cook County High Schools (2023)",
           x = "White Share (%)", 
           y = "Low-Income Share (%)") +
  theme_minimal()

  # The scatter-plots show that Black and Hispanic shares are positively correlated
  # with economic disadvantage. Whereas, schools with higher White or Asian shares 
  # are inversely correlated with low-income rates.  This exemplifies that the 
  # relationship between race and poverty in Cook County remains highly stratified 
  # by geography, or in this case school districts.

##############################################################################
#
# comparing high school test scores and more across Chicago vs. Suburban Cook
#
##############################################################################

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

# compare low income share to SAT math scores across regions  
ggplot(schools.df, aes(x = percent_lowinc, y = satmathaveragescore, color = region)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "SAT Math vs. Low-Income Share",
           x = "% Low-Income Students",
           y = "Average SAT Math Score") +
  theme_minimal()

# compare low income share to SAT reading scores across regions  
ggplot(schools.df, aes(x = percent_lowinc, y = satreadingaveragescore , color = region)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "SAT Reading vs. Low-Income Share",
           x = "% Low-Income Students",
           y = "Average SAT Reading Score") +
  theme_minimal()

  
############################################
#                         
# high school test scores to housing prices               
#                         
############################################

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

# remove Chicago submarket - use regex() for finer control of the matching behaviour
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

#############################################
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
print(final_table)

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

# plot
stargazer(versus.df, 
          type = "text",  
          title = "Housing Markets and High School SAT Performance (2023)", 
          summary = FALSE, 
          rownames = FALSE, 
          digits = 1) 

################################################################
#                         
# sandbox - high school test scores to additional relationships               
#                         
################################################################

# high school drop out rates for females x SAT math scores
ggplot(schools.df, aes(x = hs_dropout_fem, y = satmathaveragescore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method ="lm", se = FALSE, color = "steelblue") + 
  labs(
    title = "HS Dropout Rates for Females x SAT Math Scores",
    x     = "% Dropout Rate",
    y     = "SAT Math Score"
  )

# graduation rates x SAT math scores 
ggplot(schools.df, aes(x = avg_class_size, y = satmathaveragescore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method ="lm", se = FALSE, color = "steelblue") + 
  labs(
    title = "Average Class Size x SAT Math Scores",
    x     = "Average Class Size #",
    y     = "SAT Math Score"
  )

    # The fitted regression line slopes upward, implying that larger class sizes 
    # tend to have higher SAT math scores. Yet this relationship is merely a passing 
    # remark, as many of the points are widely scattered, suggesting a weak 
    # relationship.  At first look, this plot also appears counter-intuitive as we
    # expect small classes to seed higher test scores. The positive relationship 
    # could be a result of the spatial clustering of resources among the city's 
    # selective enrollment schools and the county's well-heeled suburbs. Such 
    # conditions allow large classes to thrive, as shown by the large number of 
    # data points above the regression line. Whereas, under resourced schools 
    # have smaller class sizes and still under perform on the SAT. Ultimately, 
    # a more rigorous analysis controlling for income, racial composition and more 
    # would be required to isolate the true impact of class size on SAT scores. 

# % housing insecure x SAT math scores
ggplot(schools.df, aes(x = percent_homeless, y = satmathaveragescore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method ="lm", se = FALSE, color = "steelblue") + 
  labs(
    title = "Rates of Housing Insecurity x SAT Math Scores",
    x     = "% Housing Insecurity",
    y     = "SAT Math Score"
  )

    # The regression line slopes sharply downward, meaning that schools with more 
    # housing insecure students tend to have lower SAT math scores. Combing through 
    # the distribution we see at 0-5% housing insecurity, scores pool around 
    # 450-550. Though as the rate of housing insecurity climbs, average test scores 
    # fall. The relationship appears strongest at the 30-40% threshold when scores 
    # plummet to 350, emblematic of serious academic hurt and perhaps broader material 
    # hardship.

