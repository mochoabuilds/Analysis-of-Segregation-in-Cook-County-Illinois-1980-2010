#######################################################################
                                        
# Analysis of Segregation in Cook County, 1970-2010

# by Michael Valentino Ochoa 

# analysis: working only with cleaned files from data_cleaning section

#######################################################################


#############################################
                         
# kernel density estimates of race/ethnicity                    
                          
#############################################

# kernel density estimates for blackshare
ggplot(cook, aes(x = blackshare)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "Density of Blackshare",
           x = "Percent of Tract Population", 
           y = "Density")  +
  theme(plot.title = element_text(size = 14))

  # The kernel density estimate for black share skews to the right, suggesting 
  # most of Cook County's tracts have a limited African-American population 
  # share, with some areas having a majority share.
  
# kernel density estimates for whiteshare 
ggplot(cook, aes(x = whiteshare)) + 
  geom_density(fill = "orange", alpha = 0.4) + 
  labs(title = "Density of Whiteshare", 
           x = "Percent of Tract Population", 
           y = "Density") +
  theme(plot.title = element_text(size = 14))

  # The estimate for whiteshare presents as bimodal, implying some tracts are 
  # predominantly white, while other tracts have a modest share.

# kernel density estimates for nonwhiteshare
ggplot(cook, aes(x = nonwhiteshare)) +
  geom_density(fill = "darkgreen", alpha = 0.4) +
  labs(title = "Density of Non-whiteshare", 
           x = "Percent of Tract Population", 
           y = "Density") + 
  theme(plot.title = element_text(size = 14))
    
  # The estimate for nonwhiteshare inversely mirrors the whiteshare distribution, 
  # its bimodal shape emblematic of Cook County's racial segregation.

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

###########################################################################

# calculate fraction of tracts in each census where ethnicity/race >= 0.8 

###########################################################################

# plot results by race/ethnicity over time
ggplot(frac_cook, aes(x = year, y = Fraction, color = Group)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
         title = "Fraction of Tracts Greater Than 80%",
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
  
# plot DI by year over time
ggplot(di_long, aes(x = year, y = index, color = pair)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Dissimilarity Index in Cook County, 1970-2010",
    x     = "Year",
    y     = "Dissimilarity Index (0-1)",
    color = "Group Comparison"
  ) +
  theme_minimal()


######################################################################

# (neighborhood) tract composition x various socioeconomic indicators

######################################################################

# black share x log household income
ggplot(cook_ses, aes(x = blackshare, y = log_pchhinc)) +
  geom_point(alpha = 0.3, color = "orangered") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Black Share x Log Household Income",
        x = "Black Share (% of tract population)",
        y = "Log Mean Household Income") +
  theme_minimal()

# hispanic share x log household income
ggplot(cook_ses, aes(x = hispshare, y = log_pchhinc)) +
  geom_point(alpha = 0.3, color = "purple") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Hispanic Share x Log Household Income",
        x = "Hispanic Share (% of tract population)", 
        y = "Log Mean Household Income") +
  theme_minimal()
  
# white share x log household income
ggplot(cook_ses, aes(x = whiteshare, y = log_pchhinc)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "White Share x Household Income",
           x = "White Share (% of tract population)", 
           y = "Log Mean Household Income") +
  theme_minimal()


# black share x education
ggplot(cook_ses, aes(x = blackshare, y = share_ba)) +
  geom_point(alpha = 0.3, color = "orangered") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Black Share x Educational Attainment",
        x = "Black Share (% of tract population)", 
        y = "Bachelor's Degree or Higher") +
  theme_minimal()

# hisp share x education
ggplot(cook_ses, aes(x = hispshare, y = share_ba)) +
  geom_point(alpha = 0.3, color = "purple") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Hispanic Share x Educational Attainment",
        x = "Hispanic Share (% of tract population)", 
        y = "Bachelor's Degree or Higher") +
  theme_minimal()

# white share x education 
ggplot(cook_ses, aes(x = whiteshare, y = share_ba)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "White Share x Educational Attainment",
    x = "White Share (proportion of tract population)", 
    y = "Bachelor's Degree or Higher") +
  theme_minimal()

#########################################
                                      
# high school test scores in cook county 
                        
#########################################

# plot low-income share of students across cook county
ggplot(schools.df, aes(x = percent_lowinc)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Share of Low-Income Students Across Cook County High Schools",
        x = "% of Low-Income Students",
        y = "Number of Schools") +
  theme_minimal()


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

###############################################################

# high school test scores and more x Chicago vs. Suburban Cook

###############################################################

# low income share x SAT math across regions  
ggplot(schools.df, aes(x = percent_lowinc, y = satmathaveragescore, color = region)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "SAT Math vs. Low-Income Share",
           x = "% Low-Income Students",
           y = "Average SAT Math") +
  theme_minimal()

# low income share x SAT reading across regions
ggplot(schools.df, aes(x = percent_lowinc, y = satreadingaveragescore , color = region)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "SAT Reading vs. Low-Income Share",
           x = "% Low-Income Students",
           y = "Average SAT Reading Score") +
  theme_minimal()


###########################################
                        
# high school test scores x housing prices               
                         
###########################################

# plot
stargazer(versus.df, 
          type = "text",  
          title = "Housing Markets and High School SAT Performance (2023)", 
          summary = FALSE, 
          rownames = FALSE, 
          digits = 1) 


##############################################################
                        
# bonus - high school test scores to additional relationships               

##############################################################

# high school drop out rates for females x SAT math 
ggplot(schools.df, aes(x = hs_dropout_fem, y = satmathaveragescore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method ="lm", se = FALSE, color = "steelblue") + 
  labs(
    title = "HS Dropout Rates for Females x SAT Math Scores",
    x     = "% Dropout Rate",
    y     = "SAT Math"
  )

# graduation rates x SAT math  
ggplot(schools.df, aes(x = avg_class_size, y = satmathaveragescore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method ="lm", se = FALSE, color = "steelblue") + 
  labs(
    title = "Average Class Size x SAT Math Scores",
    x     = "Average Class Size #",
    y     = "SAT Math"
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

# % housing insecure x SAT math 
ggplot(schools.df, aes(x = percent_homeless, y = satmathaveragescore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method ="lm", se = FALSE, color = "steelblue") + 
  labs(
    title = "Rates of Housing Insecurity x SAT Math Scores",
    x     = "% Housing Insecurity",
    y     = "SAT Math"
  )

    # The regression line slopes sharply downward, meaning that schools with more 
    # housing insecure students tend to have lower SAT math scores. Combing through 
    # the distribution we see at 0-5% housing insecurity, scores pool around 
    # 450-550. Though as the rate of housing insecurity climbs, average test scores 
    # fall. The relationship appears strongest at the 30-40% threshold when scores 
    # plummet to 350, emblematic of serious academic hurt and perhaps broader material 
    # hardship.

