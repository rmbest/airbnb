library(tidyverse)


# ggplot2 theme to make things look pretty
theme_clean = function(base_size = 18, font = ""){
  
  theme_minimal(base_size = base_size) + 
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(color = "darkgrey"), 
          panel.grid.major.x = element_blank(), 
          panel.background = element_blank(), 
          plot.background = element_blank(), 
          axis.ticks = element_blank())
}

theme_set(theme_clean())


# Load data ---------------------------------------------------------------

age_gender_bkts = read_csv("age_gender_bkts.csv")
countries = read_csv("countries.csv")
sample_submission_ndf = read_csv("sample_submission_NDF.csv")
sessions = read_csv("sessions.csv")
test_users = read_csv("test_users.csv")
train_users = read_csv("train_users_2.csv")


# Check out age and gender ------------------------------------------------

age_gender_bkts = age_gender_bkts %>% 
  mutate(age = age_bucket %>% str_replace("\\W\\d*", replacement = "") %>% as.numeric)

age_gender_bkts %>% 
  group_by(gender, country_destination) %>% 
  summarize(pop = sum(population_in_thousands)) %>% 
  ggplot(aes(x = country_destination, y = pop, group = gender, fill = gender)) + 
  geom_col(position = "dodge")

age_gender_bkts %>% 
  group_by(gender, age) %>% 
  summarize(pop = sum(population_in_thousands)) %>% 
  ggplot(aes(x = age, y = pop, group = gender, color = gender)) + 
  geom_line(size = 1.8)

age_gender_bkts %>% 
  ggplot(aes(x = age, y = population_in_thousands, group = gender, color = gender)) + 
  geom_line() + 
  facet_wrap(~country_destination)

# GB terror level high Aug 29, 2014 


