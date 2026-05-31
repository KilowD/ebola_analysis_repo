if (!require(pacman))  # checks if pacman is installed
  install.packages("pacman") # if pacman doesnt exist on this machine install it
suppressPackageStartupMessages({pacman::p_load( # installs and loads packages
  tidyverse,
  plotly,
  janitor,
  visdat,
  esquisse,
  here,
  readxl,
  flextable,
  gt,
  reactable,
  lubridate,
  Sleuth3,
  Sleuth2,
  reshape,
  glue,
  Lock5Data 
)})

options(dplyr.summarise.inform=FALSE) # Don't annoy me with summarise messages


#DATASET PRACTICE

Students <- 
  expand.grid(Year='freshman', Gender=1, rep=1:23) %>%
  add_row(Year='freshman', Gender=0, rep=1:25) %>%
  add_row(Year='junior',   Gender=1, rep=1:30) %>%
  add_row(Year='junior',   Gender=0, rep=1:32) %>%
  add_row(Year='senior',   Gender=1, rep=1:18) %>%
  add_row(Year='senior',   Gender=0, rep=1:19) %>%
  add_row(Year='sophomore', Gender=1, rep=1:10) %>%
  add_row(Year='sophomore', Gender=0, rep=1:12)


# Variables that are character strings are coerced to factors.
# Variables that are numeric are not, and should be explicitly turned to factors.
Students <- Students %>% 
  mutate( Gender = factor(Gender) )

Students %>%
  ggplot( aes(x=Year, fill=Gender)) + 
  geom_bar() +
  coord_flip()


# Change Gender from 0/1  to Female/Male
Students <- Students %>%
  mutate( Gender = fct_recode(Gender, Male   = '1'),
          Gender = fct_recode(Gender, Female = '0') )

# ---OR

Students <- Students %>%
  mutate( Gender = if_else (Gender == '1','Male','Female'))

# Change "Freshman" to the gender neutral "First Year" 
Students <- Students %>%
  mutate( Year = fct_recode(Year, `first year` = 'freshman'))

Students %>%
  ggplot( aes(x=Year, fill=Gender)) +
  geom_bar() +
  coord_flip()

# APPLY A FUNCTION to to all LABELS 
Students %>%
  mutate( Year = fct_relabel(Year, stringr::str_to_upper) ) %>%
  ggplot( aes(x=Year, fill=Gender)) + geom_bar() + coord_flip()


# Reorder Levels
# Seniors first, leave the rest in the order they already were
Students %>%
  mutate( Year = fct_relevel(Year, 'senior') ) %>%
  mutate( Year = fct_relabel(Year, stringr::str_to_upper) ) %>%
  ggplot( aes(x=Year, fill=Gender)) +
  geom_bar() +
  coord_flip()


# reset all the levels orders
Students %>%
  mutate( Year = fct_relevel(Year, 'senior', 'junior','sophomore','first year') ) %>%
  mutate( Year = fct_relabel(Year, stringr::str_to_upper) ) %>%
  ggplot( aes(x=Year, fill=Gender)) +
  geom_bar() +
  coord_flip()



# Reverse order of what I already had
Students %>%
  mutate( Year = fct_relevel(Year, 'senior', 'junior','sophomore','first year') ) %>%
  mutate( Year = fct_rev(Year) ) %>%
  ggplot( aes(x=Year, fill=Gender)) +
  geom_bar() + 
  coord_flip()


# In the order of the most number of records
Students %>%
  mutate( Year = fct_infreq(Year) ) %>%
  ggplot( aes(x=Year, fill=Gender)) +
  geom_bar() +
  coord_flip()



# 2nd DATASET

Dems <- tribble(
  ~Candidate, ~Percent, ~AgeOnElection,
  'Elizabeth Warren', 27, 71,
  'Joe Biden',    25, 77,
  'Bernie Sanders',   12, 79,
  'Pete Buttigieg',   10, 38,
  'Kamala Harris',    3, 56,
  'Cory Booker',  2, 51,
  'Tulsi Gabbard',    2, 39,
  'Amy Klobuchar',    2, 60, 
  'Tom Steyer',   2, 63,
  'Andrew Yang',  2, 45, 
  'Other',    3, NA,
  'No one',   1, NA,
  'Undecided',    9, NA)


# Reorder Candidates based on the polling percent. The order of Smallest to largest 
# results in 'No one' at the bottom and Elizabeth Warren at the top. 
Dems %>%
  mutate( Candidate = fct_reorder(Candidate, Percent) ) %>%
  ggplot( aes(x=Candidate, y=Percent)) +
  geom_col() +
  coord_flip()


# Consider moving the Other and Undecided categories as the first categories before “No one.”
Dems %>%
  mutate( Candidate = fct_reorder(Candidate, Percent) ) %>%
  mutate( Candidate = fct_relevel(Candidate, 'Other', after=0) ) %>%
  mutate( Candidate = fct_relevel(Candidate, 'Undecided', after=0) ) %>%
  ggplot( aes(x=Candidate, y=Percent)) +
  geom_col() +
  coord_flip()



# Add or subtract Levels
# Lets collapse No one, Other and Undecided into a single Other category

Dems %>%
  mutate( Candidate = fct_collapse(Candidate, other = c('No one', 'Other', 'Undecided')) )



# Collapse the factor, then summarize by adding up the percentages  
Dems %>%
  mutate( Candidate = fct_collapse(Candidate, other = c('No one', 'Other', 'Undecided')) ) %>%
  group_by(Candidate) %>% 
  summarize(Percent = sum(Percent)) %>%
  mutate( Candidate = fct_reorder(Candidate, Percent) ) %>%
  mutate( Candidate = fct_relevel(Candidate, 'other', after=0) ) %>%
  ggplot( aes(x=Candidate, y=Percent)) +
  geom_col() +
  coord_flip()


# EXERCISES

# 1
# In the package Lock5Data there is a dataset FloridaLakes that contains water sample measurements
# from 53 lakes in Florida. Produce a bar graph that shows the Lake and AvgMercury variables, and make
# hat the lakes are ordered by Average Mercury content. To fix the issue of lake labels being 
# squished together, you could rotate the labels using a 
# + theme(axis.text.x = element_text(angle = 90, hjust = 1)) or flipping the coordinate axes.
# However, you’ll likely want to resize the figure so the labels aren’t so squished.
# the chunk options fig.height and fig.width control the size of the resulting figure.

FloridaLakes%>%
  mutate( Lake = fct_reorder(Lake, AvgMercury) ) %>%
  ggplot(aes(x=Lake , y = AvgMercury)) +
  geom_col()+
  coord_flip()


# 2
# In the package Lock5Data, there is a dataset FootballBrain that has brain measurements
# for 75 individuals. The Group variable has three levels: Control is somebody that did not
# play football, FBNoConcuss is a football player with no history of concussions, or FBConcuss
# which is a football player with concussion history. The variable Cogniton measures their testing
# composite reaction time score. Change the Group labels to something that would make sense to a
# reader and create a box-plot graph of the groups vs cognition. Because there is no data for
# the Control group, don’t show it on your graph. Also notice that the original data set column name
# misspells “cognition.”

FootballBrain %>%
  mutate(Group = fct_recode(Group,
                            Control_variable = 'Control',
                            No_Concussion_History ='FBNoConcuss',
                            Concussion_History ='FBConcuss'))%>%
  filter(Group != "Control_variable") %>%
  ggplot(aes(x=Group, y = Cogniton))+
  geom_boxplot(fill = 'brown') +
  labs(
    x = "Group",
    y = "Cognition Score",
    title = "Cognition Scores by Football Concussion History"
  ) +
  theme_classic()





  
  
  
  

