# Ebola Sierra Leone analysis
# Tendai Admire Pasipamire
# 2026-05-25


# Load Packages ----
# pacman stands for package manager
# p_load() checks is package is available on user machine, if not it installs it
if (!require(pacman))  # checks if pacman is installed
  install.packages("pacman") # if pacman doesnt exist on this machine install it
suppressPackageStartupMessages({pacman::p_load( # installs and loads packages
  tidyverse, 
  inspectdf,
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
  glue
)})

options(dplyr.summarise.inform = FALSE)

# Create a tiny data frame that is easy to see what is happening
Mentors <- tribble(
  ~l.name, ~Gender, ~Exam1, ~Exam2, ~Final,
  'Cox',     'M',     93.2,   98.3,     96.4,
  'Kelso',   'M',     80.7,   82.8,     81.1)
Residents <- tribble(
  ~l.name, ~Gender, ~Exam1, ~Exam2, ~Final,
  'Dorian',  'M',     89.3,   70.2,   85.7,
  'Turk',    'M',     70.9,   85.5,   92.2)


# ADDING NEW ROWS

# We can give it as much or as little information as we want 
# any missing information will be denoted as missing using a NA .
# 
Residents <- Residents  %>%  # We save the result by overwritting the Residents dataset
  add_row( l.name='Reid', Exam1=95.3, Exam2=92.0)

Residents

# COMBINE TWO DATA FRAMES by using bind_rows ()
# now to combine two data frames by stacking Mentors first and then Residents
grades <- Mentors %>%
  bind_rows(Residents)

grades

# SUBSETTING

library(dplyr)
grades %>% 
  select( Exam1, Exam2 )  # Exam1 and Exam2
#OR
grades %>% 
  select( starts_with('Exam') )   # Exam1 and Exam2


grades %>% 
  select( starts_with(c('Exam','F')) ) # All three exams 
#OR
grades %>% 
  select( where(is.numeric) ) # select only the numerical numerical columns

# select numerical or character columns
grades %>% select( where(is.numeric), where(is.character) )

grades %>%
  select( Exam1:Final )    # Columns Exam1 through to Final

grades %>% 
  select( -Exam1 )         # Negative indexing by name drops a column

grades %>%
  select( 1:2 )            # Can select column by column position


# FILTER ROWS ( SLICING )
# slice() , slice_head() , slice_tail()
# grab the first 2 rows
grades %>%
  slice(1:2)

# UPDATE and CREATE new columns with mutate()

grades <- grades %>%  
  mutate( 
    average =  rowMeans(
      select(., Exam1, Exam2, Final),
      na.rm = TRUE
    ),
    grade = cut(average, c(0, 60, 70, 80, 90, 100),  # cut takes numeric variable
                c( 'F','D','C','B','A'))    # and makes a factor
  )

grades


#UPDATING
grades <- grades %>%
  mutate(
    Final = if_else(l.name == 'Reid', 98, Final ),
    Gender = if_else(l.name == 'Reid', "M", Gender)
    ) # If name == "Reid" replace final with 98 otherwise keep existing FINAL Value
grades


# STILL ON UPDATING
people <- data.frame(
  name = c('Barack','Michelle', 'George', 'Laura', 'Bernie', 'Deborah'),
  gender = c(1,0,1,0,1,0),
  party = c(1,1,2,2,3,3)
)
people

# Now we’ll update the gender and party columns to code these columns in a readable fashion.
# using CASE_WHEN()


people <- people %>%
  mutate( gender = if_else( gender == 0, 'Female', 'Male') ) %>% #if gender ==0 then Female otherwise Male
  mutate( party = case_when( party == 1 ~ 'Democratic',  # if party==1 then Democartic...else default to 'None Stated'
                             party == 2 ~ 'Republican', 
                             party == 3 ~ 'Independent',
                             TRUE       ~ 'None Stated' ) ) # Often the last case is a catch all case
people


