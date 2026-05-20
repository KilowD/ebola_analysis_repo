# Earthquakes analysis
# Tendai Admire Pasipamire
# 2026-05-19



# Load Packages ----
# pacman stands for package manager..
# p_load() 
if (!require(pacman))
  install.packages("pacman") # if pacman doesnt exist on this machine install it
pacman::p_load(
  tidyverse, #meta-pakage , it loads other packages inside of it
  inspectdf,
  plotly,
  janitor,
  visdat,
  esquisse,
  here,
  readxl,
  gt,          # cool tables
  flextable,   # cool tables
  reactable,   # cool tables
  outbreaks    # collection of Disease Outbreak data
)

# DATA 1: Earthquakes dataset

quakes

# TASK1: Show the Top 5 Earthquakes

quakes%>% # and then take this dataframe
  arrange(desc(mag)) %>% # and then arrange it by mag in descending order
  slice_head(n = 5) # and then take the first 5 rows


# TASK2: Show the 4 deepest quakes , then pass the output to gt

quakes%>%
  arrange(desc(depth))%>%
  slice_head(n=4)%>%
  gt::gt()


# DATA 1: varicella_sim_berlin

# TASK3: Show the names of the five youngest people in the data and output to gt

df <- as_tibble(varicella_sim_berlin)
df%>%
  arrange(age)%>%
  slice_head(n=5)%>%
  select(firstname,lastname, age) %>%
  gt()
  

#TASK 4: Show the sex of 4 oldest people

df <- as_tibble(varicella_sim_berlin)
df%>%
  arrange(desc(age))%>%
  select(firstname,lastname,sex,age)%>%
  slice_head(n=4)%>%
  gt()
  