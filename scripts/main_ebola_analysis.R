# Ebola Sierra Leone analysis
# Tendai Admire Pasipamire
# 2026-01-26
 
# Assignment ( <- ) operator shortcut = ALT- 


# Load Packages ----
# pacman stands for package manager
# p_load() checks is package is available on user machine, if not it installs it
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

# Load data ----
ebola_sierra_leone_data <- read_csv(here("data/ebola_sierra_leone.csv")) # path relative to the defined folder of your project
yaounde_data <- read_excel(here("data/yaounde_data.xlsx"))

#rename the dataset name and remove the oldone ----
ebola_sierra_leone <-  ebola_sierra_leone_data
rm(ebola_sierra_leone_data)

# Explore the dataset ----
#view(yaounde_data)
# view(ebola_sierra_leone)  # view the data set
# rm(ebola_sierra_leone) # remove the data set from the environment

 
# general overview of  data
head(ebola_sierra_leone, n= 10) # default is 6
tail(ebola_sierra_leone)
ncol(ebola_sierra_leone)
nrow(ebola_sierra_leone)
dim(ebola_sierra_leone) # this gives us both ncol() and nrow()
summary(ebola_sierra_leone)

visdat::vis_dat(ebola_sierra_leone) # visualisation of the data

# Categorical overview of data
cat_summary_plot <- show_plot(inspectdf::inspect_cat(ebola_sierra_leone)) # we answer Questions like women vs men in numbers per sex category
plotly::ggplotly(cat_summary_plot)

# Numerical overview of data
num_summary_plot <- show_plot(inspectdf::inspect_num(ebola_sierra_leone)) # we answer Questions like women vs men in numbers per sex category
plotly::ggplotly(num_summary_plot)

# Analyzing single variables : numeric ----

# ATTACH -- means the OBJECT has been attached to the R search path..
# so objects in the OBJECT can be accessed by simply giving their names e.g age
attach(ebola_sierra_leone)
age_vec <- age

mean(age_vec, na.rm = TRUE) # ignore NA
median(age_vec, na.rm = T)
sd(age_vec, na.rm = T)
summary(age_vec)
length(age_vec)

# Visualizing single variables : numeric ----
# ggplot are the most recommended visuals through the esquisee package
hist(age_vec)
boxplot(age_vec)


# ggplot
# esquisse ----

# esquisse::esquisser(ebola_sierra_leone)..JUST USE it to generate producible CODE below

# the CODE below is generated after running esquisser(ebola_sierra_leone)
ggplot(ebola_sierra_leone,aes(x = age)) +
  geom_density(adjust = 1L, fill = "#112446") +
  theme_classic() 




# Visualizing single variables : categorical ----
tabyl(district) # i can call district direct since its attached above 

tabyl(ebola_sierra_leone,district,sex)


#esquisser(ebola_sierra_leone)
ggplot(ebola_sierra_leone) +
  aes(x = district, fill = district) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# QUESTIONS ----

# Q1: When was the first case reported
df <- ebola_sierra_leone
summarise(df,first_case = min(date_of_sample, na.rm = T))

# Q2: What was the median age of those affected?
df%>%
  filter(status == "confirmed") %>%
  summarise(median_age_of_infected = median(age, na.rm = T)) 


# Q3: Had there been more cases in men or women ?
df %>%
  filter(status == "confirmed") %>%
  group_by(sex) %>%
  summarise(total_cases_per_gender = n()) %>%
  arrange(desc(total_cases_per_gender))

# OR the below
df %>%
  filter(status == "confirmed") %>%
  tabyl(sex)

# OR Graphically
ggplot(ebola_sierra_leone) +
  aes(x = sex, fill = sex) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()


# Q4: What district had the most reported cases
df %>%
  filter(status == "confirmed") %>%
  group_by(district) %>%
  summarise(total_cases_by_district = n()) %>%
  arrange(desc(total_cases_by_district))

# Q5: Was the outbreak growing or receding by the end of June 2014?
df %>%
  filter(status == "confirmed") %>%
  mutate(year_month = format(date_of_onset , "%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(total_cases = n())%>%
  arrange(year_month)

# OR 
ggplot(ebola_sierra_leone, aes(x = date_of_onset)) +
  geom_bar(fill = "#112446", alpha =0.5) +
  theme_classic()


# Cases by district ----
district_tab <- janitor::tabyl(ebola_sierra_leone, district)
district_tab_arranged <- arrange(district_tab, n) # arranges in ASC order of the n variable
print(district_tab_arranged)

# OR

df%>%
  group_by(district)%>%
  summarise(district_count = n())%>%
  arrange(-district_count)%>% # - arranges in DESC order
  mutate(percentage = district_count / sum(district_count))



# output a data frame to a file ----
write_csv(x = district_tab_arranged, file = "outputs/district tabulation.csv")
write_csv(x = district_tab_2, file = "outputs/district_tabulation.csv")

# Visualize categorical variables ----
categ_vars_plot <- show_plot(inspectdf::inspect_cat(ebola_sierra_leone))
categ_vars_plot

# output a plot to a file ----
# for help put a cursor above the function and press f1
ggsave(categ_vars_plot ,filename = "outputs/categorical_plot.png" )
ggsave(
  categ_vars_plot,
  filename = "outputs/categorical_plot.pdf",
  width = 10 ,
  height = 10
)

# numerical overview ----
num_summary_plot <- show_plot(inspectdf::inspect_num(ebola_sierra_leone))
num_summary_plot
ggsave(
  filename = "outputs/numberical_summary_plot.pdf",
  num_summary_plot,
  width = 10 ,
  height = 10
)





