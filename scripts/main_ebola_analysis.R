# Ebola Sierra Leone analysis
# Tendai Admire Pasipamire
# 2026-01-26


# Load Packages ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  inspectdf,
  plotly,
  janitor,
  visdat,
  esquisse,
  here
)

# Load data ----
  ebola_sierra_leone <- read_csv(here("data/ebola_sierra_leone.csv"))

# Cases by district ----
district_tab <- janitor::tabyl(ebola_sierra_leone,district)
district_tab_arranged <- arrange(district_tab,n)
      

# output a data frame to a file
write_csv(x=district_tab_arranged, file = "outputs/district tabulation.csv")

# Visualize categorical variables ----
categ_vars_plot <- show_plot(inspectdf::inspect_cat(ebola_sierra_leone)) 

# output a plot to a file
# for help put a cursor above the function and press f1
ggsave(filename = "outputs/categorical_plot.png",categ_vars_plot)
ggsave(filename = "outputs/categorical_plot.pdf",categ_vars_plot, width = 10 , height = 10)

# numerical overview ----
num_summary_plot <- show_plot(inspectdf::inspect_num(ebola_sierra_leone))  
ggsave(filename = "outputs/numberical_summary_plot.pdf",num_summary_plot, width = 10 , height = 10)




# Explore data ----

head(ebola_sierra_leone, n=10)
tail(ebola_sierra_leone) 
ncol(ebola_sierra_leone)
nrow(ebola_sierra_leone)
dim(ebola_sierra_leone)
summary(ebola_sierra_leone)

# general overview of data ----
visdat::vis_dat(ebola_sierra_leone)

# categorical overview ----
cat_summary_plot <- show_plot(inspectdf::inspect_cat(ebola_sierra_leone)) # show a plot of cat_summary
ggplotly(cat_summary_plot)

# numerical overview ----
num_summary_plot <- show_plot(inspectdf::inspect_num(ebola_sierra_leone))  
ggplotly(num_summary_plot)

# Analyzing single variables : numeric ----
attach(ebola_sierra_leone)

age_vec <- age
mean(age_vec, na.rm = TRUE) # ignore NA
median(age_vec,na.rm = T)
sd(age_vec,na.rm = T)
summary(age_vec)
length(age_vec)

# Visualizing single variables : numeric ----
hist(age_vec)
boxplot(age_vec)


# ggplot
# esquisse ----

#esquisser(ebola_sierra_leone)
ggplot(ebola_sierra_leone) +
  aes(x = age) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()


# Visualizing single variables : categorical ----
barplot(table(district)) # this base plotting , not recommended

#esquisser(ebola_sierra_leone)
ggplot(ebola_sierra_leone) +
  aes(x = district) +
  geom_bar(fill = "#112446") +
  theme_minimal()

#









