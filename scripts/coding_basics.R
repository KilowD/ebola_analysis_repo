# R as a calculator ----
2^6

sqrt(100)

# Formatting Code ----

sqrt(100)
sqrt(100)

# Creating Objects in R ----
# for the assignment operator <- press alt-

my_obj <- 2+2 

firstname <- "Kilow"
surname <-  "pams"
c(firstname,surname)

#remove objects ----

rm(another_object)

# Datasets are objects too! ----
data()
ebola_data <- read.csv("https://tinyurl.com/ebola-data-sample")
diabetes_china <- read.csv("https://tinyurl.com/diabetes-china")
View(diabetes_china)

# Object renaming 
Ebola_stats <-  ebola_data 
rm(ebola_data)

# Override Objects contents
firstname = "Suarez"

# Working with Objects

my_number <- 100

sqrt(my_number)

attach(Ebola_stats)

table(sex)
summary(sex)
summary(Ebola_stats)

eight <- 9
answer <- eight - 8

Patient_distribution <- table(district)
Patient_distribution

# Errors with Objects ----
first_name <- "Kilow"
last_name <-  "pams"

 

# Functions ----
head(Ebola_stats)
head(x=Ebola_stats, n = 9)
head(Ebola_stats,9)


# Function nesting ----
paste("My name is :", tolower("PAMS"))

my_name <- tolower("PAMS")
paste("My name is :",my_name)

# Packages
  #   install.packages("tableone")
library(tableone)

CreateTableOne(data = Ebola_stats)
summary(Ebola_stats)

# pacman ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(outbreaks, tableone,janitor)

