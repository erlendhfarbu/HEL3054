# language tip if æ ø å cannot be displayed
Sys.getlocale()
#Sys.getlocale(locale="Norwegian Bokmål_Norwegian") #mac
Sys.getlocale(locale = "nb_NO.UTF-8") #windows
print((c("æ", "ø", "å")))

2+2
45*6
10-4
(10+4)*5

# calculate 3 squared
3^2

sqrt(4)

#Single values
age <- 45
weight <- 76
height <- 1.82
BMI <- weight/height^2
weight

# Create vectors
age <- c(40, 45, 50, 46)
weight <- c(60, 80, 95, 76)
height <- c(1.60, 1.8, 1.85, 1.75)
BMI <- weight/height^2

sex <- c("Female", "Male", "Female", "Male")

age
age[1]
age[3]
length(age)
?length

#choose on condition
age[age > 45]

# Put vectors into tibble
health_data <- tibble(id=1:4, 
                      sex,
                      age,
                      height,
                      weight)
health_data
#View the data
View(health_data)
#get dimensions of dataframe/tibbel
dim(health_data)

#Yet another way of looking at the data
glimpse(health_data)

#create a new variable in a tibble

health_data <- health_data |> mutate(
  bmi=weight/height^2)

health_data

# select variables
health_data |> select(id, age)
health_data

# select and put into new tibble/dataframe
small <- health_data |> select(id, age)
small

# select participants with criteria/condtitions
health_data |> filter(age>45)
health_data <- health_data |> mutate(active= c(
  "Yes",
  "No",
  "Yes",
  "No")
)
health_data
active2 <- c("yes", "no", "yes", "no")
health_data <- health_data |> mutate(active2)
health_data

# have a look at summaries
health_data |> summarise(mean_age=mean(age))
?summarise

health_data |> summarise(mean_age=mean(age), 
                         mean_bmi=mean(bmi))
#saving the summaries into a new tibble
our_first_table <- health_data |> summarise(mean_age=mean(age), 
                                            mean_bmi=mean(bmi))
our_first_table
#Grouping on active, and counting number of participant in each group
our_first_table <- health_data |> group_by(active) |> summarise(number=n(), mean_age=mean(age), 
                                            mean_bmi=mean(bmi))
our_first_table

# store our tibble/dataframe
write_csv(health_data, "my_first_dataset.csv")

getwd()

#list all objects
ls()
# make a list of all objects
list_of_objects <- ls()
#remove everything
rm(list=list_of_objects)
# or directly
rm(list=ls())
# can select objects and put inside rm(object)

#lists every object you have in the environment
ls()
#now its empty

read.csv("my_first_dataset.csv")
data <- read.csv("my_first_dataset.csv") 

# calculate mean weigth by sex
data |> group_by(sex) |> summarise(number=n(), mean_age=mean(age), 
                                             mean_bmi=mean(weight))
