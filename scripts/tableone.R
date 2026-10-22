library(tableone) # you need to install if first if you do not have it

# ---------------------------------------------------------------------------
# Create a descriptive table using the tableone package
# ---------------------------------------------------------------------------



# Select the variables to include in the table

variables_for_table <- c(
  "age",
  "bmi",
  "sex", # make sure this is a factor/chr variable using class(nhanes_data$sex). If not, you need to specify it in the function CreateTableOne later. Or change the variable
  "bmi_category" #make sure this is a factor/chr variable using class(nhanes_data$bmi_category)
)


# Create Table 1, stratified by physical activity

tableone_descriptive <- CreateTableOne(
  vars = variables_for_table,
  strata = "activity",
  data = nhanes_data,
  test = TRUE,
  addOverall = FALSE
)


# Display the table

print(
  tableone_descriptive,
  showAllLevels = TRUE,
  contDigits = 1,
  catDigits = 1,
  pDigits = 3,
)

# by printing it this way you can copy it from Console, paste into Excel by using "text import wizard", and then to word.
print(
  tableone_descriptive,
  showAllLevels = TRUE,
  contDigits = 1,
  catDigits = 1,
  pDigits = 3,
  noSpaces = TRUE,
  quote = TRUE
)

#Store it in your computer as a csv file and then import it into excel.
tableone_descriptive <- print(tableone_descriptive, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tableone_descriptive, file = "outputs/tableone_descriptive.csv")

