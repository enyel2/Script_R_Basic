#### Regresion multiple

housing <- read.table("http://jaredlander.com/data/housing.csv", sep = ",", header = TRUE,
                      stringsAsFactors = FALSE)

dim(housing)
names(housing)
sapply(housing, class)

names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                    "SqFt", "Income", "IncomePerSqFt", "Expense", "ExpensePerSqFt",
                    "NetIncome", "Value", "ValuePerSqFt", "Boro")

names(housing)
head(housing)
