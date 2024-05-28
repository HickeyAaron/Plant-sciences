###############################################################################
###############################################################################
#################### Stomatal counts categorisation ##########################
###############################################################################
###############################################################################

# STEP 1: Environment setup
# STEP 2: Trains a random forest model based on input training data and apply 
# the model to the raw data to very roughly categorise the raw data
# STEP 3: Apply a programmatic curation step to refine the output
# STEP 4: Data manipulation to identify sets of stomata
# STEP 5: Format categorisation output into an appropriate dataframe

###############################################################################
###############################################################################
###################### STEP 1: Environment setup ##############################
###############################################################################
###############################################################################

setwd("/Users/aaronhickey/Downloads")

# Install necessary packages
install.packages(c("googlesheets4","randomForest"))

library(googlesheets4)
library(dplyr)
library(tidyr)
library(randomForest)

# Authorize Google Sheets
gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets")

# Replace with your Google Sheet ID
sheet_id <- "https://docs.google.com/spreadsheets/d/1VzO0u2xZF-UNMwoNIjxkngERsSwk7rEcC51MJckiQB0/edit#gid=0"

# Read the training data from the sheet
data <- read_sheet(sheet_id, sheet = "Sheet1")

# Read datasheet which contains "training" data
# data <- read.csv("SCAVENGERS_27-03-24.csv") 

# Read the raw data that needs categorisation
# Format for current script is STOM_LEN, STOM_WID, PORE_LEN, PORE_WID 
# all in a single column with no header
# raw_data <- read.csv("ML_test.csv", header = FALSE, col.names = c("Value"))

raw_data <- read.csv("Results.csv", header = FALSE, col.names = c("Count", "Angle", "Value"))
raw_data <- raw_data[-1, ]

# Reshape the data to long format
long_data <- data %>%
  pivot_longer(cols = c(PORE_LEN, PORE_WID, STOM_WID, STOM_LEN), 
               names_to = "Measurement", values_to = "Value")

# Create a category column based on the measurement
long_data <- long_data %>%
  mutate(Category = case_when(
    Measurement == "PORE_LEN" ~ "PORE_LEN",
    Measurement == "PORE_WID" ~ "PORE_WID",
    Measurement == "STOM_WID" ~ "STOM_WID",
    Measurement == "STOM_LEN" ~ "STOM_LEN"
  ))

###############################################################################
###############################################################################
############## STEP 2: Model training and rough categorisation ################
###############################################################################
###############################################################################

# Select relevant columns for training
train_data <- long_data %>% select(Value, Category)

# Handle missing values by removing rows with NA values
train_data <- train_data %>% drop_na()

# Ensure that 'Category' is a factor and 'Value' is numeric
train_data <- train_data %>%
  mutate(Category = as.factor(Category),
         Value = as.numeric(Value))

# Split data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(train_data), size = 0.8 * nrow(train_data))
train_set <- train_data[train_indices, ]
test_set <- train_data[-train_indices, ]

# Train the random forest model
model <- randomForest(Category ~ Value, data = train_set, ntree = 100)

# Print the model summary
print(model)

# Predict on test data
predictions <- predict(model, test_set)

# Evaluate the model
confusion_matrix <- table(Predicted = predictions, Actual = test_set$Category)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Predict categories for the new data
raw_data <- raw_data %>%
  mutate(Value = as.numeric(Value))  # Ensure raw data is numeric

predicted_categories <- predict(model, raw_data)

###############################################################################
###############################################################################
########################## STEP 3: Refinement #################################
###############################################################################
###############################################################################

enforce_constraints <- function(values, categories) {
  n <- length(values)
  result <- data.frame(Value = values, Category = categories)
  
  for (i in 1:(n-1)) {
    if (result$Category[i] == "STOM_LEN") {
      result$Category[i + 1] <- "STOM_WID"
    } else if (result$Category[i] == "STOM_WID") {
      if (i < n - 1 && result$Category[i + 1] == "STOM_WID") {
        result$Category[i + 1] <- "STOM_LEN"
      } else if (i < n - 1 && !(result$Category[i + 1] %in% c("STOM_LEN", "PORE_LEN"))) {
        result$Category[i + 1] <- ifelse(i %% 2 == 1, "STOM_LEN", "PORE_LEN")
      }
    } else if (result$Category[i] == "PORE_LEN") {
      if (i < n && result$Category[i + 1] == "STOM_WID") {
        result$Category[i] <- "STOM_LEN"
        result$Category[i + 1] <- "STOM_WID"
      } else {
        result$Category[i + 1] <- "PORE_WID"
      }
    } else if (result$Category[i] == "PORE_WID") {
      result$Category[i + 1] <- "STOM_LEN"
    }
  }
  
  return(result)
}

# Apply the function to enforce constraints
result <- enforce_constraints(raw_data$Value, predicted_categories)
print(result)

###############################################################################
###############################################################################
########################## STEP 4: Stomata grouping ###########################
###############################################################################
###############################################################################

# Create a new column to hold the set number
result$Set <- NA

# Initialize set counter
set_number <- 1

# Initialize variable to hold the last observed category
last_category <- NULL

# Iterate through the rows of the result dataframe
for (i in 1:nrow(result)) {
  # Check if the current row's category is STOM_LEN
  if (result$Category[i] == "STOM_LEN") {
    # Check if the last observed category is not NULL and was not STOM_LEN
    if (!is.null(last_category) && last_category != "STOM_LEN") {
      # Increment the set counter
      set_number <- set_number + 1
    }
    # Assign the set number to the current row
    result$Set[i] <- set_number
  } else {
    # Assign the set number based on the set number of the previous row
    result$Set[i] <- result$Set[i - 1]
  }
  # Update the last observed category
  last_category <- result$Category[i]
}

print(result)

###############################################################################
###############################################################################
################### STEP 5: Populating PORE values to 0 #######################
###############################################################################
###############################################################################

# Identify unique set numbers
unique_sets <- unique(result$Set)

# Iterate through each unique set
for (set_num in unique_sets) {
  # Subset the data for the current set
  subset_data <- result[result$Set == set_num, ]
  
  # Check if the subset contains PORE_LEN and PORE_WID
  if (!("PORE_LEN" %in% subset_data$Category)) {
    # Add PORE_LEN category with value 0
    result <- rbind(result, data.frame(Value = 0, Category = "PORE_LEN", Set = set_num))
  }
  
  if (!("PORE_WID" %in% subset_data$Category)) {
    # Add PORE_WID category with value 0
    result <- rbind(result, data.frame(Value = 0, Category = "PORE_WID", Set = set_num))
  }
}

# Sort the result dataframe by Set and Value
result <- result[order(result$Set, result$Value), ]

# Pivot wider with handling duplicates
formatted_data <- pivot_wider(result, names_from = Category, values_from = Value, values_fn = list)

formatted_data <- as.data.frame(lapply(formatted_data, unlist))

print(formatted_data)

write_csv <- write.csv(formatted_data, "ML_test_output.csv")
