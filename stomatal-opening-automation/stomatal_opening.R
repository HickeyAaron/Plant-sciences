###############################################################################
###############################################################################
#################### Stomatal counts categoriesation ##########################
###############################################################################
###############################################################################

# STEP 1: Environment setup
# STEP 2: Trains a decision tree model based on input training data and apply 
# the model to the raw data will very roughly categorise the raw data
# STEP 3: Apply a programatic curation step we can refine the output
# STEP 4: Data manipulation to identify sets of stomata
# STEP 5: Format categorisation output into an appropriate dataframe

###############################################################################
###############################################################################
###################### STEP 1: Environment setup ##############################
###############################################################################
###############################################################################

library(dplyr)
library(tidyr)
library(rpart)

# Read datasheet which contains "training" data
data <- read.csv("SCAVENGERS_27-03-24.csv") 

# Read the raw data that needs categorisation
# Format for current script is STOM_LEN, STOM_WID, PORE_LEN, PORE_WID 
# all in a single column with no header

raw_data <- read.csv("ML_test.csv", header = FALSE, col.names = c("Value"))

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

# Split data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(train_data), size = 0.8 * nrow(train_data))
train_set <- train_data[train_indices, ]
test_set <- train_data[-train_indices, ]

# Train the decision tree model
model <- rpart(Category ~ Value, data = train_set, method = "class")

# Print the model summary
print(summary(model))

# Predict on test data
predictions <- predict(model, test_set, type = "class")

# Evaluate the model
confusion_matrix <- table(Predicted = predictions, Actual = test_set$Category)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))


# Predict categories for the new data
predicted_categories <- predict(model, raw_data, type = "class")


###############################################################################
###############################################################################
########################## STEP 3: Refinement #################################
###############################################################################
###############################################################################


# Function to enforce the block and order constraints
enforce_constraints <- function(values, categories) {
  n <- length(values)
  result <- data.frame(Value = values, Category = categories)
  
  i <- 1
  while (i <= n) {
    # Categorize STOM_LEN
    if (categories[i] == "STOM_LEN" && i < n) {
      result$Category[i] <- "STOM_LEN"
      result$Category[i + 1] <- "STOM_WID"
      i <- i + 2
    }
    # Categorize PORE_LEN
    else if (categories[i] == "PORE_LEN" && i < n) {
      result$Category[i] <- "PORE_LEN"
      result$Category[i + 1] <- "PORE_WID"
      i <- i + 2
    }
    # Recategorize UNKNOWN following PORE_WID as STOM_LEN
    else if (i > 1 && categories[i] == "UNKNOWN" && result$Category[i - 1] == "PORE_WID") {
      result$Category[i] <- "STOM_LEN"
      if (i < n) {
        result$Category[i + 1] <- "STOM_WID"
      }
      i <- i + 2
    }
    # Recategorize UNKNOWN preceding PORE_WID as PORE_LEN
    else if (i < n && categories[i] == "UNKNOWN" && categories[i + 1] == "PORE_WID") {
      result$Category[i] <- "PORE_LEN"
      i <- i + 1
    }
    # Ensure STOM_WID is not repeated consecutively
    else if (categories[i] == "STOM_WID" && i > 1 && result$Category[i - 1] == "STOM_WID") {
      result$Category[i] <- "STOM_LEN"
      if (i < n) {
        result$Category[i + 1] <- "STOM_WID"
      }
      i <- i + 2
    }
    # Ensure STOM_WID is not followed by another STOM_WID
    else if (categories[i] == "STOM_WID" && i < n && categories[i + 1] == "STOM_WID") {
      result$Category[i] <- "STOM_LEN"
      if (i < n) {
        result$Category[i + 1] <- "STOM_WID"
      }
      i <- i + 2
    }
    # Handle remaining UNKNOWN categories
    else if (categories[i] == "UNKNOWN") {
      if (i > 1 && result$Category[i - 1] == "STOM_LEN") {
        result$Category[i] <- "STOM_WID"
      } else {
        result$Category[i] <- "UNKNOWN"
      }
      i <- i + 1
    }
    # Move to the next value if no conditions match
    else {
      i <- i + 1
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

write.csv(formatted_data, "ML_test_output.csv")


