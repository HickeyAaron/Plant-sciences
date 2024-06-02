###############################################################################
###############################################################################
#################### Stomatal counts categorisation ##########################
###############################################################################
###############################################################################

# General setup should be changing the various filepaths in step 1. 
# Proceed to step 2 and check the QC output. If there are odd number of rows
# in any of the raw data input csv files it should output which files. 
# After that comment out unneccessary steps in step 1, highlight the 
# entire script and run

# STEP 1: Environment setup
# STEP 2: Raw data quality control for even numbers check
# STEP 3: Trains a random forest model based on input training data 
# STEP 4: Define a function to perform a programmatic curation step to refine the output
# STEP 5: Stomata grouping 
# STEP 6: Apply the model to the raw data and 
# Format categorisation output into an appropriate dataframe

###############################################################################
###############################################################################
###################### STEP 1: Environment setup ##############################
###############################################################################
###############################################################################

# Install necessary packages
# install.packages(c("googlesheets4","randomForest"))

library(googlesheets4)
library(dplyr)
library(tidyr)
library(randomForest)

# Authorize Google Sheets
# gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets")

# Replace with your Google Sheet ID
sheet_id <- "https://docs.google.com/spreadsheets/d/1VzO0u2xZF-UNMwoNIjxkngERsSwk7rEcC51MJckiQB0/edit#gid=0"

# Read the training data from the sheet
data <- read_sheet(sheet_id, sheet = "Sheet1")

# Directory containing the raw data input CSV files
input_directory <- "/Users/aaronhickey/georgia"
output_file <- "/Users/aaronhickey/georgia/combined_results.csv"

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
########################### STEP 2: QC of input data ##########################
###############################################################################
###############################################################################

# List all CSV files in the directory
csv_files <- list.files(input_directory, pattern = "\\.csv$", full.names = TRUE)

# Function to check if a number is even
is_even <- function(x) {
  x %% 2 == 0
}

# Loop through each CSV file
for (file in csv_files) {
  # Read the CSV file with header
  data <- read.csv(file)
  
  # Remove the first row (the header)
  data_without_header <- data[-1,]
  
  # Get the number of rows in the data without the header
  num_rows <- nrow(data_without_header) + 1
  
  # Check if the number of rows is even
  if (!is_even(num_rows)) {
    # Print a message if the number of rows is odd
    cat(file, "- uneven row number.\n")
  }
}

###############################################################################
###############################################################################
############## STEP 3: Model training and rough categorisation ################
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

###############################################################################
###############################################################################
########################## STEP 4: Programatic Curation #######################
###############################################################################
###############################################################################

# Function to enforce constraints with error handling
enforce_constraints <- function(values, categories, file_name) {
  n <- length(values)
  result <- data.frame(Value = values, Category = categories)
  
  tryCatch({
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
  }, error = function(e) {
    stop(paste("Error in file:", file_name, "\n", e))
  })
}

###############################################################################
###############################################################################
########################## STEP 5: Stomata grouping ###########################
###############################################################################
###############################################################################

# Function to group stomata
group_stomata <- function(data) {
  # Create a new column to hold the set number
  data$Set <- NA
  
  # Initialize set counter
  set_number <- 1
  
  # Initialize variable to hold the last observed category
  last_category <- NULL
  
  # Iterate through the rows of the result dataframe
  for (i in 1:nrow(data)) {
    # Check if the current row's category is STOM_LEN
    if (data$Category[i] == "STOM_LEN") {
      # Check if the last observed category is not NULL and was not STOM_LEN
      if (!is.null(last_category) && last_category != "STOM_LEN") {
        # Increment the set counter
        set_number <- set_number + 1
      }
      # Assign the set number to the current row
      data$Set[i] <- set_number
    } else {
      # Assign the set number based on the set number of the previous row
      data$Set[i] <- data$Set[i - 1]
    }
    # Update the last observed category
    last_category <- data$Category[i]
  }
  
  return(data)
}

###############################################################################
###############################################################################
#################### STEP 6: Apply model and process data #####################
###############################################################################
###############################################################################

# Initialize an empty list to store the results
all_results <- list()

# Iterate through each file and process it
for (file in csv_files) {
  # Read the CSV file
  raw_data <- read.csv(file, header = FALSE)
  
  # Check and assign column names based on the number of columns
  col_names <- c("Count", "Angle", "Value")
  if (ncol(raw_data) > length(col_names)) {
    col_names <- c("Count", "Angle", "Value", paste0("Extra_", 1:(ncol(raw_data) - length(col_names))))
  }
  colnames(raw_data) <- col_names
  raw_data <- raw_data[-1, ]
  
  # Extract file name without extension
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Predict categories for the new data
  raw_data <- raw_data %>%
    mutate(Value = as.numeric(Value))
  
  predicted_categories <- predict(model, raw_data)
  
  # Combine data with predicted categories and file name
  result <- data.frame(Image = file_name, Value = raw_data$Value, Category = predicted_categories)
  
  # Ensure the first row in the results is always STOM_LEN
  if (result$Category[1] != "STOM_LEN") {
    result$Category[1] <- "STOM_LEN"
  }
  
  # Apply the function to enforce constraints
  result <- enforce_constraints(result$Value, result$Category)
  
  # Group the stomata
  result <- group_stomata(result)
  
  # Append the result to the list
  all_results[[file_name]] <- result
}

# Combine all results into a single dataframe
combined_results <- do.call(rbind, all_results)

# Ensure PORE values are populated
unique_sets <- unique(combined_results$Set)

for (set_num in unique_sets) {
  subset_data <- combined_results[combined_results$Set == set_num, ]
  
  if (!("PORE_LEN" %in% subset_data$Category)) {
    combined_results <- rbind(combined_results, data.frame(Image = unique(subset_data$Image), Value = 0, Category = "PORE_LEN", Set = set_num))
  }
  
  if (!("PORE_WID" %in% subset_data$Category)) {
    combined_results <- rbind(combined_results, data.frame(Image = unique(subset_data$Image), Value = 0, Category = "PORE_WID", Set = set_num))
  }
}

# Reorganise data to include image column and order appropriately for QC
combined_results$Image <- rownames(combined_results)
rownames(combined_results) <- NULL

combined_results$Image <- sub("(?<=\\.)(\\d)(?=$)", "0\\1", combined_results$Image, perl=TRUE)
combined_results <- combined_results[order(combined_results$Image, combined_results$Set), ]

combined_results$Image <- combined_results$Image

# Reorder the columns with Image as the first column
formatted_data <- combined_results[, c("Image", names(combined_results)[-which(names(combined_results) == "Image")])]

# Pivot wider with handling duplicates
formatted_data <- pivot_wider(combined_results, names_from = Category, values_from = Value, values_fn = list)


# Function to take the first non-null value in each group
first_non_null <- function(x) {
  return(x[!is.na(x)][1])
}

# Remove the suffix from the Image column
combined_results <- combined_results %>%
  mutate(CoreImage = sub("\\.\\d+$", "", Image))

# Group by CoreImage and Set, then summarize to get one row per CoreImage and Set
grouped_data <- combined_results %>%
  group_by(CoreImage, Set) %>%
  summarize(
    STOM_LEN = first_non_null(Value[Category == "STOM_LEN"]),
    STOM_WID = first_non_null(Value[Category == "STOM_WID"]),
    PORE_LEN = first_non_null(Value[Category == "PORE_LEN"]),
    PORE_WID = first_non_null(Value[Category == "PORE_WID"])
  ) %>%
  ungroup()

# Rename CoreImage back to Image if needed
grouped_data <- grouped_data %>%
  rename(Image = CoreImage)

# Replace all the NAs with 0
grouped_data <- grouped_data %>%
  mutate(across(everything(), ~replace(., is.na(.), 0)))

print(grouped_data)

# Save the combined results to a CSV file
write.csv(grouped_data, file = output_file, row.names = FALSE)
