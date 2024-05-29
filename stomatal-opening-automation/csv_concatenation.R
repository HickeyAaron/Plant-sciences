library(dplyr)

# setwd("/Users/aaronhickey/Ilastik/test_output/")

# Set the directory where your CSV files are located
directory <- "/path/to/csv/files"


# List all CSV files in the directory
csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)


# Function to read and modify each CSV file
process_csv <- function(file) {
  # Read the CSV file
  data <- read.csv(file, header = TRUE)
  
  # Extract file name without extension
  file_name <- tools::file_path_sans_ext(basename(file))

  # Remove 'format_' from the file name
  modified_file_name <- gsub("^format_", "", file_name)
  
  # Add a new column 'Sample' with the file name
  data$Image <- file_name
  
  # Return the modified data
  return(data)
}

# Apply the function to each CSV file
processed_data <- lapply(csv_files, process_csv)

# Concatenate all processed data frames into one
combined_data <- do.call(rbind, processed_data)

head(Clean_data)

# Write the cleaned data to a single CSV file
write.csv(Clean_data, file = "combined_samples.csv", row.names = FALSE)
