# Load necessary libraries
library(vcfR)
library(data.table)
library(zoo)

setwd("/Users/aaronhickey/Documents/GitHub/X_AcTm/Figures/Figure 3 - QTL/VCF_smoothing/")

# Function to check for the specific pattern in a vector of calls
check_pattern_vector <- function(vec) {
  # Check for "./." values in the vector
  if (any(vec == "./.")) {
    return(FALSE)  # Return FALSE if any "./." values are present
  }
  
  # Check if there are 4 identical calls, a different call, and then 4 identical calls
  identical_calls_1 <- vec[1] == vec[2] && vec[1] == vec[3] && vec[1] == vec[4]
  different_call <- vec[1] != vec[5]
  identical_calls_2 <- vec[1] == vec[6] && vec[1] == vec[7] && vec[1] == vec[8] && vec[1] == vec[9]
  
  # Debug print
  #print(paste("Pattern Check:", identical_calls_1, different_call, identical_calls_2))
  
  return(identical_calls_1 && different_call && identical_calls_2)
}

# Function to smooth the genotype at a given position
smooth_genotype <- function(genotypes, pos) {
  if (length(genotypes) >= 9) {
    # Set the 5th position genotype to match the surrounding genotypes
    genotypes[pos + 4] <- genotypes[pos]
    
    # Debug print
    #print(paste("Smoothing at pos", pos, ":", genotypes))
  }
  return(genotypes)
}

# Read the VCF file
vcf_file <- "Itupi_Usisya_allFounders+F2s_meanGQ60_genoGQ40.vcf"  # Replace with your actual VCF file path
vcf <- read.vcfR(vcf_file)

# Store the original VCF for comparison
original_vcf <- vcf

# Initialize QC log
qc_log <- data.frame(Position = integer(), Column = character(), stringsAsFactors = FALSE)
modification_count <- 0

# Iterate through each sample column to find and modify the pattern
for (col in 2:ncol(vcf@gt)) {  # Skip the first column which is FORMAT
  # Extract the column data
  column_data <- vcf@gt[, col]
  
  # Split the genotypes 
  column_data_split <- strsplit(column_data, ":")
  
  # Extract just the GT fields
  gt_fields <- sapply(column_data_split, function(x) x[1])
  
  # Iterate through the genotypes in this column to find and modify patterns
  for (row in seq_len(length(gt_fields) - 8)) {
    # Check for the pattern in the current window of genotypes
    if (gt_fields[row] != "./." && check_pattern_vector(gt_fields[row:(row + 8)])) {
      # Smooth the genotypes at the identified position
      gt_fields[row:(row + 8)] <- smooth_genotype(gt_fields[row:(row + 8)], 1)
      
      # Log the modification
      qc_log <- rbind(qc_log, data.frame(Position = row, Column = colnames(vcf@gt)[col], stringsAsFactors = FALSE))
      modification_count <- modification_count + 1
    }
  }
  
  # Combine the modified GT fields back with the other data (if any)
  for (i in seq_along(column_data_split)) {
    column_data_split[[i]][1] <- gt_fields[i]
    column_data[i] <- paste(column_data_split[[i]], collapse = ":")
  }
  
  # Replace the original column with modified genotypes
  vcf@gt[, col] <- column_data
}

output_file <- "modified_file.vcf.gz"
write.vcf(vcf, file = output_file)

# Save the QC log
qc_log_file <- "qc_log.txt"
read.table(qc_log, file = qc_log_file, sep = "\t", quote = FALSE, row.names = FALSE)





vcf_file <- "F2s_G0s.biallelic_positions_hardfiltered_GQ30_annotated.vcf"  # Replace with your actual VCF file path
vcf <- read.vcfR(vcf_file)

# Define file paths
modified_vcf_file <- "F2s_G0s_GQ30.smoothed.vcf"
smooth_vcf <- read.vcfR(modified_vcf_file)

qc_log_file <- "qc_log.txt"
# Read the QC log
qc_log <- read.table(qc_log_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)



# Check if the QC log is empty
if (nrow(qc_log) > 0) {
  # Get the first modification for inspection
  first_modification <- qc_log[1, ]
  
  # Extract position and column
  position <- first_modification$Position
  column <- first_modification$Column
  
  # Read the modified VCF file
  vcf <- read.vcfR(modified_vcf_file)
  
  # Inspect the modified region
  modified_genotypes <- vcf@gt[position:(position+8), column]
  
  # Print the modified region
  print(modified_genotypes)
} else {
  print("No modifications found in the QC log.")
}







# Can Modify the function above with the below to add a GQ filter but I found 
# that the sites that were being removed were always below a filter level that 
# would be reasonable to apply anyway.

# # Quality threshold
# quality_threshold <- 70  # Set your desired quality threshold
# 
# # Iterate through each sample column to find and modify the pattern
# for (col in 2:ncol(vcf@gt)) {  # Skip the first column which is FORMAT
#   # Extract the column data
#   column_data <- vcf@gt[, col]
#   
#   # Split the genotypes 
#   column_data_split <- strsplit(column_data, ":")
#   
#   # Extract just the GT fields
#   gt_fields <- sapply(column_data_split, function(x) x[1])
#   
#   # Extract the GQ fields (assuming the ninth field is GQ)
#   gq_fields <- as.numeric(sapply(column_data_split, function(x) x[9]))
#   
#   # Iterate through the genotypes in this column to find and modify patterns
#   for (row in seq_len(length(gt_fields) - 8)) {
#     # Check for the pattern in the current window of genotypes
#     if (gt_fields[row] != "./." && check_pattern_vector(gt_fields[row:(row + 8)])) {
#       # Smooth the genotypes at the identified position if the quality score at position 5 is below the threshold
#       if (gq_fields[row + 4] <= quality_threshold) {
#         gt_fields[row:(row + 8)] <- smooth_genotype(gt_fields[row:(row + 8)], 1, gq_fields[row:(row + 8)], quality_threshold)
#         
#         # Log the modification
#         qc_log <- rbind(qc_log, data.frame(Position = row, Column = colnames(vcf@gt)[col], stringsAsFactors = FALSE))
#         modification_count <- modification_count + 1
#       }
#     }
#   }
#   
#   # Combine the modified GT fields back with the other data (if any)
#   for (i in seq_along(column_data_split)) {
#     column_data_split[[i]][1] <- gt_fields[i]
#     column_data[i] <- paste(column_data_split[[i]], collapse = ":")
#   }
#   
#   # Replace the original column with modified genotypes
#   vcf@gt[, col] <- column_data
# }
