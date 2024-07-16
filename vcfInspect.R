# Load necessary libraries
library(vcfR)
library(data.table)
library(zoo)


vcf_file <- "F2s_G0s.biallelic_positions_hardfiltered_GQ30_annotated.vcf"  # Replace with your actual VCF file path
vcf <- read.vcfR(vcf_file)

# Define file paths
modified_vcf_file <- "F2s_G0s_GQ30.smoothed.vcf"
smooth_vcf <- read.vcfR(modified_vcf_file)

qc_log_file <- "qc_log.txt"
# Read the QC log
qc_log <- read.table(qc_log_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)



# Step 2: Extract the data
vcf_data <- vcf@fix
chrom <- vcf_data[, "CHROM"]
pos <- as.numeric(vcf_data[, "POS"])

# Step 3: Locate the specific site
target_chrom <- "chr1"
target_pos <- 3092084

# Find the index of the target site
site_index <- which(chrom == target_chrom & pos == target_pos)

# Check if the site is found
if (length(site_index) == 0) {
  cat("The site chr1_3092084 is not found in the VCF file.\n")
} else {
  # Calculate the indices for the surrounding 8 sites (4 before and 4 after)
  start_index <- max(1, site_index - 4)
  end_index <- min(nrow(vcf_data), site_index + 4)
  
  # Extract the specific site and surrounding sites information
  surrounding_sites <- vcf_data[start_index:end_index, ]
  print(surrounding_sites)
  
  # Optional: If you need the genotype information too
  genotype_data <- vcf@gt[start_index:end_index, ]
  print(genotype_data)
}

