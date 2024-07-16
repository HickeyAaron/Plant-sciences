# Load the vcfR package
# install.packages("vcfR")
library(vcfR)

# Step 1: Read the VCF file
vcf_file <- "path_to_your_file.vcf"
vcf <- read.vcfR(vcf_file)

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

