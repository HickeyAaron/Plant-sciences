library(vcfR)
library(dplyr)

# Load the VCF file
vcf <- read.vcfR("input.vcf")

# Convert VCF to a data frame
vcf_df <- as.data.frame(vcf@fix)

# Filter based on MAF and missing data
vcf_filtered <- vcf_df %>%
  filter(MAF >= 0.05, missing <= 0.1)

# Randomly sample a subset of SNPs
set.seed(123)
vcf_sampled <- vcf_filtered %>%
  sample_n(size = 90000)  # Adjust the number of sites as needed

# Save the filtered and sampled VCF file
vcf@fix <- as.matrix(vcf_sampled)
write.vcf(vcf, "filtered_sampled_output.vcf")



gt <- extract.gt(vcf)

# Calculate MAF
calc_maf <- function(gt) {
  gt <- as.numeric(gt)
  p <- mean(gt, na.rm = TRUE) / 2
  return(min(p, 1 - p))
}
maf <- apply(gt, 1, calc_maf)

# Calculate missing data proportion
missing <- rowMeans(is.na(gt))

# Combine MAF and missing data with the VCF data
vcf_df <- as.data.frame(vcf@fix)
vcf_df$MAF <- maf
vcf_df$missing <- missing

# Filter based on MAF and missing data
vcf_filtered <- vcf_df %>%
  filter(MAF >= 0.05, missing <= 0.1)

# Randomly sample a subset of SNPs
set.seed(123)
vcf_sampled <- vcf_filtered %>%
  sample_n(size = 90000)  # Adjust the number of sites as needed

# Save the filtered and sampled VCF file
vcf@fix <- as.matrix(vcf_sampled)
write.vcf(vcf, "filtered_sampled_output.vcf")
