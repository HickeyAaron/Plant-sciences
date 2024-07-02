library("onemap")
library("vcfR")


setwd("/Users/aaronhickey/Documents/GitHub/X_AcTm/Figures/Figure 3 - QTL/Linkage_map/")

raw_vcf <- read.vcfR("F2s_G0s.smoothed.vcf")

onemap_vcf <- onemap_read_vcfR(vcf = "./Data/F2s_G0s.smoothed.vcf",
                               parent1 = "2021cicX11218980", 
                               parent2 = "2021cicX11218979", 
                               cross = "f2 intercross",
                               verbose = T)

saveRDS(onemap_vcf, "Onemap_F2_G0s.smoothed.rds")


bins <- find_bins(onemap_vcf, exact = FALSE)
bins

binned <- create_data_bins(onemap_vcf, bins)
LOD_sug <- suggest_lod(binned)

seg_test <- test_segregation(binned)
plot(seg_test)

no_dist <- select_segreg(seg_test, distorted = FALSE, numbers = TRUE) 
two_points <- rf_2pts(binned, LOD = LOD_sug)

mark_all_f2 <- make_seq(two_points, no_dist)
tmau_cal_groups <- group(mark_all_f2, LOD = LOD_sug)

genProbs <- extract_depth(vcfR.object = raw_vcf,
                          onemap.object = tmau_cal_groups$data.name,
                          parent1 = "2021cicX11218980", 
                          parent2 = "2021cicX11218979", 
                          vcf.par = c("PL"))

for (i in c(1:22)) {
  assign(paste("LG_", i, sep = ""), make_seq(tmau_cal_groups, arg = {{i}}))
}

for (i in c(1:22)) {
  assign(paste("LG_ug_", i, sep = ""), ug(get(paste("LG_", i, sep = "")), hmm = FALSE))
}

LG_ug_23 <- LG_ug_22
LG_ug_22 <- LG_ug_21
rm(LG_ug_21)

LG_ug_list <- list(LG_ug_1, LG_ug_2, LG_ug_3, LG_ug_4, LG_ug_5, LG_ug_6, LG_ug_7,
                   LG_ug_8, LG_ug_9, LG_ug_10, LG_ug_11, LG_ug_12, LG_ug_13, LG_ug_14,
                   LG_ug_15, LG_ug_16, LG_ug_17, LG_ug_18, LG_ug_19, LG_ug_20, LG_ug_22, LG_ug_23)


map_avoid_unlinked_list <- lapply(LG_ug_list, map_avoid_unlinked, genotypes_probs = genProbs)
draw_map(map_avoid_unlinked_list, names = FALSE, grid = TRUE, cex.mrk = 0.7)

write_map(map.list = map_avoid_unlinked_list, file.out = "Tmau_Acal_map")
