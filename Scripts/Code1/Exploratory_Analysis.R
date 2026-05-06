set.seed(13579)
# =========================================================
# 0. LOAD REQUIRED PACKAGES
# =========================================================
# NOTE: Install packages manually if needed
library(readxl) # import
library(clipr)  # export
library(psych)   # psychometrics, factor analysis
library(GPArotation)  # rotation
library(Rcsdp)  # optimization
library(dplyr) # manipulation
library(tidyr) # reshaping
library(stringr) # strings
library(tibble)  # data frames
library(rlang)   # programming
library(ggplot2) # visualization
library(igraph)   # network, MDS, community detection
library(ggrepel)  # labeling, MDS graph
library(scales)  # transform, scaling
library(magick)   # merge, imaging
library(crayon)  # formating

# =========================================================
# 1. DATA IMPORT
# =========================================================
# IMPORTANT: Update paths before running
desktop_path <- "data/"
apma <- read_excel(file.path(desktop_path, "apma.xlsx"))
apma=apma[-1,]  # remove header row if necessary

# =========================================================
# 2. DEFINE ITEM SETS (BY YEAR)
# =========================================================
items_2017 <- c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I12", "I13", "I16", "I17",
                "I18", "I19", "I20", "I21", "I22", "I23","I24", "I25", "I26", "I27", "I28", "I29", 
                "I30", "I31", "I32", "I33", "I34", "I35", "I36", "I37", "I38", "I39","I40", "I41", 
                "I42", "I43", "I44", "I45", "I46", "I47", "I48", "I49", "I50", "I51", "I52", "I53",
                "I54", "I55", "I56", "I57", "I58", "I59", "I60", "I61", "I62", "I63","I65", "I66",
                "I67", "I68", "I69", "I70", "I71", "I72", "I73", "I74", "I75", "I76", "I77", "I78", 
                "I79", "I80", "I81", "I82", "I83", "I84", "I85")
length(items_2017) #80 items
year_2017<-apma[apma$year==1,items_2017]
nrow(year_2017)  #286 respondents
items_2019 <- c("I1","I2","I6","I7","I9","I12","I13","I24","I25","I28","I29","I32","I34","I35","I36",
                "I40","I41","I43","I44","I45","I46","I47","I48","I49","I50","I51","I52","I53","I55",
                "I56","I62","I63","I64","I67","I68","I69","I70","I71","I73","I74","I75","I76","I77",
                "I78","I80","I81","I82","I84","I85")
length(items_2019)  #49 items
year_2019<-apma[apma$year==2,items_2019]
nrow(year_2019)  #576 respondents
items_2020 <- c("I1","I2","I6","I7","I9","I10","I11","I12","I13","I14","I15","I24","I25","I28","I29",
                "I32","I34","I35","I36","I40","I41","I43","I44","I45","I46","I47","I48","I49","I50",
                "I51","I52","I53","I55","I56","I62","I63","I64","I67","I68","I69","I70","I71","I73",
                "I74","I75","I76","I77","I78","I80","I81","I82","I84","I85","I86","I87","I88","I89",
                "I90","I91","I92","I93","I94")
length(items_2020)  #62 items
year_2020<-apma[apma$year==3,items_2020]
nrow(year_2020)  #215 respondents
items_2021 <- c("I1","I2","I5","I6","I7","I9","I12","I13","I24","I25","I28","I29","I32","I34","I35",
                "I36","I40","I41","I42","I43","I44","I45","I46","I47","I48","I49","I50","I51","I52",
                "I53","I54","I55","I56","I57","I58","I62","I63","I64","I66","I67","I68","I69","I70",
                "I71","I72","I73","I74","I75","I76","I77","I78","I79","I80","I81","I82","I84","I85",
                "I86","I87","I88","I89","I90","I94","I95")
length(items_2021) #64 items
year_2021<-apma[apma$year==4,items_2021]
nrow(year_2021)  #528 respondents
items_2022 <- c("I1","I2","I5","I6","I7","I9","I12","I13","I24","I25","I28","I29","I32","I34","I35",
                "I36","I40","I41","I42","I43","I44","I45","I46","I47","I48","I49","I50","I51","I52",
                "I53","I54","I55","I56","I57","I58","I62","I63","I64","I66","I67","I68","I69","I70",
                "I71","I72","I73","I74","I75","I76","I77","I78","I79") 
length(items_2022)  #52 items
year_2022<-apma[apma$year==5,items_2022]
nrow(year_2022)  #484 respondents
items_2023 <- c("I1","I2","I5","I6","I7","I9","I12","I13","I24","I25","I28","I29","I32","I34","I35",
                "I36","I40","I41","I42","I43","I44","I45","I46","I47","I48","I49","I50","I51","I52",
                "I53","I54","I55","I56","I57","I58","I62","I63","I64","I66","I67","I68","I69","I70",
                "I71","I72","I73","I74","I75","I76","I77","I78","I79")
length(items_2023)  #52 items
year_2023<-apma[apma$year==6,items_2023]
nrow(year_2023)  #613 respondents

items_2024 <- c("I1","I2","I5","I6","I7","I9","I12","I13","I24","I25","I28","I29","I32","I34","I35",
                "I36","I40","I41","I42","I43","I44","I45","I46","I47","I48","I49","I50","I51","I53",
                "I54","I55","I56","I57","I58","I62","I63","I64","I66","I67","I68","I69","I70","I71",
                "I72","I73","I74","I75","I76","I77","I78","I79","I80","I81","I82","I84","I85","I86",
                "I88","I89","I90")
length(items_2024)  #60 items
year_2024<-apma[apma$year==7,items_2024]
nrow(year_2024)  #682 respondents

# =========================================================
# 3. COMMON ITEMS & SAMPLE OVERLAP
# =========================================================
# Compute common item matrix and shared IDs
items_common <- Reduce(intersect, list(items_2017, items_2019, items_2020, 
                                      items_2021, items_2022, items_2023, items_2024))
length(items_common) #42 common items
year_common<-apma[,items_common]
nrow(year_common)  #3384 respondents

# n.of common item matrix
items_list <- list("2017"=items_2017,"2019"=items_2019,"2020"=items_2020,"2021"=items_2021,
                   "2022"=items_2022,"2023"=items_2023,"2024"=items_2024)
common_item_matrix <- matrix(NA, nrow=length(items_list), ncol=length(items_list),
                        dimnames=list(names(items_list), names(items_list)))
for(i in 1:length(items_list)){
  for(j in 1:length(items_list)){
    common_item_matrix[i,j] <- length(intersect(items_list[[i]], items_list[[j]]))
  }
}
write_clip(common_item_matrix)

# n.of common subject matrix
year_labels <- c("2017", "2019", "2020", "2021", "2022", "2023", "2024")

ids_list <- lapply(1:length(year_labels), function(y) {
  unique(apma$ID[apma$year == y])
})
names(ids_list) <- year_labels
common_id_matrix <- matrix(NA, nrow=length(ids_list), ncol=length(ids_list),
                           dimnames=list(year_labels, year_labels))
for(i in seq_along(ids_list)) {
  for(j in seq_along(ids_list)) {
    common_id_matrix[i,j] <- length(intersect(ids_list[[i]], ids_list[[j]]))
  }
}
write_clip(common_id_matrix)

# =========================================================
# 4. POLYCHORIC CORRELATIONS (multicollinearity)
# =========================================================
years <- c(2017, 2019, 2020, 2021, 2022, 2023, 2024)
data_list <- list(
  "2017" = year_2017,
  "2019" = year_2019,
  "2020" = year_2020,
  "2021" = year_2021,
  "2022" = year_2022,
  "2023" = year_2023,
  "2024" = year_2024
)
cor_matrices <- list()
high_corr_pairs <- list()
for (yr in years) {
  dat <- data_list[[as.character(yr)]]
  pc <- polychoric(dat)$rho
  cor_matrices[[as.character(yr)]] <- pc
  
  high_corr_index <- which(abs(pc) >= 0.9 & upper.tri(pc), arr.ind = TRUE)
  
  if (nrow(high_corr_index) > 0) {
    high_corr_df <- data.frame(
      var1 = rownames(pc)[high_corr_index[, 1]],
      var2 = colnames(pc)[high_corr_index[, 2]],
      correlation = pc[high_corr_index]
    )
    high_corr_pairs[[as.character(yr)]] <- high_corr_df
    cat(paste0("\n===", yr, "===\n"))
    print(high_corr_df)
  } else {
    cat(paste0("\n===", yr, "===\nNo correlations ≥ 0.9\n"))
    high_corr_pairs[[as.character(yr)]] <- NULL
  }
}
write_clip(cor_matrices[["2024"]])
write_clip(high_corr_pairs[["2024"]])

pc_common<-polychoric(year_common)$rho
high_cor_pairs_common <- which(abs(pc_common) >= 0.9 & upper.tri(pc_common), arr.ind = TRUE)
high_cor_pairs_named_common <- data.frame(
  var1 = rownames(pc_common)[high_cor_pairs_common[, 1]],
  var2 = colnames(pc_common)[high_cor_pairs_common[, 2]],
  correlation = pc_common[high_cor_pairs_common]
)
print(high_cor_pairs_named_common)
write_clip(pc_common) 

# =========================================================
# 5. ITEM REDUCTION
# =========================================================
remove_items <- c("I44", "I49", "I52", "I88")
items_2017_filtered <- setdiff(items_2017, remove_items) 
length(items_2017_filtered)  #77 items
year_2017_filtered<-apma[apma$year==1,items_2017_filtered]
nrow(year_2017_filtered) #286 respondents

items_2019_filtered <- setdiff(items_2019, remove_items) 
length(items_2019_filtered) #46 items
year_2019_filtered<-apma[apma$year==2,items_2019_filtered]
nrow(year_2019_filtered)  #576 respondents

items_2020_filtered <- setdiff(items_2020, remove_items) 
length(items_2020_filtered) #58 items
year_2020_filtered<-apma[apma$year==3,items_2020_filtered]
nrow(year_2020_filtered)  #215 respondents

items_2021_filtered <- setdiff(items_2021, remove_items) 
length(items_2021_filtered) #60 items
year_2021_filtered<-apma[apma$year==4,items_2021_filtered]
nrow(year_2021_filtered)  #528 respondents

items_2022_filtered <- setdiff(items_2022, remove_items) 
length(items_2022_filtered) #49 items
year_2022_filtered<-apma[apma$year==5,items_2022_filtered]
nrow(year_2022_filtered)  #484 respondents

items_2023_filtered <- setdiff(items_2023, remove_items) 
length(items_2023_filtered) #49 items
year_2023_filtered<-apma[apma$year==6,items_2023_filtered]
nrow(year_2023_filtered)  #613 respondents

items_2024_filtered <- setdiff(items_2024, remove_items)  
length(items_2024_filtered) #57 items
year_2024_filtered<-apma[apma$year==7,items_2024_filtered]
nrow(year_2024_filtered)  #682 respondents

items_common_filtered<- Reduce(intersect, list(items_2017_filtered, items_2019_filtered, 
                                               items_2020_filtered,items_2021_filtered, 
                                               items_2022_filtered, items_2023_filtered,
                                               items_2024_filtered))
length(items_common_filtered)  # 40 common items
year_common_filtered<-apma[,items_common_filtered]
nrow(year_common_filtered)  #3384 respondents

# n.of common item matrix
items_list_filtered <- list(
  "2017" = items_2017_filtered, "2019" = items_2019_filtered, "2020" = items_2020_filtered,
  "2021" = items_2021_filtered, "2022" = items_2022_filtered, "2023" = items_2023_filtered,
  "2024" = items_2024_filtered)

common_item_matrix_filtered <- matrix(NA, nrow=length(items_list_filtered), ncol=length(items_list_filtered),
                             dimnames=list(names(items_list_filtered), names(items_list_filtered)))
for(i in 1:length(items_list_filtered)){
  for(j in 1:length(items_list_filtered)){
    common_item_matrix_filtered[i,j] <- length(intersect(items_list_filtered[[i]], items_list_filtered[[j]]))
  }
}
write_clip(common_item_matrix_filtered)

# =========================================================
# 6. FACTOR NUMBER ESTIMATION
# =========================================================
# Parallel analysis across extraction methods
fm_val<-c("minres","uls","wls","pa")
year_list_filtered <- list(
  "2017" = year_2017_filtered, "2019" = year_2019_filtered, "2020" = year_2020_filtered,
  "2021" = year_2021_filtered, "2022" = year_2022_filtered, "2023" = year_2023_filtered,
  "2024" = year_2024_filtered)

recommended_number_fac <- data.frame()
for(year_name in names(year_list_filtered)){
  nb.fa_data <- year_list_filtered[[year_name]]
  cor_matrix <- polychoric(nb.fa_data)$rho
  n_obs <- nrow(nb.fa_data)
  
  for(fm in fm_val){
    cat("Running fa.parallel for year", year_name, "with fm =", fm, "\n")
    try({
      pa_result <- fa.parallel(cor_matrix, n.obs = n_obs, fm = fm, fa = "fa")
      recommended_nfac <- pa_result$nfa
      recommended_number_fac<- rbind(recommended_number_fac, data.frame(Year = year_name,
                                                   Method = fm,
                                                   Recommended_Factors = recommended_nfac))
    }, silent = TRUE)
  }
}

print(recommended_number_fac)
write_clip(recommended_number_fac)

# =========================================================
# 7. MULTI-MODEL Exploratory Factor Analysis (EFA, 264 Models)
# =========================================================
# Evaluate all combinations:
# 4 extraction methods × 6 rotations × 11 factor solutions
fm_val<-c("minres","uls","wls","pa")
rotate_val<-c("oblimin","promax", "geominQ","varimax", "equamax", "geominT" )

year_list_filtered <- list(
  "2017" = year_2017_filtered, "2019" = year_2019_filtered, "2020" = year_2020_filtered,
  "2021" = year_2021_filtered, "2022" = year_2022_filtered, "2023" = year_2023_filtered,
  "2024" = year_2024_filtered)

all_factor_model_results <- list()
all_final_choices <- list()
for(year_name in names(year_list_filtered)){
  efa_data <- year_list_filtered[[year_name]]
  cor_matrix <- polychoric(efa_data)$rho
  
  factor_model_results <- data.frame()
  for (fm in fm_val) {
    for (rot in rotate_val) {
      for (nfacs in 2:12){   
        cat("Year:", year_name, "| fm:", fm, "| rotate:", rot, "| nfactors:", nfacs, "\n")
        try({
          efa_result <- fa(cor_matrix, nfactors=nfacs, rotate=rot, fm=fm, max.iter = 2000)
          
          cum_var <- efa_result$Vaccounted[3,nfacs]
          rmsr_val <- efa_result$rms 
          
          factor_model_results <- rbind(factor_model_results,
                                        data.frame(Method=fm, Rotation=rot, NFac=nfacs,
                                                   Cumulative_Var=cum_var, RMSR=rmsr_val))
          
        }, silent=TRUE)
      }  
    }
  }
  
  all_factor_model_results[[year_name]] <- factor_model_results
  filtered <- subset(factor_model_results, RMSR <= 0.06 & Cumulative_Var >= 0.60)
  if (nrow(filtered) > 0) {
    lowest_nfac_rows <- subset(filtered, NFac == min(filtered$NFac))
    best_rmsr_rows <- subset(lowest_nfac_rows, RMSR == min(lowest_nfac_rows$RMSR))
    final_choice <- subset(best_rmsr_rows, Cumulative_Var == max(best_rmsr_rows$Cumulative_Var))
    all_final_choices[[year_name]] <- final_choice
  } else {
    cat("No model met the criteria for year", year_name, "\n")
    all_final_choices[[year_name]] <- NULL
  }
}


write_clip(all_factor_model_results[["2024"]])
write_clip(all_final_choices[["2024"]])
print(all_final_choices)

# =========================================================
# 7.1. VISUALIZATION (RMSR & CUMULATIVE VARIANCE)
# =========================================================
# Generate plots per year
year_list_filtered <- list(
  "2017" = year_2017_filtered, "2019" = year_2019_filtered, "2020" = year_2020_filtered,
  "2021" = year_2021_filtered, "2022" = year_2022_filtered, "2023" = year_2023_filtered,
  "2024" = year_2024_filtered)

custom_colors <- c(
  "minres" = "#E7298A",
  "pa"     = "#7570B3",
  "uls"    = "#FF6600",
  "wls"    = "#1B9E77"
)
rmsr_min <- 0.02
rmsr_max <- 0.08
cumvar_min <- 0.40
cumvar_max <- 0.80
scale_cumvar_to_rmsr <- function(x) {
  (x - cumvar_min) / (cumvar_max - cumvar_min) * (rmsr_max - rmsr_min) + rmsr_min
}
scale_rmsr_to_cumvar <- function(x) {
  (x - rmsr_min) / (rmsr_max - rmsr_min) * (cumvar_max - cumvar_min) + cumvar_min
}

for(year_name in names(year_list_filtered)){
  cat("Generating plot for year", year_name, "...\n")

factor_model_results <-all_factor_model_results[[year_name]] %>%
  mutate(Cumulative_Var_scaled = scale_cumvar_to_rmsr(Cumulative_Var))

first_nfac_intersection <- factor_model_results %>%
  filter(RMSR <= 0.06, Cumulative_Var >= 0.60) %>%
  group_by(Method, Rotation) %>%
  summarise(first_nfac_below = min(NFac), .groups = "drop")

grf=
ggplot(factor_model_results, aes(x = NFac)) +
  geom_line(aes(y = RMSR, color = Method), size = 0.5) +
  geom_point(aes(y = Cumulative_Var_scaled, shape = Method, color = Method), size = 1)+
  facet_wrap(~ Rotation, ncol = 3 ) +
  scale_x_continuous(breaks = 2:12, limits = c(2, 12)) +
  scale_y_continuous(
    name = "RMSR",
    limits = c(rmsr_min, rmsr_max),
    breaks = seq(rmsr_min, rmsr_max, by = 0.01),
    sec.axis = sec_axis(
      transform = ~ scale_rmsr_to_cumvar(.)*100,
      name = "Cumulative Variance (%)")
  )+ 
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = c("minres" = 15, "uls" = 16, "wls" = 17, "pa" = 18))+
  geom_vline(data = first_nfac_intersection, color = "red", 
             aes(xintercept = first_nfac_below, color = Method), 
             linetype = "dashed", size =0.4, show.legend = FALSE) +
  labs(
    title = paste("RMSR and Cumulative Variance by Method & Rotation (Year:", year_name, ")"),
    x = "Number of Factors",
    color = "Method",
    shape = "Method"
  ) +
  theme_minimal(base_size = 10, base_family = "serif") +
  theme(
    panel.spacing = unit(0.9, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "grey80", size= 0.5),
    panel.border = element_blank(),
    axis.text.y = element_text(size = 8,color = "black"),
    axis.text.x = element_text(size = 8,color = "black"),
    axis.title.y = element_text(margin = margin(r = 15), size = 10, color = "black", face="bold"),
    axis.title.y.right = element_text(margin = margin(l = 15), size = 10, color = "black", face = "bold"),
    axis.title.x = element_text(margin = margin(t = 15), size = 10, color = "black", face="bold"),
    legend.title = element_text(size = 10,color = "black",face = "bold"),
    legend.text = element_text(size =10,color = "black", face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold",color = "black",margin = margin(t = 15,b = 10)),
    strip.text = element_text(size = 10, face = "bold", color = "black",margin = margin(t = 10, b = 10)) 
  )

ggsave(
  filename = file.path(desktop_path, paste0(year_name, "_RMSR_CumVar_by_Factor_and_Rotation.jpeg")),
  plot = grf,
  width = 10, height = 6, units = "in",
  dpi = 300
)
}

# =========================================================
# 7.2. IMAGE MERGING
# =========================================================
# Combine yearly plots into composite figures
years <- c("2017", "2019", "2020", "2021", "2022", "2023", "2024")
file_names <- file.path(desktop_path, paste0(years, "_RMSR_CumVar_by_Factor_and_Rotation.jpeg"))
images <- lapply(file_names, image_read)

img_width <- image_info(images[[1]])$width
img_height <- image_info(images[[1]])$height

rows <- list()
for (i in seq(1, length(images), by = 2)) {
  if (i + 1 <= length(images)) {
    row_img <- image_append(c(images[[i]], images[[i + 1]]))
  } else {
    side_blank <- image_blank(width = img_width / 2, height = img_height, color = "white")
    row_img <- image_append(c(side_blank, images[[i]], side_blank))
  }
  rows[[length(rows) + 1]] <- row_img
}

final_image <- image_append(image_join(rows), stack = TRUE)
image_write(final_image, path = file.path(desktop_path, "RMSR_CumVar_merged_images.jpeg"), format = "jpeg")

# =========================================================
# 8. CO-OCCURRENCE MATRIX
# =========================================================
# Based on shared factor membership across models
fm_val<-c("minres","uls","wls","pa")
rotate_val<-c("oblimin","promax", "geominQ","varimax", "equamax", "geominT" )
year_list_filtered <- list(
  "2017" = year_2017_filtered, "2019" = year_2019_filtered, "2020" = year_2020_filtered,
  "2021" = year_2021_filtered, "2022" = year_2022_filtered, "2023" = year_2023_filtered,
  "2024" = year_2024_filtered)

same_factor_matrix_list <- list()

for(year_name in names(year_list_filtered)){
  nb.fa_data <- year_list_filtered[[year_name]]
  cor_matrix <- polychoric(nb.fa_data)$rho
  item_factor_assignments <- data.frame()
for (fm in fm_val) {
  for (rot in rotate_val) {
    for (nfacs in 2:12) {
      cat("Trying: fm =", fm, ", rotate =", rot, ", factor =", nfacs, "\n")
      try({
        efa_result <- fa(cor_matrix, nfactors = nfacs, rotate = rot, fm = fm, max.iter = 2000)
        loadings_df <- as.data.frame(unclass(efa_result$loadings)[, 1:nfacs])
        loadings_df$item <- rownames(loadings_df)
        loadings_df$assigned_factor <- apply(loadings_df[, 1:nfacs], 1, function(x) paste0("F", which.max(abs(x))))
        loadings_df$Method <- fm
        loadings_df$Rotation <- rot
        loadings_df$NFac <- nfacs
        assign_df <- loadings_df[, c("item", "assigned_factor", "Method", "Rotation", "NFac")]
        item_factor_assignments <- rbind(item_factor_assignments, assign_df)
        
      }, silent = TRUE)
    }
  }
}


items <- unique(item_factor_assignments$item)
same_factor_matrix <- matrix(0, nrow = length(items), ncol = length(items), dimnames = list(items, items))

long_data <- item_factor_assignments %>%
  mutate(simulation = paste(Method, Rotation, NFac, sep = "_")) %>%
  select(item, assigned_factor, simulation)
for (sim in unique(long_data$simulation)) {
  sim_data <- filter(long_data, simulation == sim)
  items_in_sim <- sim_data$item
  factors_in_sim <- sim_data$assigned_factor
  for (i in seq_along(items)) {
    for (j in seq_along(items)) {
      item1 <- items[i]
      item2 <- items[j]
      if (item1 == item2) {
        same_factor_matrix[item1, item2] <- same_factor_matrix[item1, item2] + 1
      } else {
        factor1 <- factors_in_sim[match(item1, items_in_sim)]
        factor2 <- factors_in_sim[match(item2, items_in_sim)]
        if (!is.na(factor1) && !is.na(factor2) && factor1 == factor2) {
          same_factor_matrix[item1, item2] <- same_factor_matrix[item1, item2] + 1
        }
      }
    }
  }
}
  same_factor_matrix_list[[year_name]] <- same_factor_matrix
}

write_clip(same_factor_matrix_list[["2024"]])

# =========================================================
# 8.1. MDS & COMMUNITY DETECTION
# =========================================================
# Louvain clustering + 2D MDS projection
threshold <- 200
group_results_list <- list()
for (year in names(same_factor_matrix_list)) {
  mat <- same_factor_matrix_list[[year]]
  
  adj_mat <- mat
  adj_mat[adj_mat < threshold] <- 0
  g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = TRUE, diag = FALSE)
  
  comm <- cluster_louvain(g)
  membership <- membership(comm)
  groups <- split(names(membership), membership)
  
  group_results_list[[year]] <- groups
  
  max_cooccur <- max(mat)
  dist_mat <- max_cooccur - mat
  dist_obj <- as.dist(dist_mat)
  mds_res <- cmdscale(dist_obj, k = 2)
  
  mds_df <- data.frame(
    Item = rownames(mds_res),
    Dim1 = mds_res[,1],
    Dim2 = mds_res[,2],
    Group = factor(membership[rownames(mds_res)]) 
  )

grf2 <- ggplot(mds_df, aes(x = Dim1, y = Dim2, color = Group, label = Item)) +
  geom_point(size = 1.5) +
  geom_text_repel(size = 2.5, max.overlaps = 20) +
  theme_minimal() +
  labs(
    title = paste("Multi-Dimensional Scaling plot (Year:", year, ")"),
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Community"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "grey", linewidth = 0.5),
    legend.position = "right",
    plot.title = element_text(margin = margin(t=10, b = 15),hjust = 0.5, size = 11, color = "black",face = "bold"),
    axis.text.y = element_text(size = 8,color = "black"),
    axis.text.x = element_text(size = 8,color = "black"),
    axis.title.y = element_text(margin = margin(r = 15), size = 10, color = "black", face="bold"),
    axis.title.x = element_text(margin = margin(t = 15), size = 10, color = "black", face="bold"),
    legend.text = element_text(size =8,color = "black"),
    legend.title = element_text(size = 10, color = "black",face = "bold")
    )

ggsave(
  filename = file.path(desktop_path, paste0(year,"_MDS_plot",".jpeg")),
  plot = grf2,
  width = 10, height = 6, units = "in",  dpi = 300
)
}

write_clip(group_results_list[["2024"]])

# =========================================================
# 8.2. MERGE MDS FIGURES
# =========================================================
years <- c("2017", "2019", "2020", "2021", "2022", "2023", "2024")
file_names <- file.path(desktop_path, paste0(years,"_MDS_plot",".jpeg"))
images <- lapply(file_names, image_read)

img_width <- image_info(images[[1]])$width
img_height <- image_info(images[[1]])$height

rows <- list()
for (i in seq(1, length(images), by = 2)) {
  if (i + 1 <= length(images)) {
    row_img <- image_append(c(images[[i]], images[[i + 1]]))
  } else {
    side_blank <- image_blank(width = img_width / 2, height = img_height, color = "white")
    row_img <- image_append(c(side_blank, images[[i]], side_blank))
  }
  rows[[length(rows) + 1]] <- row_img
}

final_image <- image_append(image_join(rows), stack = TRUE)
image_write(final_image, path = file.path(desktop_path, "MDS_merged_images.jpeg"), format = "jpeg")

# =========================================================
# 9. FINAL EFA MODEL (PER YEAR)
# =========================================================
# KMO, Bartlett, RMSR, loadings

efa_data <- year_2024_filtered

  cor_matrix <- polychoric(efa_data)$rho  
  kmo_result <- KMO(cor_matrix)
  write_clip(kmo_result$MSA)
  write_clip(cbind(kmo_result$MSAi))
  
  bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(efa_data))
  bartlett_df <- data.frame(
    Test = c("Chi-squared", "Degrees of freedom", "p-value"),
    Value = c(bartlett_result$chisq, bartlett_result$df, bartlett_result$p.value)
  )
  write_clip(bartlett_df)
  
  efa_model <- fa(cor_matrix, nfactors=7, rotate="oblimin", fm="uls", max.iter = 2000)
  
  rmsr_value <- round(efa_model$rms, 4)
  cum_variance <- round(efa_model$Vaccounted["Cumulative Var", ] * 100, 2)
  summary_df <- data.frame(
    Measure = c("RMSR", "Cumulative Variance (%)"),
    Value = c(rmsr_value, max(cum_variance))
  )
  write_clip(summary_df)
  
  loadings <- efa_model$loadings
  write_clip(loadings)
  
  loadings_filtered <- as.matrix(loadings)
  loadings_filtered[abs(loadings_filtered) < 0.3 | loadings_filtered < 0] <- NA
  write_clip(loadings_filtered)
  
  factor_labels <- paste0("F", 1:ncol(loadings))
  names(factor_labels) <- colnames(loadings)
  
  result_list <- apply(loadings, 1, function(x) {
    sorted_vals <- sort(x, decreasing = TRUE)
    first_factor_idx <- which(x == sorted_vals[1])[1]
    first_factor_label <- factor_labels[first_factor_idx]
    first_factor_value <- sorted_vals[1]
    second_factor_value <- NA
    second_factor_label <- ""
    for(i in 2:length(sorted_vals)) {
      if(!is.na(sorted_vals[i]) && sorted_vals[i] > 0.30) {
        idx <- which(x == sorted_vals[i])[1]
        if(idx != first_factor_idx) {
          second_factor_label <- factor_labels[idx]
          second_factor_value <- sorted_vals[i]
          break
        }
      }
    }
    second_factor_text <- if(!is.na(second_factor_value)) 
      paste0(second_factor_label, " (", round(second_factor_value, 3), ")") else ""
    c(
      First_Factor = paste0(first_factor_label, " (", round(first_factor_value, 3), ")"),
      Second_Factor = second_factor_text
    )
  })
  
  result_factor_load <- data.frame(
    Item = rownames(efa_model$loadings),
    First_Factor = result_list["First_Factor", ],
    Second_Factor = result_list["Second_Factor", ],
    stringsAsFactors = FALSE
  )
  
write_clip(result_factor_load)
  
# =========================================================
# 9.1. FACTOR LOADINGS SUMMARY
# =========================================================
# First and second loadings per item
df=readxl::read_excel(file.path(desktop_path, "factor_loadings.xlsx"))
years <- c("2017", "2019", "2020", "2021", "2022", "2023", "2024")
for (yr in years) {
  
  item_col <- yr
  first_col <- paste0(yr, "_First_Factor")
  second_col <- paste0(yr, "_Second_Factor")
  
  if (!all(c(item_col, first_col, second_col) %in% names(df))) {
    cat(paste0("\n", yr, " year columns missing, skipping.\n"))
    next
  }
  
  temp_df <- df %>%
    select(Item = all_of(item_col),
           First = all_of(first_col),
           Second = all_of(second_col)) %>%
    filter(!is.na(Item), !is.na(First))
  
  temp_df <- temp_df %>%
    mutate(Factor1 = str_extract(First, "F\\d+"),
           HasSecond = !is.na(Second) & Second != "")
  
  grouped <- temp_df %>%
    group_by(Factor1) %>%
    summarise(
      items = paste(Item, collapse = "|||"),
      bold_flags = paste(HasSecond, collapse = "|||"),
      .groups = "drop"
    )
  
  cat(paste0("\n", yr, " Year:\n"))
  for (i in 1:nrow(grouped)) {
    items <- unlist(strsplit(grouped$items[i], "\\|\\|\\|"))
    bold_flags <- unlist(strsplit(grouped$bold_flags[i], "\\|\\|\\|"))
    
    line <- ""
    for (j in seq_along(items)) {
      if (bold_flags[j] == "TRUE") {
        line <- paste0(line, bold(items[j]), " - ")
      } else {
        line <- paste0(line, items[j], " - ")
      }
    }
   
    line <- substr(line, 1, nchar(line) - 3)
    
    cat(grouped$Factor1[i], ": ", line, "\n")
  }
}
