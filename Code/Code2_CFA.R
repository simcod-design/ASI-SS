set.seed(13579)
# NOTE:
# Code 1 (Steps 0–3) must be completed before running this script.
# These steps include data import, item definitions, and overlap analyses.
# =========================================================
# 10. LOAD REQUIRED PACKAGES (CFA STAGE)
# =========================================================
library(lavaan)    # CFA
library(semPlot)   # SEM visualization

# =========================================================
# UTILITY FUNCTION
# =========================================================
try_fit <- function(model, data) {
  tryCatch(
    cfa(model, data = data, estimator = "WLSMV", ordered = colnames(data)),
    error = function(e) e
  )
}
# =========================================================
# 11. DATA IMPORT (FROM CODE 1)
# =========================================================
# Data structures (apma, items_*, year_*) are assumed to be 
# preloaded from Code 1 outputs.

# =========================================================
# 12. DEFINE ITEM SETS (BY YEAR)
# =========================================================
# Item sets are inherited from Code 1 and reused here.

# =========================================================
# 13. COMMON ITEMS & SAMPLE OVERLAP
# =========================================================
# Overlap structures (common items and respondents) are 
# reused from Code 1.

# =========================================================
# 14. ITEM REDUCTION - STAGE 1 (CFA – 75 ITEMS)
# =========================================================
#Remove items based on: multicollinearity, low co-occurence stability, inconsistent EFA loadings
remove_items1 <- c("I44", "I49", "I52", "I88",
                  "I17", "I18", "I20", "I21", "I22", "I23", "I26", "I27",
                   "I33", "I37", "I38", "I39", "I59", "I60", "I61", "I65")
items_2017_cfa1 <- setdiff(items_2017, remove_items1) 
length(items_2017_cfa1)  #61 items
year_2017_cfa1<-apma[apma$year==1,items_2017_cfa1]
nrow(year_2017_cfa1) #286 respondents

items_2019_cfa1 <- setdiff(items_2019, remove_items1) 
length(items_2019_cfa1) #46 items
year_2019_cfa1<-apma[apma$year==2,items_2019_cfa1]
nrow(year_2019_cfa1)  #576 respondents

items_2020_cfa1 <- setdiff(items_2020, remove_items1) 
length(items_2020_cfa1) #58 items
year_2020_cfa1<-apma[apma$year==3,items_2020_cfa1]
nrow(year_2020_cfa1)  #215 respondents

items_2021_cfa1 <- setdiff(items_2021, remove_items1) 
length(items_2021_cfa1) #60 items
year_2021_cfa1<-apma[apma$year==4,items_2021_cfa1]
nrow(year_2021_cfa1)  #528 respondents

items_2022_cfa1 <- setdiff(items_2022, remove_items1) 
length(items_2022_cfa1) #49 items
year_2022_cfa1<-apma[apma$year==5,items_2022_cfa1]
nrow(year_2022_cfa1)  #484 respondents

items_2023_cfa1 <- setdiff(items_2023, remove_items1) 
length(items_2023_cfa1) #49 items
year_2023_cfa1<-apma[apma$year==6,items_2023_cfa1]
nrow(year_2023_cfa1)  #613 respondents

items_2024_cfa1 <- setdiff(items_2024, remove_items1)  
length(items_2024_cfa1) #57 items
year_2024_cfa1<-apma[apma$year==7,items_2024_cfa1]
nrow(year_2024_cfa1)  #682 respondents

items_common_cfa1<- Reduce(intersect, list(items_2017_cfa1, items_2019_cfa1, 
                                               items_2020_cfa1,items_2021_cfa1, 
                                               items_2022_cfa1, items_2023_cfa1,
                                               items_2024_cfa1))
length(items_common_cfa1)  # 40 common items
year_common_cfa1<-apma[,items_common_cfa1]
nrow(year_common_cfa1)  #3384 respondents

# n.of common item matrix
items_list_cfa1 <- list(
  "2017" = items_2017_cfa1, "2019" = items_2019_cfa1, "2020" = items_2020_cfa1,
  "2021" = items_2021_cfa1, "2022" = items_2022_cfa1, "2023" = items_2023_cfa1,
  "2024" = items_2024_cfa1)

common_item_matrix_cfa1 <- matrix(NA, nrow=length(items_list_cfa1), ncol=length(items_list_cfa1),
                                      dimnames=list(names(items_list_cfa1), names(items_list_cfa1)))

for(i in 1:length(items_list_cfa1)){
  for(j in 1:length(items_list_cfa1)){
    common_item_matrix_cfa1[i,j] <- length(intersect(items_list_cfa1[[i]], items_list_cfa1[[j]]))
  }
}
write_clip(common_item_matrix_cfa1)

# =========================================================
# 15. CFA MODEL SPECIFICATION (Initial model)
# =========================================================
# Define first-order factors and second-order latent construct
# based on EFA + co-occurrence results
factor_list <- list(
  F1 = c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I10", "I11", "I12", "I13", "I14", "I15", "I19", "I34", "I35", "I36", "I40", "I41"),
  F2 = c("I9", "I16", "I32", "I45"),
  F3 = c("I24", "I25", "I28", "I29", "I30", "I31"),
  F4 = c("I43", "I46", "I47", "I48", "I50", "I51", "I53", "I54", "I55", "I56"),
  F5 = c("I42", "I69", "I70", "I71", "I72", "I73", "I74", "I75", "I76", "I77", "I78", "I79", "I80", "I81", "I82", "I83", "I84", "I85"),
  F6 = c("I57", "I58", "I62", "I63", "I64", "I66", "I67", "I68"),
  F7 = c("I86", "I87", "I89", "I90", "I91", "I92", "I93", "I94", "I95")
)

data1 <-  year_2017_cfa1
available_items <- colnames(data1)

cfa_factors <- lapply(factor_list, function(items) {
  intersect(items, available_items)
})
cfa_factors<-Filter(function(x) length(x) > 0, cfa_factors)

first_order_part <- paste0(
  names(cfa_factors), " =~ ",
  sapply(cfa_factors, function(items) paste(items, collapse = " + ")),
  collapse = "\n"
)
second_order_factors <- names(cfa_factors)
if (length(second_order_factors) > 1) {
  second_order_part <- paste0("G =~ ", paste(second_order_factors, collapse = " + "))
  model_cfa1 <- paste(first_order_part, second_order_part, sep = "\n")
} else {
  model_cfa1 <- first_order_part
}

# =========================================================
# 16. CFA MODEL ESTIMATION (single year validation)
# =========================================================
# Fit CFA model for a single survey year (validation step)
fit <- try_fit(model_cfa1, data1)
if (inherits(fit, "error")) {
  err_msg <- fit$message
  cat("Error message:\n", err_msg, "\n")
  item_names <- colnames(data1) 
  
  extra_rows <- data.frame(
    matrix(rep(0:2, each = length(item_names)), nrow = 3, byrow = TRUE)
  )
  colnames(extra_rows) <- item_names
  data1_extended <- rbind(data1, extra_rows)
  
  fit_model  <- cfa(model_cfa1, data = data1_extended, estimator = "WLSMV", ordered = colnames(data1_extended))
  
} else {
  fit_model <- fit
  data1_extended <- data1
}

summary(fit_model, fit.measures = TRUE, standardized = TRUE)
loadings <- parameterEstimates(fit_model, standardized = TRUE) %>%
  filter(op == "=~") %>%
  select(factor = lhs, item = rhs, loading = std.all)
write_clip(loadings)

fit_vals <- fitMeasures(fit_model, 
                        c("chisq", "df", "pvalue", 
                          "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", 
                          "srmr", "cfi", "tli", "gfi"))
write_clip(cbind(fit_vals))
chisq_df_ratio <- round(fit_vals["chisq"] / fit_vals["df"],2)

jpeg(filename = file.path(desktop_path, paste0(2017,"_cfa1_plot",".jpeg")),
     width =2500, height = 3000, res = 300)
                  
semPaths(fit_model, style="lisrel", weighted=FALSE,curveAdjacent = T,
         whatLabels = "std", edge.label.cex = .4,
         label.prop=0.6, edge.label.color = "black", rotation = 2, 
         equalizeManifests = FALSE, optimizeLatRes = T, node.width = 1.5,  
         edge.width =0.5, shapeMan = "rectangle", shapeLat = "ellipse", 
         sizeMan = 3, sizeMan2=1.5,  sizeLat = 4, residScale = 9,
         sizeLat2= 3, nDigits=3, nCharNodes = 0, nCharEdges = 4, 
         curve= 1.5,curvature = 1.75, unCol = "black",intercepts=F,residuals=F,
         thresholds=F,fixedStyle = F,mar = c(2, 8, 1, 8),layout="tree2")

fit_text <- paste0(
  "Chi-Square / df = ", round(chisq_df_ratio, 3), "\n",
  "RMSEA = ", round(fit_vals["rmsea"], 3),
  " [", round(fit_vals["rmsea.ci.lower"], 3), " - ", round(fit_vals["rmsea.ci.upper"], 3), "]",
  ", p = ", format.pval(fit_vals["rmsea.pvalue"], digits = 3), "\n",
  "SRMR = ", round(fit_vals["srmr"], 3),
  ", CFI = ", round(fit_vals["cfi"], 3),
  ", TLI = ", round(fit_vals["tli"], 3),
  ", GFI = ", round(fit_vals["gfi"], 3)
)
text(x = -0.8, y = -0.9, labels = fit_text, cex = 0.8, font = 6)
title(sub = "Second-order factor model of the CFA (year=2017)",
      line = 1.6, font.sub = 4, cex.sub = 0.9)
dev.off()
while(dev.cur() > 1) dev.off()

# =========================================================
# 17. ITEM REFINEMENT - STAGE 2 (CFA – 73 ITEMS)
# =========================================================
# Additional item removal based on:
# - low factor loadings in CFA
# - model refinement decisions
# A total of 22 items were removed at this stage
remove_items <- c("I44", "I49", "I52", "I88",
                  "I17", "I18", "I20", "I21", "I22", "I23", "I26", "I27",
                  "I33", "I37", "I38", "I39", "I59", "I60", "I61", "I65",
                  "I94","I95")

items_2017_cfa <- setdiff(items_2017, remove_items) 
length(items_2017_cfa)  #61 items
year_2017_cfa<-apma[apma$year==1,items_2017_cfa]
nrow(year_2017_cfa) #286 respondents

items_2019_cfa <- setdiff(items_2019, remove_items) 
length(items_2019_cfa) #46 items
year_2019_cfa<-apma[apma$year==2,items_2019_cfa]
nrow(year_2019_cfa)  #576 respondents

items_2020_cfa <- setdiff(items_2020, remove_items) 
length(items_2020_cfa) #57 items
year_2020_cfa<-apma[apma$year==3,items_2020_cfa]
nrow(year_2020_cfa)  #215 respondents

items_2021_cfa <- setdiff(items_2021, remove_items) 
length(items_2021_cfa) #58 items
year_2021_cfa<-apma[apma$year==4,items_2021_cfa]
nrow(year_2021_cfa)  #528 respondents

items_2022_cfa <- setdiff(items_2022, remove_items) 
length(items_2022_cfa) #49 items
year_2022_cfa<-apma[apma$year==5,items_2022_cfa]
nrow(year_2022_cfa)  #484 respondents

items_2023_cfa <- setdiff(items_2023, remove_items) 
length(items_2023_cfa) #49 items
year_2023_cfa<-apma[apma$year==6,items_2023_cfa]
nrow(year_2023_cfa)  #613 respondents

items_2024_cfa <- setdiff(items_2024, remove_items)  
length(items_2024_cfa) #57 items
year_2024_cfa<-apma[apma$year==7,items_2024_cfa]
nrow(year_2024_cfa)  #682 respondents

items_common_cfa<- Reduce(intersect, list(items_2017_cfa, items_2019_cfa, 
                                          items_2020_cfa,items_2021_cfa, 
                                          items_2022_cfa, items_2023_cfa,
                                          items_2024_cfa))
length(items_common_cfa)  # 40 common items
year_common_cfa<-apma[,items_common_cfa]
nrow(year_common_cfa)  #3384 respondents

# n.of common item matrix
items_list_cfa <- list(
  "2017" = items_2017_cfa, "2019" = items_2019_cfa, "2020" = items_2020_cfa,
  "2021" = items_2021_cfa, "2022" = items_2022_cfa, "2023" = items_2023_cfa,
  "2024" = items_2024_cfa)

common_item_matrix_cfa <- matrix(NA, nrow=length(items_list_cfa), ncol=length(items_list_cfa),
                                 dimnames=list(names(items_list_cfa), names(items_list_cfa)))
for(i in 1:length(items_list_cfa)){
  for(j in 1:length(items_list_cfa)){
    common_item_matrix_cfa[i,j] <- length(intersect(items_list_cfa[[i]], items_list_cfa[[j]]))
  }
}
write_clip(common_item_matrix_cfa)

# =========================================================
# 18. CFA MODEL SPECIFICATION (Refined model)
# =========================================================
# Define refined factor structure (73 items)
# after CFA-based item removal
factor_list <- list(
  F1 = c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I10", "I11", "I12", "I13", "I14", "I15", "I19", "I34", "I35", "I36", "I40", "I41"),
  F2 = c("I9", "I16", "I32", "I45"),
  F3 = c("I24", "I25", "I28", "I29", "I30", "I31"),
  F4 = c("I43", "I46", "I47", "I48", "I50", "I51", "I53", "I54", "I55", "I56"),
  F5 = c("I42", "I69", "I70", "I71", "I72", "I73", "I74", "I75", "I76", "I77", "I78", "I79", "I80", "I81", "I82", "I83", "I84", "I85"),
  F6 = c("I57", "I58", "I62", "I63", "I64", "I66", "I67", "I68"),
  F7 = c("I86", "I87", "I89", "I90", "I91", "I92", "I93")
)
data1 <-  year_2017_cfa
available_items <- colnames(data1)
cfa_1_factors <- lapply(factor_list, function(items) {
  intersect(items, available_items)
})
cfa_1_factors<-Filter(function(x) length(x) > 0, cfa_1_factors)

first_order_part <- paste0(
  names(cfa_1_factors), " =~ ",
  sapply(cfa_1_factors, function(items) paste(items, collapse = " + ")),
  collapse = "\n"
)
second_order_factors <- names(cfa_1_factors)
if (length(second_order_factors) > 1) {
  second_order_part <- paste0("G =~ ", paste(second_order_factors, collapse = " + "))
  model_cfa <- paste(first_order_part, second_order_part, sep = "\n")
} else {
  model_cfa <- first_order_part
}

# =========================================================
# 19. CFA MODEL ESTIMATION (single year validation)
# =========================================================
# Fit CFA model for a single survey year (validation step)
fit <- try_fit(model_cfa, data1)
if (inherits(fit, "error")) {
  err_msg <- fit$message
  cat("Error message:\n", err_msg, "\n")
  item_names <- colnames(data1) 
  
  extra_rows <- data.frame(
    matrix(rep(0:2, each = length(item_names)), nrow = 3, byrow = TRUE)
  )
  colnames(extra_rows) <- item_names
  data1_extended <- rbind(data1, extra_rows)
  
  fit_model  <- cfa(model_cfa, data = data1_extended, estimator = "WLSMV", ordered = colnames(data1_extended))
  
} else {
  fit_model <- fit
  data1_extended <- data1
}

summary(fit_model, fit.measures = TRUE, standardized = TRUE)
loadings <- parameterEstimates(fit_model, standardized = TRUE) %>%
  filter(op == "=~") %>%
  select(factor = lhs, item = rhs, loading = std.all)
write_clip(loadings)

fit_vals <- fitMeasures(fit_model, 
                        c("chisq", "df", "pvalue", 
                          "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", 
                          "srmr", "cfi", "tli", "gfi"))
write_clip(cbind(fit_vals))
chisq_df_ratio <- round(fit_vals["chisq"] / fit_vals["df"],2)

jpeg(filename = file.path(desktop_path, paste0(2017,"_cfa_1_plot",".jpeg")),
     width =2500, height = 3000, res = 300)

semPaths(fit_model, style="lisrel", weighted=FALSE,curveAdjacent = T,
         whatLabels = "std", edge.label.cex = .4,
         label.prop=0.6, edge.label.color = "black", rotation = 2, 
         equalizeManifests = FALSE, optimizeLatRes = T, node.width = 1.5,  
         edge.width =0.5, shapeMan = "rectangle", shapeLat = "ellipse", 
         sizeMan = 3, sizeMan2=1.5,  sizeLat = 4, residScale = 9,
         sizeLat2= 3, nDigits=3, nCharNodes = 0, nCharEdges = 4, 
         curve= 1.5,curvature = 1.75, unCol = "black",intercepts=F,residuals=F,
         thresholds=F,fixedStyle = F,mar = c(2, 8, 1, 8),layout="tree2")

fit_text <- paste0(
  "Chi-Square / df = ", round(chisq_df_ratio, 3), "\n",
  "RMSEA = ", round(fit_vals["rmsea"], 3),
  " [", round(fit_vals["rmsea.ci.lower"], 3), " - ", round(fit_vals["rmsea.ci.upper"], 3), "]",
  ", p = ", format.pval(fit_vals["rmsea.pvalue"], digits = 3), "\n",
  "SRMR = ", round(fit_vals["srmr"], 3),
  ", CFI = ", round(fit_vals["cfi"], 3),
  ", TLI = ", round(fit_vals["tli"], 3),
  ", GFI = ", round(fit_vals["gfi"], 3)
)

text(x = -0.8, y = -0.9, labels = fit_text, cex = 0.8, font = 6)
title(sub = "Second-order factor model of the CFA (year=2017)",
      line = 1.6, font.sub = 4, cex.sub = 0.9)
dev.off()

while(dev.cur() > 1) dev.off()

# =========================================================
# 20. CFA ESTIMATION ACROSS YEARS
# =========================================================
year_list_cfa <- list(
  "2017" = year_2017_cfa, "2019" = year_2019_cfa, "2020" = year_2020_cfa,
  "2021" = year_2021_cfa, "2022" = year_2022_cfa, "2023" = year_2023_cfa,
  "2024" = year_2024_cfa)

factor_list <- list(
  F1 = c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I10", "I11", "I12", "I13", "I14", "I15", "I19", "I34", "I35", "I36", "I40", "I41"),
  F2 = c("I9", "I16", "I32", "I45"),
  F3 = c("I24", "I25", "I28", "I29", "I30", "I31"),
  F4 = c("I43", "I46", "I47", "I48", "I50", "I51", "I53", "I54", "I55", "I56"),
  F5 = c("I42", "I69", "I70", "I71", "I72", "I73", "I74", "I75", "I76", "I77", "I78", "I79", "I80", "I81", "I82", "I83", "I84", "I85"),
  F6 = c("I57", "I58", "I62", "I63", "I64", "I66", "I67", "I68"),
  F7 = c("I86", "I87", "I89", "I90", "I91", "I92", "I93")
)
results <- list()
for (yr in names(year_list_cfa)) {
  data1 <- year_list_cfa[[yr]]
  available_items <- colnames(data1)
  
  cfa_factors <- lapply(factor_list, function(items) {
    intersect(items, available_items)
  })
  cfa_factors <- Filter(function(x) length(x) > 0, cfa_factors)
  
  first_order_part <- paste0(
    names(cfa_factors), " =~ ",
    sapply(cfa_factors, function(items) paste(items, collapse = " + ")),
    collapse = "\n"
  )
  second_order_factors <- names(cfa_factors)
  
  if (length(second_order_factors) > 1) {
    second_order_part <- paste0("G =~ ", paste(second_order_factors, collapse = " + "))
    model_cfa <- paste(first_order_part, second_order_part, sep = "\n")
  } else {
    model_cfa <- first_order_part
  }
  
  try_fit <- function(model, data) {
    tryCatch(
      cfa(model, data = data, estimator = "WLSMV", ordered = colnames(data)),
      error = function(e) e
    )
  }
  
  fit <- try_fit(model_cfa, data1)
  if (inherits(fit, "error")) {
    err_msg <- fit$message
    cat("Hata mesajD1:\n", err_msg, "\n")
    item_names <- colnames(data1) 
    
    extra_rows <- data.frame(
      matrix(rep(0:2, each = length(item_names)), nrow = 3, byrow = TRUE)
    )
    colnames(extra_rows) <- item_names
    data1_extended <- rbind(data1, extra_rows)
    
    fit_model  <- cfa(model_cfa, data = data1_extended, estimator = "WLSMV", ordered = colnames(data1_extended))
    
  } else {
    fit_model <- fit
    data1_extended <- data1
  }
  
  loadings <- parameterEstimates(fit_model, standardized = TRUE) %>%
    filter(op == "=~") %>%
    select(factor = lhs, item = rhs, loading = std.all)
  
  fit_vals <- fitMeasures(fit_model, 
                          c("chisq", "df", "pvalue", 
                            "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", 
                            "srmr", "cfi", "tli", "gfi"))
  chisq_df_ratio <- round(fit_vals["chisq"] / fit_vals["df"], 2)
  
 jpeg(filename = file.path(desktop_path, paste0(yr, "_CFA_plot.jpeg")),
       width = 2500, height = 3000, res = 300)
  
  semPaths(fit_model, style = "lisrel", weighted = FALSE, curveAdjacent = TRUE,
           whatLabels = "std", edge.label.cex = 0.4, label.prop = 0.6, edge.label.color = "black",
           rotation = 2, equalizeManifests = FALSE, optimizeLatRes = TRUE, node.width = 1.5,
           edge.width = 0.5, shapeMan = "rectangle", shapeLat = "ellipse", 
           sizeMan = 3, sizeMan2 = 1.5, sizeLat = 4, residScale = 9, sizeLat2 = 3,
           nDigits = 3, nCharNodes = 0, nCharEdges = 4, curve = 1.5, curvature = 1.75,
           unCol = "black", intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
           fixedStyle = FALSE, mar = c(2, 8, 1, 8), layout = "tree2")
  
  fit_text <- paste0(
    "Chi-Square / df = ", round(chisq_df_ratio, 3), "\n",
    "RMSEA = ", round(fit_vals["rmsea"], 3),
    " [", round(fit_vals["rmsea.ci.lower"], 3), " - ", round(fit_vals["rmsea.ci.upper"], 3), "]",
    ", p = ", format.pval(fit_vals["rmsea.pvalue"], digits = 3), "\n",
    "SRMR = ", round(fit_vals["srmr"], 3),
    ", CFI = ", round(fit_vals["cfi"], 3),
    ", TLI = ", round(fit_vals["tli"], 3),
    ", GFI = ", round(fit_vals["gfi"], 3)
  )
  
  text(x = -0.8, y = -0.9, labels = fit_text, cex = 0.8, font = 6)
  title(sub = paste("Second-order factor model of the CFA (year =", yr, ")"),
        line = 1.6, font.sub = 4, cex.sub = 0.9)
  dev.off()
  
 results[[yr]] <- list(
    fit_model = fit_model,
    loadings = loadings,
    fit_vals = fit_vals,
    chisq_df_ratio = chisq_df_ratio
  )
}
results[["2021"]]$fit_model    
results[["2017"]]$loadings     
results[["2017"]]$fit_vals     
   
write_clip(results[["2021"]]$loadings)   
write_clip(cbind(results[["2021"]]$fit_vals))
while(dev.cur() > 1) dev.off()

# =========================================================
# 21. VISUALIZATION – MERGED CFA DIAGRAMS
# =========================================================
# Combine CFA diagrams across years into a single composite image
years <- c("2017", "2019", "2020", "2021", "2022", "2023", "2024")
file_names <- file.path(desktop_path, paste0(years,"_CFA_plot",".jpeg"))
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
image_write(final_image, path = file.path(desktop_path, "CFA_merged_images.jpeg"), format = "jpeg")
