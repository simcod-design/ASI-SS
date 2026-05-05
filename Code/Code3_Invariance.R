set.seed(135799)
# Requires:
# Code 1 (data + overlap)
# Code 2 (final 73-item CFA structure)

# =========================================================
# 22. LOAD REQUIRED PACKAGES (INVARIANCE STAGE)
# =========================================================
library(lavaan) # CFA / invariance
library(semPlot) #semplot

# =========================================================
# 23. DATA IMPORT (FROM CODE 1)
# =========================================================
# Data structures (apma, items_*, year_*) are assumed to be 
# preloaded from Code 1 outputs.

# =========================================================
# 24. DEFINE ITEM SETS (BY YEAR)
# =========================================================
# Item sets are inherited from Code 1 and reused here.

# =========================================================
# 25. COMMON ITEMS & SAMPLE OVERLAP
# =========================================================
# Overlap structures (common items and respondents) are 
# reused from Code 1.

# =========================================================
# 26. ITEM REFINEMENT - STAGE 2 (CFA – 73 ITEMS)
# =========================================================
# Use the finalized 73-item measurement structure derived from Code 2

# =========================================================
# 27. DEFINE YEAR LISTS
# =========================================================
year_list_cfa <- list(
  "2017" = year_2017_cfa, "2019" = year_2019_cfa, "2020" = year_2020_cfa,
  "2021" = year_2021_cfa, "2022" = year_2022_cfa, "2023" = year_2023_cfa,
  "2024" = year_2024_cfa)
year_list_cfa_id <- list(
  "2017" = apma[apma$year==1,"ID"], "2019" = apma[apma$year==2,"ID"], "2020" = apma[apma$year==3,"ID"],
  "2021" = apma[apma$year==4,"ID"], "2022" = apma[apma$year==5,"ID"], "2023" = apma[apma$year==6,"ID"],
  "2024" = apma[apma$year==7,"ID"])

# =========================================================
# 28. DEFINE FACTOR STRUCTURE
# =========================================================

factor_list <- list(
  F1 = c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I10", "I11", "I12", "I13", "I14", "I15", "I19", "I34", "I35", "I36", "I40", "I41"),
  F2 = c("I9", "I16", "I32", "I45"),
  F3 = c("I24", "I25", "I28", "I29", "I30", "I31"),
  F4 = c("I43", "I46", "I47", "I48", "I50", "I51", "I53", "I54", "I55", "I56"),
  F5 = c("I42", "I69", "I70", "I71", "I72", "I73", "I74", "I75", "I76", "I77", "I78", "I79", "I80", "I81", "I82", "I83", "I84", "I85"),
  F6 = c("I57", "I58", "I62", "I63", "I64", "I66", "I67", "I68"),
  F7 = c("I86", "I87", "I89", "I90", "I91", "I92", "I93")
)

# =========================================================
# 29. REPEATED CROSS-SECTIONAL INVARIANCE (COMMON ITEMS)
# =========================================================
# Independent samples across years (no shared individuals)
results_table1 <- data.frame(
  Year_Pair = character(),
  Common_Items = character(),
  Model_Dynamic = character(),
  n_year1 = numeric(),
  n_year2 = numeric(),
  n_common_items = numeric(),
  stringsAsFactors = FALSE
)

results_table2 <- data.frame(
  Year_Pair = character(),
  Model = character(),
  chisq = numeric(),
  df = numeric(),
  pvalue = numeric(),
  CFI = numeric(),
  TLI = numeric(),
  RMSEA = numeric(),
  RMSEA_Lower = numeric(),
  RMSEA_Upper = numeric(),
  SRMR = numeric(),
  Delta_CFI = numeric(),
  Delta_RMSEA = numeric(),
  stringsAsFactors = FALSE
)

years <- names(year_list_cfa)
for (i in 1:(length(years)-1)) {
  
  year1 <- years[i]
  year2 <- years[i+1]
  
  data1 <- cbind(group = "Year1",ID= year_list_cfa_id[[year1]], year_list_cfa[[year1]])
  data2 <- cbind(group = "Year2", ID= year_list_cfa_id[[year2]], year_list_cfa[[year2]])
  
  common_items <- intersect(colnames(year_list_cfa[[year1]]), colnames(year_list_cfa[[year2]]))
 
  data1_c <- data1[, c("group", "ID", common_items), drop = FALSE]
  data2_c <- data2[, c("group", "ID", common_items), drop = FALSE]
  
  common_ids <- intersect(data1_c$ID, data2_c$ID)
  n1_only <- nrow(data1_c) - length(common_ids)
  n2_only <- nrow(data2_c) - length(common_ids)
  w1 <- (1/n1_only) / ((1/n1_only) + (1/n2_only))
  w2 <- 1 - w1
  n1_keep <- round(length(common_ids) * w1)
  n2_keep <- length(common_ids) - n1_keep
  
  set.seed(123)
  ids1 <- sample(common_ids, n1_keep)
  ids2 <- setdiff(common_ids, ids1)
  
  data1_new <- data1_c %>%
    filter(!(ID %in% common_ids)) %>%
    bind_rows(data1_c %>% filter(ID %in% ids1))
  
  data2_new <- data2_c %>%
    filter(!(ID %in% common_ids)) %>%
    bind_rows(data2_c %>% filter(ID %in% ids2))
  
  data_combined <- bind_rows(data1_new, data2_new) %>%
    distinct(ID, .keep_all = TRUE)
  
 # data_combined$group / year2 - year1
  n_year1 <- sum(data_combined$group == "Year1")
  n_year2 <- sum(data_combined$group == "Year2")
  
  cfa_factors <- lapply(factor_list, function(items) {
    intersect(items, common_items)
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
    model_dynamic <- paste(first_order_part, second_order_part, sep = "\n")
  } else {
    model_dynamic <- first_order_part
  }
  
  model_dynamic_single_line <- gsub("\n", "; ", model_dynamic)
  
  try_fit <- function(model, data, group=NULL) {
    tryCatch(
      cfa(model, data = data, estimator = "WLSMV", ordered = colnames(data),
          group= group),
      error = function(e) e
    )
  }
  fit <- try_fit(model_dynamic,data_combined[,-2],group="group")
  
  if (inherits(fit, "error")) {
    err_msg <- fit$message
    cat("Hata mesajı:\n", err_msg, "\n")
    matches <- str_match(err_msg, "group (\\d+)")
    
    if(!is.na(matches[1,2])){
      grp <- as.numeric(matches[1,2])
      item_names <- setdiff(colnames(data_combined[,-2]), "group")
      
      extra_rows <- data.frame(
        matrix(rep(0:2, each = length(item_names)), nrow = 3, byrow = TRUE)
      )
      colnames(extra_rows) <- item_names
      extra_rows$group <- ifelse(grp == 1, "Year1", "Year2")
      extra_rows <- extra_rows[, colnames(data_combined[,-2])]
      
      data_model <- bind_rows(data_combined[,-2], extra_rows)
      
    } else {
      stop("Hata mesajından grup bulunamadı")
    }
    
  } else {
    data_model <- data_combined[,-2]
  }
    
  # Configural model
  fit_config <- cfa(model_dynamic, data=data_model, group= "group",
                    estimator="WLSMV", ordered=common_items)
  # Metric model
  fit_metric <- cfa(model_dynamic, data=data_model , group="group",
                    group.equal=c("loadings"),
                    estimator="WLSMV", ordered=common_items)
  # Scalar model
  fit_scalar <- cfa(model_dynamic, data=data_model , group="group",
                    group.equal=c("loadings","intercepts"),
                    estimator="WLSMV", ordered=common_items)
  
  fit_measures <- function(fit) {
    c(fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")))
  }
  
  fit_config_measures <- fit_measures(fit_config)
  fit_metric_measures <- fit_measures(fit_metric)
  fit_scalar_measures <- fit_measures(fit_scalar)
  
  delta_metric <- c(
    dCFI = fit_metric_measures["cfi"] - fit_config_measures["cfi"],
    dRMSEA = fit_metric_measures["rmsea"] - fit_config_measures["rmsea"]
  )
  
  delta_scalar <- c(
    dCFI = fit_scalar_measures["cfi"] - fit_metric_measures["cfi"],
    dRMSEA = fit_scalar_measures["rmsea"] - fit_metric_measures["rmsea"]
  )
  
  results_table1 <- results_table1 %>%
    bind_rows(
      data.frame(
        Year_Pair = paste0(year1,"_",year2),
        Common_Items = paste(common_items, collapse=", "),
        Model_Dynamic = model_dynamic_single_line,
        n_year1 = sum(data_combined$group == "Year1"),
        n_year2 = sum(data_combined$group == "Year2"),
        n_common_items = length(common_items),
        stringsAsFactors = FALSE
      )
    )
    
  results_table2 <- results_table2 %>%
    bind_rows(
      data.frame(
        Year_Pair = paste0(year1,"_",year2),
        Model = "Configural",
        chisq = as.numeric(fit_config_measures["chisq"]),
        df = as.numeric(fit_config_measures["df"]),
        pvalue = as.numeric(fit_config_measures["pvalue"]),
        CFI = as.numeric(fit_config_measures["cfi"]),
        TLI = as.numeric(fit_config_measures["tli"]),
        RMSEA = as.numeric(fit_config_measures["rmsea"]),
        RMSEA_Lower = as.numeric(fit_config_measures["rmsea.ci.lower"]),
        RMSEA_Upper = as.numeric(fit_config_measures["rmsea.ci.upper"]),
        SRMR = as.numeric(fit_config_measures["srmr"]),
        Delta_CFI = NA_real_,
        Delta_RMSEA = NA_real_,
        stringsAsFactors = FALSE
      ),
      data.frame(
        Year_Pair = paste0(year1,"_",year2),
        Model = "Metric",
        chisq = as.numeric(fit_metric_measures["chisq"]),
        df = as.numeric(fit_metric_measures["df"]),
        pvalue = as.numeric(fit_metric_measures["pvalue"]),
        CFI = as.numeric(fit_metric_measures["cfi"]),
        TLI = as.numeric(fit_metric_measures["tli"]),
        RMSEA = as.numeric(fit_metric_measures["rmsea"]),
        RMSEA_Lower = as.numeric(fit_metric_measures["rmsea.ci.lower"]),
        RMSEA_Upper = as.numeric(fit_metric_measures["rmsea.ci.upper"]),
        SRMR = as.numeric(fit_metric_measures["srmr"]),
        Delta_CFI = as.numeric(delta_metric["dCFI.cfi"]),
        Delta_RMSEA = as.numeric(delta_metric["dRMSEA.rmsea"]),
        stringsAsFactors = FALSE
      ),
      data.frame(
        Year_Pair = paste0(year1,"_",year2),
        Model = "Scalar",
        chisq = as.numeric(fit_scalar_measures["chisq"]),
        df = as.numeric(fit_scalar_measures["df"]),
        pvalue = as.numeric(fit_scalar_measures["pvalue"]),
        CFI = as.numeric(fit_scalar_measures["cfi"]),
        TLI = as.numeric(fit_scalar_measures["tli"]),
        RMSEA = as.numeric(fit_scalar_measures["rmsea"]),
        RMSEA_Lower = as.numeric(fit_scalar_measures["rmsea.ci.lower"]),
        RMSEA_Upper = as.numeric(fit_scalar_measures["rmsea.ci.upper"]),
        SRMR = as.numeric(fit_scalar_measures["srmr"]),
        Delta_CFI = as.numeric(delta_scalar["dCFI.cfi"]),
        Delta_RMSEA = as.numeric(delta_scalar["dRMSEA.rmsea"]),
        stringsAsFactors = FALSE
      )
    )
}

write_clip(results_table1)
write_clip(results_table2)

# =========================================================
# 30. PANEL-BASED LONGITUDINAL INVARIANCE (COMMON IDs)
# =========================================================
# Same individuals observed across consecutive years
results_table3 <- data.frame(
  Year_Pair = character(),
  Common_Items = character(),
  Model_Dynamic = character(),
  n_year1 = numeric(),
  n_year2 = numeric(),
  n_common_items = numeric(),
  stringsAsFactors = FALSE
)

results_table4 <- data.frame(
  Year_Pair = character(),
  Model = character(),
  chisq = numeric(),
  df = numeric(),
  pvalue = numeric(),
  CFI = numeric(),
  TLI = numeric(),
  RMSEA = numeric(),
  RMSEA_Lower = numeric(),
  RMSEA_Upper = numeric(),
  SRMR = numeric(),
  Delta_CFI = numeric(),
  Delta_RMSEA = numeric(),
  stringsAsFactors = FALSE
)

years <- names(year_list_cfa)
for (i in 1:(length(years)-1)) {
    year1 <- years[i]
    year2 <- years[i+1]
    data1 <- cbind(group = "Year1",ID= year_list_cfa_id[[year1]], year_list_cfa[[year1]])
    data2 <- cbind(group = "Year2", ID= year_list_cfa_id[[year2]], year_list_cfa[[year2]])
    common_ids <- intersect(data1$ID, data2$ID)
    data1_ci <- data1[data1$ID %in% common_ids, ]
    data2_ci <- data2[data2$ID %in% common_ids, ]
       
    common_items <- intersect(colnames(data1_ci)[-c(1,2)], colnames(data2_ci)[-c(1,2)])
    data1_c <- data1_ci[,  c("group","ID",common_items)]
    data2_c <- data2_ci[,  c("group","ID",common_items)]
      
    data_combined <- bind_rows(data1_c, data2_c) 
    n_year1 <- sum(data_combined$group == "Year1")
    n_year2 <- sum(data_combined$group == "Year2")
    
    cfa_factors <- lapply(factor_list, function(items) {
      intersect(items, common_items)
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
      model_dynamic <- paste(first_order_part, second_order_part, sep = "\n")
    } else {
      model_dynamic <- first_order_part
    }
    
    model_dynamic_single_line <- gsub("\n", "; ", model_dynamic)
        
    try_fit <- function(model, data, group=NULL) {
      tryCatch(
        cfa(model, data = data, estimator = "WLSMV", ordered = colnames(data),
            group= group),
        error = function(e) e
      )
    }
    fit <- try_fit(model_dynamic,data_combined[,-2],group="group")
        
    if (inherits(fit, "error")) {
      err_msg <- fit$message
      cat("Hata mesajı:\n", err_msg, "\n")
      
      item_names <- setdiff(colnames(data_combined[,-2]), "group")
      
      extra_rows1 <- data.frame(matrix(rep(0:2, each = length(item_names)), nrow = 3, byrow = TRUE))
      colnames(extra_rows1) <- item_names
      extra_rows1$group <- "Year1"
      
      extra_rows2 <- data.frame(matrix(rep(0:2, each = length(item_names)), nrow = 3, byrow = TRUE))
      colnames(extra_rows2) <- item_names
      extra_rows2$group <- "Year2"
      
      data_model <- bind_rows(data_combined[,-2], extra_rows1, extra_rows2)
      
      } else  {
      data_model <- data_combined[,-2]
    }
        
    # Configural model
    fit_config <- cfa(model_dynamic, data=data_model, group= "group",
                      estimator="WLSMV", ordered=common_items)
    # Metric model
    fit_metric <- cfa(model_dynamic, data=data_model , group="group",
                      group.equal=c("loadings"),
                      estimator="WLSMV", ordered=common_items)
    # Scalar model
    fit_scalar <- cfa(model_dynamic, data=data_model , group="group",
                      group.equal=c("loadings","intercepts"),
                      estimator="WLSMV", ordered=common_items)
    
    fit_measures <- function(fit) {
      c(fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")))
    }
    fit_config_measures <- fit_measures(fit_config)
    fit_metric_measures <- fit_measures(fit_metric)
    fit_scalar_measures <- fit_measures(fit_scalar)
    
    delta_metric <- c(
      dCFI = fit_metric_measures["cfi"] - fit_config_measures["cfi"],
      dRMSEA = fit_metric_measures["rmsea"] - fit_config_measures["rmsea"]
    )
    
    delta_scalar <- c(
      dCFI = fit_scalar_measures["cfi"] - fit_metric_measures["cfi"],
      dRMSEA = fit_scalar_measures["rmsea"] - fit_metric_measures["rmsea"]
    )
    
    results_table3 <- results_table3 %>%
      bind_rows(
        data.frame(
          Year_Pair = paste0(year1,"_",year2),
          Common_Items = paste(common_items, collapse=", "),
          Model_Dynamic = model_dynamic_single_line,
          n_year1 = sum(data_combined$group == "Year1"),
          n_year2 = sum(data_combined$group == "Year2"),
          n_common_items = length(common_items),
          stringsAsFactors = FALSE
        )
      )
       
    results_table4 <- results_table4 %>%
      bind_rows(
        data.frame(
          Year_Pair = paste0(year1,"_",year2),
          Model = "Configural",
          chisq = as.numeric(fit_config_measures["chisq"]),
          df = as.numeric(fit_config_measures["df"]),
          pvalue = as.numeric(fit_config_measures["pvalue"]),
          CFI = as.numeric(fit_config_measures["cfi"]),
          TLI = as.numeric(fit_config_measures["tli"]),
          RMSEA = as.numeric(fit_config_measures["rmsea"]),
          RMSEA_Lower = as.numeric(fit_config_measures["rmsea.ci.lower"]),
          RMSEA_Upper = as.numeric(fit_config_measures["rmsea.ci.upper"]),
          SRMR = as.numeric(fit_config_measures["srmr"]),
          Delta_CFI = NA_real_,
          Delta_RMSEA = NA_real_,
          stringsAsFactors = FALSE
        ),
        data.frame(
          Year_Pair = paste0(year1,"_",year2),
          Model = "Metric",
          chisq = as.numeric(fit_metric_measures["chisq"]),
          df = as.numeric(fit_metric_measures["df"]),
          pvalue = as.numeric(fit_metric_measures["pvalue"]),
          CFI = as.numeric(fit_metric_measures["cfi"]),
          TLI = as.numeric(fit_metric_measures["tli"]),
          RMSEA = as.numeric(fit_metric_measures["rmsea"]),
          RMSEA_Lower = as.numeric(fit_metric_measures["rmsea.ci.lower"]),
          RMSEA_Upper = as.numeric(fit_metric_measures["rmsea.ci.upper"]),
          SRMR = as.numeric(fit_metric_measures["srmr"]),
          Delta_CFI = as.numeric(delta_metric["dCFI.cfi"]),
          Delta_RMSEA = as.numeric(delta_metric["dRMSEA.rmsea"]),
          stringsAsFactors = FALSE
        ),
        data.frame(
          Year_Pair = paste0(year1,"_",year2),
          Model = "Scalar",
          chisq = as.numeric(fit_scalar_measures["chisq"]),
          df = as.numeric(fit_scalar_measures["df"]),
          pvalue = as.numeric(fit_scalar_measures["pvalue"]),
          CFI = as.numeric(fit_scalar_measures["cfi"]),
          TLI = as.numeric(fit_scalar_measures["tli"]),
          RMSEA = as.numeric(fit_scalar_measures["rmsea"]),
          RMSEA_Lower = as.numeric(fit_scalar_measures["rmsea.ci.lower"]),
          RMSEA_Upper = as.numeric(fit_scalar_measures["rmsea.ci.upper"]),
          SRMR = as.numeric(fit_scalar_measures["srmr"]),
          Delta_CFI = as.numeric(delta_scalar["dCFI.cfi"]),
          Delta_RMSEA = as.numeric(delta_scalar["dRMSEA.rmsea"]),
          stringsAsFactors = FALSE
        )
      )
    
  }

write_clip(results_table3)
write_clip(results_table4)
