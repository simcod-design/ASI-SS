  =========================================================
  WORKFLOW DESCRIPTION FOR CODE 1
  =========================================================
  This script implements a comprehensive multi-stage analytical pipeline to derive a robust multidimensional measurement
structure from retrospective survey data characterized by partially overlapping item sets across years.
 
  The workflow consists of the following steps:
 
  0. Load required packages
     - Load all R libraries used for data handling, psychometric analysis,
       visualization, and network-based methods.
 
  1. Data import
     - Import the raw dataset and perform initial preprocessing.
 
  2. Define item sets by year
     - Specify item pools for each survey year (2017–2024),
       reflecting the partially overlapping measurement design.
 
  3. Common items and sample overlap
     - Identify common items across years
     - Compute item overlap and shared respondent matrices
 
  4. Polychoric correlations
     - Estimate polychoric correlation matrices for ordinal data
     - Detect highly correlated item pairs (multicollinearity)
 
  5. Item reduction
     - Remove items exhibiting high inter-item correlations
     - Reconstruct filtered datasets for each year
 
  6. Factor number estimation
     - Perform parallel analysis across multiple extraction methods
     - Identify candidate ranges for the number of factors  
 
  7. Multi-model exploratory factor analysis (EFA)
     - Estimate 264 candidate models per year
       (4 extraction methods × 6 rotations × 11 factor solutions)
     - Evaluate models based on RMSR and cumulative explained variance
 
  7.1 Visualization
     - Generate RMSR and cumulative variance plots across models
 
  7.2 Image merging
     - Combine yearly plots into composite figures
 
  8. Co-occurrence matrix construction
     - Compute item co-occurrence frequencies across candidate models
     - Identify stable item pairings based on repeated factor membership
 
  8.1 MDS and community detection
     - Apply multidimensional scaling (MDS)
     - Detect item clusters using the Louvain community detection algorithm
 
  8.2 Merge MDS figures
     - Combine MDS plots across years into composite visualizations
 
  9. Final EFA model
     - Estimate the final factor structure
     - Evaluate model adequacy using KMO, Bartlett test, RMSR, and cumulative        
       explained variance
 
  9.1 Factor loadings summary
     - Extract primary and secondary factor loadings
     - Summarize item–factor assignments across dimensions
 
  =========================================================
  End of workflow
  =========================================================
