=========================================================
WORKFLOW DESCRIPTION FOR CODE 2
=========================================================
To ensure accurate model specification and to diagnose potential estimation issues, 
the CFA model was first implemented for a single survey year. Following successful 
validation, the finalized model structure was systematically applied across all survey years using an automated looping procedure, 
enabling consistent estimation and direct comparability of results across measurement occasions.
This script extends the analytical pipeline by implementing the confirmatory phase, focusing on the validation and refinement of 
the factor structure derived from exploratory analyses (Code 1).

Note: This script assumes that Code 1 (Steps 0–3) has been completed.
The workflow consists of the following steps:

10. Load required packages  
    - Load R libraries used for confirmatory factor analysis (CFA),  
      data processing, and visualization  

11. Data import and preparation (from Code 1)  
    - Use preloaded datasets and item structures generated in Code 1  

12. Year-specific item structure  
    - Reuse item pools defined for each survey year  

13. Cross-year overlap structure  
    - Reuse common item sets and shared respondent structures  

14. Item reduction (Stage 1, CFA)  
    - Remove items based on:  
      • high inter-item correlations (multicollinearity)  
      • low co-occurrence stability  
      • inconsistent EFA loadings  
    - Establish the preliminary factor structure (75 items)  

15. CFA model specification (Initial model)  
    - Define first-order factors based on EFA results  
    - Specify second-order latent construct  

16. CFA estimation (Single-year validation)  
    - Fit the model for a single survey year  
    - Evaluate model convergence and parameter estimates  
    - Extract factor loadings and model fit indices  
    - Generate path diagram  

17. Item refinement (Stage 2, CFA)  
    - Remove items with low factor loadings identified in CFA  
    - Refine the measurement structure (final 73 items)  

18. CFA model specification (Refined model)  
    - Update factor structure after item removal  
    - Maintain second-order factor model  

19. CFA estimation (Single-year validation)  
    - Re-estimate the refined model  
    - Confirm model stability after refinement  

20. CFA estimation across years  
    - Apply the finalized model across all survey years  
    - Adapt model specification to year-specific item availability  
    - Extract loadings and model fit indices for each year  
    - Generate year-specific path diagrams  

21. Visualization and output integration  
    - Merge CFA diagrams across years into composite figures  
    - Prepare outputs for reporting and supplementary materials  

=========================================================
End of workflow
=========================================================
