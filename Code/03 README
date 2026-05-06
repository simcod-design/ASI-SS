=========================================================
WORKFLOW DESCRIPTION FOR CODE 3
=========================================================
This script implements pairwise longitudinal measurement invariance analyses across consecutive survey years,
based on the factor structure derived in Code 2.
Two complementary analytical designs are applied:
1. Repeated cross-sectional design
   - Independent samples across consecutive years
   - Restricted to overlapping item sets
2. Panel-based longitudinal design
   - Same individuals observed across consecutive years
   - Restricted to both common items and respondents

These approaches allow the evaluation of measurement stability across both independent annual samples and 
repeated observations of the same individuals.

Note: This script assumes that Code 1 (data preparation and overlap structure) and Code 2 (final CFA model with 73 items) have been completed.

The workflow consists of the following steps:

22. Load required packages
    - Load R libraries for CFA and invariance testing
    - Data manipulation and result export

23. Data import
    - Use datasets generated in Code 1

24. Define item sets
    - Reuse year-specific item pools from previous steps  

25. Common items and sample overlap  
    - Utilize previously identified common items and respondent overlap structures  

26. Item refinement (CFA-based final structure)  
    - Use the finalized 73-item measurement structure derived from Code 2  

27. Define year lists  
    - Create lists of datasets and corresponding respondent IDs for each year  

28. Define factor structure
    - Specify first-order factors
    - Construct second-order latent variable model

29. Repeated cross-sectional invariance analysis
    - Identify common items across consecutive years
    - Construct combined dataset with group variable
    - Estimate configural, metric, and scalar models
    - Compute model fit indices and Δ-fit statistics

30. Panel-based longitudinal invariance analysis
    - Identify common individuals across consecutive years
    - Restrict analysis to overlapping respondents
    - Apply same invariance testing procedure

Model estimation and comparison
    - Extract χ², df, CFI, TLI, RMSEA, SRMR
    - Compute ΔCFI and ΔRMSEA

Output generation
    - Store results in structured tables
    - Export outputs for reporting

=========================================================
End of workflow
=========================================================
