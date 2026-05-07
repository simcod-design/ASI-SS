ASI-SS
======
This repository contains R scripts and supplementary materials for reproducing the analyses presented in the study on longitudinal measurement structures derived 
from retrospective survey data with partially overlapping items.

The project introduces a comprehensive analytical framework for deriving and validating multidimensional measurement structures using retrospective survey data with partially overlapping items and samples.

The repository is organized into three main analytical stages:

1. Code 1 – Exploratory Structure Derivation  
   - Polychoric correlation estimation  
   - Item reduction (multicollinearity control)  
   - Parallel analysis for factor number estimation  
   - Multi-model exploratory factor analysis (EFA)  
   - Co-occurrence stability analysis  
   - Multidimensional scaling (MDS)  
   - Network-based community detection  
   - Final exploratory factor structure identification  

2. Code 2 – Confirmatory Factor Analysis (CFA)  
   - Specification of first- and second-order factor models  
   - Single-year model validation  
   - Item refinement based on CFA results  
   - Final model estimation  
   - Application of the finalized model across all survey years  

3. Code 3 – Longitudinal Measurement Invariance  
   - Pairwise invariance testing across consecutive years  
   - Two complementary designs:  
     • Repeated cross-sectional analysis (independent samples, common items)  
     • Panel-based longitudinal analysis (common individuals and items)  
   - Estimation of configural, metric, and scalar invariance models  
   - Evaluation using ΔCFI and ΔRMSEA criteria  

--------------------------
Supplementary Material
--------------------------
The "Supplementary" folder contains Supplementary Material, which provides:
- Item-level information  
- Polychoric correlation matrices  
- Co-occurrence matrices  
- Stability estimates  
- Candidate model outputs  
- EFA and CFA results  
- Final factor structure  

These materials ensure full reproducibility of all analytical stages.

--------------------------
Figures
--------------------------
The "Figures" folder contains the visual outputs presented in the manuscript, generated from the analytical procedures implemented in the R scripts within the "Code" folder. These include:
- MDS configurations  
- Network-based clustering results  
- Model evaluation plots   

---------------------------
Reproducibility
---------------------------
All analyses reported in the manuscript can be reproduced using the provided R scripts.
The analytical pipeline follows a sequential structure:
Step 1 → Run Code 1 (exploratory analyses)  
Step 2 → Run Code 2 (confirmatory analyses)  
Step 3 → Run Code 3 (measurement invariance)  

Each script builds on outputs generated in the previous stage.

---------------------------
Data Availability
---------------------------
Due to institutional restrictions, the original data cannot be shared.  
However, all analysis scripts and intermediate outputs are provided to ensure 
maximum transparency and reproducibility.

---------------------------
Software Requirements
---------------------------
The analyses were conducted in R. Key packages include:
- lavaan  
- psych  
- GPArotation  
- dplyr  
- tidyr  
- stringr  
- ggplot2  
- igraph  
- ggrepel
- semPlot
(Additional packages are specified within each script.)

---------------------------
Citation
---------------------------
If you use this repository, please cite:

Demir, P., Yılmaz, M. A., AYBU Quality Board & Yuksel, S.  
Beyond Classical Scale Development: Constructing Robust Longitudinal Measurement 
Structures from Retrospective Survey Data with Partially Overlapping Items.
