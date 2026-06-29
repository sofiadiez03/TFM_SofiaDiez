# A Phase and Amplitude-Based Distance for Functional Data

This repository contains the complete, reproducible computational workflow developed for my Master's Thesis in Big Data Analytics (2025-2026) at Universidad Carlos III de Madrid. 

The project introduces novel distance metrics in Functional Data Analysis (FDA) designed to explicitly separate phase variability and amplitude variability, overcoming the limitations of standard metrics like the $\mathcal{L}^2$ distance.

---

## How to Reproduce the Results

To ensure absolute reproducibility of all simulations, figures, and case studies presented in the thesis document, follow these simple steps:

1. **Clone or Download the Repository:**
   Download the project as a ZIP file or clone it via Git.
2. **Open the Project:**
   Launch RStudio by double-clicking the `TFM_SofiaDiez.Rproj` file in the root directory. This automatically sets the correct relative working paths, so you do not need to use `setwd()`.
3. **Run the Scripts:**
   Navigate to the `scripts/` folder and execute the desired files.

---

## 📂 Repository Structure

The project is structured following the standard guidelines for reproducible research in data science:

```text
├── TFM_SofiaDiez.Rproj       # RStudio Project file (maintains relative paths)
├── README.md                 # This documentation file
├── .gitignore                # Filters out temporary R files
│
├── R/                        # Core mathematical functions and helper metrics
│   ├── sq_dist.R                   # Implementation of the proposed distance metrics
│   ├── sim_fun_univ.R              # Helper functions for simulating functional data
│   ├── Clustering.R                # Clustering utility functions
│   └── este_regist_curves.R        # Curve registration and alignment utilities
│
├── data/                     # Data resources used in the case studies
│   ├── asfrTRbo_read.txt            # Raw demographic data from HFD
│   └── HFC_ASFRstand_BO_clean.txt   # Cleaned demographic data from HFC
│
└── scripts/                  # Executable workflows ordered by thesis chapters
    ├── MDS.R                       # Multidimensional Scaling analysis & visualizations
    ├── KNN_Two_Groups.R            # K-Nearest Neighbors evaluation for 2 classes
    ├── KNN_Three_Groups.R          # K-Nearest Neighbors evaluation for 3 classes
    ├── Simulation_Study_1.R        # Section 5.1: Own simulated models (L1, L2, L3, L4)
    ├── Simulation_Study_2.R        # Section 5.2: Replication of Qiao's simulation design
    ├── CS_Tecator.R                # Section 5.3: Case Study - Tecator meat spectroscopy dataset
    └── CS_Fertility_2020.R         # Section 5.4: Case Study - Eurostat + HFD + HFC fertility application
