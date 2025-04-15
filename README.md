# PTRMSR
PTRMSR package


## Installation
The installation of PTRMSR requires the installation of other packages
```R
if (!require("BiocManager", quietly = TRUE)) {install.packages("BiocManager")}
if (!require("devtools", quietly = TRUE)) {install.packages("devtools")}
BiocManager::install("rhdf5")
BiocManager::install("MSnbase")
install.packages(c("reshape2","pheatmap","ggplot2","ellipse","plotly","FactoMineR")
devtools::install_github("https://github.com/ChemoSens/CSUtils")
devtools::install_github("https://github.com/ChemoSens/PTRMSR")
```
