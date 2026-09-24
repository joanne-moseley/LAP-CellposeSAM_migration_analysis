##--------------Install Dependencies------------------
# these packages are required for migration_simulation.R and/or migration_analysis.qmd

packages <- c("tidyverse",'readr', "snakecase", "stringr", "lme4", "RColorBrewer","viridis", "ggrepel", "patchwork", "gganimate",
              "bestNormalize", "RNOmni", "lmerTest", "car", "emmeans", "ggsignif", "factoextra")

for (i in packages) {
  if (!requireNamespace(i, quietly = TRUE)) {   # check if already installed
    install.packages(i)
  }
}