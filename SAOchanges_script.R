# User Configuration ---------------------------------------------------------------------
# If you are running and offline version, change the "directory" variable to match the 
# current location of the south-atlantic-change folder, and set the "save_results" 
# variable to TRUE. If you are running the code in a binder environment, nothing below 
# here should need to be changed by the user.

directory <- "~/south-atlantic-changes"
save_results <- FALSE

# Load Packages --------------------------------------------------------------------------
library(oce)
library(MASS)
library(signal)
library(ggpubr)
library(seacarb)
library(sfsmisc)
library(tidyverse)

# Save folder creation -------------------------------------------------------------------
# If the "save_results" variable is TRUE, this code will create the "Results" folder and
# subfolders for saving the results in the directory path.

setwd(directory)
if (save_results == TRUE) {
  dir.create("Results")
  subfolder_names <- c("A9.5", "A10", "A10.5", "A13", "A16", "A17")
  for (n in 1:length(subfolder_names)) {
    dir_name <- paste0(directory, "/", "Results/", subfolder_names[n])
    dir.create(dir_name)
  }
  rm(n, subfolder_names)
}

# Anthropogenic and Natural DIC Changes --------------------------------------------------
# This code will apply the "calculate_changes" function based in the eMLR method described
# in Piñango et al. (2022) to every section.

source("./Scripts/Calculations/calculate_changes.R")
source("./Scripts/Calculations/calculate_changes_modified.R")

# A9.5
changes_DIC_A95 <- calculate_changes("./Data/A9.5", save_results, "A9.5")
# A10
changes_DIC_A10 <- calculate_changes("./Data/A10", save_results, "A10")
# A10.5
changes_DIC_A105 <- calculate_changes_modified("./Data/A10.5", save_results, "A10.5")
# A13
changes_DIC_A13 <- calculate_changes("./Data/A13", save_results, "A13")
# A16
changes_DIC_A16 <- calculate_changes("./Data/A16", save_results, "A16")
# A17
changes_DIC_A17 <- calculate_changes("./Data/A17", save_results, "A17")

# Inventory changes associated with Cant uptake ------------------------------------------
# This code will apply the "calculate_inventories" function to every section.

source("./Scripts/Calculations/calculate_inventories.R")

# A9.5
inventories_A95 <- calculate_inventories(
  changes_DIC_A95,
  2018 - 2009,
  save_results, "A9.5"
)
# A10
inventories_A10 <- calculate_inventories(
  changes_DIC_A10,
  c(2003 - 1992, 2011 - 1992, 2011 - 2003),
  save_results, "A10"
)
# A10.5
inventories_A105 <- calculate_inventories(
  changes_DIC_A105,
  2017 - 2011,
  save_results, "A10.5"
)
# A13
inventories_A13 <- calculate_inventories(
  changes_DIC_A13,
  2010 - 1995,
  save_results, "A13"
)
# A16
inventories_A16 <- calculate_inventories(
  changes_DIC_A16,
  c(2005 - 1989, 2013 - 1989, 2013 - 2005),
  save_results, "A16"
)
# A17
inventories_A17 <- calculate_inventories(
  changes_DIC_A17,
  c(2013 - 1994, 2019 - 1994, 2019 - 2013),
  save_results, "A17"
)

# pH and carbonate changes by anthropogenic an natural perturbations ---------------------
# This code will apply the "calculate_pH" function to every section.

source("./Scripts/Calculations/calculate_pH.R")

# A9.5
pH_A95 <- calculate_pH(
  changes_DIC_A95,
  2018 - 2009,
  save_results, "A9.5"
)
# A10
pH_A10 <- calculate_pH(
  changes_DIC_A10,
  c(2003 - 1992, 2011 - 1992, 2011 - 2003),
  save_results, "A10"
)
# A10.5
pH_A105 <- calculate_pH(
  changes_DIC_A105,
  2011 - 2017,
  save_results, "A10.5"
)
# A13
pH_A13 <- calculate_pH(
  changes_DIC_A13,
  2010 - 1995,
  save_results, "A13"
)
# A16
pH_A16 <- calculate_pH(
  changes_DIC_A16,
  c(2005 - 1989, 2013 - 1989, 2013 - 2005),
  save_results, "A16"
)
# A17
pH_A17 <- calculate_pH(
  changes_DIC_A17,
  c(2013 - 1994, 2019 - 1994, 2019 - 2013),
  save_results, "A17"
)

# Figures --------------------------------------------------------------------------------
# Figure 3
source("./Scripts/Figures/Figure 3.R")
plot(figure3)

# Figure 5
source("./Scripts/Figures/Figure 5.R")
plot(figure5)

# Figure 6
source("./Scripts/Figures/Figure 6.R")
plot(figure6)

# Summary Tables -------------------------------------------------------------------------
# DIC changes by Cant uptake and natural process and mean changes in the carbonate system
# as result of the anthropogenic uptake.
source("./Scripts/summary_Cant_changes.R")

# Cant inventory changes in the South Atlantic Ocean.
source("./Scripts/summary_inventories.R")