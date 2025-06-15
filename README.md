# UCLA MS Thesis: Modeling the Co-evolution of Friendship and Delinquency Amongst Adolescents

This repository contains all R code for the thesis **"Modeling the Co-evolution of Friendship and Delinquency Amongst Adolescents"**.  
It includes scripts for data cleaning, imputation, network construction, statistical modeling (SAOM and STERGM) and diagnostics.

## Repository Structure
├── README.md 

├── clean_data.R 

├── imputation.R 

├── saom_pipeline.R 

├── stergm_pipeline.R 

├── diagnostics.R 

├── saom_results/ 

│ └── saom_simple_simple_results/

├── stergm_results/ 

│ └── stergm_simple_simple_results/

└── data/

## Data Access

**Raw data is not included** due to licensing.  
To request access to the Knecht dataset, see [DANS project page]([https://www.narcis.nl/publication/RecordID/oai:dans.knaw.nl:easy-dataset:57886](https://doi.org/10.17026/dans-z9b-h2bp)).

All code assumes you have downloaded and unzipped the SPSS files to a local `data/` directory.

## Requirements

- **R version**: 4.2 or later recommended
- **Packages**: See below

## Usage & Tutorial
Below is a minimal end-to-end example that reproduces the main analysis pipeline.


### Load Packages
```{r}
library(haven)       # SPSS file reading
library(dplyr)       # Data wrangling
library(tidyr)       # Data reshaping
library(purrr)       # Functional programming
library(readr)       
library(tidyverse)   # Includes ggplot2, dplyr, tidyr, etc.
library(RSiena)      # SAOM modeling
library(network)     # Network objects for statnet
library(sna)         # Social network analysis
library(mice)        # Multiple imputation
library(igraph)      # General network analysis
library(scales)      # Plotting utilities
library(ggnetwork)   # Network visualization
library(ggplot2)     
library(grid)        # For unit()
library(glue)        # String interpolation
library(patchwork)   # Plot composition
library(intergraph)  # Conversions between network/igraph
library(tergm)       # Temporal ERGM
library(ergm)        # Exponential random graph models
```

### Load Data and Preprocess
```{r}
# Set the base directory where your SPSS files are stored
# data_dir <- "...data/"
specified_school_id <- "12"
subset_classes <- c("f") # restrict to just class 'f'; set NULL to analyze all classes

# Load and preprocess raw data for each wave
PupilsV_raw <- process_data("PupilsWaveV.sav", data_dir = data_dir, wave = "V", specified_school_id = specified_school_id)
PupilsW_raw <- process_data("PupilsWaveW_geanonimiseerd.sav", data_dir = data_dir, wave = "W", specified_school_id = specified_school_id)
PupilsX_raw <- process_data("PupilsWaveX.sav", data_dir = data_dir, wave = "X", specified_school_id = specified_school_id)
PupilsY_raw <- process_data("PupilsWaveY.sav", data_dir = data_dir, wave = "Y", specified_school_id = specified_school_id)

# Filter data: keep only students present in all four waves for the specified school/class
filtered_waves <- get_filtered_wave_data_by_class(PupilsV_raw, PupilsW_raw, PupilsX_raw, PupilsY_raw, specified_school_id = "12")
PupilsV <- filtered_waves$V
PupilsW <- filtered_waves$W
PupilsX <- filtered_waves$X
PupilsY <- filtered_waves$Y
```

### SAOM (RSiena)
```{r}
# Define effects function for SAOM Simple model (edit as needed for other specs)
effects_simple <- function(siena_data) {
  eff <- getEffects(siena_data)
  eff <- includeEffects(eff, outdegree, recip, transTrip, balance, name = "friendship_siena")
  for (var in c("age_siena", "advice_siena")) {
    eff <- includeEffects(eff, sameX, name = "friendship_siena", interaction1 = var)
  }
  eff <- includeEffects(eff, inPop, name = "delinquency_siena")
  
  return(eff)
}


# Run SAOM models with defined effects function
run_saom_for_school_custom(
  PupilsV = PupilsV,
  PupilsW = PupilsW,
  PupilsX = PupilsX,
  PupilsY = PupilsY,
  specified_school_id = specified_school_id,
  effect_function = effects_simple,
  output_folder =  file.path(getwd(),"saom_results", "saom_simple_simple_results"),
  subset_classes = subset_classes,
  method = "simple" # change to "mice" for MICE imputation
)
```
Example goodness-of-fit plots can be found in the `saom_results/saom_simple_simple_results` folder.

### STERGM (statnet/tergm)
```{r}
# Specify STERGM Simple formation and dissolution formulas
form_simple <- ~edges + mutual + nodecov("delinquency") + nodematch("delinquency") + nodematch("advice") + nodematch("age")
diss_simple <- ~edges + mutual + nodecov("delinquency") + nodematch("delinquency") + nodematch("advice") + nodematch("age")

stergm_simple_simple_results <- run_stergm_for_school_custom(
  PupilsV, PupilsW, PupilsX, PupilsY,
  specified_school_id = specified_school_id,
  formation = form_simple,
  dissolution = diss_simple,
  output_folder = file.path(getwd(),"stergm_results","stergm_simple_simple_results"),
  subset_classes = subset_classes,
  method = "simple"
)
```

Example MCMC diagnostics can be found in the `stergm_results/stergm_simple_simple_results` folder.
