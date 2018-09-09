# Description --------------------------------------------------------------------------------------
# Script to test package

# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(magrittr)
library(here)

source("R/RC.R")
source("R/dataWranglingCleaning.R")
source("R/stats.R")

load("data/N.rda")
load("data/RC.rda"); setDT(RC)


RC[, genCI(response    = responses,
           stim        = stimuli,
           noiseMatrix = N,
           baseImg     = "data/base.jpeg",
           filename    = paste0(id, ".png") %>% unique), id]

