################################################################################
# This script is meant to be a memory aid for how to use the targets package   #
# you should first familiarise yourself with the targets package by reading    #
# at least the first 2 chapters of https://books.ropensci.org/targets/         #
################################################################################

# for development purposes only: make sure to fully qualify function names in
# any of the R scripts in the R/ folder, or list them as imports in R/ZZZ.R
library(tidyverse)
library(devtools)
library(targets)

# run R CMD CHECK to test for potential issues
check()

# edit the targets ----
tar_edit()

# re-run the targets ----
tar_make()

# visualise the targets ----
tar_visnetwork()

# load all of the targets into an environment ----
td <- tar_manifest()$name %>%
  set_names() %>%
  map(~do.call(tar_read, list(as.symbol(.x)))) %>%
  as.environment()

names(td)
