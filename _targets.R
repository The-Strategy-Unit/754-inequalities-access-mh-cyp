library(targets)
library(InequalitiesAccessCYPMH)

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

# Set target-specific options such as packages.
tar_option_set(
  packages = c("InequalitiesAccessCYPMH"),
  imports = c("InequalitiesAccessCYPMH")
)

check_for_issues()

# End this file with a list of target objects.
list(
  tar_target(fingertips_data, get_fingertips_data()),
  tar_target(stp_lookups, get_stp_lookups()),
  tar_target(ccg_successors, get_ccg_successors()),
  tar_target(ccg_gss_to_ods, get_ccg_gss_to_ods()),
  tar_target(lad_successors, get_lad_successors(stp_lookups)),
  tar_target(imd, get_imd()),
  tar_target(last_edit_cypmh, "ref/last_update_cypmh.txt", format = "file"),
  tar_target(ethnicity_file, "ref/ethnicity.csv", format = "file"),
  tar_target(ethnicity, readr::read_csv(ethnicity_file, col_types = "cc")),
  tar_target(source_referral_file, "ref/source_referral.csv", format = "file"),
  tar_target(source_referral, readr::read_csv(source_referral_file, col_types = "cc")),
  # cypmh data ----
  tar_target(raw_cypmh,
             get_cypmh(Sys.getenv("CON_STR"),
                       Sys.getenv("CYPMH_TABLE_NAME"))),
  tar_target(cypmh,
             process_cypmh(raw_cypmh,
                           imd,
                           ethnicity,
                           source_referral)),
)
