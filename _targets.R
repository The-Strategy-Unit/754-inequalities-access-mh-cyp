library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

# Set target-specific options such as packages.
#tar_option_set(packages = "dplyr")

devtools::document()
devtools::load_all()

check_for_issues()

# End this file with a list of target objects.
list(
  tar_target(fingertips_data, get_fingertips_data()),
  tar_target(stp_lookups, get_stp_lookups()),
  tar_target(ccg_successors, get_ccg_successors()),
  tar_target(ccg_gss_to_ods, get_ccg_gss_to_ods())
)
