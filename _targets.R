suppressPackageStartupMessages({
  library(targets)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(forcats)
  library(recipes)
  library(InequalitiesAccessCYPMH)
})

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

# Set target-specific options such as packages.
tar_option_set(
  packages = c("InequalitiesAccessCYPMH",
               "dplyr",
               "tidyr",
               "purrr",
               "forcats",
               "recipes"),
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
  tar_target(lsoa_01_to_stp, get_lsoa_01_to_stp()),
  tar_target(last_edit_cypmh, "ref/last_update_cypmh.txt", format = "file"),
  tar_target(ethnicity_file, "ref/ethnicity.csv", format = "file"),
  tar_target(ethnicity, readr::read_csv(ethnicity_file, col_types = "cc")),
  tar_target(source_referral_file, "ref/source_referral.csv", format = "file"),
  tar_target(source_referral, readr::read_csv(source_referral_file, col_types = "cc")),
  # cypmh data ----
  tar_target(
    raw_cypmh,
    get_cypmh(Sys.getenv("CON_STR"),
    Sys.getenv("CYPMH_TABLE_NAME"))
  ),
  tar_target(
    cypmh,
    process_cypmh(
      raw_cypmh,
      imd,
      lsoa_01_to_stp,
      ethnicity,
      source_referral
    )
  ),
  tar_target(cypmh_recipe, {
    recipe(util_description ~ ., cypmh$train) %>%
      step_cut(spell_days, breaks = c(0, 1, 42, 128, 364, 365)) %>%
      step_cut(matches("cpa_.*_days"), breaks = c(-Inf, 0, 28, 91, 203, 365)) %>%
      step_cut(cpa_reviews,
               admissions,
               el_spells,
               nel_spells,
               breaks = c(-Inf, 0, 1, Inf)) %>%
      step_cut(starts_with("opa_"), breaks = c(-Inf, 0, 2, Inf)) %>%
      step_cut(con_community_psychiatric_nurse,
               con_clinical_psychologist,
               con_occupational_therapist,
               con_contacts_social_worker,
               breaks = c(-Inf, 0, 4, Inf)) %>%
      step_cut(ae_attends, breaks = c(-Inf, 0, 1, 10, Inf)) %>%
      step_select(-bed_days,
                  -starts_with("contact_"),
                  -source_referral,
                  -marital_status,
                  -employment_status,
                  -accomodation_status,
                  -con_physiotherapist,
                  -days_prior) %>%
      step_mutate_at(starts_with("bed_days_"),
                     days_detention,
                     con_consultant_pscycotherapy,
                     fn = ~.x > 0) %>%
      step_mutate_at(age_first_contact, fn = ~pmin(age_first_contact, age)) %>%
      step_mutate_at(imd, fn = ~.x %>%
                       forcats::fct_recode(
                         "Unknown" = "0",
                         "1" = "1", "1" = "2",
                         "2" = "3", "2" = "4",
                         "3" = "5", "3" = "6",
                         "4" = "7", "4" = "8",
                         "5" = "9", "5" = "10"
                       ) %>%
                       forcats::fct_relevel("3")) %>%
      step_mutate_at(stp, fn = ~forcats::fct_relevel(.x, "E54000000")) %>%
      step_mutate_at(ethnicity, fn = ~forcats::fct_relevel(.x, "White")) %>%
      step_select(-starts_with("con_"), -starts_with("cpa_"))
  }),
  tar_target(cypmh_recipe_dummy, step_dummy(
    cypmh_recipe,
    where(is.character),
    where(is.factor),
    -.data$util_description
  )),
  tar_target(cypmh_recipe_logistic, step_mutate_at(
    cypmh_recipe,
    util_description,
    fn = ~.x %in% c("Persistent problems", "Chronic or complex")
  )),
  tar_target(cypmh_recipe_logistic_dummy, step_dummy(
    cypmh_recipe_logistic,
    where(is.character),
    where(is.factor),
    -.data$util_description
  )),
  tar_target(cypmh_baked, {
    rec <- cypmh_recipe %>%
      step_mutate_at(imd, fn = ~ifelse(.x == "Unknown",
                                       sample(1:5, n(), TRUE),
                                       as.character(.x)) %>%
                       fct_relevel("3", "1", "2", "4", "5")) %>%
      prep()
    map(cypmh, bake, object = rec)
  }),
  tar_target(cypmh_dummy_baked, map(cypmh, bake, object = prep(cypmh_recipe_dummy))),
  tar_target(cypmh_logistic_baked, {
    rec <- cypmh_recipe_logistic %>%
      step_mutate_at(imd, fn = ~ifelse(.x == "Unknown",
                                       sample(1:5, n(), TRUE),
                                       as.character(.x)) %>%
                       fct_relevel("3", "1", "2", "4", "5")) %>%
      prep()
    map(cypmh, bake, object = rec)
  }),
  tar_target(cypmh_logistic_dummy_baked, map(cypmh, bake, object = prep(cypmh_recipe_logistic_dummy))),
  # cypmh modelling
  ## 4 class ----
  ### ordinal regression ----
  tar_target(
    cypmh_model_ordinal_regression,
    MASS::polr(util_description ~ ., data = cypmh_baked$train)
  ),
  tar_target(
    cypmh_model_ordinal_regression_p,
    stats::predict(
      cypmh_model_ordinal_regression,
      newdata = cypmh_baked$test
    )
  ),
  ### mlp ----
  tar_target(
    cypmh_model_mlp,
    cypmh_model_run_mlp(cypmh_dummy_baked$train),
    format = "keras"
  ),
  tar_target(
    cypmh_model_mlp_p,
    stats::predict(
      cypmh_model_mlp,
      cypmh_dummy_baked$test %>%
        dplyr::select(-.data$util_description) %>%
        as.matrix()
    )
  ),
  ### xgboost ----
  tar_target(
    cypmh_model_xgboost,
    cypmh_model_run_xgboost(cypmh_dummy_baked$train)
  ),
  tar_target(
    cypmh_model_xgboost_p,
    stats::predict(
      cypmh_model_xgboost,
      cypmh_dummy_baked$test %>%
        dplyr::select(-.data$util_description) %>%
        as.matrix()
    ) %>%
      matrix(ncol = 4, byrow = TRUE)
  ),
  ## 2 class ----
  ### logistic regression ----
  tar_target(
    cypmh_model_logistic_regression,
    stats::glm(
      util_description ~ .,
      data = cypmh_logistic_baked$train,
      family = stats::binomial
    )
  ),
  tar_target(
    cypmh_model_logistic_regression_p,
    stats::predict(
      cypmh_model_logistic_regression,
      newdata = cypmh_logistic_baked$test,
      type = "response"
    )
  ),
  ### mlp ----
  tar_target(
    cypmh_model_mlp_logistic,
    cypmh_model_run_mlp_logistic(cypmh_logistic_dummy_baked$train),
    format = "keras"
  ),
  tar_target(
    cypmh_model_mlp_logistic_p,
    stats::predict(
      cypmh_model_mlp_logistic,
      cypmh_logistic_dummy_baked$test %>%
        dplyr::select(-.data$util_description) %>%
        as.matrix()
    )
  ),
  ### xgboost ----
  tar_target(
    cypmh_model_xgboost_logistic,
    cypmh_model_run_xgboost_logistic(cypmh_logistic_dummy_baked$train)
  ),
  tar_target(
    cypmh_model_xgboost_logistic_p,
    stats::predict(
      cypmh_model_xgboost_logistic,
      cypmh_logistic_dummy_baked$test %>%
        dplyr::select(-.data$util_description) %>%
        as.matrix()
    )
  ),

  tar_target(
    report_modelling_results_file,
    "reports/modelling_results.Rmd",
    format = "file"
  ),
  tar_target(
    report_modelling_results,
    rmarkdown::render(
      report_modelling_results_file,
      params = list(
        response    = cypmh_logistic_baked$test$util_description,
        lrg_model   = cypmh_model_logistic_regression,
        xgb_model   = cypmh_model_xgboost_logistic,
        # not actually using this, save loading keras/tensorflow
        # mlp_model   = cypmh_model_mlp_logistic,
        lrg_results = cypmh_model_logistic_regression_p,
        xgb_results = cypmh_model_mlp_logistic_p,
        mlp_results = cypmh_model_xgboost_logistic_p,
        cypmh_cols  = cypmh_baked$train %>%
          select(-util_description) %>%
          colnames() %>%
          sort(TRUE)
      )
    )
  )
)
