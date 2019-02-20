library("rSFSW2")
t_job_start <- Sys.time()

################################################################################
#------ 1) LOAD AN EXISTING SIMULATION PROJECT ------------------
SFSW2_prj_meta <- readRDS("data/rSFSW2_ProjectFiles/SFSW2_project_descriptions_Onesite.rds")
dir_prj <- file.path(getwd(), "data/rSFSW2_ProjectFiles")

#------ Turn on/off actions to be carried out by simulation framework
actions <- list(
  # Input checking
  check_inputs = FALSE,

  # Simulation runs
  # "sim_create", "sim_execute", and "sim_aggregate" can be used individually if
  # "saveRsoilwatInput" and/or "saveRsoilwatOutput" are true
  #   - Prepare/collect inputs for a rSOILWAT2 run (formerly, 'create')
  sim_create = TRUE,
  #   - Execute SOILWAT2 simulations (formerly 'execute')
  sim_execute = TRUE,
  #   - Calculate aggregated response variables from  SOILWAT2 output and store
  #     results in temporary text files on disk (formerly, "aggregate')
  sim_aggregate = TRUE,

  # Output handling
  #   - Copy simulation results from temporary text files to a output
  #     SQL-database (formerly, 'concatenate')
  concat_dbOut = FALSE,
  #   - Calculate 'ensembles' across climate scenarios and stores the results
  #     in additional SQL-databases as specified by 'ensemble.families' and
  #     'ensemble.levels'
  ensemble = FALSE,
  #   - Check completeness of output database
  check_dbOut = FALSE
)


################################################################################
#------ 2) LOAD THE SETTINGS FOR THIS RUN --------------------------------------
# Setting objects:
#   opt_behave, opt_parallel, opt_verbosity, opt_out_run, opt_chunks
source(file.path("data/rSFSW2_ProjectFiles/SFSW2_project_settings.R"), verbose = FALSE,
       keep.source = FALSE)

SFSW2_prj_meta <- update_actions(SFSW2_prj_meta, actions,
                                 wipe_dbOutput = opt_out_run[["wipe_dbOutput"]])


################################################################################
#------ 3) POPULATE PROJECT WITH INPUT DATA (REPEAT UNTIL COMPLETE) ------------
temp <- populate_rSFSW2_project_with_data(SFSW2_prj_meta, opt_behave,
                                          opt_parallel, opt_chunks, opt_out_run, opt_verbosity)


if (isTRUE(opt_verbosity[["verbose"]]) &&
    !identical(SFSW2_prj_meta, temp[["SFSW2_prj_meta"]])) {
  warning("'SFSW2_prj_meta' has changed: modify/reset input tracker status ",
          "'SFSW2_prj_meta[['input_status']]', if needed ",
          "(see help `?update_intracker`) and re-run project.",
          call. = FALSE, immediate. = TRUE)
}

SFSW2_prj_meta <- temp[["SFSW2_prj_meta"]]
SFSW2_prj_inputs <- temp[["SFSW2_prj_inputs"]]


################################################################################
#------ 4) ATTEMPT TO CHECK INPUT DATA -----------------------------------------

if (isTRUE(actions[["check_inputs"]])) {

  temp <- check_rSFSW2_project_input_data(SFSW2_prj_meta, SFSW2_prj_inputs,
                                          opt_chunks, opt_verbosity)

  SFSW2_prj_meta <- temp[["SFSW2_prj_meta"]]
  SFSW2_prj_inputs <- temp[["SFSW2_prj_inputs"]]

  if (isTRUE(opt_verbosity[["verbose"]]) &&
      !all(stats::na.exclude(SFSW2_prj_meta[["input_status"]][, "checked"]))) {
    warning("'SFSW2_prj_meta[['input_status']]': some input tracker checks ",
            "failed; fix inputs, if needed, and re-run project.",
            call. = FALSE, immediate. = TRUE)
  }
}



################################################################################
#------ 5) RUN SIMULATION EXPERIMENT (REPEAT UNTIL COMPLETE) -------------------

if (any(unlist(actions[c("sim_create", "sim_execute", "sim_aggregate")]))) {

  SFSW2_prj_meta <- simulate_SOILWAT2_experiment(SFSW2_prj_meta,
                                                 SFSW2_prj_inputs, opt_behave, opt_parallel, opt_chunks, opt_out_run,
                                                 opt_verbosity)
}
