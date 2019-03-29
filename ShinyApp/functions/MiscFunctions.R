
conv_res <- function(x) {
  round((28.15625 + round((x - 28.15625) / 0.0625, 0) * 0.0625), digits = 5)
}



delete_test_output2 <- function(dir_test, delete_filepaths = NULL) {
  
    files_to_delete <- c(delete_filepaths, list.files(dir_test, 
                        pattern = "last.dump", recursive = TRUE, full.names = TRUE), 
                        list.files(dir_test, pattern = ".log", recursive = TRUE, 
                        full.names = TRUE), list.files(dir_test, pattern = ".Rapp.history", 
                        recursive = TRUE, full.names = TRUE), list.files(dir_test, 
                        pattern = ".Rhistory", recursive = TRUE, full.names = TRUE), 
                        list.files(dir_test, pattern = "_olog_cluster.txt", recursive = TRUE, 
                        full.names = TRUE), list.files(dir_test, pattern = "ClimDB_failedLocations_", 
                        recursive = TRUE, full.names = TRUE), list.files(dir_test, 
                        pattern = "backup", recursive = TRUE, full.names = TRUE), 
    
                        file.path(dir_test, "1_Input", "SWRuns_InputAll_PreProcessed.rds"), 
                        file.path(dir_test, "1_Input", "SWRuns_InputMaster_fill_v11.csv"), 
                        file.path(dir_test, "1_Input", "SWRuns_InputData_TreatmentDesign_v17_fill.csv"),
                        file.path(dir_test, "1_Input", "SWRuns_InputData_ExperimentalDesign_v09_fill.csv"), 
                        file.path(dir_test, "1_Input", "SWRuns_InputData_SoilLayers_v9.csv"), 
                        file.path(dir_test, "1_Input", "datafiles", "SWRuns_InputData_prod_v11.csv"),
                        file.path(dir_test, "1_Input", "datafiles", "SWRuns_InputData_soils_v12.csv"),
                        file.path(dir_test, "1_Input", "datafiles", "SWRuns_InputData_cloud_v10.csv"), 
                        file.path(dir_test, "1_Input", "dbWeatherData_test.sqlite3"), 
                        file.path(dir_test,  "SFSW2_project_descriptions_Onesite.rds"),
                        file.path(dir_test,  "SFSW2_project_descriptions.rds")
                        
                        
                        )
    
    dirs_to_delete <- c(file.path(dir_test, "3_Runs"), file.path(dir_test, "4_Simulation"))
    
    try(unlink(unlist(files_to_delete)), silent = TRUE)
    try(unlink(unlist(dirs_to_delete), recursive = TRUE), silent = TRUE)
    
    invisible(TRUE)
}


my_checkboxGroupInput <- function(variable, label, choices, selected, colors){
  choices_names <- choices
  if(length(names(choices))>0) my_names <- names(choices)
  div(id=variable,class="form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
      HTML(paste0('<label class="control-label" for="',variable,'">',label,'</label>')),
      div( class="shiny-options-group",
           HTML(paste0('<div class="checkbox" style="color:', colors,'">',
                       '<label>',
                       '<input type="checkbox" name="', variable, 
                       '" value="', choices, 
                       '"', ifelse(choices %in% selected, 'checked="checked"', ''), 
                       '/>',
                       '<span>', choices_names,'</span>',
                       '</label>',
                       '</div>', collapse = " "))
      )
  )
}