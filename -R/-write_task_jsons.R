#' write_task_jsons: Write meta-data for tasks created with json functions (called within proc_task.R)
#'
#' This function exports json meta-data files for task data
#' @param bids_wd string with absolute path to bids directory (contains rawdata/ and phenotype/)
#' @param overwrite logical (TRUE/FALSE) to indicate if json files should be overwritten


write_task_jsons <- function(bids_wd, overwrite) {

  #### Set up/initial checks #####

  # check that overwrite argument exists and is logical
  overwrite_arg <- methods::hasArg(overwrite)

  if (isFALSE(overwrite_arg)) {
    stop("must enter overwrite argument (TRUE/FALSE)")
  } else {
    if (isFALSE(is.logical(overwrite))) {
      stop("overwrite argument must be logical (TRUE/FALSE)")
    }
  }

  # check that bids_wd argument exists and is a string
  bids_wd_arg <- methods::hasArg(bids_wd)

  if (isFALSE(bids_wd_arg)) {
    stop("must enter bids_wd argument")
  } else {
    if (!is.character(bids_wd)) {
      stop("bids_wd argument must be string")
    }
  }


  #### Define paths for export ####
  phenotype_wd <- file.path(bids_wd, "phenotype")
  raw_wd <- file.path(bids_wd, "rawdata")

  # generate phenotype_wd if it doesn't exist
  if (!file.exists(phenotype_wd)){
    dir.create(phenotype_wd)
  }

  # generate raw_wd if it doesn't exist
  if (!file.exists(raw_wd)){
    dir.create(raw_wd)
  }

  #### Export meta-data #####

  # List of json functions and corresponding filenames
  json_functions <- list(

    json_sst_beh = "task-sst_beh.json",
    json_sst_bold = "task-sst_events.json",
    json_foodview = "task-foodview_events.json",
    json_toolbox_beh = "task-toolbox_beh.json",
    json_toolbox_phenotype = "toolbox_scores.json",
    json_rrv = "task-rrv_beh.json",
    json_spacegame = "task-spacegame_beh.json"

  )

  # Create an empty list to store JSON
  json_list <- list()

  # run json functions and export
  for (func_name in names(json_functions)) {

    # Get the function by name
    func <- get(func_name)

    # Call the function
    json <- func()

    # Append JSON to a list
    json_list[[func_name]] <- json

    if (func_name == "json_toolbox_phenotype") {

      # define filename for export in phenotype_wd
      filename <- file.path(phenotype_wd, json_functions[[func_name]])

    } else {

      # define filename for export in raw_wd
      filename <- file.path(raw_wd, json_functions[[func_name]])
    }

    # If overwrite is false
    if (isFALSE(overwrite)) {

      # Write json if it doesn't exist
      if (!file.exists(filename)) {
        write(json, filename)   # Write json
      }

    } else {
      write(json, filename)
    }
  }

  return(json_list)

}

