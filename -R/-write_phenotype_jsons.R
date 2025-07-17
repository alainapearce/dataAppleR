#' write_phenotype_jsons: Write meta-data for phenotype data created with json functions
#'
#' This function exports json meta-data files for phenotype data (participants.json not exported, as this goes into bids/)
#' This function is not currently called as part of proc_redcap() as write_phenotype_data() is called instead to export both .tsv and associated meta-data
#'
#' @param export_dir string with absolute path to export directory (typically bids phenotype directory)
#' @param overwrite logical (TRUE/FALSE) to indicate if json files should be overwritten


write_phenotype_jsons <- function(export_dir, overwrite) {

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

  # check that export_dir argument exists and is a string
  export_dir_arg <- methods::hasArg(export_dir)

  if (isFALSE(export_dir_arg)) {
    stop("must enter export_dir argument")
  } else {
    if (is.character(export_dir)) {

      # generate export_dir if it doesn't exist
      if (!file.exists(export_dir)){
        dir.create(file.path(export_dir))
      }

    } else {
      stop("export_dir argument must be string")
    }
  }

  #### Export meta-data #####

  # List of json functions and corresponding filenames
  json_functions <- list(

    json_anthro = "anthropometrics.json",
    json_audit = "audit.json",
    json_bes = "bes.json",
    json_bisbas = "bisbas.json",
    json_brief2 = "brief2.json",
    json_cbq = "cbq.json",
    json_cchip = "cchip.json",
    json_cebq = "cebq.json",
    json_cfpq = "cfpq.json",
    json_cfq = "cfq.json",
    json_chaos = "chaos.json",
    json_class = "class.json",
    json_cshq = "cshq.json",
    json_demographics = "demographics.json",
    json_debq = "debq.json",
    json_dexa = "dexa.json",
    json_efcr = "efcr.json",
    json_ffbs = "ffbs.json",
    json_fsq = "fsq.json",
    json_fhfi = "fsq.json",
    json_hfssm = "fhfi.json",
    json_household = "household.json",
    json_infancy = "infancy.json",
    json_intake = "intake.json",
    json_kbas = "kbas.json",
    json_lbc = "lbc.json",
    json_loc = "loc.json",
    json_mri_visit = "mri_visit.json",
    json_pmum = "pmum.json",
    json_pptq = "pptq.json",
    json_pss = "pss.json",
    json_ptsca = "ptsca.json",
    json_puberty = "puberty.json",
    json_pwlb = "pwlb.json",
    json_rank = "rank.json",
    json_scpf = "scpf.json",
    json_sic = "sic.json",
    json_sleeplog = "sleeplog.json",
    json_spsrq = "spsrq.json",
    json_stq = "stq.json",
    json_tfeq = "tfeq.json"

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

    # define filename for export
    filename <- paste0(export_dir, json_functions[[func_name]])

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

