#' proc_task_derivs: Exports derivative databases with task beh summary metrics and associated meta-data
#'
#' This function calls deriv_{task} functions and exports TSVs with summary behavioral metrics and associated meta-data
#'
#' @param task_data list of processed task data returned by proc_task()
#' @param export_dir string with absolute path to export directory (typically behavioral summary directory)
#' @examples
#'
#' \dontrun{
#'
#' # process task data
#' base_dir = "/Users/bari/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/b-childfoodlab_Shared/Active_Studies/MarketingResilienceRO1_8242020/ParticipantData/"
#' task_data <- proc_task(base_wd = base_dir, return_data = TRUE)
#'
#' # export derivative databases
#' proc_task_derivs(task_data, "path/to/export/")
#' }
#'
#' @export

proc_task_derivs <- function(task_data, export_dir) {

  #### Set up/initial checks #####

  # check that overwrite argument exists and is logical
  task_data_arg <- methods::hasArg(task_data)

  if (isFALSE(task_data_arg)) {
    stop("must enter task_data argument")
  } else {
    if (isFALSE(is.list(task_data))) {
      stop("task_data argument must be list")
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


  #### define write_data function ----
  write_data <- function(data, export_dir, filename) {
    write.table(
      data,
      paste0(export_dir, filename),
      quote = FALSE,
      sep = '\t',
      col.names = TRUE,
      row.names = FALSE,
      na = "n/a" # use 'n/a' for missing values for BIDS compliance
    )

    print(paste("exporting", filename))
  }

  #### RRV derivatives ----

  print("Creating RRV summary database")

  # process derivative data
  rrv_deriv_data <- deriv_rrv(data = task_data$rrv_data)

  # process meta-data
  json_rrv_deriv <- json_deriv_rrv()

  # write deriv databases
  write_data(rrv_deriv_data[['summary']], export_dir, "rrv.tsv")
  write_data(rrv_deriv_data[['summary_long']], export_dir, "rrv_long.tsv")

  # write meta-data
  write(json_rrv_deriv[['rrv_summary_json']], paste0(export_dir, "rrv.json"))
  write(json_rrv_deriv[['rrv_summary_long_json']], paste0(export_dir, "rrv_long.json"))

  #### SST derivatives ----

  print("Creating SST summary database")

  # process derivative data
  sst_deriv_data <- deriv_sst(data = task_data$sst)

  # process meta-data
  json_sst_deriv <- json_deriv_sst()

  # write deriv databases
  write_data(sst_deriv_data[['summary_long_by_cond']], export_dir, "sst_long_by_cond.tsv")
  write_data(sst_deriv_data[['summary_long_by_run']], export_dir, "sst_long_by_run.tsv")
  write_data(sst_deriv_data[['summary_long_by_block']], export_dir, "sst_long_by_block.tsv")

  # write meta-data
  write(json_sst_deriv[['sst_byblock_json']], paste0(beh_sum_dir, "sst_long_by_block.json"))
  #write(json_sst_deriv[['sst_byrun_json']], paste0(beh_sum_dir, "sst_long_by_run.json")) # not yet returned by json_deriv_sst()

  #### Foodview derivatives  ----

  print("Creating Foodview summary database")

  # process derivative data
  foodview_deriv_data <- deriv_foodview(data = task_data$foodview)

  # process meta-data
  json_foodview_deriv <- json_deriv_foodview()

  # write deriv databases
  write_data(foodview_deriv_data[['summary_long_by_cond']], export_dir, "foodview_long_by_cond.tsv")
  write_data(foodview_deriv_data[['summary_long_by_block']], export_dir, "foodview_long_by_block.tsv")

  # write meta-data
  write(json_foodview_deriv[['foodview_bycond_json']], paste0(export_dir, "foodview_long_by_cond.json"))
  write(json_foodview_deriv[['foodview_byblock_json']], paste0(export_dir, "foodview_long_by_block.json"))

  #### Spacegame derivatives  ----

  #print("Creating Space Game summary database")

  # process derivative data
  #spacegame_deriv_data <- deriv_spacegame(task_data$spacegame)

}

