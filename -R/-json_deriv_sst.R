#' json_deriv_sst: Generates json file for the derivative SST behavioral data
#'
#' This function generates 2 json files for SST summary data reported in derivatives/beh_databases: (1) summary metrics computed by ad run, (2) summary metrics computed by block
#'
#' @return A list of 2 strings (labels: XX, sst_byblock_json) with data stored in JSON format containing meta-data the summary SST database
#'
#'
#' @export

json_deriv_sst <- function() {

  # JSON for dataframe by block ----

  sst_byblock_list <- list(
    'FileLevelMetadata' = list(
      Description = "SST behavior summarized by block",
      Sources = "bids::rawdata/sub*/ses-1/func/sub*sst*events.tsv"),
    sub = list(Description = 'participant id number'),
    type = list(Description = 'Type of task',
                Levels = list ('Beh' = 'task for behavioral administration (no fmri)',
                               'fMRI' = 'task for administration during fMRI')),
    run_num = list(Description = 'SST run number (1-6)'),
    block_num = list(Description = 'Block number within a run (1-2)'),
    commercial_cond = list( Description = 'commercial condition',
                            Levels = list ('food' = 'food',
                                           'toy' = 'toy')),
    img_cat = list(Description = 'block taste category',
                    Levels = list ('sweet' = 'sweet foods',
                                      'savory' = 'savory foods')),
    racehorse_check = list(Description = 'check of the racehorse assumption for the Stop-Signal Task: mean go reaction time (go_rt_mean) > mean reaction time during unsuccessful stops (us_rt_mean)',
                            Levels = list ('1' = 'met assumption',
                                           '0' = 'failed to meet assumption',
                                           'n/a' = 'not applicable, e.g., no unsuccesful stops in given subset of trials'),
                            Derivative = TRUE),
    n_stop_trials = list(Description = 'number of stop trials',
                        Derivative = TRUE),
    n_go_trials = list(Description = 'number of go trials',
                      Derivative = TRUE),
    go_rt_mean = list(Description = 'average reaction time on go trials with responses',
                      Unit = "ms",
                      Derivative = TRUE),
    n_go_cor = list(Description = 'number of correct go (left/right) responses',
                    Derivative = TRUE),
    go_correct_rt_mean = list(Description = 'average reaction time for correct go responses',
                              Unit = "ms",
                              Derivative = TRUE),
    n_go_error = list(Description = 'number of go trial errors (left/right)',
                      Derivative = TRUE),
    go_error_rt_mean = list(Description = 'average reaction time on go trials with errors',
                            Unit = "ms",
                            Derivative = TRUE),
    n_go_miss = list( Description = 'number of missed go trial responses',
                       Derivative = TRUE),
    prop_stop_fail = list( Description = 'proportion of of failed stop trials',
                       Derivative = TRUE),
    us_rt_mean = list( Description = 'average reaction time on unsucessful stop trials ',
                       Unit = "ms",
                       Derivative = TRUE),
    ssd_mean = list(Description = 'average stop-signal delay',
                    Unit = "ms",
                    Derivative = TRUE),
    ssrt_mean = list( Description = 'stop-signal reaction time calculated using the mean method (mean go rt - mean ssd). Only calcualted if racehorse assumption is met',
                       Unit = "ms",
                       Derivative = TRUE),
    ssrt_int = list( Description = 'stop-signal reaction time calculated using the integration method (nth rt - mean ssd). Only calcualted if racehorse assumption is met',
                      Unit = "ms",
                      Derivative = TRUE)
  )

  # convert and return JSONS ----

  # convert formatting to JSON
  # sst_byrun_json <- RJSONIO::toJSON(sst_byrun_list, pretty = TRUE)
  sst_byblock_json <- RJSONIO::toJSON(sst_byblock_list, pretty = TRUE)

  # double check
  # if (isFALSE(RJSONIO::isValidJSON(sst_byrun_json, asText = TRUE))){
  #   print('sst_byrun_json JSON file may be invalid')
  # }

  if (isFALSE(RJSONIO::isValidJSON(sst_byblock_json, asText = TRUE))){
    print('sst_byblock_json JSON file may be invalid')
  }

  return(list(
    #sst_byrun_json = sst_byrun_json,
              sst_byblock_json = sst_byblock_json))

}
