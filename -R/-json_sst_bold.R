#' json_sst_bold: Generates a json file for the SST fmri events (data in rawdata/func)
#'
#' This function generates a json file for cleaned stop signal data collected during fmri (onsets, responses)
#'
#' @return A string with data stored in JSON format containing meta-data for the XXX
#'
#'
#' @export

json_sst_bold <- function() {

  sst_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Stop Signal Task developed in Keller Lab for Project REACH'),
    onset = list( Description = 'onset time relative to start of run',
                  Unit = "seconds"),
    duration = list( Description = 'stimulus duration. Calculated by subtracting the onset for the subsequent stimulus from the onset of the present stimulus',
                     Derivative = TRUE,
                      Unit = "seconds"),
    sub = list( Description = 'participant ID'),
    run_num = list( Description = 'SST run number',
                Levels = list ('1' = 'run 1',
                               '2' = 'run 2',
                               '3' = 'run 3',
                               '4' = 'run 4',
                               '5' = 'run 5',
                               '6' = 'run 6')),
    set = list( Description = 'run set (A-F)'),
    run_cond = list( Description = 'run condition, based on the type of commercials shown during the run',
                     Levels = list ('food' = 'food',
                                    'toy' = 'toy')),
    stim_file_name = list( Description = 'basename of stimulus file'),
    trial_num = list( Description = ''),
    type = list( Description = 'task type',
                 Levels = list ('Prac' = 'pratice task',
                                'Beh' = 'behavioral task',
                                'fMRI' = 'fmri task')),
    block = list( Description = 'block number within a run',
                  Levels = list ('1' = 'first block',
                                 '2' = 'second block')),
    img_cat = list( Description = 'food image category',
                    Levels = list ('sweet' = 'sweet',
                                   'savory' = 'savory')),
    go_stim = list( Description = 'go signal (napkin) side',
                 Levels = list ('1' = 'left side',
                                '2' = 'right side')),
    signal = list( Description = '',
                 Levels = list ('0' = 'no stop signal given (go trial)',
                                '1' = 'stop signal given (stop trial)')),
    reqSSD = list( Description = 'requested stop signal delay'),
    correct = list( Description = '',
                    Levels = list ('1' = 'missed go response',
                                   '2' = 'incorrect go response (go_stim != response)',
                                   '3' = 'failed stop trial',
                                   '4' = 'correct response (successful stop or correct go response)'),
                    Derivative = TRUE),
    response = list( Description = 'response',
                  Levels = list ('0' = 'no response',
                                 '1' = 'left side',
                                 '2' = 'right side')),
    trueSSD = list( Description = 'true stop signal delay (signal onset - go onset)'),
    response_time = list( Description = 'response time',
                          Unit = "seconds")
  )


  # convert formatting to JSON
  sst_json <- RJSONIO::toJSON(sst_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(sst_json, asText = TRUE))){
    print('sst events JSON file may be invalid')
  }

  return(sst_json)

}
