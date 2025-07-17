#' json_foodview: Generates a json file for the food view fmri events (data in rawdata/func)
#'
#' This function generates a json file for cleaned food view task data collected during fmri (onsets, responses)
#'
#' @return A string with data stored in JSON format containing meta-data for the food view task
#'
#'
#' @export

json_foodview <- function() {

  foodview_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Food View Task developed in Keller Lab for Project REACH'),
    onset = list( Description = 'onset time of stimulus relative to start of run. Derived from sys_onset_time by subtracting the first sys_onset_time in a run from all sys_onset_time in that run and dividing by 1000',
                  Unit = "seconds",
                  Derivative = TRUE),
    duration = list( Description = 'stimulus duration. Calculated by subtracting the onset of the present stimulus from the onset of the subsequent stimulus',
                     Derivative = TRUE,
                     Unit = "seconds"),
    sub = list( Description = 'Participant ID'),
    run_num = list( Description = 'Food View Task run number',
                Levels = list ('1' = 'run 1',
                               '2' = 'run 2',
                               '3' = 'run 3',
                               '4' = 'run 4')),
    commercial_cond = list( Description = 'commercial condition',
                            Levels = list ('food' = 'food',
                                           'toy' = 'toy')),
    stim_file_name = list( Description = 'basename of stimulus file'),
    response = list( Description = 'response to question "Do you want to eat this right now?',
                     Levels = list ('0' = 'no response',
                                    '1' = 'left response button (Yes)',
                                    '2' = 'right response button (No)',
                                    '-99' = 'Response other than 1 or 2. For the first 2 particpants (3, 6), -99 responses resulted from scanner pulses accidently interfering with response collection. The MATLAB program was then modified to only capture responses left and right buttons')),
    response_time = list( Description = 'response time',
                          Unit = "milliseconds"),
    food_ed = list( Description = 'food image energy density category',
                 Levels = list ('low' = 'low energy density',
                                'high' = 'high energy density')),
    food_taste = list( Description = 'food image taste category',
                  Levels = list ('sweet' = 'sweet',
                                 'savory' = 'savory')),
    sys_onset_time = list( Description = 'System onset time of stimulus',
                          Unit = "milliseconds")
  )


  # convert formatting to JSON
  foodview_json <- RJSONIO::toJSON(foodview_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(foodview_json, asText = TRUE))){
    print('food view events JSON file may be invalid')
  }

  return(foodview_json)

}
