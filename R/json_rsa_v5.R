#' json_rsa_v5: Generates a json file for RSA during the standard visit elements
#'
#' This function generates a json file for RSA during the standard visit elements
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_rsa_v5 <- function() {

  rsa_v5_list <- list(
    participant_id = list( Description = 'participant id number'),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1',
                                          '2' =	'Child visit protocol 2',
                                          '3' =	'Child visit protocol 3',
                                          '4'	= 'Child visit protocol 4',
                                          '5'	= 'Child visit protocol 5')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    preload_condition = list( Description = 'Preload condition',
                              Levels = list ('0' = 'no preload',
                                             '1' = 'apple slices',
                                             '2' = 'apple sauce',
                                             '3' = 'apple juice',
                                             '4' = 'apple juice sweetend with non-nutritive sweetener (i.e., diet)')),
    rsa_baseline_start = list( Description = 'Start time for baseline RSA recording',
                                Unit = 'H:M:S'),
    rsa_baseline_end = list( Description = 'End time for baseline RSA recording',
                               Unit = 'H:M:S'),
    rsa_preload_start = list( Description = 'Start time for preload RSA recording',
                               Unit = 'H:M:S'),
    rsa_preload_end = list( Description = 'End time for preload RSA recording',
                               Unit = 'H:M:S'),
    rsa_meal_start = list( Description = 'Start time for meal RSA recording',
                               Unit = 'H:M:S'),
    rsa_meal_end = list( Description = 'End time for meal RSA recording',
                               Unit = 'H:M:S'),
    rsa_postmeal_end = list( Description = 'End time for the 2-minute post-meal RSA recording period',
                               Unit = 'H:M:S'),
    rsa_notes = list( Description = 'Notes on RSA including notes to indicate any information about additonal rsa_blanktime* recording times'),
    rsa_blanktime1 = list( Description = 'Misc start/end time as indicated in the rsa_notes field',
                               Unit = 'H:M:S'),
    rsa_blanktime2 = list( Description = 'Misc start/end time as indicated in the rsa_notes field',
                           Unit = 'H:M:S'),
    rsa_blanktime3 = list( Description = 'Misc start/end time as indicated in the rsa_notes field',
                           Unit = 'H:M:S'),
    rsa_blanktime4 = list( Description = 'Misc start/end time as indicated in the rsa_notes field',
                           Unit = 'H:M:S'),
    rsa_eah_start = list( Description = 'Start time for EAH RSA recording',
                           Unit = 'H:M:S'),
    rsa_eah_end = list( Description = 'End time for EAH RSA recording',
                          Unit = 'H:M:S'),
    rsa_notes_eah = list( Description = 'Notes on RSA including notes to indicate any information about additonal rsa_blanktime_eah* recording times'),
    rsa_blanktime_eah1 = list( Description = 'Misc start/end time as indicated in the rsa_notes_eah field',
                           Unit = 'H:M:S'),
    rsa_blanktime_eah2 = list( Description = 'Misc start/end time as indicated in the rsa_notes_eah field',
                               Unit = 'H:M:S'),
    rsa_blanktime_eah3 = list( Description = 'Misc start/end time as indicated in the rsa_notes_eah field',
                               Unit = 'H:M:S'),
    rsa_blanktime_eah4 = list( Description = 'Misc start/end time as indicated in the rsa_notes_eah field',
                               Unit = 'H:M:S'),
    rsa_notes_postmeal = list( Description = 'Notes on RSA postmeal')
  )

  # convert formatting to JSON
  rsa_v5_json <- RJSONIO::toJSON(rsa_v5_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(rsa_v5_json, asText = TRUE))){
    print('Visit RSA JSON file may be invalid')
  }

  return(rsa_v5_json)
}
