#' json_parent_updates: Generates a json file for parent visit updates
#'
#' This function generates a json file for visit parent visit updates
#'
#' @return A string with data stored in JSON format containing meta-data for parent visit updates
#'
#'
#' @export

json_parent_updates <- function() {

  updates_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Updates from parents regarding fasting compliance, child health, and screening criteria'),

    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'session ID'),
    visit_date = list( Description = 'Date of visit this parent-reported survey was completed',
                       Unit = 'YYYY-MM-DD'),
    update_visit_number = list( Description = 'Visit Number:',
                                     Levels = list ('1' = '1',
                                                    '2' = '2',
                                                    '3' = '3',
                                                    '4' = '4',
                                                    '5' = '5')),
    update_fasting = list( Description = 'Fasting: Has child fasted for the past 3 hours?',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    update_med_history = list( Description = 'Are there any changes in your child\'s medical history since your last visit, including any medical, psychological, or learning disability diagnoses?',
                                    Levels = list ('0' = 'No',
                                                   '1' = 'Yes')),
    update_med_history_desc = list( Description = 'Please specify changes in medical history:'),
    update_prescription = list( Description = 'Are there any changes in your child\'s prescription or over the counter medications since your last visit?',
                                     Levels = list ('0' = 'No',
                                                    '1' = 'Yes')),
    update_prescription_desc = list( Description = 'Please specify changes in prescriptions or over the counter medications:'),
    update_dental = list( Description = 'Are there any changes in your child\'s dental work, including braces or retainers, since your last visit?',
                               Levels = list ('0' = 'No',
                                              '1' = 'Yes')),
    update_dental_desc = list( Description = 'Please specify changes in dental work:'),
    update_new_piercings = list( Description = 'Has your child acquired any new piercings since your last visit?',
                                      Levels = list ('0' = 'No',
                                                     '1' = 'Yes')),
    update_new_illness = list( Description = 'Has your child experienced a cold, flu, or any other illnesses in the past week?',
                                    Levels = list ('0' = 'No',
                                                   '1' = 'Yes')),
    update_new_illness_desc = list( Description = 'Please specify illness (specify symptoms if no diagnoses received):'),
    update_diet_change = list( Description = 'Have you noticed any changes in your child\'s dietary habits or appetite in the past 24 hours? This could include eating more or less than he/she normally does, eating a food he/she normally doesn\'t or refusing to eat a food he/she normally eats.',
                                    Levels = list ('0' = 'No',
                                                   '1' = 'Yes')),
    update_diet_change_desc = list( Description = 'Please specify changes in diet:')

  )

  # convert formatting to JSON
  updates_json <- RJSONIO::toJSON(updates_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(updates_json, asText = TRUE))){
    print('updates_json file may be invalid')
  }

  return(updates_json)

}
