#' json_anthro: Generates a json file for raw anthropometrics data collected during the visit
#'
#' This function generates a json file for raw anthropometrics data collected during the visit
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_anthro <- function() {

  anthro_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1',
                                          '5'	= 'Child visit protocol 5')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    child_height_1_cm = list( Description = 'child height measurement 1',
                             Unit = "cm"),
    child_height_2_cm = list( Description = 'child height measurement 2',
                              Unit = "cm"),
    child_weight_1_kg = list( Description = 'child weight measurement 1',
                              Unit = "kg"),
    child_weight_2_kg = list( Description = 'child weight measurement 2',
                              Unit = "kg"),
    child_height_average = list( Description = 'average of child_height_1_cm and child_height_2_cm',
                                 Unit = "cm",
                                 Derivative = TRUE),
    child_weight_average = list( Description = 'average of child_weight_1_kg and child_weight_2_kg',
                                 Unit = "kg",
                                 Derivative = TRUE),
    parent1_sex = list( Description = 'Sex of parent being measured',
                        Levels = list ('female' = 'female',
                                       'male' = 'male')),
    parent1_height_1_cm = list( Description = 'parent height measurement 1',
                                Unit = "cm"),
    parent1_height_2_cm = list( Description = 'parent height measurement 2',
                                Unit = "cm"),
    parent1_weight_1_kg = list( Description = 'parent weight measurement 1',
                                Unit = "kg"),
    parent1_weight_2_kg = list( Description = 'parent weight measurement 2',
                                Unit = "kg"),
    parent1_height_average_cm = list( Description = 'average of parent1_height_1_cm and parent1_height_2_cm',
                                      Unit = "cm",
                                      Derivative = TRUE),
    parent1_weight_average_kg = list( Description = 'average of parent1_weight_1_kg and parent1_weight_2_kg',
                                      Unit = "kg",
                                      Derivative = TRUE)
  )

  # convert formatting to JSON
  anthro_json <- RJSONIO::toJSON(anthro_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(anthro_json, asText = TRUE))){
    print('Anthropometrics data for collected during a visit JSON file may be invalid')
  }

  return(anthro_json)
}
