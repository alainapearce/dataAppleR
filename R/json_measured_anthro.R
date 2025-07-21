#' json_measured_anthro: Generates a json file for raw anthropometrics data collected during the visit
#'
#' This function generates a json file for raw anthropometrics data collected during the visit
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_measured_anthro <- function() {

  anthro_measured_list <- list(
    participant_id = list( Description = 'participant id number'),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1',
                                          '2' =	'Child visit protocol 2',
                                          '3' =	'Child visit protocol 3',
                                          '4'	= 'Child visit protocol 4',
                                          '5'	= 'Child visit protocol 5')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    parent_relationship = list( Description = 'Relationship of the parent being measured at the visit',
                                Levles = list ('0' = 'Mother',
                                               '1' = 'Father',
                                               '2' = 'Other')),
    child_height1_cm = list( Description = 'child height measurement 1',
                             Unit = "cm"),
    child_height2_cm = list( Description = 'child height measurement 2',
                              Unit = "cm"),
    child_weight1_kg = list( Description = 'child weight measurement 1',
                              Unit = "kg"),
    child_weight2_kg = list( Description = 'child weight measurement 2',
                              Unit = "kg"),
    child_height_mean = list( Description = 'average of child_height1_cm and child_height2_cm',
                                 Unit = "cm",
                                 Derivative = TRUE),
    child_weight_average = list( Description = 'average of child_weight1_kg and child_weight2_kg',
                                 Unit = "kg",
                                 Derivative = TRUE),
    parent1_height1_cm = list( Description = 'parent height measurement 1',
                                Unit = "cm"),
    parent1_height2_cm = list( Description = 'parent height measurement 2',
                                Unit = "cm"),
    parent1_weight1_kg = list( Description = 'parent weight measurement 1',
                                Unit = "kg"),
    parent1_weight2_kg = list( Description = 'parent weight measurement 2',
                                Unit = "kg"),
    parent1_height_mean_cm = list( Description = 'average of parent1_height_1_cm and parent1_height_2_cm',
                                      Unit = "cm",
                                      Derivative = TRUE),
    parent1_weight_mean_kg = list( Description = 'average of parent1_weight1_kg and parent1_weight2_kg',
                                      Unit = "kg",
                                      Derivative = TRUE)
  )

  # convert formatting to JSON
  anthro_measured_json <- RJSONIO::toJSON(anthro_measured_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(anthro_measured_json, asText = TRUE))){
    print('Anthropometrics data for collected during a visit JSON file may be invalid')
  }

  return(anthro_measured_json)
}
