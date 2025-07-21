#' json_food_paradigm_v5: Generates a json file for visit 5 food paradigm notes and data
#'
#' This function generates a json file for visit 5food paradigm notes and data
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_food_paradigm_v5 <- function() {

  food_paradigm_list_v5 <- list(
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
    meal_start = list( Description = 'Meal start time',
                                 Unit = "hh:mm"),
    meal_notes = list( Description = 'Notes about child meal'),
    meal_end = list( Description = 'Meal end time',
                               Unit = "hh:mm"),
    meal_duration = list( Description = 'Meal duration. Derived in redcap from test_meal_start_time and test_meal_end_time',
                               Derivative = TRUE),
    eah_start = list( Description = 'EAH start time',
                       Unit = "hh:mm"),
    eah_end = list( Description = 'EAH end time',
                     Unit = "hh:mm")
  )

  # convert formatting to JSON
  food_paradigm_json_v5 <- RJSONIO::toJSON(food_paradigm_list_v5, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(food_paradigm_json_v5, asText = TRUE))){
    print('Food paradigm JSON file may be invalid')
  }

  return(food_paradigm_json_v5)

}
