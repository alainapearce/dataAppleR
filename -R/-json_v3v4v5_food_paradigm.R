#' json_v3v4_food_paradigm: Generates a json file for visits 3-5 food paradigm notes and data
#'
#' This function generates a json file for visit 3-5 food paradigm notes and data
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_v3v4v5_food_paradigm <- function() {

  v3v4v5_food_paradigm_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1',
                                          '3' =	'Child visit protocol 3',
                                          '4' =	'Child visit protocol 4',
                                          '5'	= 'Child visit protocol 5')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    advertisement_condition = list( Description = 'Advertisement Condition',
                                    Levels = list ('0' =	'Food',
                                                   '1' =	'Toy',
                                                   '2'	= 'Boring')),
    test_meal_book = list( Description = 'Book the child selected to listen to during the test meal'),
    test_meal_start_time = list( Description = 'Meal start time',
                                 Unit = "hh:mm"),
    test_meal_15_min_check = list( Description = 'Child was informed that there are 15 minutes remaining to eat their meal:',
                                   Levels = list ('1' =	'Yes',
                                                  '0'	= 'No')),
    test_meal_end_time = list( Description = 'Meal end time',
                               Unit = "hh:mm"),
    test_meal_duration = list( Description = 'Meal duration. Derived in redcap from test_meal_start_time and test_meal_end_time',
                               Derivative = TRUE),
    test_meal_notes = list( Description = 'Researcher notes about meal protocol'),
    eah_notes = list( Description = 'Researcher notes about eah protocol'),
    eah_prep_notes = list( Description = 'Researcher notes about EAH food preparation'),
    meal_prep_notes = list( Description = 'Researcher notes about meal food preparation')
  )

  # convert formatting to JSON
  v3v4v5_food_paradigm_json <- RJSONIO::toJSON(v3v4v5_food_paradigm_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(v3v4v5_food_paradigm_json, asText = TRUE))){
    print('food paradigm info for visits 3-5 JSON file may be invalid')
  }

  return(v3v4v5_food_paradigm_json)

}
