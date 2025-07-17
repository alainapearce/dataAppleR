#' json_v3v4v5_intake: Generates a json file for intake data for visits 3-5
#'
#' This function generates a json file for intake data for visits 3-5
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_v3v4v5_intake <- function() {

  v3v4v5_intake_list <- list(
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
    brownie_pre_w_o_plate = list( Description = 'Pre-EAH weight of brownies without plate',
                                  Unit = 'grams'),
    brownie_pre_w_plate = list( Description = 'Pre-EAH weight of brownies with plate',
                                Unit = 'grams'),
    brownie_post_w_plate = list( Description = 'Post-EAH weight of brownies with plate',
                                 Unit = 'grams'),

    corn_chip_pre_w_o_plate = list( Description = 'Pre-EAH weight of corn chips without plate',
                                    Unit = 'grams'),
    corn_chip_pre_w_plate = list( Description = 'Pre-EAH weight of corn chips with plate',
                                  Unit = 'grams'),
    corn_chip_post_w_plate = list( Description = 'Post-EAH weight of corn chips with plate',
                                   Unit = 'grams'),

    kiss_pre_w_o_plate = list( Description = 'Pre-EAH weight of Hersheys kiss without plate',
                               Unit = 'grams'),
    kiss_pre_w_plate = list( Description = 'Pre-EAH weight of Hersheys kiss with plate',
                             Unit = 'grams'),
    kiss_post_w_plate = list( Description = 'Post-EAH weight of Hersheys kiss with plate',
                              Unit = 'grams'),

    ice_cream_pre_w_o_plate = list( Description = 'Pre-EAH weight of Ice cream without plate',
                                    Unit = 'grams'),
    ice_cream_pre_w_plate = list( Description = 'Pre-EAH weight of Ice cream with plate',
                                  Unit = 'grams'),
    ice_cream_post_w_plate = list( Description = 'Post-EAH weight of Ice cream with plate',
                                   Unit = 'grams'),

    oreo_pre_w_o_plate = list( Description = 'Pre-EAH weight of oreos without plate',
                               Unit = 'grams'),
    oreo_pre_w_plate = list( Description = 'Pre-EAH weight of oreos with plate',
                             Unit = 'grams'),
    oreo_post_w_plate = list( Description = 'Post-EAH weight of oreos with plate',
                              Unit = 'grams'),

    popcorn_pre_w_o_plate = list( Description = 'Pre-EAH weight of popcorn without plate',
                                  Unit = 'grams'),
    popcorn_pre_w_plate = list( Description = 'Pre-EAH weight of popcorn with plate',
                                Unit = 'grams'),
    popcorn_post_w_plate = list( Description = 'Post-EAH weight of popcorn with plate',
                                 Unit = 'grams'),

    pretzel_pre_w_o_plate = list( Description = 'Pre-EAH weight of pretzels without plate',
                                  Unit = 'grams'),
    pretzel_pre_w_plate = list( Description = 'Pre-EAH weight of pretzels with plate',
                                Unit = 'grams'),
    pretzel_post_w_plate = list( Description = 'Post-EAH weight of pretzels with plate',
                                 Unit = 'grams'),

    skittle_pre_w_o_plate = list( Description = 'Pre-EAH weight of skittles without plate',
                                  Unit = 'grams'),
    skittle_pre_w_plate = list( Description = 'Pre-EAH weight of skittles with plate',
                                Unit = 'grams'),
    skittle_post_w_plate = list( Description = 'Post-EAH weight of skittles with plate',
                                 Unit = 'grams'),

    starburst_pre_w_o_plate = list( Description = 'Pre-EAH weight of starbursts without plate',
                                    Unit = 'grams'),
    starburst_pre_w_plate = list( Description = 'Pre-EAH weight of starbursts with plate',
                                  Unit = 'grams'),
    starburst_post_w_plate = list( Description = 'Post-EAH weight of starbursts with plate',
                                   Unit = 'grams'),

    water_eah_pre_w_o_plate = list( Description = 'Pre-EAH weight of water without container',
                                    Unit = 'grams'),
    water_eah_pre_w_plate = list( Description = 'Pre-EAH weight of water with container',
                                  Unit = 'grams'),
    water_eah_post_w_plate = list( Description = 'Post-EAH weight of water with container',
                                   Unit = 'grams'),
    bread_pre_w_o_plate = list( Description = 'Pre-meal weight of bread (grilled cheese component) without plate',
                                Unit = 'grams'),
    butter_pre_w_o_plate = list( Description = 'Pre-meal weight of butter (grilled cheese component) on bread slice A without plate',
                                 Unit = 'grams'),
    butter_2_pre_w_o_plate = list( Description = 'Pre-meal weight of butter (grilled cheese component) on bread slice B without plate',
                                   Unit = 'grams'),
    cheese_pre_w_o_plate = list( Description = 'Pre-meal weight of cheese (grilled cheese component) without plate',
                                 Unit = 'grams'),

    grilled_cheese_pre_w_o_plate = list( Description = 'Pre-meal weight of grilled cheese without plate',
                                         Unit = 'grams'),
    grilled_cheese_pre_w_plate = list( Description = 'Pre-meal weight of grilled cheese with plate',
                                       Unit = 'grams'),
    grilled_cheese_post_w_plate = list( Description = 'Post-meal weight of grilled cheese with plate',
                                        Unit = 'grams'),

    tender_pre_w_o_plate = list( Description = 'Pre-meal weight of chicken tender without plate',
                                 Unit = 'grams'),
    tender_pre_w_plate = list( Description = 'Pre-meal weight of chicken tender with plate',
                               Unit = 'grams'),
    tender_post_w_plate = list( Description = 'Post-meal weight of chicken tender with plate',
                                Unit = 'grams'),

    carrot_pre_w_o_plate = list( Description = 'Pre-meal weight of carrots without plate',
                                 Unit = 'grams'),
    carrot_pre_w_plate = list( Description = 'Pre-meal weight of carrots with plate',
                               Unit = 'grams'),
    carrot_post_w_plate = list( Description = 'Post-meal weight of carrots with plate',
                                Unit = 'grams'),

    chips_pre_w_o_plate = list( Description = 'Pre-meal weight of potato chips without plate',
                                Unit = 'grams'),
    chips_pre_w_plate = list( Description = 'Pre-meal weight of potato chips with plate',
                              Unit = 'grams'),
    chips_post_w_plate = list( Description = 'Post-meal weight of potato chips with plate',
                               Unit = 'grams'),

    fruit_pre_w_o_plate = list( Description = 'Pre-meal weight of fruit cocktail without plate',
                                Unit = 'grams'),
    fruit_pre_w_plate = list( Description = 'Pre-meal weight of fruit cocktail with plate',
                              Unit = 'grams'),
    fruit_post_w_plate = list( Description = 'Post-meal weight of fruit cocktail with plate',
                               Unit = 'grams'),

    water_pre_w_o_plate = list( Description = 'Pre-meal weight of water without container',
                                Unit = 'grams'),
    water_pre_w_plate = list( Description = 'Pre-meal weight of water with container',
                              Unit = 'grams'),
    water_post_w_plate = list( Description = 'Post-meal weight of water with container',
                               Unit = 'grams'),

    ranch_pre_w_o_plate = list( Description = 'Pre-meal weight of ranch dressing without container',
                                Unit = 'grams'),
    ranch_pre_w_plate = list( Description = 'Pre-meal weight of ranch dressing with container',
                              Unit = 'grams'),
    ranch_post_w_plate = list( Description = 'Post-meal weight of ranch dressing with container',
                               Unit = 'grams'),

    ketchup_pre_w_o_plate = list( Description = 'Pre-meal weight of ketchup without container',
                                Unit = 'grams'),
    ketchup_pre_w_plate = list( Description = 'Pre-meal weight of ketchup with container',
                              Unit = 'grams'),
    ketchup_post_w_plate = list( Description = 'Post-meal weight of ketchup with container',
                               Unit = 'grams')
  )

  # convert formatting to JSON
  v3v4v5_intake_json <- RJSONIO::toJSON(v3v4v5_intake_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(v3v4v5_intake_json, asText = TRUE))){
    print('Intake visit 3-5 JSON file may be invalid')
  }

  return(v3v4v5_intake_json)

}
