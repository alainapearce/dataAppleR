#' json_liking_v5: Generates a json file for VAS liking data for visit 5
#'
#' This function generates a json file for VAS liking data  for visit 5
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_liking_v5 <- function() {

  liking_list_v5 <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Visual Analog Liking Scale - 5 faces'),
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
    liking_preload = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this preload?',
                           Levels = list ('0' =	'Super Bad',
                                          '1'	= 'Bade',
                                          '2'	= 'Maybe Good or Maybe Bad',
                                          '3'	= 'Good',
                                          '4'	= 'Super Good',
                                          '5' = 'Not Applicable (i.e., no preload condition)')),
    liking_mac = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this macaroni and cheese?',
                           Levels = list ('0' =	'Super Bad',
                                          '1'	= 'Bade',
                                          '2'	= 'Maybe Good or Maybe Bad',
                                          '3'	= 'Good',
                                          '4'	= 'Super Good')),
    liking_broccoli= list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this broccoli?',
                           Levels = list ('0' =	'Super Bad',
                                          '1'	= 'Bade',
                                          '2'	= 'Maybe Good or Maybe Bad',
                                          '3'	= 'Good',
                                          '4'	= 'Super Good')),
    liking_graham = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this graham cracker?',
                          Levels = list ('0' =	'Super Bad',
                                         '1'	= 'Bade',
                                         '2'	= 'Maybe Good or Maybe Bad',
                                         '3'	= 'Good',
                                         '4'	= 'Super Good')),
    liking_grape = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this grape?',
                         Levels = list ('0' =	'Super Bad',
                                        '1'	= 'Bade',
                                        '2'	= 'Maybe Good or Maybe Bad',
                                        '3'	= 'Good',
                                        '4'	= 'Super Good')),
    liking_carrot = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this carrot?',
                          Levels = list ('0' =	'Super Bad',
                                         '1'	= 'Bade',
                                         '2'	= 'Maybe Good or Maybe Bad',
                                         '3'	= 'Good',
                                         '4'	= 'Super Good')),
    liking_water = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this water?',
                         Levels = list ('0' =	'Super Bad',
                                        '1'	= 'Bade',
                                        '2'	= 'Maybe Good or Maybe Bad',
                                        '3'	= 'Good',
                                        '4'	= 'Super Good')),
    chip_liking = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this chip?',
                         Levels = list ('0' =	'Super Bad',
                                        '1'	= 'Bade',
                                        '2'	= 'Maybe Good or Maybe Bad',
                                        '3'	= 'Good',
                                        '4'	= 'Super Good')),
    mm_liking = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this M&M?',
                         Levels = list ('0' =	'Super Bad',
                                        '1'	= 'Bade',
                                        '2'	= 'Maybe Good or Maybe Bad',
                                        '3'	= 'Good',
                                        '4'	= 'Super Good')),
    brownie_liking = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this brownie?',
                         Levels = list ('0' =	'Super Bad',
                                        '1'	= 'Bade',
                                        '2'	= 'Maybe Good or Maybe Bad',
                                        '3'	= 'Good',
                                        '4'	= 'Super Good')),
    cookie_liking = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this cookie?',
                         Levels = list ('0' =	'Super Bad',
                                        '1'	= 'Bade',
                                        '2'	= 'Maybe Good or Maybe Bad',
                                        '3'	= 'Good',
                                        '4'	= 'Super Good')),
    starburst_liking = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this starburst?',
                         Levels = list ('0' =	'Super Bad',
                                        '1'	= 'Bade',
                                        '2'	= 'Maybe Good or Maybe Bad',
                                        '3'	= 'Good',
                                        '4'	= 'Super Good')),
    frito_liking = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this frito?',
                         Levels = list ('0' =	'Super Bad',
                                        '1'	= 'Bade',
                                        '2'	= 'Maybe Good or Maybe Bad',
                                        '3'	= 'Good',
                                        '4'	= 'Super Good'))
  )

  # convert formatting to JSON
  liking_json_v5 <- RJSONIO::toJSON(liking_list_v5, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(liking_json_v5, asText = TRUE))){
    print('Liking JSON file may be invalid')
  }

  return(liking_json_v5)
}
