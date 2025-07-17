#' json_liking: Generates a json file for visits 1 and 5 VAS liking data
#'
#' This function generates a json file for visits 1 and 5 VAS liking data
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_liking <- function() {

  liking_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Visual Analog Liking Scale - 5 faces'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit = list( Description = 'Visit number',
                  Levels = list( '1' = 'visit 1',
                                 '3' = 'visit 1',
                                 '4' = 'visit 1',
                                 '5' = 'visit 1')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    liking_popcorn = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this popcorn?',
                           Levels = list ('0' =	'Hate it',
                                          '1'	= 'Dislike it',
                                          '2'	= 'It\'s okay',
                                          '3'	= 'Like it',
                                          '4'	= 'Love it')),
    liking_pretzel = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this pretzel?',
                           Levels = list ('0' =	'Hate it',
                                          '1'	= 'Dislike it',
                                          '2'	= 'It\'s okay',
                                          '3'	= 'Like it',
                                          '4'	= 'Love it')),
    liking_corn_chip = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this corn chip?',
                             Levels = list ('0' =	'Hate it',
                                            '1'	= 'Dislike it',
                                            '2'	= 'It\'s okay',
                                            '3'	= 'Like it',
                                            '4'	= 'Love it')),
    liking_oreo = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this cookie?',
                        Levels = list ('0' =	'Hate it',
                                       '1'	= 'Dislike it',
                                       '2'	= 'It\'s okay',
                                       '3'	= 'Like it',
                                       '4'	= 'Love it')),
    liking_brownie = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this brownie?',
                           Levels = list ('0' =	'Hate it',
                                          '1'	= 'Dislike it',
                                          '2'	= 'It\'s okay',
                                          '3'	= 'Like it',
                                          '4'	= 'Love it')),
    liking_starburst = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this Starburst?',
                             Levels = list ('0' =	'Hate it',
                                            '1'	= 'Dislike it',
                                            '2'	= 'It\'s okay',
                                            '3'	= 'Like it',
                                            '4'	= 'Love it')),
    liking_skittle = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this Skittle?',
                           Levels = list ('0' =	'Hate it',
                                          '1'	= 'Dislike it',
                                          '2'	= 'It\'s okay',
                                          '3'	= 'Like it',
                                          '4'	= 'Love it')),
    liking_chocolate = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this chocolate (Hershey Kiss)?',
                             Levels = list ('0' =	'Hate it',
                                            '1'	= 'Dislike it',
                                            '2'	= 'It\'s okay',
                                            '3'	= 'Like it',
                                            '4'	= 'Love it')),
    liking_icecream = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this ice cream?',
                            Levels = list ('0' =	'Hate it',
                                           '1'	= 'Dislike it',
                                           '2'	= 'It\'s okay',
                                           '3'	= 'Like it',
                                           '4'	= 'Love it')),
    liking_grilled_cheese = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this grilled cheese?',
                                  Levels = list ('0' =	'Hate it',
                                                 '1'	= 'Dislike it',
                                                 '2'	= 'It\'s okay',
                                                 '3'	= 'Like it',
                                                 '4'	= 'Love it')),
    liking_chicken_tender = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this chicken tender?',
                                  Levels = list ('0' =	'Hate it',
                                                 '1'	= 'Dislike it',
                                                 '2'	= 'It\'s okay',
                                                 '3'	= 'Like it',
                                                 '4'	= 'Love it')),
    liking_potato_chips = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this potato chip?',
                                Levels = list ('0' =	'Hate it',
                                               '1'	= 'Dislike it',
                                               '2'	= 'It\'s okay',
                                               '3'	= 'Like it',
                                               '4'	= 'Love it')),
    liking_carrot = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this baby carrot?',
                          Levels = list ('0' =	'Hate it',
                                         '1'	= 'Dislike it',
                                         '2'	= 'It\'s okay',
                                         '3'	= 'Like it',
                                         '4'	= 'Love it')),
    liking_fruit = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this fruit cocktail',
                         Levels = list ('0' =	'Hate it',
                                        '1'	= 'Dislike it',
                                        '2'	= 'It\'s okay',
                                        '3'	= 'Like it',
                                        '4'	= 'Love it')),
    liking_water = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this water?',
                         Levels = list ('0' =	'Hate it',
                                        '1'	= 'Dislike it',
                                        '2'	= 'It\'s okay',
                                        '3'	= 'Like it',
                                        '4'	= 'Love it'))
  )

  # convert formatting to JSON
  liking_json <- RJSONIO::toJSON(liking_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(liking_json, asText = TRUE))){
    print('Liking for visits 1 and 5 JSON file may be invalid')
  }

  return(liking_json)
}
