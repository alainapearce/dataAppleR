#' json_fsq: Generates a json file for the Feeding Strategies Questionnaire
#'
#' This function generates a json file for the Feeding Strategies Questionnaire
#' This function is only accurate for data collected in Study REACH
#'
#' @return A string with data stored in JSON format containing meta-data for the Feeding Strategies Questionnaire
#'
#'
#' @export

json_fsq <- function() {

  fsq_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Feeding Strategies Questionnaire. Participants were provided with the following instructions: "In this section, you will be asked 13 questions about feeding strategies for your child. IN THE PAST MONTH, how frequently did you use the following strategies to determine how much to serve your child at a meal at home? Use the sliding scale to answer the question. The slider can be placed anywhere along the sliding scale." For items 1-11 the slider was ancored with Never (value 0), Somtimes (value 50), and Always (value 100)',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit this parent-reported survey was completed',
                       Unit = 'YYYY-MM-DD'),
    fsq1 = list( Description = 'I used the serving size from the Nutrition Facts Label.'),
    fsq2 = list( Description = 'I served an amount of food that looks appropriate on the plate.'),
    fsq3 = list( Description = 'I served the amount of food my child usually eats.'),
    fsq4 = list( Description = 'I used measuring tools (cups, spoons, scale).'),
    fsq5 = list( Description = 'I used restaurant portion sizes as a reference.'),
    fsq6 = list( Description = 'I used a portion size recommended by health professionals.'),
    fsq7 = list( Description = 'I served the amount of food suggested by a recipe.'),
    fsq8 = list( Description = 'I served an amount similar to what an adult in the family would eat.'),
    fsq9 = list( Description = 'I served an amount of food similar to what another child in the family would eat.'),
    fsq10 = list( Description = 'I served individually portioned foods (eg. frozen entrees or meals, individual bags of chips, containers of yogurt).'),
    fsq11 = list( Description = 'I assumed my child knew to eat an appropriate amount of food and leave the rest.'),
    fsq12 = list( Description = 'What helpful strategies have you used to determine appropriate amounts of food for your child? Explain why this was helpful. (free response)'),
    fsq13 = list( Description = 'Would you benefit from additional information about portion sizes for your child?',
                  Levels = list ('0' = 'No',
                                 '1' = 'Yes')),
    fsq_resources = list( Description = 'If so, what kind of information would you like and from where would you like to get it? (free response'))

  # convert formatting to JSON
  fsq_json <- RJSONIO::toJSON(fsq_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(fsq_json, asText = TRUE))){
    print('fsq JSON file may be invalid')
  }

  return(fsq_json)

}
