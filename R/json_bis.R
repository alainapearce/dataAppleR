#' json_bis: Generates a json file for the Body Image scale
#'
#' This function generates a json file for the scored Body Image scale and raw participant responses.
#'
#' @return A string with data stored in JSON format containing meta-data for the Body Image scale
#'
#'
#' @export

json_bis <- function() {

  bis_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Body Image Scale. Trained research assistants adminsitered the scale the following: I want you to look at the pictures of different chidlren. Each picture shows a child with a different body shape.',
      Reference = 'NEED',
      TermURL = 'NEED'),
    participant_id = list( Description = 'participant id number'),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1',
                                          '2' =	'Child visit protocol 2',
                                          '3' =	'Child visit protocol 3',
                                          '4'	= 'Child visit protocol 4',
                                          '5'	= 'Child visit protocol 5')),
    visit_date = list( Description = 'Date of visit this parent-reported survey was completed',
                       Unit = 'YYYY-MM-DD'),
    bis_1 = list( Description = 'Out of these pictures, which body shape looks most like yours?',
                 Levels = list ('1' = 'Picture A',
                                '2' = 'Picture B',
                                '3' = 'Picture C',
                                '4' = 'Picture D',
                                '5' = 'Picture E',
                                '6' = 'Picture F',
                                '7' = 'Picture G',
                                '8' = 'Prefer not to answer')),
    bis_2 = list( Description = 'Out of these pictures, which body shape would you most like to have?',
                  Levels = list ('1' = 'Picture A',
                                 '2' = 'Picture B',
                                 '3' = 'Picture C',
                                 '4' = 'Picture D',
                                 '5' = 'Picture E',
                                 '6' = 'Picture F',
                                 '7' = 'Picture G',
                                 '8' = 'Prefer not to answer')),
    bis_score = list( Description = 'Total score',
                     Derivative = TRUE))

  # convert formatting to JSON
  bis_json <- RJSONIO::toJSON(bis_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(bis_json, asText = TRUE))){
    print('Body Image Scale JSON file may be invalid')
  }

  return(bis_json)

}
