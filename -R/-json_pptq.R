#' json_pptq: Generates a json file for the Pictorial Personality Traits Questionnaire for Children
#'
#' This function generates a json file for the scored Pictorial Personality Traits Questionnaire for Children  and raw participant responses.
#'
#' @return A string with data stored in JSON format containing meta-data for the Pictorial Personality Traits Questionnaire for Children
#'
#'
#' @export

json_pptq <- function() {

  pptq_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Pictorial Personality Traits Questionnaire for Children. Participants were provided the following instructions: "What kind of person are you? What do you usually do; On this survey, you\'ll find descriptions of different situations. Here is one example: "When it rains..." The situations are shown in pictures. In each picture, the leading character is wearing a striped scaft. Think about it, what do you do most often in such a situation? Are you more similar to the person with the scarf in the left or right picture? This is important! In every situation, you can only choose one small square.  If sometimes you behave in one way and other times in another way, mark the box in the middle ("It depends")."',
      Reference = 'Mackiewicz M, Cieciuch J. Pictorial Personality Traits Questionnaire for Children (PPTQ-C)-A New Measure of Children\'s Personality Traits. Front Psychol. 2016 Apr 14;7:498. doi: 10.3389/fpsyg.2016.00498. PMID: 27252661; PMCID: PMC4879772.',
      TermURL = 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4879772/'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit this child-reported survey was completed',
                       Unit = 'YYYY-MM-DD'),
    pptq1 = list( Description = 'I usually play...',
                  Levels = list ('1' =	'On my own',
                                 '2'	= 'It depends',
                                 '3'	= 'With others')),
    pptq2 = list( Description = 'When I go to school...',
                  Levels = list ('1' =	'I often feel worried',
                                 '2'	= 'It depends',
                                 '3'	= 'I don\'t feel worried')),
    pptq3 = list( Description = 'When I see birds flying...',
                  Levels = list ('1' =	'it doesn\'t impress me',
                                 '2'	= 'It depends',
                                 '3'	= 'it impresses me a lot')),
    pptq4 = list( Description = 'When I am asked to do housework...',
                  Levels = list ('1' =	'I am usually willing to do it',
                                 '2'	= 'It depends',
                                 '3'	= 'I am unwilling to do it')),
    pptq5 = list( Description = 'When a classmate needs something...',
                  Levels = list ('1' =	'I don\'t notice it',
                                 '2'	= 'It depends',
                                 '3'	= 'I notice it and help them')),
    pptq6 = list( Description = 'When other children play...',
                  Levels = list ('1' =	'I join them',
                                 '2'	= 'It depends',
                                 '3'	= 'I don\'t join them')),
    pptq7 = list( Description = 'When something goes wrong...',
                  Levels = list ('1' =	'I stay calm',
                                 '2'	= 'It depends',
                                 '3'	= 'I get nervous quickly')),
    pptq8 = list( Description = 'When I am on a trip, I mostly enjoy...',
                  Levels = list ('1' =	'exploring and discovering new things',
                                 '2'	= 'It depends',
                                 '3'	= 'having fun and relaxing')),
    pptq9 = list( Description = 'My bedroom is...',
                  Levels = list ('1' =	'messy',
                                 '2'	= 'It depends',
                                 '3'	= 'tidy')),
    pptq10 = list( Description = 'When I see that I can help somebody...',
                   Levels = list ('1' =	'I help them',
                                  '2'	= 'It depends',
                                  '3'	= 'I don\'t help them')),
    pptq11 = list( Description = 'When someone jokes...',
                   Levels = list ('1' =	'it doesn\'t make me laugh',
                                  '2'	= 'It depends',
                                  '3'	= 'I laugh with them')),
    pptq12 = list( Description = 'Usually..',
                   Levels = list ('1' =	'I worry about things',
                                  '2'	= 'It depends',
                                  '3'	= 'I don\'t worry about things')),
    pptq13 = list( Description = 'Learning new things...',
                   Levels = list ('1' =	'is not enjoyable for me',
                                  '2'	= 'It depends',
                                  '3'	= 'is enjoyable for me')),
    pptq14 = list( Description = 'When I am given money...',
                   Levels = list ('1' =	'I save it for later',
                                  '2'	= 'It depends',
                                  '3'	= 'I spend it straight away')),
    pptq15 = list( Description = 'When I have something new...',
                   Levels = list ('1' =	'I don\'t lend it to anybody',
                                  '2'	= 'It depends',
                                  '3'	= 'I lend it to others')),
    pptq_extraversion = list( Description = 'Extraversion',
                       Derivative = TRUE),
    pptq_neuroticism = list( Description = 'Neuroticism',
                              Derivative = TRUE),
    pptq_openness = list( Description = 'Openness',
                              Derivative = TRUE),
    pptq_conscientiousness = list( Description = 'Conscientiousness',
                              Derivative = TRUE),
    pptq_agreeableness = list( Description = 'Agreeableness',
                            Derivative = TRUE)
    )


  # convert formatting to JSON
  pptq_json <- RJSONIO::toJSON(pptq_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(pptq_json, asText = TRUE))){
    print('pptq JSON file may be invalid')
  }

  return(pptq_json)

}
