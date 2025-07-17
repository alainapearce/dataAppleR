#' json_eah_wanting: Generates a json file for the Eating in the Absence of Hunger (EAH) Wanting Questionnaire
#'
#' This function generates a json file for the Absence of Hunger (EAH) Wanting Questionnaire
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_eah_wanting <- function() {

  eah_wanting_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Eating in the Absence of Hunger (EAH) Wanting Questionnaire. Participants (parents) were provided the following instructions: "For this next part we want to know how much you want different items. The sides say "Not at all" (0) and "A lot" (100). If you do not want the item at all, you should point to the "Not at all" part of the slide. If you REALLY want the item, you should point to the "A lot" part of the slide. If you\'re somewhere in between, point to where the circle should be"'),
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
    advertisement_condition = list( Description = 'Advertisement Condition',
                                    Levels = list ('0' =	'Food',
                                                   '1' =	'Toy',
                                                   '2'	= 'Boring')),
    eah_wanting_water = list( Description = 'EAH Wanting Questionnaire example: How much do you want to drink water right now?'),
    eah_wanting_corn_chips = list( Description = 'EAH Wanting Questionnaire item: How much do you want to eat corn chips right now?'),
    eah_wanting_pretzels = list( Description = 'EAH Wanting Questionnaire item: How much do you want to eat pretzels right now?'),
    eah_wanting_starburst = list( Description = 'EAH Wanting Questionnaire item: How much do you want to eat Starbursts right now?'),
    eah_wanting_brownies = list( Description = 'EAH Wanting Questionnaire item: How much do you want to eat brownies right now?'),
    eah_wanting_markers = list( Description = 'EAH Wanting Questionnaire item: How much do you want to play with these markers right now?'),
    eah_wanting_crayons = list( Description = 'EAH Wanting Questionnaire item: How much do you want to play with these crayons right now?'),
    eah_wanting_ice_cream = list( Description = 'EAH Wanting Questionnaire item: How much do you want to eat ice cream right now?'),
    eah_wanting_color_book_2 = list( Description = 'EAH Wanting Questionnaire item: How much do you want to play with this coloring book right now?'),
    eah_wanting_oonies_2 = list( Description = 'EAH Wanting Questionnaire item: How much do you want to play with this Oonies Inflator right now?'),
    eah_wanting_pencils = list( Description = 'EAH Wanting Questionnaire item: How much do you want to play with these colored pencils right now?'),
    eah_wanting_activity_book = list( Description = 'EAH Wanting Questionnaire item: How much do you want to play with this activity book right now?'),
    eah_wanting_oreos = list( Description = 'EAH Wanting Questionnaire item: How much do you want to eat Oreos right now?'),
    eah_wanting_popcorn = list( Description = 'EAH Wanting Questionnaire item: How much do you want to eat popcorn right now?'),
    eah_wanting_color_book_1 = list( Description = 'EAH Wanting Questionnaire item: How much do you want to play with this coloring book right now?'),
    eah_wanting_legos = list( Description = 'EAH Wanting Questionnaire item: How much do you want to play with Legos right now?'),
    eah_wanting_squeakee = list( Description = 'EAH Wanting Questionnaire item: How much do you want to play with Squeakee the Balloon Dog right now?'),
    eah_wanting_hershey_kiss = list( Description = 'EAH Wanting Questionnaire item: How much do you want to eat Hershey kisses right now?'),
    eah_wanting_oonies = list( Description = 'EAH Wanting Questionnaire item: How much do you want to play with Oonies right now?'),
    eah_wanting_skittles = list( Description = 'EAH Wanting Questionnaire item: How much do you want to eat skittles right now?'))

  # convert formatting to JSON
  eah_wanting_json <- RJSONIO::toJSON(eah_wanting_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(eah_wanting_json, asText = TRUE))){
    print('EAH wanting JSON file may be invalid')
  }

  return(eah_wanting_json)

}
