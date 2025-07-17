#' json_chaos: Generates a json file for the Confusion, Hubbub, and Order Scale
#'
#' This function generates a json file for the scored Confusion, Hubbub, and Order Scale and raw participant responses.
#'
#' @return A string with data stored in JSON format containing meta-data for the Confusion, Hubbub, and Order Scale
#'
#'
#' @export

json_chaos <- function() {

  chaos_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Confusion, Hubbub, and Order Scale. Participants were provided the following instructions: "For each statement below, please assign a number between 1 and 4 to indicate how much each statement describes your home environment. Please use the following scale: 1 = Very much like your own home; 2 = somewhat like your own home; 3 = A little bit like your own home; 4 = Not at all like your own home"',
      Reference = 'Matheny, A. P., Jr., Wachs, T. D., Ludwig, J. L., Phillips, K. (1995). Bringing order out of chaos: Psychometric characteristics of the confusion, hubbub, and order scale. Journal of Applied Developmental Psychology, 16(3), 429 to 444.',
      TermURL = 'https://doi.org/10.1016/0193-3973(95)90028-4'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit this parent-reported survey was completed',
                       Unit = 'YYYY-MM-DD'),
    chaos1 = list( Description = 'There is very little commotion in our home.',
                  Levels = list ('0' = '1 - Very much like your own home',
                                 '1' = '2 - Somewhat like your own home',
                                 '2' = '3 - A little like your own home',
                                 '3' = '4 - Not at all like your own home')),
    chaos2 = list( Description = 'We can usually find things when we need them.',
                   Levels = list ('0' = '1 - Very much like your own home',
                                  '1' = '2 - Somewhat like your own home',
                                  '2' = '3 - A little like your own home',
                                  '3' = '4 - Not at all like your own home')),
    chaos3 = list( Description = 'We almost always seem to be rushed.',
                   Levels = list ('0' = '1 - Very much like your own home',
                                  '1' = '2 - Somewhat like your own home',
                                  '2' = '3 - A little like your own home',
                                  '3' = '4 - Not at all like your own home')),
    chaos4 = list( Description = 'We are usually able to stay on top of things.',
                   Levels = list ('0' = '1 - Very much like your own home',
                                  '1' = '2 - Somewhat like your own home',
                                  '2' = '3 - A little like your own home',
                                  '3' = '4 - Not at all like your own home')),
    chaos5 = list( Description = 'No matter how hard we try, we always seem to be running late.',
                   Levels = list ('0' = '1 - Very much like your own home',
                                  '1' = '2 - Somewhat like your own home',
                                  '2' = '3 - A little like your own home',
                                  '3' = '4 - Not at all like your own home')),
    chaos6 = list( Description = 'It\'s a real zoo in our home.',
                   Levels = list ('0' = '1 - Very much like your own home',
                                  '1' = '2 - Somewhat like your own home',
                                  '2' = '3 - A little like your own home',
                                  '3' = '4 - Not at all like your own home')),
    chaos7 = list( Description = 'At home we can talk to each other without being interrupted.',
                   Levels = list ('0' = '1 - Very much like your own home',
                                  '1' = '2 - Somewhat like your own home',
                                  '2' = '3 - A little like your own home',
                                  '3' = '4 - Not at all like your own home')),
    chaos8 = list( Description = 'There is often a fuss going on at our home.',
                   Levels = list ('0' = '1 - Very much like your own home',
                                  '1' = '2 - Somewhat like your own home',
                                  '2' = '3 - A little like your own home',
                                  '3' = '4 - Not at all like your own home')),
    chaos9 = list( Description = 'No matter what our family plans, it usually doesn\'t seem to work out.',
                   Levels = list ('0' = '1 - Very much like your own home',
                                  '1' = '2 - Somewhat like your own home',
                                  '2' = '3 - A little like your own home',
                                  '3' = '4 - Not at all like your own home')),
    chaos10 = list( Description = 'You can\'t hear yourself think in our home.',
                    Levels = list ('0' = '1 - Very much like your own home',
                                   '1' = '2 - Somewhat like your own home',
                                   '2' = '3 - A little like your own home',
                                   '3' = '4 - Not at all like your own home')),
    chaos11 = list( Description = 'I often get drawn into other people\'s arguments at home.',
                    Levels = list ('0' = '1 - Very much like your own home',
                                   '1' = '2 - Somewhat like your own home',
                                   '2' = '3 - A little like your own home',
                                   '3' = '4 - Not at all like your own home')),
    chaos12 = list( Description = 'Our home is a good place to relax.',
                    Levels = list ('0' = '1 - Very much like your own home',
                                   '1' = '2 - Somewhat like your own home',
                                   '2' = '3 - A little like your own home',
                                   '3' = '4 - Not at all like your own home')),
    chaos13 = list( Description = 'The telephone takes up a lot of our time at home.',
                    Levels = list ('0' = '1 - Very much like your own home',
                                   '1' = '2 - Somewhat like your own home',
                                   '2' = '3 - A little like your own home',
                                   '3' = '4 - Not at all like your own home')),
    chaos14 = list( Description = 'The atmosphere in our home is calm.',
                    Levels = list ('0' = '1 - Very much like your own home',
                                   '1' = '2 - Somewhat like your own home',
                                   '2' = '3 - A little like your own home',
                                   '3' = '4 - Not at all like your own home')),
    chaos15 = list( Description = 'First thing in the day, we have a regular routine at home.',
                    Levels = list ('0' = '1 - Very much like your own home',
                                   '1' = '2 - Somewhat like your own home',
                                   '2' = '3 - A little like your own home',
                                   '3' = '4 - Not at all like your own home')),
    chaos_XX = list( Description = 'XX',
                    Derivative = TRUE))

  # convert formatting to JSON
  chaos_json <- RJSONIO::toJSON(chaos_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(chaos_json, asText = TRUE))){
    print('chaos JSON file may be invalid')
  }

  return(chaos_json)

}
