#' json_fullness_v5: Generates a json file for Freddy Fullness data for visit 5
#'
#' This function generates a json file for Freddy Fullness data for visit 5
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_fullness_v5 <- function() {

  fullness_list_v5 <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Peter Fullness Scale',
      Reference = 'NEED'),
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
    prepreload_fullness = list( Description = 'Pre-preload fullness rating on a 4 point likert scale with 4 images of a child with their belly full to varying extents',
                                  Levels = list ('0' = 'Empty Belly',
                                                 '1' = 'Partly Full',
                                                 '2' = 'Almost Full',
                                                 '3' = 'Full Belly')),
    postpreload_fullness = list( Description = 'Post-preload fullness rating on a 4 point likert scale with 4 images of a child with their belly full to varying extents',
                                 Levels = list ('0' = 'Empty Belly',
                                                '1' = 'Partly Full',
                                                '2' = 'Almost Full',
                                                '3' = 'Full Belly')),
    premeal_fullness = list( Description = 'Pre-meal fullness rating on a 4 point likert scale with 4 images of a child with their belly full to varying extents',
                             Levels = list ('0' = 'Empty Belly',
                                            '1' = 'Partly Full',
                                            '2' = 'Almost Full',
                                            '3' = 'Full Belly')),
    postmeal_fullness = list( Description = 'Post-meal fullness rating on a 4 point likert scale with 4 images of a child with their belly full to varying extents',
                              Levels = list ('0' = 'Empty Belly',
                                             '1' = 'Partly Full',
                                             '2' = 'Almost Full',
                                             '3' = 'Full Belly')),
    preeah_fullness = list( Description = 'Pre-EAH fullness rating on a 4 point likert scale with 4 images of a child with their belly full to varying extents',
                              Levels = list ('0' = 'Empty Belly',
                                             '1' = 'Partly Full',
                                             '2' = 'Almost Full',
                                             '3' = 'Full Belly')),
    posteah_fullness = list( Description = 'Post-EAH fullness rating on a 4 point likert scale with 4 images of a child with their belly full to varying extents',
                              Levels = list ('0' = 'Empty Belly',
                                             '1' = 'Partly Full',
                                             '2' = 'Almost Full',
                                             '3' = 'Full Belly'))
  )

  # convert formatting to JSON
  fullness_json_v5 <- RJSONIO::toJSON(fullness_list_v5, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(fullness_json_v5, asText = TRUE))){
    print('Freddy fullness JSON file may be invalid')
  }

  return(fullness_json_v5)
}
