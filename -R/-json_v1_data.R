#' json_v1_data: Generates a json file for general visit 1 notes and data
#'
#' This function generates a json file for ageneral visit 1 notes and data
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_v1_data <- function() {

  v1_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    v1_notes = list( Description = 'Notes about visit 1'),
    v1_date = list( Description = 'Visit 1 date'),
    child_assent = list( Description = 'Child assent status',
                         Levels = list ('1' = 'Child assented to study',
                                        '0' = 'Child did NOT assent to study')),
    dxa_notes = list( Description = 'Notes about DXA scan'),
    rrv_task_notes = list( Description = 'Notes about RRV task')
  )

  # convert formatting to JSON
  v1_json <- RJSONIO::toJSON(v1_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(v1_json, asText = TRUE))){
    print('V1 data JSON file may be invalid')
  }

  return(v1_json)

}
