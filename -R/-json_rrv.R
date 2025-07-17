#' json_rrv: Generates a json file for RRV data in bids/rawdata
#'
#' This function generates a json file for RRV data in bids/rawdata
#'
#' @return A string with data stored in JSON format containing meta-data for the RRV task
#'
#'
#' @export

json_rrv <- function() {

  rrv_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
    ID = list( Description = 'ID entered into RRV software for data collection'),
    screen = list( Description = '',
                   Levels = list ('1' = 'screen 1',
                                  '2' = 'screen 2')),
    reinforcer = list( Description = 'Reinforcer',
                     Levels = list ('Candy' = 'candy',
                                    'Toy' = 'toy')),
    type = list( Description = 'type of game',
                 Levels = list ('slot machine' = 'slot machine')),
    session = list( Description = 'RRV task session number. Each session corresponds to a different reinforcement schedule'),
    session_time = list( Description = 'Time spent within a session for a given reinforcer/screen',
                       Unit = "seconds"),
    schedule = list( Description = 'Reinforcement schedule',
                  Levels = list ('Variable Response Based 1:4' = '1 reinforcer earned per 4 responses',
                                 'Variable Response Based 1:8' = '1 reinforcer earned per 8 responses')),
    block = list( Description = 'Block number within a given session for a given reinforcer/screen. Each block is a 10 interval'),
    block_responses = list( Description = 'Number of responses made within a block for a given reinforcer'),
    block_reinforcers = list( Description = 'Number of reinforcers earned within a block for a given reinforcer'),
    session_responses = list( Description = 'Number of responses made within a session for a given reinforcer'),
    session_reinforcers = list( Description = 'Number of reinforcers earned within a session for a given reinforcer'),
    session_average_responses = list( Description = '??'),
    session_average_reinforcers = list( Description = '??'),
    session_blocks = list( Description = 'Number of blocks within a session for a given reinforcer'),
    session_nonresp_blocks = list( Description = 'Number of blocks with 0 responses within a session for a given reinforcer')
    )


  # convert formatting to JSON
  rrv_json <- RJSONIO::toJSON(rrv_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(rrv_json, asText = TRUE))){
    print('rrv JSON file may be invalid')
  }

  return(rrv_json)

}
