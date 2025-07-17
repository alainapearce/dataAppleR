#' json_deriv_rrv: Generates 2 json file for the derivative RRV data
#'
#' This function generates 2 json files RRV databases reported in derivatives/beh_databases: (1) for total summary outcomes, (2) for session summary outcomes (long data)
#'
#' @return A list of 2 strings (labels: rrv_summary_json, rrv_summary_long_json) with data stored in JSON format containing meta-data the summary RRV database
#'
#'
#' @export

json_deriv_rrv <- function() {

  rrv_summary_list <- list(
    'FileLevelMetadata' = list(
      Description = "Relative Reinforcing Value of Food Task behavior summarized across the whole task",
      Sources = "bids::rawdata/sub*/ses-1/beh/sub*rrv_beh.tsv"),
    participant_id = list( Description = 'participant id number'),
    pmax_responded_food = list( Description = 'Maximum schedule with responses for food reinforcer',
                                Levels = list("0" = "no responses made",
                                              "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                              "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                              "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                              "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                              "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                              "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                              "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                              "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                              "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)"),
                                Derivative = TRUE),
    pmax_completed_food = list( Description = 'Maximum schedule completed (i.e., 5 reinforcers recieved) for food reinforcer',
                                Levels = list("0" = "no schedule completed",
                                              "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                              "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                              "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                              "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                              "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                              "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                              "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                              "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                              "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)"),
                                Derivative = TRUE),
    pmax_responded_toy = list( Description = 'Maximum schedule with responses for toy reinforcer',
                               Levels = list("0" = "no responses made",
                                             "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                             "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                             "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                             "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                             "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                             "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                             "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                             "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                             "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)"),
                               Derivative = TRUE),
    pmax_completed_toy = list( Description = 'Maximum schedule completed (i.e., 5 reinforcers recieved) for toy reinforcer',
                               Levels = list("0" = "no schedule completed",
                                             "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                             "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                             "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                             "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                             "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                             "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                             "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                             "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                             "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)"),
                               Derivative = TRUE),
    rrv_food_responded = list( Description = 'Relative reinforcing value of food, based on pmax_responded: pmax_responded_food/(pmax_responded_food + pmax_responded_toy)',
                                Derivative = TRUE),
    rrv_food_completed = list( Description = 'Relative reinforcing value of food, based on pmax_completed: pmax_completed_food/(pmax_completed_food + pmax_completed_toy)',
                                Derivative = TRUE),
    total_responses_food = list( Description = 'Total number of responses across all schedules for food reinforcer',
                               Derivative = TRUE),
    total_responses_toy = list( Description = 'Total number of responses across all schedules for toy reinforcer',
                               Derivative = TRUE),
    total_reinforcers_food = list( Description = 'Total number of reinforcers earned across all schedules for food reinforcer',
                                 Derivative = TRUE),
    total_reinforcers_toy = list( Description = 'Total number of reinforcers earned across all schedules for toy reinforcer',
                                Derivative = TRUE),
    mean_response_rate_food = list( Description = 'Mean response rate (responses / min) for food reinforcer',
                                Derivative = TRUE),
    mean_response_rate_toy = list( Description = 'Mean response rate (responses / min) for toy reinforcer',
                                    Derivative = TRUE)
    )


  rrv_summary_long_list <- list(
    'FileLevelMetadata' = list(
      Description = "Relative Reinforcing Value of Food Task behavior summarized by session/reinforcement schedule for each reinforcer",
      Sources = "bids::rawdata/sub*/ses-1/beh/sub*rrv_beh.tsv"),
    participant_id = list( Description = 'participant id number'),
    reinforcer = list( Description = 'Reinforcer',
                       Levels = list ('Candy' = 'candy',
                                      'Toy' = 'toy')),
    session = list( Description = 'RRV task session number. Each session corresponds to a different reinforcement schedule'),
    session_time = list( Description = 'Time spent within a session for a given reinforcer/screen',
                         Unit = "seconds"),
    schedule = list( Description = 'Reinforcement schedule',
                     Levels = list ('Variable Response Based 1:4' = '1 reinforcer earned per 4 responses',
                                    'Variable Response Based 1:8' = '1 reinforcer earned per 8 responses')),
    session_responses = list( Description = 'Number of responses made within a session for a given reinforcer'),
    session_reinforcers = list( Description = 'Number of reinforcers earned within a session for a given reinforcer'),
    session_blocks = list( Description = 'Number of blocks within a session for a given reinforcer'),
    session_nonresp_blocks = list( Description = 'Number of blocks with 0 responses within a session for a given reinforcer'),
    session_average_responses = list( Description = '??'),
    session_average_reinforcers = list( Description = '??')
  )

  # convert formatting to JSON
  rrv_summary_json <- RJSONIO::toJSON(rrv_summary_list, pretty = TRUE)
  rrv_summary_long_json <- RJSONIO::toJSON(rrv_summary_long_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(rrv_summary_json, asText = TRUE))){
    print('RRV summary derivative JSON file may be invalid')
  }

  if (isFALSE(RJSONIO::isValidJSON(rrv_summary_long_json, asText = TRUE))){
    print('RRV long summary derivative JSON file may be invalid')
  }

  return(list(rrv_summary_json = rrv_summary_json,
              rrv_summary_long_json = rrv_summary_long_json))

}
