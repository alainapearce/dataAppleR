#' json_spacegame: Generates json file for spacegame data (data in rawdata/beh)
#'
#' @return A string with data stored in JSON format containing meta-data for the space game
#'
#'
#' @export

json_spacegame <- function() {

  spacegame_list <- list(
    'FileLevelMetadata' = list(
      Description = "Space Game: a 2-Stage Reinforcement Learning/Reward-Related Decision Making Task ",
      TermURL = "https://www.cognitiveatlas.org/task/id/trm_5667451917a34/",
      Sources = "bids::rawdata/sub*/ses-1/beh/sub*spacegame_beh.tsv"),
    sub = list(Description = 'participant id number'),

    date = list(Description = 'Date task was completed'),
    block = list(Description = 'Block number for the task - the number does not correspond to conditions so should always follow sequence 1, 2, 3, 4'),
    trial = list(Description = 'trial number'),
    timeout_earth = list(Description = 'Indicates if participant timedout before responding during stage 1 - selecting a rocket',
                         Levels = list ('0' = 'did not timeout',
                                        '1' = 'did timeout')),
    timeout_planet = list(Description = 'Indicates if participant timedout before responding during stage 2 - pressing SPACE bar to get treasure from alien',
                          Levels = list ('0' = 'did not timeout',
                                         '1' = 'did timeout')),
    state_earth = list(Description = 'Indicates the state the earth is in for each trial (specific pairings of rockets)',
                       Levels = list ('1' = 'state 1',
                                      '2' = 'state 2')),
    state_planet = list(Description = 'Indicates the state the planet is in for each trial (reward state - high or low)',
                        Levels = list ('1' = 'state 1',
                                       '2' = 'state 2')),
    stim_right = list(Description = 'Indicates which rocket was presented during stage 1 (earth) on the left side of screen',
                      Levels = list ('1' = 'rocket 1',
                                     '2' = 'rocket 2',
                                     '3' = 'rocket 3',
                                     '4' = 'rocket 4')),
    stim_right = list(Description = 'Indicates which rocket was presented during stage 1 (earth) on the right side of screen',
                      Levels = list ('1' = 'rocket 1',
                                     '2' = 'rocket 2',
                                     '3' = 'rocket 3',
                                     '4' = 'rocket 4')),
    rt_earth = list(Description = 'Reaction time of response during stage 1 - rocket choice. Note: a value of -1 indicates the response window was missed',
                    Unit = "ms"),
    rt_planet = list(Description = 'Reaction time of response during stage 2 - asking alien for treasure. Note: a value of -1 indicates the response window was missed',
                     Unit = 'ms'),
    choice_earth = list(Description = 'choice made during stage 1 - rocket choice',
                        Levels = list ('1' = 'rocket 1',
                                       '2' = 'rocket 2',
                                       '3' = 'rocket 3',
                                       '4' = 'rocket 4')),
    response = list(Description = 'participant response during stage 1 - rocket choice',
                    Levels = list ('1' = 'left rocket',
                                   '2' = 'right rocket')),
    points = list(Description = 'points earned that trial'),
    stake = list(Description = 'the stake for the trial'),
    score = list(Description = 'running tally of points/total score after each trial'),
    rewards1 = list(Description = 'possible reward for the left rocket choice'),
    rewards2 = list(Description = 'possible reward for the right rocket choice'),
    missed_earth = list(Description = 'indicates if response window was missed for stage 1 - rocket choice',
                        Levels = list ('0' = 'not missed',
                                       '1' = 'missed')),
    missed_planet = list(Description = 'indicates if response window was missed for stage 2 - asking alien for treasure',
                         Levels = list ('0' = 'not missed',
                                        '1' = 'missed')),
    earth_same = list(Description = 'whether the current trial state for earth is different from previous trial\'s earth state',
                      Levels = list ('0' = 'different from previous trial state',
                                     '1' = 'same as previous trial state state')),
    stay_planet = list(Description = 'whether the current trial state for planet is different from previous trial\'s plant state',
                       Levels = list ('0' = 'different from previous trial state',
                                      '1' = 'same as previous trial state state'))

  )


  # convert and return JSONS ----

  # convert formatting to JSON

  spacegame_json <- RJSONIO::toJSON(spacegame_list, pretty = TRUE)

  if (isFALSE(RJSONIO::isValidJSON(spacegame_json, asText = TRUE))){
    print('spacegame_json JSON file may be invalid')
  }

  return(spacegame_json)

}
