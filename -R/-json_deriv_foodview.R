#' json_deriv_foodview: Generates json file for the derivative Foodview data
#'
#' This function generates 2 json files for FoodView summary data reported in derivatives/beh_databases: (1) summary metrics computed by ad condition, (2) summary metrics computed by block
#'
#' @return A list of 2 strings (labels: foodview_bycond_json, foodview_byblock_json) with data stored in JSON format containing meta-data the summary Foodview database
#'
#'
#' @export

json_deriv_foodview <- function() {

  # JSON for dataframe by condition ----
  foodview_bycond_list <- list(
    'FileLevelMetadata' = list(
      Description = "Foodview task behavior summarized by commerical condition across all blocks/runs in the task",
      Sources = "bids::rawdata/sub*/ses-1/func/sub*foodview*events.tsv"),
    sub = list( Description = 'participant id number'),
    commercial_cond = list( Description = 'commerical condition',
                                Levels = list("toy" = "toy",
                                              "food" = "food")),
    n_image = list(Description = 'number of image events (i.e., the presentation of a food item where a child is to respond to the question "do you want this item?"',
                    Derivative = TRUE),
    p_resp = list(Description = 'proportion of image events with response out of the total number of image events. For a response to counted, the response_time had to be >0ms.',
                    Derivative = TRUE),
    p_want_of_resp = list(Description = 'proportion of items a child reported wanting out of the total number of responses they provided. For a response to counted, the response_time had to be >0ms',
                          Derivative = TRUE),
    avg_rt = list(Description = 'average reaction time for responses to all items. For a response to counted, the response_time had to be >0ms.',
                  unit = "ms",
                  Derivative = TRUE),
    hed_n_image = list(Description = 'number of high ED image events',
                  Derivative = TRUE),
    hed_p_resp = list(Description = 'proportion of high ED events with a response out of the total number of high ED events. For a response to counted, the response_time had to be >0ms.',
                  Derivative = TRUE),
    hed_p_want_of_resp = list( Description = 'proportion of high ED items a child reported wanting out of the total number of high ED responses they provided. For a response to counted, the response_time had to be >0ms.',
                  Derivative = TRUE),
    hed_avg_rt = list(Description = 'average reaction time for responses to items in high ED food blocks. For a response to counted, the response_time had to be >0ms.',
                      unit = "ms",
                      Derivative = TRUE),
    led_n_image = list(Description = 'number of low ED image events for given commercial_cond',
                  Derivative = TRUE),
    led_p_resp = list(Description = 'proportion of low ED events with a response out of the total number of low ED events. For a response to counted, the response_time had to be >0ms.',
                  Derivative = TRUE),
    led_p_want_of_resp = list( Description = 'proportion of low ED items a child reported wanting out of the total number of low ED responses they provided. For a response to counted, the response_time had to be >0ms.',
                    Derivative = TRUE),
    led_avg_rt = list( Description = 'average reaction time for responses to items in low ED food blocks. For a response to counted, the response_time had to be >0ms.',
                       unit = "ms",
                       Derivative = TRUE),
    sweet_n_image = list( Description = 'number of sweet image events for given commercial_cond',
                    Derivative = TRUE),
    sweet_p_want_of_resp = list( Description = 'proportion of sweet items a child reported wanting out of the total number of sweet responses they provide. For a response to counted, the response_time had to be >0ms.',
                    Derivative = TRUE),
    sweet_avg_rt = list( Description = 'average reaction time for responses to items in sweet food blocks. For a response to counted, the response_time had to be >0ms.',
                         unit = "ms",
                         Derivative = TRUE),
    savory_n_image = list( Description = 'number of savory image events for given commercial_cond',
                         Derivative = TRUE),
    savory_p_resp = list( Description = 'proportion of savory events with a response out of the total number of savory events. For a response to counted, the response_time had to be >0ms.',
                         Derivative = TRUE),
    savory_p_want_of_resp = list( Description = 'proportion of savory items a child reported wanting out of the total number of savory responses they provide. For a response to counted, the response_time had to be >0ms.',
                         Derivative = TRUE),
    savory_avg_rt = list( Description = 'average reaction time for responses to items in savory food blocks. For a response to counted, the response_time had to be >0ms.',
                          unit = "ms",
                          Derivative = TRUE)
  )

  # JSON for dataframe by block ----

  foodview_byblock_list <- list(
    'FileLevelMetadata' = list(
      Description = "Foodview task behavior summarized by block",
      Sources = "bids::rawdata/sub*/ses-1/func/sub*foodview*events.tsv"),
    sub = list( Description = 'participant id number'),
    run_num = list( Description = 'Food View Task run number',
                    Levels = list ('1' = 'run 1',
                                   '2' = 'run 2',
                                   '3' = 'run 3',
                                   '4' = 'run 4')),
    block_num = list( Description = 'Block number within a run',
                    Levels = list ('1' = 'block 1',
                                   '2' = 'block 2',
                                   '3' = 'block 3',
                                   '4' = 'block 4')),
    commercial_cond = list( Description = 'commercial condition',
                            Levels = list ('food' = 'food',
                                           'toy' = 'toy')),
    food_ed = list( Description = 'block energy density category',
                    Levels = list ('low' = 'low energy density foods',
                                   'high' = 'high energy density foods')),
    food_taste = list( Description = 'block taste category',
                       Levels = list ('sweet' = 'sweet foods',
                                      'savory' = 'savory foods')),
    n_image = list( Description = 'Number of images in a block',
                    derivative = TRUE),
    n_resp = list( Description = 'Number of responses in given block. For a response to counted, the response_time had to be >0ms.',
                   derivative = TRUE),
    n_want = list( Description = 'Number of want responses in given block. For a response to counted, the response_time had to be >0ms.',
                   derivative = TRUE),
    p_resp = list( Description = 'Proportion of responses out of images in a given block. For a response to counted, the response_time had to be >0ms.',
                   derivative = TRUE),
    p_resp_of_want = list( Description = 'Proportion of "want" responses out of all responses in a given block. For a response to counted, the response_time had to be >0ms.',
                    derivative = TRUE),
    avg_rt = list(Description = 'Average reaction time for responses in given block. For a response to counted, the response_time had to be >0ms.',
                  Unit = "ms")
  )

  # convert and return JSONS ----

  # convert formatting to JSON
  foodview_bycond_json <- RJSONIO::toJSON(foodview_bycond_list, pretty = TRUE)
  foodview_byblock_json <- RJSONIO::toJSON(foodview_byblock_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(foodview_bycond_json, asText = TRUE))){
    print('foodview_bycond_json JSON file may be invalid')
  }

  if (isFALSE(RJSONIO::isValidJSON(foodview_byblock_json, asText = TRUE))){
    print('foodview_byblock_json JSON file may be invalid')
  }

  return(list(foodview_bycond_json = foodview_bycond_json,
              foodview_byblock_json = foodview_byblock_json))

}
