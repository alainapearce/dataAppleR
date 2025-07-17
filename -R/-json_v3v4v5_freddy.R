#' json_v3v4v5_freddy: Generates a json file for visits 3-5 Freddy Fullness data
#'
#' This function generates a json file for visits 3-5 Freddy Fullness data
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_v3v4v5_freddy <- function() {

  v3v4v5_freddy_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Freddy Fullness Scale',
      Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1',
                                          '3' =	'Child visit protocol 3',
                                          '4' =	'Child visit protocol 4',
                                          '5'	= 'Child visit protocol 5')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    advertisement_condition = list( Description = 'Advertisement Condition',
                                    Levels = list ('0' =	'Food',
                                                   '1' =	'Toy',
                                                   '2'	= 'Boring')),
    pre_meal_ads_fullness_time = list( Description = 'Pre-meal-advertisement (i.e., before the advertisement that preceeded the meal) fullness rating time',
                                       Unit = "hh:mm:ss"),
    pre_meal_fullness_time = list( Description = 'Pre-meal fullness rating time',
                                       Unit = "hh:mm:ss"),
    post_meal_fullness_time = list( Description = 'Post-meal fullness rating time',
                                  Unit = "hh:mm:ss"),
    pre_ad_eah_fullness_time = list( Description = 'Pre-EAH-advertisement (i.e., before the advertisement that preceeded the meal) fullness rating time',
                                   Unit = "hh:mm:ss"),
    pre_eah_fullness_time = list( Description = 'Pre-EAH fullness rating time',
                                     Unit = "hh:mm:ss"),
    post_eah_fullness_time = list( Description = 'Post-EAH fullness rating time',
                                     Unit = "hh:mm:ss"),
    pre_ad_meal_fullness = list( Description = 're-meal-advertisement (i.e., before the advertisement that preceeded the meal) fullness rating on a 150 mm visual analgue scale',
                                  Unit = "mm"),
    pre_meal_fullness = list( Description = 'Pre-meal fullness rating on a 150 mm visual analgue scale',
                              Unit = "mm"),
    post_meal_fullness = list( Description = 'Post-meal fullness rating on a 150 mm visual analgue scale',
                               Unit = "mm"),
    pre_ad_eah_fullness = list( Description = 'Pre-EAH-advertisement (i.e., before the advertisement that preceeded the meal) fullness fullness rating on a 150 mm visual analgue scale',
                               Unit = "mm"),
    pre_eah_fullness = list( Description = 'Pre-EAH fullness rating on a 150 mm visual analgue scale',
                               Unit = "mm"),
    post_eah_fullness = list( Description = 'Post-EAH fullness rating on a 150 mm visual analgue scale',
                               Unit = "mm")
  )

  # convert formatting to JSON
  v3v4v5_freddy_json <- RJSONIO::toJSON(v3v4v5_freddy_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(v3v4v5_freddy_json, asText = TRUE))){
    print('Freddy fullness for visits 3-5 JSON file may be invalid')
  }

  return(v3v4v5_freddy_json)
}
