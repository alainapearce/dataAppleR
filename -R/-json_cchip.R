#' json_cchip: Generates a json file for the Community Childhood Hunger Identification Project
#'
#' This function generates a json file for the scored Community Childhood Hunger Identification Project and raw participant responses.
#'
#' @return A string with data stored in JSON format containing meta-data for the Community Childhood Hunger Identification Project
#'
#'
#' @export

json_cchip <- function() {

  cchip_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Community Childhood Hunger Identification Project',
      Reference = 'Wehler CA, Scott RI, Anderson JJ. The community childhood hunger identification project: A model of domestic hunger. Demonstration project in Seattle, Washington. Journal of Nutrition Education. 1992;24(1). doi:10.1016/S0022-3182(12)80135-X',
      TermURL = 'https://www.sciencedirect.com/science/article/abs/pii/S002231821280135X'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit this parent-reported survey was completed',
                       Unit = 'YYYY-MM-DD'),
    cchip1 = list( Description = 'Thinking about the past 12 months, did your household ever run out of money to buy food to make a meal?',
                  Levels = list ('0' = 'No',
                                 '1' = 'Yes')),
    cchip2 = list( Description = 'Thinking about the past 12 months, during how many months did your household run out of money to buy food to make a meal? (free response)'),
    cchip3 = list( Description = 'When your household runs out of money to make a meal, how many days per month are you usually without food? (free response)'),
    cchip4 = list( Description = 'Thinking about the past 30 days, how many days was your household out of money to buy food to make a meal? (free response)'),
    cchip5 = list( Description = 'Thinking about the past 12 months, did your household ever rely on a limited number of foods to feed your children because you were running out of money to buy food for a meal?',
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes')),
    cchip6 = list( Description = 'Thinking about the past 12 months, during how many months did your household rely on a limited number of foods to feed your children because you were running out of money to buy food for a meal? (free response)'),
    cchip7 = list( Description = 'When your household runs out of money to buy food for a meal, how many days per month do you usually rely on a limited number of foods to feed your children? (free response)'),
    cchip8 = list( Description = 'Thinking about the past 30 days, how many days did your household rely on a limited number of foods to feed your children because you were running out of money to buy food for a meal? (free response)'),
    cchip9 = list( Description = 'Thinking about the past 12 months, did you or adult members of your household ever eat less than you felt you should because there was not enough money for food?',
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes')),
    cchip10 = list( Description = 'Thinking about the past 12 months, during how many months did you or adult members of your household ever eat less than you felt you should because there was not enough money for food? (free response)'),
    cchip11 = list( Description = 'Thinking about the past 30 days, how many days did you or adult members of your household eat less than you felt you should because there was not enough money for food? (free response)'),
    cchip12 = list( Description = 'Thinking about the past 30 days, how many days did you or adult members of your household eat less than you felt you should because there was not enough money for food? (free response)'),
    cchip13 = list( Description = 'Thinking about the past 12 months, did you or adult members of your household ever cut the size of meals or skip meals because there was not enough money for food?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    cchip14 = list( Description = 'Thinking about the past 12 months, during how many months did you or adult members of your household cut the size of meals or skip meals because there was not enough money for food? (free response)'),
    cchip15 = list( Description = 'When your household does not have enough money for food, how many days per month do you or adult members of your household usually cut the size of meals or skip meals? (free response)'),
    cchip16 = list( Description = 'Thinking about the past 30 days, how many days did you or adult members of your household cut the size of meals or skip meals because there was not enough money for food? (free response)'),
    cchip17 = list( Description = 'Thinking about the past 12 months, did your children ever say they were hungry because there wasn\'t enough food in the house?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    cchip18 = list( Description = 'Thinking about the past 12 months, during how many months did your children say they were hungry because there wasn\'t enough food in the house? (free response)'),
    cchip19 = list( Description = 'When there isn\'t enough food in the house, how many days per month do your children usually say they are hungry? (free response)'),
    cchip20 = list( Description = 'Thinking about the past 30 days, how many days did your children say they were hungry because there wasn\'t enough food in the house? (free response)'),
    cchip21 = list( Description = 'Thinking about the past 12 months, did your children ever eat less than you felt they should because there was not enough money for food?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    cchip22 = list( Description = 'Thinking about the past 12 months, during how many months did your children eat less than you felt they should because there was not enough money for food? (free response)'),
    cchip23 = list( Description = 'When there isn\'t enough money for food, how many days per month do your children usually eat less than you feel they should? (free response)'),
    cchip24 = list( Description = 'Thinking about the past 30 days, how many days did your children eat less than you felt they should because there was not enough money for food? (free response)'),
    cchip25 = list( Description = 'Thinking about the past 12 months, did your children ever go to bed hungry because there was not enough money for food?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    cchip26 = list( Description = 'Thinking about the past 12 months, during how many months did your children ever go to bed hungry because there was not enough money for food? (free response)'),
    cchip27 = list( Description = 'When there isn\'t enough money for food, how many days per month do your children usually go to bed hungry? (free response)'),
    cchip28 = list( Description = 'Thinking about the past 30 days, how many days did your children go to bed hungry because there was not enough money for food? (free response)'),
    cchip29 = list( Description = 'Thinking about the past 12 months, did you ever cut the size of your children\'s meals, or did they ever skip meals, because there was not enough money for food?',
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes')),
    cchip30 = list( Description = 'Thinking about the past 12 months, during how many months did you ever cut the size of your children\'s meals, or did they ever skip meals, because there was not enough money for food? (free response)'),
    cchip31 = list( Description = 'When there isn\'t enough money for food, how many days per month do you usually cut the size of your children\'s meals or do they skip meals? (free response)'),
    cchip32 = list( Description = 'Thinking about the past 30 days, how many days did you cut the size of your children\'s meals, or did they skip meals, because there was not enough money for food? (free response)'),
    cchip_total = list( Description = 'Food Insecurity Score',
                       Derivative = TRUE),
    cchip_category = list( Description = 'Food Insecurity Category',
                           Levels = list (
                             'Hungry' = 'Hungry',
                             'At Risk for Hunger' = 'At Risk for Hunger',
                             'Not Hungry' = 'Not Hungry'),
                           Derivative = TRUE))

  # convert formatting to JSON
  cchip_json <- RJSONIO::toJSON(cchip_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(cchip_json, asText = TRUE))){
    print('cchip JSON file may be invalid')
  }

  return(cchip_json)

}
