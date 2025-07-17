#' json_kbas: Generates a json file for the Kid's Brand Awareness Survey
#'
#' This function generates a json file for Kid's Brand Awareness Survey
#' @return A string with data stored in JSON format containing meta-data for the KBAS
#'
#'
#' @export

json_kbas <- function() {

  kbas_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Kid\'s Brand Awareness Survey. This survey was developed in the Keller Lab for Project REACH. Two versions of the survey were administered (counterbalanced) which varied by category (food, toy) order. Participants were provided the following instructions: "Now we are going to play a game. For this game, we want you to match the correct picture with the brand logo or character. Some of the pictures will be of foods and others will be of toys. There are no wrong answers to this game. We just want to find out how many different foods and toy brands you can recognize. Does that make sense?"'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    kbas_version = list( Description = 'Version of KBAS adminisered',
                         Derivative = TRUE,
                         Levels = list ('A' = 'Version A: food items 1-25 then toy items 1-25',
                                        'B' = 'Version B: toy items 1-25 then food items 1-25')),
    kbas_food_score = list( Description = 'KBAS food score: number of correct responses to food items. Derived in redcap',
                         Derivative = TRUE),
    kbas_toy_score = list( Description = 'KBAS toy score: number of correct responses to toy items. Derived in redcap',
                         Derivative = TRUE),
    kbas_food_1 = list(Description = 'Subway',
                  Levels = list (
                    '0' = 'A',
                    '1' = 'B',
                    '2' = 'C',
                    '3' = 'D'
                  )),
    kbas_food_2 = list(Description = 'Wendy\'s',
                  Levels = list (
                    '0' = 'A',
                    '1' = 'B',
                    '2' = 'C',
                    '3' = 'D'
                  )),
    kbas_food_3 = list( Description = 'Frosted Flakes',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_food_4 = list( Description = 'Pop Tart',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_food_5 = list( Description = 'Cheeto',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_food_6 = list( Description = 'Goldfish',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_food_7 = list( Description = 'Capri Sun',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_food_8 = list( Description = 'Nesquick',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_food_9 = list( Description = 'Fruit roll-up',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_food_10 = list( Description = 'Lays',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_11 = list( Description = 'Dorito',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_12 = list( Description = 'Taco Bell',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_13 = list( Description = 'Coca Cola',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_14 = list( Description = 'Chick-fil-A',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_15 = list( Description = 'Frito',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_16 = list( Description = 'Lucky Charms',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_17 = list( Description = 'Kool Aid',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_18 = list( Description = 'McDonalds',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_19 = list( Description = 'M&M',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_20 = list( Description = 'Oreo',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_21 = list( Description = 'Sprite',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_22 = list( Description = 'Trix',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_23 = list( Description = 'Cheerios',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_24 = list( Description = 'Chips Ahoy',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_food_25 = list( Description = 'Cookie Crisp',
                    Levels = list (
                      '0' = 'A',
                      '1' = 'B',
                      '2' = 'C',
                      '3' = 'D'
                    )),
    kbas_toy_1 = list( Description = 'Lego',
                  Levels = list (
                    '0' = 'A',
                    '1' = 'B',
                    '2' = 'C',
                    '3' = 'D'
                  )),
    kbas_toy_2 = list( Description = 'American Girl Doll',
                  Levels = list (
                    '0' = 'A',
                    '1' = 'B',
                    '2' = 'C',
                    '3' = 'D'
                  )),
    kbas_toy_3 = list( Description = 'Nintendo',
                  Levels = list (
                    '0' = 'A',
                    '1' = 'B',
                    '2' = 'C',
                    '3' = 'D'
                  )),
    kbas_toy_4 = list( Description = 'Barbie',
                  Levels = list (
                    '0' = 'A',
                    '1' = 'B',
                    '2' = 'C',
                    '3' = 'D'
                  )),
    kbas_toy_5 = list( Description = 'Jurassic World',
                  Levels = list (
                    '0' = 'A',
                    '1' = 'B',
                    '2' = 'C',
                    '3' = 'D'
                  )),
    kbas_toy_6 = list( Description = 'Nerf',
                  Levels = list (
                    '0' = 'A',
                    '1' = 'B',
                    '2' = 'C',
                    '3' = 'D'
                  )),
    kbas_toy_7 = list( Description = 'Hot Wheels',
                  Levels = list (
                    '0' = 'A',
                    '1' = 'B',
                    '2' = 'C',
                    '3' = 'D'
                  )),
    kbas_toy_8 = list( Description = 'Play Doh',
                  Levels = list (
                    '0' = 'A',
                    '1' = 'B',
                    '2' = 'C',
                    '3' = 'D'
                  )),
    kbas_toy_9 = list( Description = 'Little Pony',
                  Levels = list (
                    '0' = 'A',
                    '1' = 'B',
                    '2' = 'C',
                    '3' = 'D'
                  )),
    kbas_toy_10 = list( Description = 'Monopoly',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_11 = list( Description = 'Sqishmallow',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_12 = list( Description = 'Mega Block',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_13 = list( Description = 'Funko Pop',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_14 = list( Description = 'Hatchimal',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_15 = list( Description = 'MineCraft',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_16 = list( Description = 'Ranger',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_17 = list( Description = 'Star Wars',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_18 = list( Description = 'Littlest Pet',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_19 = list( Description = 'Frozen',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_20 = list( Description = 'Apple',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_21 = list( Description = 'Crayola',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_22 = list( Description = 'Xbox',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_23 = list( Description = 'Uno',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_24 = list( Description = 'Youtube',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    kbas_toy_25 = list( Description = 'Netflix',
                   Levels = list (
                     '0' = 'A',
                     '1' = 'B',
                     '2' = 'C',
                     '3' = 'D'
                   )),
    fkbas_ood_1_score = list( Description = 'Score for food_1. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_food_2_score = list( Description = 'Score for food_2. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_food_3_score = list( Description = 'Score for food_3. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_food_4_score = list( Description = 'Score for food_4. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_food_5_score = list( Description = 'Score for food_5. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_food_6_score = list( Description = 'Score for food_6. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_food_7_score = list( Description = 'Score for food_7. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_food_8_score = list( Description = 'Score for food_8. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_food_9_score = list( Description = 'Score for food_9. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_food_10_score = list( Description = 'Score for food_10. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_11_score = list( Description = 'Score for food_11. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_12_score = list( Description = 'Score for food_12. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_13_score = list( Description = 'Score for food_13. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_14_score = list( Description = 'Score for food_14. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_15_score = list( Description = 'Score for food_15. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_16_score = list( Description = 'Score for food_16. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_17_score = list( Description = 'Score for food_17. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_18_score = list( Description = 'Score for food_18. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_19_score = list( Description = 'Score for food_19. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_20_score = list( Description = 'Score for food_20. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_21_score = list( Description = 'Score for food_21. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_22_score = list( Description = 'Score for food_22. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_23_score = list( Description = 'Score for food_23. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_24_score = list( Description = 'Score for food_24. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_food_25_score = list( Description = 'Score for food_25. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_1_score = list( Description = 'Score for toy_1. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_toy_2_score = list( Description = 'Score for toy_2. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_toy_3_score = list( Description = 'Score for toy_3. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_toy_4_score = list( Description = 'Score for toy_4. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_toy_5_score = list( Description = 'Score for toy_5. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_toy_6_score = list( Description = 'Score for toy_6. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_toy_7_score = list( Description = 'Score for toy_7. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_toy_8_score = list( Description = 'Score for toy_8. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_toy_9_score = list( Description = 'Score for toy_9. Derived in RedCap',
                         Derivative = TRUE,
                         Levels = list ('0' = 'Incorrect item selected',
                                        '1' = 'Correct item selected')),
    kbas_toy_10_score = list( Description = 'Score for toy_10. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_11_score = list( Description = 'Score for toy_11. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_12_score = list( Description = 'Score for toy_12. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_13_score = list( Description = 'Score for toy_13. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_14_score = list( Description = 'Score for toy_14. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_15_score = list( Description = 'Score for toy_15. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_16_score = list( Description = 'Score for toy_16. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_17_score = list( Description = 'Score for toy_17. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_18_score = list( Description = 'Score for toy_18. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_19_score = list( Description = 'Score for toy_19. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_20_score = list( Description = 'Score for toy_20. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_21_score = list( Description = 'Score for toy_21. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_22_score = list( Description = 'Score for toy_22. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_23_score = list( Description = 'Score for toy_23. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_24_score = list( Description = 'Score for toy_24. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected')),
    kbas_toy_25_score = list( Description = 'Score for toy_25. Derived in RedCap',
                          Derivative = TRUE,
                          Levels = list ('0' = 'Incorrect item selected',
                                         '1' = 'Correct item selected'))
      )

  # convert formatting to JSON
  kbas_json <- RJSONIO::toJSON(kbas_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(kbas_json, asText = TRUE))){
    print('kbas JSON file may be invalid')
  }

  return(kbas_json)

}
