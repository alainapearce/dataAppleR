#' json_toolbox_phenotype: Generates a json file for the NIH toolbox behavioral score data (saved in bids/phenotype/toolbox_scores.tsv)
#'
#' This function generates a json file for cleaned NIH toolbox score data
#'
#' @return A string with data stored in JSON format containing meta-data for NIH toolbox score data
#'
#'
#' @export

json_toolbox_phenotype <- function() {

  toolbox_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'NIH Toolbox scores. Children were administered 3 tasks from the NIH toolbox: Flanker Inhibitory Control and Attention Test (FLANKER), Dimensional Change Card Sort Test (CARTSORT), List Sorting Working Memory Test (LISTSORT). Scores were automatically derived by the NIH Toolbox. Additional details can be found in the "NIH toolbox: Scoring and Interpretation Guide for the iPad"',
      Reference = '',
      TermURL = 'https://nihtoolbox.zendesk.com/hc/en-us'),
    participant_id = list( Description = ''),
    session_id = list( Description = ''),
    FLANKER_Test_Ages = list( Description = 'Flanker task version administered',
                              Levels = list ('Ages 8-11 v2.1' = 'Version 2.1 for childen ages 8-11',
                                             'Ages 3-7 v2.1' = 'Version 2.1 for childre ages 3-7')),
    CARDSORT_Test_Ages = list( Description = 'Card sorting task verison administered',
                               Levels = list ('Ages 8-11 v2.1' = 'Version 2.1 for childen ages 8-11',
                                              'Ages 3-7 v2.1' = 'Version 2.1 for childre ages 3-7')),
    LISTSORT_Test_Ages = list( Description = 'List sorting task version administered',
                               Levels = list ('Age 7+ v2.1' = 'Version 2.1 for children ages 7+')),
    FLANKER_RawScore = list( Description = 'Flanker raw score'),
    CARDSORT_RawScore = list( Description = 'Card sorting raw score'),
    LISTSORT_RawScore = list( Description = 'List sorting raw score'),
    # FLANKER_Theta = list( Description = ''),
    # CARDSORT_Theta = list( Description = ''),
    # LISTSORT_Theta = list( Description = ''),
    # FLANKER_TScore = list( Description = ''),
    # CARDSORT_TScore = list( Description = ''),
    # LISTSORT_TScore = list( Description = ''),
    # FLANKER_SE = list( Description = ''),
    # CARDSORT_SE = list( Description = ''),
    # LISTSORT_SE = list( Description = ''),
    FLANKER_ItmCnt = list( Description = 'Flanker Item Count: Count of items to which participants responded. This is a quick way to review how much data is missing for each participant.'),
    CARDSORT_ItmCnt = list( Description = 'Card sorting Item Count: Count of items to which participants responded. This is a quick way to review how much data is missing for each participant.'),
    LISTSORT_ItmCnt = list( Description = 'List sorting Item Count: Count of items to which participants responded. This is a quick way to review how much data is missing for each participant.'),
    FLANKER_DateFinished = list( Description = 'Flanker: time stamp when the instrument was finished'),
    CARDSORT_DateFinished = list( Description = 'Card sorting: time stamp when the instrument was finished'),
    LISTSORT_DateFinished = list( Description = 'List sorting: time stamp when the instrument was finished'),
    FLANKER_Computed.Score = list( Description = 'Flanker Computed Score: A computed score is used on certain tests where an "interim" calculation is required to go from the raw score to the normative score. For example, Flanker and DCCS use the computed score to sum the accurary and reaction time scores into one score. Computed score has a different specific meaning for different tests.'),
    CARDSORT_Computed.Score = list( Description = 'Card sorting Computed Score: A computed score is used on certain tests where an "interim" calculation is required to go from the raw score to the normative score. For example, Flanker and DCCS use the computed score to sum the accurary and reaction time scores into one score. Computed score has a different specific meaning for different tests.'),
    LISTSORT_Computed.Score = list( Description = 'List sorting Computed Score: A computed score is used on certain tests where an "interim" calculation is required to go from the raw score to the normative score. For example, Flanker and DCCS use the computed score to sum the accurary and reaction time scores into one score. Computed score has a different specific meaning for different tests.'),
    FLANKER_Uncorrected.Standard.Score = list( Description = 'Flanker Uncorrected Standard Score: This is an additional available score, which also uses a standard score metric (normative mean = 100, SD = 15). It compares the performance of the test-taker to those in the entire NIH Toolbox nationally representative normative sample, regardless of age or any other variable. The Uncorrected Standard Score provides a glimpse of the given participant\'s overall performance or ability when compared with the general U.S. population. Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).'),
    CARDSORT_Uncorrected.Standard.Score = list( Description = 'Card sorting Uncorrected Standard Score: This is an additional available score, which also uses a standard score metric (normative mean = 100, SD = 15). It compares the performance of the test-taker to those in the entire NIH Toolbox nationally representative normative sample, regardless of age or any other variable. The Uncorrected Standard Score provides a glimpse of the given participant\'s overall performance or ability when compared with the general U.S. population. Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).'),
    LISTSORT_Uncorrected.Standard.Score = list( Description = 'List sorting Uncorrected Standard Score: This is an additional available score, which also uses a standard score metric (normative mean = 100, SD = 15). It compares the performance of the test-taker to those in the entire NIH Toolbox nationally representative normative sample, regardless of age or any other variable. The Uncorrected Standard Score provides a glimpse of the given participant\'s overall performance or ability when compared with the general U.S. population. Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).'),
    FLANKER_Age.Corrected.Standard.Score = list( Description = 'Flanker Age-Corrected Standard Score: This score compares the score of the test-taker to those in the NIH Toolbox nationally representative normative sample at the same age, where a score of 100 indicates performance that was at the national average for the test-taking participant\'s age. Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).'),
    CARDSORT_Age.Corrected.Standard.Score = list( Description = 'Card sorting Age-Corrected Standard Score: This score compares the score of the test-taker to those in the NIH Toolbox nationally representative normative sample at the same age, where a score of 100 indicates performance that was at the national average for the test-taking participant\'s age. Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).'),
    LISTSORT_Age.Corrected.Standard.Score = list( Description = 'List sorting Age-Corrected Standard Score: This score compares the score of the test-taker to those in the NIH Toolbox nationally representative normative sample at the same age, where a score of 100 indicates performance that was at the national average for the test-taking participant\'s age. Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).'),
    FLANKER_National.Percentile..age.adjusted. = list( Description = 'Flanker Age adjusted national percentile'),
    CARDSORT_National.Percentile..age.adjusted. = list( Description = 'Card sorting Age adjusted national percentile'),
    LISTSORT_National.Percentile..age.adjusted. = list( Description = 'List sorting Age adjusted national percentile'),
    FLANKER_Fully.Corrected.T.score = list( Description = 'Flanker Fully-Corrected T-score: This score (which has a mean of 50 and an SD of 10, unlike age-corrected performance normative scores) compares the score of the test-taker to those in the NIH Toolbox nationally representative normative sample, while adjusting for key demographic variables collected during the NIH Toolbox national norming study.  Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).'),
    CARDSORT_Fully.Corrected.T.score = list( Description = 'Card sorting Fully-Corrected T-score: This score (which has a mean of 50 and an SD of 10, unlike age-corrected performance normative scores) compares the score of the test-taker to those in the NIH Toolbox nationally representative normative sample, while adjusting for key demographic variables collected during the NIH Toolbox national norming study.  Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).'),
    LISTSORT_Fully.Corrected.T.score = list( Description = 'List sorting Fully-Corrected T-score: This score (which has a mean of 50 and an SD of 10, unlike age-corrected performance normative scores) compares the score of the test-taker to those in the NIH Toolbox nationally representative normative sample, while adjusting for key demographic variables collected during the NIH Toolbox national norming study.  Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).')

  )


  # convert formatting to JSON
  toolbox_json <- RJSONIO::toJSON(toolbox_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(toolbox_json, asText = TRUE))){
    print('NIH toolbox beh JSON file may be invalid')
  }

  return(toolbox_json)

}
