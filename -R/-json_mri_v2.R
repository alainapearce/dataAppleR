#' json_mri_v2: Generates a json file for MRI visit 2 data
#'
#' This function generates a json file for MRI visit 2 data
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_mri_v2 <- function() {

  mri_v2_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    pre_snack_fullness = list( Description = 'Researcher completed pre-snack fullness prior to MRI scan',
                             Levels = list ('0' = 'No',
                                            '1' = 'Yes')),
    pre_snack_fullness_time = list( Description = 'Pre-snack fullness prior to MRI scan rating time',
                                  Unit = "hh:mm:ss"),
    post_snack_fullness = list( Description = 'Researcher completed post-snack fullness prior to MRI scan',
                               Levels = list ('0' = 'No',
                                              '1' = 'Yes')),
    post_snack_fullness_time = list( Description = 'Post-snack fullness prior to MRI scan rating time',
                                    Unit = "hh:mm:ss"),
    post_snack2_fullness = list( Description = 'Researcher completed Post 2nd snack fullness prior to MRI scan (occurs if post_snack_fullness_score is still low)',
                             Levels = list ('0' = 'No',
                                            '1' = 'Yes')),
    post_snack2_fullness_time = list( Description = 'Post 2nd snack fullness prior to MRI scan rating time (occurs if post_snack_fullness_score is still low)',
                                  Unit = "hh:mm:ss"),
    mock_fmri_notes = list( Description = 'Researcher notes about mock fmri protocol'),
    pre_cams =  list( Description = 'Researcher completed Child Anxiety Meter Scale prior to MRI scan',
                      Levels = list ('0' = 'No',
                                     '1' = 'Yes')),
    pre_fmri_fullness = list( Description = 'Researcher completed pre-fMRI fullness directly prior to MRI scan',
                             Levels = list ('0' = 'No',
                                            '1' = 'Yes')),
    pre_fmri_fullness_time = list( Description = 'Pre-fMRI fullness directly prior to MRI scan rating time',
                                    Unit = "hh:mm:ss"),
    mri_anatomy_notes = list( Description = 'Researcher notes about MPRage'),
    mri_food_run1 = list( Description = 'Researcher indication about whether run 1 of food view task was completed',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes')),
    mri_food_run2 = list( Description = 'Researcher indication about whether run 2 of food view task was completed',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes')),
    mri_food_run3 = list( Description = 'Researcher indication about whether run 3 of food view task was completed',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes')),
    mri_food_run4 = list( Description = 'Researcher indication about whether run 4 of food view task was completed',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes')),
    mri_food_viewing_notes = list( Description = 'Researcher notes about food viewing task'),
    mri_sst_run1 = list( Description = 'Researcher indication about whether run 1 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_run2 = list( Description = 'Researcher indication about whether run 2 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_run3 = list( Description = 'Researcher indication about whether run 3 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_run4 = list( Description = 'Researcher indication about whether run 4 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_run5 = list( Description = 'Researcher indication about whether run 5 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_run6 = list( Description = 'Researcher indication about whether run 6 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_notes = list( Description = 'Researcher notes about SST'),
    pre_snack_fullness_score = list( Description = 'Pre-snack fullness rating on a 150 mm visual analgue scale',
                                   Unit = "mm",
                                   Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    post_snack_fullness_score = list( Description = 'Post-snack fullness rating on a 150 mm visual analgue scale',
                                   Unit = "mm",
                                   Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    post_snack2_fullness_score = list( Description = 'Post 2nd snack fullness rating on a 150 mm visual analgue scale (occurs if post_snack_fullness_score is still low)',
                                   Unit = "mm",
                                   Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    pre_mri_fullness_score = list( Description = 'Pre-fMRI fullness rating on a 150 mm visual analgue scale',
                                   Unit = "mm",
                                 Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    pre_cams_score = list( Description = 'Pre-MRI state anxiety rating',
                         Reference = 'Ersig AL, Kleiber C, McCarthy AM, Hanrahan K. Validation of a clinically useful measure of children\'s state anxiety before medical procedures. J Spec Pediatr Nurs. 2013 Oct;18(4):311-9. doi: 10.1111/jspn.12042. Epub 2013 Jun 25. PMID: 24094126; PMCID: PMC4282760.'),
    post_cams_score = list( Description = 'Post-MRI state anxiety rating',
                          Reference = 'Ersig AL, Kleiber C, McCarthy AM, Hanrahan K. Validation of a clinically useful measure of children\'s state anxiety before medical procedures. J Spec Pediatr Nurs. 2013 Oct;18(4):311-9. doi: 10.1111/jspn.12042. Epub 2013 Jun 25. PMID: 24094126; PMCID: PMC4282760.')
  )

  # convert formatting to JSON
  mri_v2_json <- RJSONIO::toJSON(mri_v2_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(mri_v2_json, asText = TRUE))){
    print('MRI visit 2 JSON file may be invalid')
  }

  return(mri_v2_json)

}
