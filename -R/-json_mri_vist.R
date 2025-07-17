#' json_mri_visit: Generates a json file for MRI visit data
#'
#' This function generates a json file for MRI visit data (notes, CAMS, freddy fullness scores)
#'
#' @return A string with data stored in JSON format containing meta-data for the MRI visit data
#'
#'
#' @export

json_mri_visit <- function() {

  mri_visit_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    mock_fmri_complete_check = list( Description = 'Researcher indication about mock fmri protocol was completed'),
    mock_fmri_notes = list( Description = 'Researcher notes about mock fmri protocol'),
    mri_anatomy_complete_check = list( Description = 'Researcher indication about whether MPRage was completed'),
    mri_anatomy_notes = list( Description = 'Researcher notes about MPRage'),
    mri_food_run_1_check = list( Description = 'Researcher indication about whether run 1 of food view task was completed',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes')),
    mri_food_run_2_check = list( Description = 'Researcher indication about whether run 2 of food view task was completed',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes')),
    mri_food_run_3_check = list( Description = 'Researcher indication about whether run 3 of food view task was completed',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes')),
    mri_food_run_4_check = list( Description = 'Researcher indication about whether run 4 of food view task was completed',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes')),
    mri_food_viewing_notes = list( Description = 'Researcher notes about food viewing task'),
    mri_sst_run_1_check = list( Description = 'Researcher indication about whether run 1 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_run_2_check = list( Description = 'Researcher indication about whether run 2 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_run_3_check = list( Description = 'Researcher indication about whether run 3 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_run_4_check = list( Description = 'Researcher indication about whether run 4 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_run_5_check = list( Description = 'Researcher indication about whether run 5 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_run_6_check = list( Description = 'Researcher indication about whether run 6 of SST was completed',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    mri_sst_notes = list( Description = 'Researcher notes about SST'),
    cams_pre_mri = list( Description = 'Pre-MRI state anxiety rating',
                         Reference = 'Ersig AL, Kleiber C, McCarthy AM, Hanrahan K. Validation of a clinically useful measure of children\'s state anxiety before medical procedures. J Spec Pediatr Nurs. 2013 Oct;18(4):311-9. doi: 10.1111/jspn.12042. Epub 2013 Jun 25. PMID: 24094126; PMCID: PMC4282760.'),
    cams_post_mri = list( Description = 'Post-MRI state anxiety rating',
                          Reference = 'Ersig AL, Kleiber C, McCarthy AM, Hanrahan K. Validation of a clinically useful measure of children\'s state anxiety before medical procedures. J Spec Pediatr Nurs. 2013 Oct;18(4):311-9. doi: 10.1111/jspn.12042. Epub 2013 Jun 25. PMID: 24094126; PMCID: PMC4282760.'),
    freddy_pre_snack = list( Description = 'Pre-snack fullness rating on a 150 mm visual analgue scale',
                             Unit = 'mm',
                             Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    freddy_post_snack = list( Description = 'Post-snack fullness rating on a 150 mm visual analgue scale',
                              Unit = 'mm',
                              Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    freddy_post_snack2 = list( Description = 'Post- second snack snack fullness rating on a 150 mm visual analgue scale',
                               Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    freddy_pre_mri = list( Description = 'Pre-mri fullness rating on a 150 mm visual analgue scale',
                           Unit = 'mm',
                           Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.')
  )

  # convert formatting to JSON
  mri_visit_json <- RJSONIO::toJSON(mri_visit_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(mri_visit_json, asText = TRUE))){
    print('MRI visit JSON file may be invalid')
  }

  return(mri_visit_json)

}
