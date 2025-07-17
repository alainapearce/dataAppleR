#' proc_redcap: Process raw data downloaded from Study REACH REDCap
#'
#' This function:
#' \itemize{
#'    \item{1) Reads REDCap data (visit and double-entry) using the REDCap API}
#'    \item{2) Calls util_ functions to clean and compile data in dataframes}
#'    \item{3) Calls json_ functions to create strings with meta-data stored in JSON format for each dataframe}
#'    \item{4) Compiles data repeated across visits and sessions}
#'}
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @inheritParams util_redcap_de
#' @param redcap_visit_data REDCap visit data from a prior API call
#' @inheritParams util_redcap_de
#'
#' @return Will return a list including data and metadata for:
#' #' \itemize{
#'  \item{'paticipants' - BIDS specified participants.tsv file}
#'  \item{'anthropometrics' - height, weight, and computed anthropometric data}
#'  \item{'demographics' - compiled demographic data}
#'  \item{'dxa' - verified DXA data}
#'  \item{'household' - compiled demographicinformation about houshold}
#'  \item{'infancy' - compiled demographic information related to infancy}
#'  \item{'intake' - compiled intake data with computed intake values}
#'  \item{'parent_updates' - all visit updates}
#'  \item{'researcher_notes' - all visit notes}
#'  \item{'asbi' - Adaptive Social Behavioral Inventory}
#'  \item{'bevqps' - Beverage Questionnaire for Preschoolers}
#'  \item{'brief2' - Behavioral Rating Inventory of Executive Function}
#'  \item{'cbq' - Child Behavior Questionnaire}
#'  \item{'cebq' - Children's Eating Behavior Questionnaire}
#'  \item{'cfsq' - Caregiver's Feeding Style Questionnaire}
#'  \item{'dor' - Division of Responsibility}
#'  \item{'ecsi2' - Parent Eating Competence Questionnaire}
#'  \item{'tfeq' - Three Factor Eating Questionnaire}
#'  \item{'nns_ffq' - Non-Nutritive Sweetener FFQ (2 versions - parent and child)}
#'  \item{'pwlb' - Parent Weight Loss Behaviors}
#' }
#'
#' @examples
#'
#' \dontrun{
#' redcap_data <- proc_redcap(base_wd, overwrite = FALSE, overwrite_jsons = FALSE)
#'
#' }
#'
#' @seealso [write_redcap()]
#'
#' @export

proc_redcap <- function(redcap_api = FALSE, redcap_visit_data, redcap_de_data) {

  #### Set up/initial checks #####

  # check that data is passed if redcap_api = FALSE
  if (isFALSE(redcap_api)){

    # check that redcap_visit_data exist and is a data.frame
    visit_data_arg <- methods::hasArg(redcap_visit_data)

    if (isTRUE(visit_data_arg)) {
      if (!is.data.frame(redcap_visit_data)) {
        stop('redcap_visit_data must be a data.frame with recap_api = FALSE')
      }
    } else if (isFALSE(visit_data_arg)) {
      stop('redcap_visit_data must be a data.frame with recap_api = FALSE')
    }

    # check that redcap_de_data exist and is a data.frame
    de_data_arg <- methods::hasArg(redcap_de_data)

    if (isTRUE(de_data_arg)) {
      if (!is.data.frame(redcap_de_data)) {
        stop('redcap_de_data must be a data.frame with recap_api = FALSE')
      }
    } else if (isFALSE(de_data_arg)) {
      stop('redcap_de_data must be a data.frame with recap_api = FALSE')
    }

  } else {
    # get data from REDCap directly (only will work if have access and keys setup)
    Sys.setenv(reach_redcap_key = keyring::key_get('apple_redcap_key'))
    redcap_visit <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/', token = Sys.getenv('reach_redcap_key'))


    Sys.setenv(reach_de_redcap_key = keyring::key_get('apple-de_redcap_key'))
    redcap_de <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/', token = Sys.getenv('reach_de_redcap_key'))

    redcap_visit_data <- redcap_visit[['data']]
    redcap_visit_dict <- redcap_visit[['dictionary']]

    redcap_de_data <- redcap_de[['data']]
    redcap_de_dict <- redcap_de[['dictionary']]

    # remove '.factor'
    redcap_visit_data <- redcap_visit_data[, !grepl('.factor', names(redcap_visit_data))]
    redcap_de_data <- redcap_de_data[, !grepl('.factor', names(redcap_de_data))]


  }

  #### Extract visit data ####

  # # subset events and remove unnecessary columns
  redcap_long_wide <- function(event_name, data){

    #subset
    sub_dat <- data[data[['redcap_event_name']] == event_name, ]

    #remove empty columns
    sub_dat <- sub_dat[, !colSums(is.na(sub_dat)) == nrow(sub_dat)]

    #return
    return(sub_dat)
  }

  # Extract visit data
  child_visit_1_arm_1 <- redcap_long_wide('child_visit_1_arm_1', redcap_visit_data)
  parent_visit_1_arm_1 <- redcap_long_wide('parent_visit_1_arm_1', redcap_visit_data)
  child_visit_2_arm_1 <- redcap_long_wide('child_visit_2_arm_1', redcap_visit_data)
  parent_visit_2_arm_1 <- redcap_long_wide('parent_visit_2_arm_1', redcap_visit_data)
  child_visit_3_arm_1 <- redcap_long_wide('child_visit_3_arm_1', redcap_visit_data)
  parent_visit_3_arm_1 <- redcap_long_wide('parent_visit_3_arm_1', redcap_visit_data)
  child_visit_4_arm_1 <- redcap_long_wide('child_visit_4_arm_1', redcap_visit_data)
  parent_visit_4_arm_1 <- redcap_long_wide('parent_visit_4_arm_1', redcap_visit_data)
  child_visit_5_arm_1 <- redcap_long_wide('child_visit_5_arm_1', redcap_visit_data)
  parent_visit_5_arm_1 <- redcap_long_wide('parent_visit_5_arm_1', redcap_visit_data)

  #### Process visit data ####

  # make data.frame of dates, ages, and sex
  date_data <- util_redcap_dates(child_v1 = child_visit_1_arm_1, child_v2 = child_visit_2_arm_1, child_v3 = child_visit_3_arm_1, child_v4 = child_visit_4_arm_1, child_v5 = child_visit_5_arm_1, parent_v1 = parent_visit_1_arm_1)

  # visit survey data
  child_v1_data <- util_redcap_child_v1(child_visit_1_arm_1)
  parent_v1_data <- util_redcap_parent_v1(parent_visit_1_arm_1, date_data)
  child_v2_data <- util_redcap_child_v2(child_visit_2_arm_1)
  parent_v2_data <- util_redcap_parent_v2(parent_visit_2_arm_1, date_data)
  child_v3_data <- util_redcap_child_v3(child_visit_3_arm_1)
  parent_v3_data <- util_redcap_parent_v3(parent_visit_3_arm_1, date_data)
  child_v4_data <- util_redcap_child_v4(child_visit_4_arm_1)
  parent_v4_data <- util_redcap_parent_v4(parent_visit_4_arm_1, date_data)
  child_v5_data <- util_redcap_child_v5(child_visit_5_arm_1)
  parent_v5_data <- util_redcap_parent_v5(parent_visit_5_arm_1, date_data)


  #### Process double-entry data ####
  processed_de_data <- util_redcap_de(redcap_api = FALSE, redcap_de_data, date_data)

  #### Combine data across visits ####
  ## DXA data
  dxa_all <- rbind.data.frame(processed_de_data$dxa_v1$data, processed_de_data$dxa_v5$data)

  ## Merge intake-related data
  # merge intake-related data (paradigm info, liking data, wanting data, intake data, fullness data)
  merged_intake <- util_merged_intake(child_v1_data, child_v3_data, child_v4_data, child_v5_data, processed_de_data)

  intake_merge_json <- json_intake()

  ## questionnaires
  merged_qs_list <- util_merge_questionnaires(child_v1_data, child_v4_data, child_v5_data, parent_v1_data, parent_v2_data, parent_v3_data, parent_v4_data, parent_v5_data)

  updates_json <- json_parent_updates()

  ## Merge notes into notes database
  researcher_notes <- util_merged_visitnotes(child_v1_data$visit_data_child, child_v2_data$visit_data_child, child_v3_data$visit_data_child, child_v4_data$visit_data_child, child_v5_data$visit_data_child)

  notes_json <- json_researcher_notes()

  ## Anthro data
  merged_anthro <- util_merged_anthro(child_v1_data$anthro_data$data, child_v5_data$anthro_data$data,  merged_qs_list$household_all, date_data)

  anthro_merge_json <- json_anthropometrics()

  #### Generate demographics dataframe  ####
  merged_demo <- util_merged_demo(parent_v1_data$demo_data$data, merged_qs_list$household_all, merged_anthro, date_data)

  merged_demo <- merged_demo[!is.na(merged_demo['participant_id']), ]

  demographics_json <- json_demographics()

  #### Generate participants dataframe ####
  participants_data <- util_merged_participants(parent_v1_data$demo_data$data, merged_demo, date_data)

  participants_data <- participants_data[!is.na(participants_data['participant_id']), ]

  participants_json <- json_participants()

  #### Data to return ####

  # list dataframes to return, where the name is the corresponding json function without 'json_'
  return(list(
    participants = list(data = participants_data, meta = participants_json),
    anthropometrics = list(data = merged_anthro, meta = anthro_merge_json),
    demographics = list(data = merged_demo, meta = demographics_json),
    dxa = list(data = dxa_all, meta = processed_de_data$dxa_v1$meta),
    household = list(data = merged_qs_list$household_all,
                     meta = parent_v1_data$household_data$meta),
    infancy = parent_v1_data[['infancy_data']],
    intake = list(data = merged_intake, meta = intake_merge_json),
    mri_visit = child_v2_data$mri_info,
    parent_updates = list(data = merged_qs_list$updates_all, meta = updates_json),
    researcher_notes = list(data = researcher_notes, meta = notes_json),
    audit = list(data = merged_qs_list$audit_all,
                 meta = parent_v4_data$audit_data$meta),
    bes = list(data = parent_v2_data[['bes_data']]$data$bids_phenotype,
               meta = parent_v2_data[['bes_data']]$meta),
    bisbas = list(data = parent_v3_data[['bisbas_data']]$data$bids_phenotype,
                  meta = parent_v3_data[['bisbas_data']]$meta),
    brief2 = list(data = parent_v2_data[['brief_data']]$data$bids_phenotype,
                  meta = parent_v2_data[['brief_data']]$meta),
    cbq = list(data = merged_qs_list$cbq_all,
               meta = parent_v1_data$stq_data$meta),
    cchip = list(data = parent_v4_data[['cchip_data']]$data$bids_phenotype,
                 meta = parent_v4_data[['cchip_data']]$meta),
    cebq = list(data = merged_qs_list$cebq_all,
                meta = parent_v1_data$cebq_data$meta),
    cfpq = list(data = merged_qs_list$cfpq_all,
                meta = parent_v4_data$cfpq_data$meta),
    cfq = list(data = parent_v1_data[['cfq_data']]$data$bids_phenotype,
               meta = parent_v1_data[['cfq_data']]$meta),
    chaos = list(data = parent_v1_data[['chaos_data']]$data$bids_phenotype,
                 meta = parent_v1_data[['chaos_data']]$meta),
    class = list(data = merged_qs_list$class_all,
                 meta = parent_v3_data$class_data$meta),
    cshq = list(data = merged_qs_list$cshq_all,
                meta = parent_v2_data$cshq_data$meta),
    debq = list(data = parent_v3_data[['debq_data']]$data$bids_phenotype,
                meta = parent_v3_data[['debq_data']]$meta),
    efcr = list(data = parent_v1_data[['efcr_data']]$data$bids_phenotype,
                meta = parent_v1_data[['efcr_data']]$meta),
    ffbs = list(data = parent_v2_data[['ffbs_data']]$data$bids_phenotype,
                meta = parent_v2_data[['ffbs_data']]$meta),
    #once scored will need switch to bids_phenotype
    fsq = list(data = parent_v2_data[['fsq_data']]$data,
               meta = parent_v2_data[['fsq_data']]$meta),
    # fsq = list(data = parent_v2_data[['fsq_data']]$data$bids_phenotype,
    #            meta = parent_v2_data[['fsq_data']]$meta),
    hfi = list(data = parent_v4_data[['hfi_data']]$data$bids_phenotype,
               meta = parent_v4_data[['hfi_data']]$meta),
    hfias = list(data = parent_v4_data[['hfias_data']]$data$bids_phenotype,
                 meta = parent_v4_data[['hfias_data']]$meta),
    hfssm = list(data = parent_v4_data[['hfssm_data']]$data$bids_phenotype,
                 meta = parent_v4_data[['hfssm_data']]$meta),
    kbas = list(data = merged_qs_list$kbas_all,
                meta = child_v1_data$kbas_data$meta),
    lbc = list(data = parent_v1_data[['lbc_data']]$data$bids_phenotype,
               meta = parent_v1_data[['lbc_data']]$meta),
    loc = list(data = merged_qs_list$loc_all,
                    meta = child_v4_data$loc_data$meta),
    pmum = list(data = merged_qs_list$pmum_all,
                     meta = parent_v4_data$pmum_data$meta),
    pptq = list(data = child_v4_data[['pptq_data']]$data$bids_phenotype,
                meta = child_v4_data[['pptq_data']]$meta),
    pss = list(data = parent_v1_data[['pss_data']]$data$bids_phenotype,
               meta = parent_v1_data[['pss_data']]$meta),
    pstca = list(data = merged_qs_list$pstca_all,
                 meta = parent_v3_data$pstca_data$meta),
    puberty = list(data = merged_qs_list$puberty_all,
                        meta = parent_v1_data$puberty_data$meta),
    pwlb = list(data = parent_v3_data[['pwlb_data']]$data$bids_phenotype,
                meta = parent_v3_data[['pwlb_data']]$meta),
    rank = list(data = merged_qs_list$rank_all,
                     meta = parent_v1_data$rank_data$meta),
    scpf = list(data = parent_v3_data[['scpf_data']]$data$bids_phenotype,
                meta = parent_v3_data[['scpf_data']]$meta),
    #once scored will need switch to bids_phenotype
    sic = list(data = child_v4_data[['sic_data']]$data,
               meta = child_v4_data[['sic_data']]$meta),
    # sic = list(data = child_v4_data[['sic_data']]$data$bids_phenotype,
    #            meta = child_v4_data[['sic_data']]$meta),
    sleeplog = child_v3_data[['sleeplog_data']],
    spsrq = list(data = parent_v3_data[['spsrq_data']]$data$bids_phenotype,
                 meta = parent_v3_data[['spsrq_data']]$meta),
    stq = list(data = merged_qs_list$stq_all,
               meta = child_v1_data$stq_data$meta),
    tfeq = list(data = parent_v3_data[['tfeq_data']]$data$bids_phenotype,
                meta = parent_v3_data[['tfeq_data']]$meta)
    ))
}

