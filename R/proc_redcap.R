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

  ## Merge intake-related data
  # merge intake-related data (paradigm info, liking data, wanting data, intake data, fullness data)
  merged_intake <- util_merged_intake(child_v1_data, child_v2_data, child_v3_data, child_v4_data, child_v5_data, processed_de_data)

  #intake_merge_json <- json_intake()
  intake_merge_json <- NA

  #### Generate demographics dataframe  ####
  merged_demo <- util_merged_demo(visit1_demo = parent_v1_data$demo_data$data, household_all = parent_v1_data$household_data$data, anthro_data = child_v1_data$anthro_data$data, date_data)

  merged_demo <- merged_demo[!is.na(merged_demo['participant_id']), ]

  #demographics_json <- json_demographics()
  demographics_json <- NA

  #### Generate microstructure dataframe ####


  #### Generate participants dataframe ####
  # participants_data <- util_merged_participants(parent_v1_data$demo_data$data, merged_demo, date_data)
  #
  # participants_data <- participants_data[!is.na(participants_data['participant_id']), ]
  #
  # participants_json <- json_participants()



  #### Data to return ####

  # list dataframes to return, where the name is the corresponding json function without 'json_'
  return(list(
    #participants = list(data = participants_data, meta = participants_json),
    demographics = list(data = merged_demo, meta = demographics_json),
    dxa = list(data = processed_de_data$dxa$data,
               meta = processed_de_data$dxa$meta),
    household = list(data = parent_v1_data$household_data$data,
                     meta = parent_v1_data$household_data$meta),
    #rsa
    infancy = list(data = parent_v1_data$infancy_data$data,
                   meta = parent_v1_data$infancy_data$meta),
    intake = list(data = merged_intake, meta = intake_merge_json),
    #researcher_notes = list(data = researcher_notes, meta = notes_json),
    asbi = list(data = parent_v4_data$asbi_data$data,
                 meta = parent_v4_data$asbi_data$meta),
    bevq = list(data = parent_v5_data$bevq_data$data,
                # data = parent_v5_data$bevq_data$data$bids_phenotype,
                meat = parent_v5_data$bevq_data$meta),

    body_es = list(data = child_v5_data$body_es_data$data$bids_phenotype,
                   meat = child_v5_data$body_es_data$meta),
    bis = list(data = child_v5_data$bis_data$data$bids_phenotype,
               meat = child_v5_data$bis_data$meta),
    brief2 = list(data = parent_v2_data$brief_data$data$bids_phenotype,
                  meta = parent_v2_data$brief_data$meta),
    cbq = list(data = parent_v2_data$cbq_data$data$bids_phenotype,
               meta = parent_v2_data$cbq_data$meta),
    cebq = list(data = parent_v1_data$cebq_data$data$bids_phenotype,
                meta = parent_v1_data$cebq_data$meta),
    cfsq = list(data = parent_v4_data$cfsq_data$data$score_dat,
                meta = parent_v4_data$cfpq_data$meta),
    cfq = list(data = parent_v1_data$cfq_data$data$bids_phenotype,
               meta = parent_v1_data$cfq_data$meta),
    cwc = list(data = child_v5_data$cwc_data$data$bids_phenotype,
                 meta = child_v5_data$cwc_data$meta),
    ecsi2 = list(data = parent_v4_data$ecsi2_data$data$bids_phenotype,
                 meta = parent_v4_data$ecsi2_data$meta),
    # hfi = list(data = parent_v4_data[['hfi_data']]$data$bids_phenotype,
    #            meta = parent_v4_data[['hfi_data']]$meta),
    nns_parent = list(data = parent_v5_data$nns_parent_data$data,
                 meta = parent_v5_data$nns_parent_data$meta),
    nns_child = list(data = parent_v5_data$nns_child_data$data,
                      meta = parent_v5_data$nns_child_data$meta),
    nnsa = list(data = parent_v5_data$nnsa_data$data,
                meta = parent_v5_data$nnsa_data$meta),
    pst = list(data = child_v1_data$pst_data$data,
               meta = child_v1_data$pst_data$meta),
    pwlb = list(data = parent_v3_data$pwlb_data$data$bids_phenotype,
                meta = parent_v3_data$pwlb_data$meta),
    rank = list(data = parent_v1_data$rank_data$data,
                meta = parent_v1_data$rank_data$meta),
    sdor = list(data = parent_v3_data$sdore_data$data$bids_phenotype,
                meta = parent_v3_data$sdore_data$meta),
    social_dq = list(data = child_v4_data$sdq_data$data$bids_phenotype,
               meta = child_v4_data$sdq_data$data$meta),
    tfeq = list(data = parent_v3_data$tfeq_data$data$bids_phenotype,
                meta = parent_v3_data$tfeq_data$meta)
    ))
}

