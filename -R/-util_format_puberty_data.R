#' util_format_puberty_data: Prepare parent-reported puberty data for scoring (called within util_redcap_parent_v1.R and util_redcap_parent_v5.R)
#'
#' This function prepares parent-reported puberty data for scoring with dataprepr::score_puberty()
#'
#'
#' @param puberty_data Puberty data (Parental Rating Scale for Pubertal Development and Tanner) extracted from data from REDCap events
#' @param respondent string to indicate who completed the form. Must be 'child' or 'parent'
#'
#' @examples
#'
#' # process kbas data
#' puberty_data_formatted <- util_format_puberty_data(puberty_data, respondent = 'child')
#'
#' @seealso [util_redcap_parent_v1()], [util_redcap_parent_v5()]
#'
#' @export


util_format_puberty_data <- function(puberty_data, respondent) {

  if (respondent == 'parent') {

    # rename 'prs' to 'pds'
    names(puberty_data) <- gsub('prs', 'pds', names(puberty_data))

    # fix sex
    names(puberty_data) <- gsub('pds_sex', 'sex', names(puberty_data))

    # switch naming convention
    names(puberty_data)[grepl('boys', names(puberty_data))] <- paste0(names(puberty_data)[grepl('boys', names(puberty_data))], 'm')
    names(puberty_data)[grepl('girls', names(puberty_data))] <- paste0(names(puberty_data)[grepl('girls', names(puberty_data))], 'f')

    names(puberty_data) <- gsub('_boys|_girls', '', names(puberty_data))

    names(puberty_data)[names(puberty_data) == 'pds_5f'] <- 'pds_5fa'

    # unify pds 1, 2, 3, 6
    puberty_data['pds_1'] <- ifelse(puberty_data[['sex']] == 0, puberty_data[['pds_1f']], puberty_data[['pds_1m']])
    puberty_data['pds_2'] <- ifelse(puberty_data[['sex']] == 0, puberty_data[['pds_2f']], puberty_data[['pds_2m']])
    puberty_data['pds_3'] <- ifelse(puberty_data[['sex']] == 0, puberty_data[['pds_3f']], puberty_data[['pds_3m']])
    puberty_data['pds_6'] <- ifelse(puberty_data[['sex']] == 0, puberty_data[['pds_6f']], puberty_data[['pds_6m']])

    # fix tanner choice
    puberty_data['tanner_choice'] <- ifelse(puberty_data[['sex']] == 0, puberty_data[['tanner_female_choice']], puberty_data[['tanner_male_choice']])

    puberty_data <- puberty_data[, !names(puberty_data) %in% c('pds_1f', 'pds_2f', 'pds_3f', 'pds_6f', 'pds_1m', 'pds_2m', 'pds_3m', 'pds_6m', 'tanner_female_choice', 'tanner_male_choice')]

    puberty_data <- puberty_data[c('participant_id', 'session_id', 'visit_date', 'sex', 'pds_1', 'pds_2', 'pds_3', 'pds_4m', 'pds_5m', 'pds_4f', 'pds_5fa', 'pds_6', 'tanner_choice')]

  } else if (respondent == 'child') {
    # is p6 part of scoring for children? -- not in Table1 A Self-Administered sting Scale for Development Carskadon and Acebo

    # rename to match
    names(puberty_data)[names(puberty_data) == 'tanner_sex_v5'] <- 'sex'

    names(puberty_data)[names(puberty_data) == 'childreport_puberty_height'] <- 'pds_1'
    names(puberty_data)[names(puberty_data) == 'childreport_puberty_hair'] <- 'pds_2'
    names(puberty_data)[names(puberty_data) == 'childreport_puberty_acne'] <- 'pds_3'

    names(puberty_data)[names(puberty_data) == 'childreport_puberty_voice'] <- 'pds_4m'
    names(puberty_data)[names(puberty_data) == 'childreport_puberty_facialhair'] <- 'pds_5m'

    names(puberty_data)[names(puberty_data) == 'childrep_puberty_breast'] <- 'pds_4f'
    names(puberty_data)[names(puberty_data) == 'childrep_puberty_menses'] <- 'pds_5fa'
    names(puberty_data)[names(puberty_data) == 'childrep_pub_mensesonset'] <- 'pds_5fb'
    names(puberty_data)[names(puberty_data) == 'childrep_menses_lastperiod'] <- 'pds_5fc'
    names(puberty_data)[names(puberty_data) == 'childrep_menses_cycledur'] <- 'pds_5fd'

    # unify pds_6
    puberty_data['pds_6'] <- ifelse(puberty_data[['sex']] == 0, puberty_data[['childrep_puberty_girlcomp']], puberty_data[['childrep_puberty_boycomp']])

    # remove old columns
    puberty_data <- puberty_data[, !grepl('childrep', names(puberty_data))]

    # fix tanner choice
    puberty_data['tanner_choice'] <- ifelse(puberty_data[['sex']] == 0, puberty_data[['tanner_female_choice_v5']], puberty_data[['tanner_male_choice_v5']])

    # remove old columns
    puberty_data <- puberty_data[, !grepl('male', names(puberty_data))]

  } else {
    print('respondent input argument must be \'child\' or \'parent\'')
  }

  # set 4 to '99'
  puberty_data[grepl('pds', names(puberty_data))] <- sapply(names(puberty_data[grepl('pds', names(puberty_data))]), function(x) ifelse(puberty_data[[x]] == 4, 99, puberty_data[[x]]))

  # for pds_5fa, set 2 to 99
  puberty_data['pds_5fa'] <- ifelse(puberty_data[['pds_5fa']] == 2, 99, puberty_data[['pds_5fa']])

  # re-label sex
  puberty_data$sex <- ifelse(puberty_data$sex == 0, 'female', ifelse(puberty_data$sex == 1, 'male', NA))

  # return data
  return(puberty_data)
}
