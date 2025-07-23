#' write_redcap: Write selected data and json files from processed REDCap data
#'
#' This function:
#' \itemize{
#'    \item{1) Calls proc_redcap function to get clean and compiled data and metadata}
#'    \item{2) Exports all or select BIDS-compliant .csv and .json files into bids/phenotype}
#'}
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @inheritParams proc_tasks
#' @inheritParams proc_tasks
#' @param export list of strings matching the notes below to indicate the data to be written. Default = 'all' to export all data and metadata. Options include:
#' \itemize{
#'  \item{'participants' - BIDS specified participants.csv file}
#'  \item{'anthropometrics' - height, weight, and computed anthropometric data}
#'  \item{'demographics' - compiled demographic data}
#'  \item{'dxa' - verified DXA data}
#'  \item{'household' - compiled demographicinformation about houshold}
#'  \item{'infancy' - compiled demographic information related to infancy}
#'  \item{'intake' - compiled intake data with computed intake values}
#'  \item{'researcher_notes' - all visit notes}
#'  \item{'rsa' - all rsa notes}
#'  \item{'asbi' - Adaptive Social Behavioral Inventory}
#'  \item{'bevq' - BEVQ-PS, Beverage FFQ}
#'  \item{'body_es' - Body Esteem Scale}
#'  \item{'bis' - Behavioral Image Scale}
#'  \item{'brief2' - Behavioral Rating Inventory of Executive Function-2}
#'  \item{'cbq' - Child Behavior Questionnaire}
#'  \item{'cebq' - Children's Eating Behavior Questionnaire}
#'  \item{'cfsq' - Comprehensive Feeding Style Questionnaire}
#'  \item{'cfq' - Child Feeding Questionnaire}
#'  \item{'cwc' - Child Weight Concerns}
#'  \item{'ecsi2' - atter Eating Competence Inventory 2.0}
#'  \item{'hfi' - Fulkerson Home Food Inventory}
#'  \item{'nns parent' - Non-nutritive Sweetener FFQ - Parent}
#'  \item{'nns child' - Non-nutritive Sweetener FFQ - Child}
#'  \item{'nnsa' - FNon-nutritive Sweetener Attitudes}
#'  \item{'pst' - Portion Sorting Task *need*}
#'  \item{'pwlb' - Parent Weight-Loss Behavior Questionnaire}
#'  \item{'rank' - Parent ranking of foods sources? *need*}
#'  \item{'sdor' - Scatter Division of Responsibility in Feeding Scale}
#'  \item{'social_dq' - Social Desirability Sacle}
#'  \item{'tfeq' - Three Factor Eating Questionnaire}
#' }
#'
#' @param return (logical) return data to working environment. Default = FALSE.
#'
#' @return Does not return anything
#'
#'
#' @examples
#'
#' \dontrun{
#' write_redcap(base_wd, overwrite = FALSE, data_list = 'all')
#'
#' }
#'
#' @importFrom utils tail write.table read.csv head
#'
#' @export

write_redcap <- function(base_wd, overwrite = FALSE, data_list = 'all', return_data = FALSE) {

  #### Set up/initial checks #####

  # check that base_wd exist and is a string
  data_arg <- methods::hasArg(base_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(base_wd)) {
      stop('base_wd must be entered as a string')
    } else if (!file.exists(base_wd)) {
      stop('base_wd entered, but file does not exist. Check base_wd string.')
    }
  } else if (isFALSE(data_arg)) {
    stop('base_wd must be entered as a string')
  }

  #### Get REDCap Data ####
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

  # Make ID column bids compliant: Convert record_id to strings padded with zeros and add 'sub_'
  redcap_visit_data['record_id'] <- sprintf('sub-%03d', redcap_visit_data[['record_id']])

  # set paths for other directories
  phenotype_wd <- file.path(base_wd, 'phenotype')

  #### Process REDCap data ####
  proc_redcap_data <- proc_redcap(redcap_api = FALSE, redcap_visit_data, redcap_de_data)

  # quick fixes for notes where /n formatting got saved
  proc_redcap_data$intake$data[grepl('notes', names(proc_redcap_data$intake$data))] <- sapply(names(proc_redcap_data$intake$data)[grepl('notes', names(proc_redcap_data$intake$data))], function(x) gsub('\n', '', proc_redcap_data$intake$data[[x]]))


  #### function to export data and metadata ####
  #data_lsit_options <- c('participants', 'anthropometrics', 'demographics', 'dxa', 'household', 'infancy', 'intake', 'researcher_notes', 'rsa', 'asbi', 'bevq', 'body_es', 'bis', 'brief2', 'cbq', 'cebq', 'cfsq', 'cfq', 'cwc', 'ecsi2', 'hfi', 'nns_parent', 'nns_child', 'nnsa', 'pst', 'pwlb', 'rank', 'sdor', 'social_dq', 'tfeq')

   data_lsit_options <- c('demographics', 'dxa', 'household', 'infancy', 'intake', 'asbi', 'bevq', 'body_es', 'bis', 'brief2', 'cbq', 'cebq', 'cfsq', 'cfq', 'cwc', 'ecsi2', 'hfi', 'pwlb', 'rank', 'sdor', 'social_dq', 'tfeq')

  # loop through data_to_export and export data and meta-data
  redcap_export <- function(data_str, overwrite){

    if (data_str %in% data_lsit_options){

      if (data_str == 'participants'){
        filename_csv <- file.path(bids_wd, paste0(data_str, '.csv'))
        filename_json <- file.path(bids_wd, paste0(data_str, '.json'))
      } else {
        filename_csv <- file.path(phenotype_wd, paste0(data_str, '.csv'))
        filename_json <- file.path(phenotype_wd, paste0(data_str, '.json'))
      }

      # write tsv
      if ( isTRUE(overwrite) | !file.exists(filename_csv) ) {
        # use 'n/a' for missing values for BIDS compliance

        write.table(proc_redcap_data[[data_str]]$data, filename_csv, quote = FALSE, sep = ',', col.names = TRUE, row.names = FALSE, na = 'n/a')
      }

      # write json
      if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
        write(proc_redcap_data[[data_str]]$meta, filename_json)
      }

    } else {
      print(paste0(data_str, ' is not one of the available data set options to print in write_redcap(). Please see help(write_redcap)'))
    }
  }

  if (data_list == 'all'){
    data_list <- data_lsit_options
  }

  write_redcap_output <- sapply(data_list, function(x) redcap_export(x, overwrite))

  #move elsewhere##
  # export dataset_description.json
  # filename_json <- file.path(phenotype_wd, 'dataset_description.json')
  # json <- json_phe_dataset_desc(visit_data_path, data_de_path)
  # if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
  #   write(json, filename_json)
  }

  #### Return Data ####
  if (isTRUE(return_data)) {
    return(proc_redcap_data)
  }
}

