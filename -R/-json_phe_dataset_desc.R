#' json_phe_dataset_desc: Generate dataset_description JSON for phenotype
#'
#' This function generates a json file for phenotype/
#'
#' @param visit_data_path full path to the redcap visit data used to generate phenotype data with proc_redcap.R
#' @param data_de_path full path to the redcap double entry data used to generate phenotype data with proc_redcap.R
#'
#' @return A string with data stored in JSON format containing meta-data for phenotype files derived by dataREACHr
#'
#'
#' @export

json_phe_dataset_desc <- function(visit_data_path, data_de_path) {

  description_list <- list(
    Name = 'Survey, questionnarie, and phenotype data',
    BIDSVersion = "XXX",
    DatasetType = 'Phenotype',
    GeneratedBy = list( Name = "dataREACHr",
                        Version = paste0("dataREACHr is not versioned. Datasets were generated on ", Sys.Date(), " using the code from the GitHub repository at that time. Thus, this date serves as a reference point for the specific code version employed."),
                        #Version = packageVersion("datareachr"),
                        Description = paste("Datasets were generated using dataREACHr function proc_redcap.R"),
                        URL = "https://github.com/bfuchs18/dataREACHr"),
    SourceDatasets = list (
      list(
        Name = "Visit data",
        Description = "Visit data downloaded from REDCap project 'Food Marketing Resilience/Project REACH' on date specified in filename (see Version)",
        Version = basename(visit_data_path)
      ),
      list(
        Name = "Double-entry data",
        Description = "Double-entry data downloaded from REDCap project 'REACH Data Double Entry' on date specified in filename (see Version)",
        Version = basename(data_de_path)
      )
    )
  )

  # convert formatting to JSON
  description_json <- RJSONIO::toJSON(description_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(description_json, asText = TRUE))){
    print('JSON file may be invalid')
  }

  return(description_json)

}
