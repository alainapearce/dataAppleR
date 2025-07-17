#' util_copy_to_source: A function to copy a file into sourcedata
#'
#' @param task_dir (string) path to directory with all task files to be copied into sourcedata
#' @param task_str (string) task string used in filenaming (e.g., 'foodview')
#' @param sub_id (integer) subject id
#' @param ses_str (string) bids-formatted session string. e.g., 'ses-1'
#' @param sourcefile_prefix (string) string to prefix filename with in sourcedata (optional)
#' @param overwrite (logical) logical indicating whether file should be overwritten in sourcedata
#'
#' @examples
#' util_copy_to_source(task_dir = foodview_dir, task_str = 'foodview', sub_id = 'sub-001', ses_str = 'ses-1', overwrite = TRUE)
#'


util_copy_to_source <- function(task_dir, task_str, sub_id, ses_str, sourcefile_prefix, overwrite) {

  # check for sourcefile_prefix arg
  prefix_arg <- methods::hasArg(sourcefile_prefix)

  # sub_str
  sub_str <- sprintf('sub-%03d', sub_id)

  print(sub_str)

  # get files
  raw_files <- list.files(path = task_dir, pattern = task_str)

  # food view ####
  if (task_str == 'foodview'){
    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'func')

    raw_files <- raw_files[grepl(paste0('-', sub_id, '.txt'), raw_files)]
    raw_files_short <- substr(raw_files, 1, unlist(gregexpr('-', raw_files))-1)

    rename_files <- paste0(sub_str, '_', ses_str, '_task-', raw_files_short, '.tsv')
  }

  # SST ####
  if (task_str == 'stop'){
    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'func')

    raw_files <- raw_files[grepl(paste0('-', sub_id, '.txt|', sub_id, '_1st'), raw_files)]
    raw_files_short <- substr(raw_files, 1, unlist(gregexpr('-', raw_files))-1)

    if (sub_id == 37){
      raw_files_short[grepl('stop_prac', raw_files_short)] <- c('stop_prac1', 'stop_prac2')
    }

    rename_files <- paste0(sub_str, '_', ses_str, '_task-', gsub('stop', 'sst', raw_files_short), '.tsv')
  }

  # Space Game ####
  if (task_str == 'mbmfNovelStakes'){
    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh')

    raw_files <- raw_files[grepl(sprintf('%03d', sub_id), raw_files)]

    raw_files_short <- substr(raw_files, 1, unlist(gregexpr('_', raw_files))-1)

    rename_files <- paste0(sub_str, '_', ses_str, '_task-', gsub('mbmfNovelStakes', 'space', raw_files_short), '.mat')

    if (sub_id == 26){
      rename_files <- gsub('cance', '-cancel', rename_files)
    }
  }

  # NIH Toolbox ####
  if (task_str == 'nih'){
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh')


    raw_files <- list.files(path = task_dir, pattern = '.csv')

    if (sum(grepl('sub', raw_files)) > 0){
      raw_files_short <- sapply(raw_files, function(x) substr(x, unlist(gregexpr('_', x))+1, nchar(x)), simplify = TRUE, USE.NAMES = FALSE)
    } else {
      raw_files_short <- sapply(raw_files, function(x) substr(x, unlist(gregexpr(' ', x))[2]+1, nchar(x)), simplify = TRUE, USE.NAMES = FALSE)

      raw_files_short <- gsub('Assessment ', '', raw_files_short)
      raw_files_short <- gsub('Registration ', 'registration_', raw_files_short)
      raw_files_short <- tolower(raw_files_short)

    }

    rename_files <- paste0(sub_str, '_', ses_str, '_task-nih_toolbox_', raw_files_short)

    rename_files <- gsub('.csv', '.tsv', rename_files)
  }

  # RRV ####
  if (task_str == 'rrv'){

    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh')

    raw_files <- c(list.files(path = task_dir, pattern = '.csv'), list.files(path = task_dir, pattern = '.txt'))
    raw_files <- raw_files[!grepl('_csv', raw_files)]

    # # list of acceptable file names
    # acc_file_names = c(paste0('rrv_', sub_id, '.txt'), paste0('rrv_', sub_id, '_summary.csv'), paste0('rrv_', sub_id, '_game.csv'), paste0('rrv_', sub_id, '-prac.txt'), paste0('rrv_', sub_id, '-prac_summary.csv'), paste0('rrv_', sub_id, '-prac_game.csv'))

    raw_files_short <- sapply(raw_files, function(x) substr(x, unlist(gregexpr(sub_id, x))+nchar(sub_id), nchar(x)), simplify = TRUE, USE.NAMES = FALSE)

    rename_files <- paste0(sub_str, '_', ses_str, '_task-rrv', raw_files_short)

    rename_files <- gsub('.csv', '.tsv', rename_files)
    rename_files <- gsub('.txt', '.tsv', rename_files)
    # } else {
    #   print(paste('WARNING: ',task_dir, ' has files that will not be copied into sourcedata because they do not adhere to expected naming conventions for RRV'))
    # }
  }

  # PIT ####
  if (task_str == 'pit'){

    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh')

    raw_files <- list.files(path = task_dir, pattern = 'PIT')

    raw_files <- raw_files[grepl(paste0(sprintf('%03d', sub_id), '_Food'), raw_files)]

    # output warning if .csv not found
    if ( sum(grepl('.csv', raw_files)) == 0) {
      print(paste('WARNING: subject', sub_id, ses_str, 'has PIT output files but no csv' ))
    }

    raw_files_short <- substr(raw_files, tail(unlist(gregexpr('\\.', raw_files[1])), 1), nchar(raw_files))

    rename_files <- paste0(sub_str, '_', ses_str, '_task-pit', raw_files_short)

    rename_files <- gsub('.csv', '.tsv', rename_files)
    rename_files <- gsub('.txt', '.tsv', rename_files)
  }

  # PIT ####
  if (task_str == 'micro'){

    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh')

    raw_files <- list.files(path = task_dir, pattern = 'PIT')

    raw_files <- raw_files[grepl(paste0(sprintf('%03d', sub_id), '_Food'), raw_files)]

    # output warning if .csv not found
    if ( sum(grepl('.csv', raw_files)) == 0) {
      print(paste('WARNING: subject', sub_id, ses_str, 'has PIT output files but no csv' ))
    }

    raw_files_short <- substr(raw_files, tail(unlist(gregexpr('\\.', raw_files[1])), 1), nchar(raw_files))

    rename_files <- paste0(sub_str, '_', ses_str, '_task-pit', raw_files_short)

    rename_files <- gsub('.csv', '.tsv', rename_files)
    rename_files <- gsub('.txt', '.tsv', rename_files)
  }


  # Save ####
  # set sourcedata file
  source_paths <- file.path(sub_task_source_dir, rename_files)

  if (task_str == 'stop'){
    source_paths[!grepl('fmri|onsets', source_paths)] <- gsub('func', 'beh', source_paths[!grepl('fmri|onsets', source_paths)])
  }

  if (sum(file.exists(source_paths[1])) != length(source_paths) | isTRUE(overwrite)) {

    # create sub_task_source_dir if it doesnt exist
    if (!dir.exists(sub_task_source_dir)) {
      dir.create(sub_task_source_dir, recursive = TRUE)
    }

    # copy files
    save_fun <- function(file_path, source_path){

      if (grepl('csv', file_path)){
        dat_read <- read.csv(file_path, header = TRUE)
      }

      if (grepl('txt', file_path)){
        dat_read <- read.table(file_path, header = TRUE)
      }

      write.table(dat_read, source_path, sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    }

    if (task_str %in% c('foodview', 'stop', 'nih', 'rrv')){
      mapply(save_fun, file_path=file.path(task_dir, raw_files), source_path = source_paths)
    } else if (task_str %in% c('mbmfNovelStakes')){
      # copy file into sub_task_source_dir
      file.copy(from = file.path(task_dir, raw_files), to = source_paths, overwrite = overwrite)
    } else if (task_str %in% c('pit')){
      if (sum(grepl('.csv', raw_files)) > 0){
        save_fun(file_path=file.path(task_dir, raw_files[grepl('.csv', raw_files)]), source_path = source_paths[grepl('.tsv', source_paths)])
      }

      file.copy(from = file.path(task_dir, raw_files[!grepl('.csv', raw_files)]), to = source_paths[!grepl('.tsv', source_paths)], overwrite = overwrite)
    }

    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }

  } else {
    return('exists')
  }
}
