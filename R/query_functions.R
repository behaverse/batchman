
## --- query functions ----
# These are functions to list files that should be used for a process_flow_definition.
# currently includes only 1 function.

#' List filenames containing a specific word.
#'
#' @param dataset_name The name of the dataset folder in which to search for files. It is assumed that this folder is inside the `data` folder in the project root folder.
#' @param contains_term A string specifying the term the returned list of files must contain.
#'
#' @return vector of filenames.
#' @export
#'
#' @examples list_instrument_filenames()
list_instrument_filenames <- function(dataset_name, contains_term, dataset_path = '../data/'){

  # takes quite some time to list all files:
  source_files <- list.files(path = paste0(dataset_path, dataset_name, '/'),
                             pattern = '.*response_\\d.csv$',
                             recursive = TRUE)


  # can't get regex to work in one step...
  idx <- stringr::str_detect(source_files, pattern = contains_term)
  paste0(dataset_path, dataset_name, '/', source_files[idx])

}


