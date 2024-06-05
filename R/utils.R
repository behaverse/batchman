


#' Save information about platform and packages used during session
#'
#' @param output_dir path to directory in which to save the session info.
#'
#' @return hash/unique id of the session
#' @export
#'
#' @examples save_session_info("./")
save_session_info <- function(output_dir = './'){

  # create unique id for a system setup
  session_info_text <- paste(capture.output(sessionInfo()), collapse = '')
  session_info_id <- substr(digest::digest(session_info_text, algo = 'md5'),1, 6)

  # collect detailed system setup info
  session_info <- devtools::session_info()
  session_platform_info <- as.data.frame(session_info$platform)
  session_packages_info <- as.data.frame(session_info$packages)

  # add time of execution
  now <- now()
  id <- substr(digest::digest(now, algo = 'md5'),1, 6)

  session_platform_info <- session_platform_info |>
    mutate(id = as.character(id), now = now) |>
    relocate(id, now, .before = 1)

  session_packages_info <- session_packages_info |>
    mutate(id = as.character(id), now = now) |>
    relocate(id, now, .before = 1)


  # if model_registry does not exist yet, create it, otherwise update
  session_info_filename <- paste0(output_dir, '/session_info.csv')
  session_packages_filename <- paste0(output_dir, '/session_packages.csv')


  write.csv(x = session_platform_info, file = session_info_filename, append = TRUE)
  write.csv(x = session_packages_info, file = session_packages_filename, append = TRUE)

  id
}
