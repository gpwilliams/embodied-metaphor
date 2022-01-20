map_files_to_list <- function(file_path, read_function = readr::read_rds) {
  # get full path
  full_file_path <- list.files(
    file_path,
    full.names = TRUE
  )
  # read files
  files <- purrr::map(full_file_path, read_function)
  # add names to list
  names(files) <- stringr::str_sub(list.files(file_path), 1, -5)
  # return named list of files
  files
}