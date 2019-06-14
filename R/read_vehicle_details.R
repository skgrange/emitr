#' Function to read joined vehicle data files. 
#' 
#' @param file Vector of file names. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{tidy_odometer_table}}
#' 
#' @export
read_vehicle_details <- function(file, verbose = FALSE) {
  
  # Read all files
  file %>% 
    setNames(basename(file)) %>% 
    purrr::map_dfr(
      ~read_vehicle_details_worker(
        .x, 
        verbose = verbose
      ),
      .id = "file"
    )
  
}


read_vehicle_details_worker <- function(file, verbose) {
  
  # Load data
  if (verbose) message(threadr::date_message(), file)
  
  df <- readr::read_csv(
    file,
    col_types = readr::cols(
      keeper_start_date = readr::col_character(),
      keeper_previous_acquire = readr::col_character(),
      keeper_previous_dispose = readr::col_character(),
      keeper_previous_keepers = readr::col_integer(),
      vehicle_fuel_code = readr::col_integer(), 
      vehicle_export_date = readr::col_character(),
      vehicle_scrapped_date = readr::col_character(),
      vehicle_import_date = readr::col_character(),
      keeper_vic_date = readr::col_character(),
      vehicle_vin_ending = readr::col_character(),
      mvris_vehicle_series = readr::col_character()
    ),
    progress = FALSE,
    guess_max = 10000
  )
  
  # Catch some NAs
  df <- df %>% 
    dplyr::mutate_if(is.integer, ~if_else(. == 9999, NA_integer_, .))
  
  # Parse dates
  variable_dates <- stringr::str_subset(names(df), "date")
  
  df <- df %>% 
    dplyr::mutate_at(
      variable_dates, 
      ~lubridate::ymd_hms(., tz = "UTC", quiet = TRUE, truncated = 3)
    )
  
  return(df)
  
}
