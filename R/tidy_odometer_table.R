#' Function to tidy the messy odometer data contained in joined vehicle data 
#' files. 
#' 
#' @param df Input tibble/data frame, usually from 
#' \code{\link{read_vehicle_details}}.
#' 
#' @param variables Number of variables in messy data.
#' 
#' @param groups Number of groups in messy data.
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{read_vehicle_details}}
#' 
#' @export
tidy_odometer_table <- function(df, variables = 3, groups = 5) {
  
  df <- df %>% 
    filter(!is.na(registration), 
           !is.na(mot_results_mot_test_odometer_value)) %>% 
    dplyr::mutate_if(lubridate::is.POSIXt, as.character) %>% 
    tibble::rowid_to_column("row_number") %>% 
    tidyr::gather(variable, value, -c(row_number, registration)) %>% 
    arrange(row_number,
            registration)
  
  # Create sequence needed for reshaping
  n <- df %>% 
    distinct(row_number, 
             registration) %>% 
    nrow()
  
  x <- 1:groups
  x <- rep(x, each = variables)
  x <- rep(x, times = n)
  
  # Make tidy data
  df <- df %>% 
    mutate(test_number = x, 
           variable_clean = if_else(
             grepl("odometer_value", variable), "value", NA_character_
           ),
           variable_clean = if_else(
             grepl("odometer_unit", variable), "unit", variable_clean
           ),
           variable_clean = if_else(
             grepl("completed_date", variable), "date", variable_clean
           )) %>% 
    select(-variable,
           -row_number) %>% 
    rename(variable = variable_clean) %>% 
    filter(!is.na(value)) %>% 
    distinct(registration, 
             value,
             test_number,
             variable) %>% 
    tidyr::spread(variable, value, convert = TRUE) %>% 
    select(-test_number) %>% 
    mutate(date = lubridate::ymd_hms(date, tz = "UTC", truncated = 3),
           unit = ifelse(unit == "mi", "miles", unit),
           unit = ifelse(unit == "km", "kilometres", unit)) %>% 
    arrange(registration, 
            date)
  
  return(df)
  
}
