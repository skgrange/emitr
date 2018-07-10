#' Functions to check variables within a data frame for a vehicle emissions 
#' database. 
#' 
#' @param df Data frame to test. 
#' 
#' @param sort Should the variables be sorted alphabetically?
#' 
#' @return Data frame with one observation. 
#' 
#' @export
vehicle_details_check <- function(df) {
  
  df %>% 
    select(dplyr::one_of(!!priority_vehicle_details_variables())) %>% 
    dplyr::summarise_all(dplyr::funs(sum(!is.na(.)) / length(.))) %>% 
    bind_rows(zero_row_data_frame(priority_vehicle_details_variables()), .) 
  
}


#' @rdname vehicle_details_check
#' @export
vehicle_captures_check <- function(df) {
  
  df %>% 
    select(dplyr::one_of(!!priority_vehicle_capture_variables())) %>% 
    dplyr::summarise_all(dplyr::funs(sum(!is.na(.)) / length(.))) %>% 
    bind_rows(zero_row_data_frame(priority_vehicle_capture_variables()), .) 
  
}


#' @rdname vehicle_details_check
#' @export
priority_vehicle_details_variables <- function(sort = TRUE) {
  
  x <- c(
    "make", "model", "type_approval_category", "fuel_type", "euro_status", "cc",
    "manufactured_date", "vin", "registration"
  )
  
  if (sort) x <- sort(x)
  
  return(x)
  
}


#' @rdname vehicle_details_check
#' @export
priority_vehicle_capture_variables <- function(sort = TRUE) {
  
  x <- c(
    "speed", "acceleration", "vehicle_specific_power", "co", "co2", "hc", "no", 
    "no2", "nox", "nh3", "so2", "registration"
  )
  
  if (sort) x <- sort(x)
  
  return(x)
  
}


zero_row_data_frame <- function(x) {
  
  df <- data.frame(matrix(ncol = length(x), nrow = 0))
  names(df) <- x
  return(df)
  
}
