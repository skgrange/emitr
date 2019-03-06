#' Function to update dynamic variables in a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Invisible \code{con}.
#' 
#' @export
update_dynamic_variables <- function(con, verbose = FALSE) {
  
  if (verbose) message(threadr::date_message(), "Updating registration counts...")
  update_session_vehicle_counts(con)
  
  if (verbose) message(threadr::date_message(), "Updating session dates...")
  update_session_dates(con)
  
  if (verbose) message(threadr::date_message(), "Updating session counts...")
  update_session_counts(con)
  
  if (verbose) message(threadr::date_message(), "Updating `row_counts` table...")
  update_row_counts(con)
  
  return(invisible(con))
  
}


# Pulled from smonitor
update_row_counts <- function (con, estimate = FALSE) {
  table <- databaser::db_list_tables(con)
  table <- setdiff(table, "row_counts")
  df <- databaser::db_count_rows(con, table = table, estimate = estimate)
  databaser::db_insert(con, "row_counts", df, replace = TRUE)
  df <- databaser::db_read_table(con, "row_counts")
  return(invisible(df))
}
