#' Function to update \code{`sessions`}'s \code{data_start} and \code{date_end} 
#' variables in a vehicle emissions database.
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @return Invisible, a database insert. 
#' 
#' @export
update_session_dates <- function(con) {
  
  df <- databaser::db_get(
    con, 
    "SELECT session, 
    MIN(date) AS date_start,
    MAX(date) AS date_end
    FROM vehicle_captures
    GROUP BY session
    ORDER BY date_start"
  )
  
  if (nrow(df) != 0) {
    
    # Build sql
    sql_update <- stringr::str_c(
      "UPDATE sessions 
      SET date_start=", df$date_start,
      ", date_end=", df$date_end, 
      " WHERE session=", df$session
    ) %>% 
      stringr::str_squish()
    
    # Kill old values too
    sql_update <- c(
      "UPDATE sessions set date_start=NULL",
      "UPDATE sessions set date_end=NULL",
      sql_update
    )
    
    # Do
    databaser::db_execute(con, sql_update)
    
  } else {
    
    stop("`session` contains no data...", call. = FALSE)
    
  }
  
  # No return
  
}


#' Function to update \code{`sessions`}'s \code{capture_count} and 
#' \code{registration_count} variables in a vehicle emissions database.
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @return Invisible, a database insert. 
#' 
#' @export
update_session_vehicle_counts <- function(con) {

  # Count captures and registrations for each session
  df <- databaser::db_get(
    con, 
    "SELECT session, 
    COUNT(DISTINCT date) AS capture_count, 
    COUNT(DISTINCT registration) AS registration_count 
    FROM vehicle_captures 
    GROUP BY session 
    ORDER BY session"
  )
  
  if (nrow(df) != 0) {
    
    # Build sql
    sql_update <- stringr::str_c(
      "UPDATE sessions 
      SET capture_count=", df$capture_count,
      ", registration_count=", df$registration_count,
      " WHERE session=", df$session
    ) %>% 
      stringr::str_squish()
    
    # Kill old values too
    sql_update <- c(
      "UPDATE sessions SET capture_count=NULL", 
      "UPDATE sessions SET registration_count=NULL",
      sql_update
    )
    
    # Do
    databaser::db_execute(con, sql_update)
    
  } else {
    
    stop("`vehicle_captures` contains no data...", call. = FALSE)
    
  }
  
  # No return
  
}


#' Function to update \code{`sites`}'s \code{session_count} variable in a
#' vehicle emissions database.
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @return Invisible, a database insert. 
#' 
#' @export
update_session_counts <- function(con) {
  
  # Get counts
  df <- databaser::db_get(
    con,
    "SELECT site, 
    COUNT(date_start) AS session_count
    FROM sessions
    GROUP BY site
    ORDER BY site"
  )
  
  if (nrow(df) != 0) {
    
    # Build sql
    sql_update <- stringr::str_c(
      "UPDATE sites 
       SET session_count=", df$session_count,
      " WHERE site=", df$site
    ) %>% 
      stringr::str_squish()
    
    # Kill old values too
    sql_update <- c("UPDATE sites SET session_count=NULL", sql_update)
    
    # Do
    databaser::db_execute(con, sql_update)
    
  } else {
    
    stop("`sessions` contains no data...", call. = FALSE)
    
  }
  
  # No return
  
}
