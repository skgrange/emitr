#' Function to import vehicle detail data from a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param registration A vector of registrations to return. If not used, all
#' registrations will be selected. 
#' 
#' @param spread Should the table be reshaped and made wider? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Data frame. 
#' 
#' @export
import_vehicle_details <- function(con, registration = NA, spread = TRUE, 
                                   verbose = FALSE) {
  
  if (is.na(registration[1])) {
    
    sql_select <- stringr::str_c(
      "SELECT data_source, 
      registration,
      variable, 
      value 
      FROM vehicle_details"
    )
    
  } else {
    
    # Parse registration
    registration <- make_sql_registration(registration)
    
    sql_select <- stringr::str_c(
      "SELECT data_source, 
      registration,
      variable, 
      value 
      FROM vehicle_details 
      WHERE registration IN (", registration, ")"
    )
    
  }
  
  # Clean 
  sql_select <- stringr::str_squish(sql_select)
  
  # Message statement
  if (verbose) message(sql_select)
  
  # Query
  df <- databaser::db_get(con, sql_select)
  
  # Make wider
  if (spread) {
    
    if (verbose) message("Reshaping data...")
    
    df <- tryCatch({
      
      tidyr::spread(df, variable, value, convert = TRUE)
      
    }, error = function(e) {
      
      warning("Duplicate variables found, data has been lost...", call. = FALSE)
      
      df %>% 
        distinct(registration, variable, .keep_all = TRUE) %>% 
        tidyr::spread(variable, value, convert = TRUE)
      
    })
    
    if (nrow(df) != 0) {
      
      # Arrange some variables
      df <- df %>% 
        select(data_source,
               registration, 
               dplyr::matches("\\bmake\\b"),
               dplyr::matches("model"),
               dplyr::starts_with("model_variant"),
               dplyr::matches("body"),
               dplyr::matches("fuel_type"),
               dplyr::matches("vehicle_series"),
               dplyr::matches("vehicle_desc"),
               dplyr::matches("cc"),
               dplyr::matches("fuel_delivery"),
               everything())
      
      # # Correct some data types
      # if ("setup_date" %in% names(df)) {
      #   
      #   df$setup_date <- lubridate::parse_date_time(
      #     df$setup_date, 
      #     orders = c("ymd", "dmy"), 
      #     tz = "UTC"
      #   )
      #   
      # }
      # 
      # if ("visibility_date" %in% names(df)) {
      #   
      #   df$visibility_date <- lubridate::parse_date_time(
      #     df$visibility_date, 
      #     orders = c("ymd", "dmy"), 
      #     tz = "UTC"
      #   )
      #   
      # }
      # 
      # if ("manufactured_date" %in% names(df)) {
      #   
      #   df$manufactured_date <- lubridate::parse_date_time(
      #     df$manufactured_date, 
      #     orders = c("ymd", "dmy"), 
      #     tz = "UTC"
      #   )
      #   
      # }
      # 
      # if ("termination_date" %in% names(df)) {
      #   
      #   df$termination_date <- lubridate::parse_date_time(
      #     df$termination_date, 
      #     orders = c("ymd", "dmy"), 
      #     tz = "UTC"
      #   )
      #   
      # }
      
    }
    
  }
  
  return(df)
  
}


#' Function to import vehicle capture data from a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param registration A vector of registrations to return. If not used, all
#' registrations will be selected. 
#' 
#' @param site A vector of sites to return. If not used, all sites will be 
#' selected. 
#' 
#' @param spread Should the table be reshaped and made wider? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Data frame. 
#' 
#' @export
import_vehicle_captures <- function(con, registration = NA, site = NA,
                                    spread = TRUE, verbose = FALSE) {
  
  sql_select <- stringr::str_c(
    "SELECT vehicle_captures.*,
    sessions.site,
    sites.site_name
    FROM vehicle_captures
    LEFT JOIN sessions
    ON vehicle_captures.session = sessions.session
    LEFT JOIN sites
    ON sessions.site = sites.site"
  )
  
  # Add where clause for site
  if (!is.na(site[1])) {
    
    site <- site %>% 
      stringr::str_trim() %>% 
      stringr::str_c(collapse = ",")
    
    sql_select <- stringr::str_c(
      sql_select, 
      " WHERE sessions.site IN (", site, ")"
    )
    
  }
  
  if (!is.na(registration[1])) {
    
    # Parse registration
    registration <- make_sql_registration(registration)
    
    if (grepl("WHERE sessions.site", sql_select)) {
      
      sql_select <- stringr::str_c(
        sql_select, 
        " AND registration IN (", registration, ")"
      )
      
    } else {
      
      sql_select <- stringr::str_c(
        sql_select, 
        " WHERE registration IN (", registration, ")"
      )
      
    }
    
  }
  
  # Clean
  sql_select <- stringr::str_squish(sql_select)
  
  if (verbose) message(sql_select)
  
  # Query
  df <- databaser::db_get(con, sql_select) %>% 
    mutate(date = threadr::parse_unix_time(date))
  
  # A test for co2, it is also in `vehicle_details`
  df <- mutate(df, variable = ifelse(variable == "co2", "co2_capture", variable))
  
  # Reshape
  if (spread) 
    df <- tidyr::spread(select(df, -validity), variable, value, convert = TRUE)
  
  # Clean return
  df <- df %>% 
    select(site,
           site_name,
           session,
           date,
           everything()) %>% 
    arrange(site, 
            date)
  
  return(df)
  
}


make_sql_registration <- function(x) {
  
  x %>% 
    na.omit() %>% 
    stringr::str_trim() %>% 
    stringr::str_to_upper() %>% 
    unique() %>% 
    stringr::str_c("'", ., "'") %>% 
    stringr::str_c(collapse = ",")
  
}


#' Function to import sessions data from a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @return Data frame. 
#' 
#' @export
import_sessions <- function(con) {
  
  databaser::db_get(
    con, 
    "SELECT sessions.*,
    sites.site_name
    FROM sessions 
    LEFT JOIN sites ON sessions.site = sites.site
    ORDER BY session"
  ) %>% 
    mutate(day = threadr::parse_unix_time(day), 
           date_start = threadr::parse_unix_time(date_start),
           date_end = threadr::parse_unix_time(date_end)) %>% 
    select(session,
           site,
           site_name,
           instrument,
           everything())
  
}


#' Function to import sites data from a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @return Data frame. 
#' 
#' @export
import_sites <- function(con) {
  
  databaser::db_get(
    con,
    "SELECT * 
    FROM sites
    ORDER BY site"
  )
  
}



#' Function to import meteorological data from a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param site A vector of sites. 
#' 
#' @param spread Should the table be reshaped and made wider? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Data frame. 
#' 
#' @export
import_meteorology <- function(con, site = NA, spread = TRUE, verbose = FALSE) {
  
  # Query
  sql_select <- stringr::str_c(
    "SELECT observations_meteorological.*,
    sites_meteorological.site_name
    FROM observations_meteorological
    LEFT JOIN sites_meteorological
    ON observations_meteorological.site = sites_meteorological.site"
  )
  
  if (!is.na(site[1])) {
    
    sql_select <- stringr::str_c(
      sql_select, 
      " WHERE observations_meteorological.site IN (", make_sql_registration(site), ")"
    )
    
  } 
  
  # Clean
  sql_select <- stringr::str_squish(sql_select)
  
  if (verbose) message(sql_select)
  
  # Query
  df <- databaser::db_get(con, sql_select) %>% 
    mutate(date = threadr::parse_unix_time(date), 
           date_end = threadr::parse_unix_time(date_end))
  
  # Reshape
  if (spread) {
    
    df <- df %>% 
      tidyr::spread(variable, value, convert = TRUE) %>% 
      arrange(site,
              date)
    
  }
  
  return(df)
  
}


#' Function to return random registrations from a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param n Number of registrations to return. 
#' 
#' @return Character vector. 
#' 
#' @export
sample_registrations <- function(con, n = 1) {
  
  # to-do: table logic?
  
  sql_select <- stringr::str_c(
    "SELECT registration
    FROM vehicle_captures 
    WHERE registration IS NOT NULL
    ORDER BY RANDOM()
    LIMIT ", n
  ) %>% 
    stringr::str_squish()
  
  # Query
  x <- databaser::db_get(con, sql_select)[, 1]
  
  return(x)
  
}


#' Function to import emissions data from a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param registration A vector of registrations to return. If not used, all
#' registrations will be selected. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Data frame. 
#' 
#' @export
import_vehicle_emissions <- function(con, registration = NA, verbose = FALSE) {
  
  if (verbose) 
    message(threadr::str_date_formatted(), ": Importing vehicle capture data...")
  
  df_captures <- import_vehicle_captures(
    con, 
    registration = registration,
    verbose = FALSE
  ) %>% 
    arrange(date)
  
  if (verbose) 
    message(threadr::str_date_formatted(), ": Importing vehicle details data...")
  
  df_details <- import_vehicle_details(
    con, 
    registration = registration, 
    verbose = FALSE
  )
  
  # Join
  if (verbose) message(threadr::str_date_formatted(), ": Joining data...")
  df <- left_join(df_captures, df_details, by = "registration")
  
  if (nrow(df) != 0) {
    
    # Order variables
    df <- df %>% 
      select(site,
             site_name,
             session,
             date,
             registration,
             make,
             model,
             model_variant_name,
             dplyr::matches("\\bbody\\b"),
             everything()) %>% 
      arrange(date)
    
  } else {
    
    df <- data.frame()
    
  }
  
  return(df)
  
}


#' Function to return all vehicle makes from a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @return Character vector. 
#' 
#' @export
get_all_vehicle_makes <- function(con) {
  
  databaser::db_get(
    con, 
    "SELECT DISTINCT value
    FROM vehicle_details
    WHERE variable = 'make'
    ORDER BY value"
  )[, ]
  
}


#' Function to import all distinct registrations from a vehicle emissions 
#' database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @return Data frame.  
#' 
#' @export
import_distinct_registrations <- function(con) {
  
  databaser::db_get(
    con, 
    "SELECT DISTINCT 'vehicle_details' AS `table`, 
    registration
    FROM vehicle_details
    UNION 
    SELECT DISTINCT 'vehicle_captures' AS `table`, 
    registration
    FROM vehicle_captures
    ORDER BY `table`, 
    registration"
  )
  
}
