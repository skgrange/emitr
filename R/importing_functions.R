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
#' @param parse_dates Should date variables be parsed? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Tibble. 
#' 
#' @export
import_vehicle_details <- function(con, registration = NA, spread = TRUE, 
                                   parse_dates = TRUE, verbose = FALSE) {
  
  # Check inputs
  databaser::db_wildcard_check(registration)
  
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
  
  # If no data return here
  if (nrow(df) == 0) return(df)
  
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
               dplyr::matches("\\bvin\\b"),
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
      
      # Correct data types
      if (parse_dates) {
        
        if ("first_reg_date" %in% names(df)) {
          
          df$first_reg_date <- lubridate::ymd(
            df$first_reg_date, 
            tz = "UTC", 
            quiet = TRUE
          )
          
        }
        
        if ("manufactured_date" %in% names(df)) {
          
          df$manufactured_date <- lubridate::ymd(
            df$manufactured_date, 
            tz = "UTC", 
            quiet = TRUE
          )
          
        }
        
        if ("registration_date" %in% names(df)) {
          
          df$registration_date <- lubridate::ymd(
            df$registration_date, 
            tz = "UTC", 
            quiet = TRUE
          )
          
        }
        
        if ("setup_date" %in% names(df)) {
          
          df$setup_date <- lubridate::ymd(
            df$setup_date, 
            tz = "UTC", 
            quiet = TRUE
          )
          
        }
        
        if ("termination_date" %in% names(df)) {
          
          df$termination_date <- lubridate::ymd(
            df$termination_date, 
            tz = "UTC", 
            quiet = TRUE
          )
          
        }
        
        if ("visibility_date" %in% names(df)) {
          
          df$visibility_date <- lubridate::ymd(
            df$visibility_date, 
            tz = "UTC", 
            quiet = TRUE
          )
          
        }
        
      }
      
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
#' @param spread Should the table be reshaped and made wider? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Tibble. 
#' 
#' @export
import_vehicle_captures <- function(con, registration = NA, spread = TRUE, 
                                    verbose = FALSE) {
  
  # Check inputs
  databaser::db_wildcard_check(registration)
  
  # Build select statement
  sql_select <- vehicle_captures_select_statement()
  
  if (!is.na(registration[1])) {
    
    # Parse registration
    registration <- make_sql_registration(registration)
    
    sql_select <- stringr::str_c(
      sql_select, 
      " WHERE registration IN (", registration, ")"
    )
    
  }
  
  # Clean statement
  sql_select <- stringr::str_squish(sql_select)
  
  if (verbose) message(sql_select)
  
  # Query
  df <- databaser::db_get(con, sql_select)
  
  if (nrow(df) != 0) {
    
    # Parse dates
    # A test for co2, it is also in `vehicle_details`
    df <- df %>% 
      mutate(date = threadr::parse_unix_time(date),
             variable = ifelse(variable == "co2", "co2_capture", variable))
    
    # Reshape
    if (spread) df <- spread_vehicle_captures_table(df)
    
  }
  
  return(df)
  
}


vehicle_captures_select_statement <- function() {
  
  # Default statement
  "SELECT vehicle_captures.*,
    sessions.site,
    sites.site_name,
    sessions.instrument,
    sessions.vehicle_details_data_source AS data_source,
    sessions.field_campaign,
    sessions.site_met
    FROM vehicle_captures
    LEFT JOIN sessions
    ON vehicle_captures.session = sessions.session
    LEFT JOIN sites
    ON sessions.site = sites.site"
  
}


spread_vehicle_captures_table <- function(df) {
  
  # Reshape
  df <- tidyr::spread(select(df, -validity), variable, value, convert = TRUE)
  
  # Clean return
  df <- df %>% 
    select(site,
           site_name,
           session,
           instrument,
           data_source,
           field_campaign,
           site_met,
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
#' @return Tibble. 
#' 
#' @export
import_sessions <- function(con) {
  
  df <- databaser::db_get(
    con, 
    "SELECT sessions.*,
    sites.site_name,
    sites.region,
    field_campaigns.field_campaign_name
    FROM sessions 
    LEFT JOIN sites
    ON sessions.site = sites.site
    LEFT JOIN field_campaigns 
    ON sessions.field_campaign = field_campaigns.field_campaign
    ORDER BY session"
  )
  
  if (nrow(df) != 0) {
    
    df <- df %>% 
      mutate(day = threadr::parse_unix_time(day), 
             date_start = threadr::parse_unix_time(date_start),
             date_end = threadr::parse_unix_time(date_end)) %>% 
      select(session,
             site,
             site_name,
             region,
             instrument,
             everything())
    
  } else {
    
    warning("`sessions` contains no data...", call. = FALSE)
    
  } 
  
  return(df)
  
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
#' @return Tibble. 
#' 
#' @export
import_meteorology <- function(con, site = NA, spread = TRUE) {
  
  # Check inputs
  databaser::db_wildcard_check(site)
  
  # Query
  sql_select <- stringr::str_c(
    "SELECT observations_meteorological.*,
    sites_meteorological.site_name
    FROM observations_meteorological
    LEFT JOIN sites_meteorological
    ON observations_meteorological.site = sites_meteorological.site"
  )
  
  if (!is.na(site[1])) {
    
    # Parse site, registraion parser uses upper case
    site <-  site %>% 
      stringr::str_trim() %>% 
      stringr::str_c("'", ., "'") %>% 
      stringr::str_c(collapse = ",")
    
    sql_select <- stringr::str_c(
      sql_select, 
      " WHERE observations_meteorological.site IN (", site, ")"
    )
    
  } 
  
  # Clean
  sql_select <- stringr::str_squish(sql_select)
  
  # Query
  df <- databaser::db_get(con, sql_select)
  
  if (nrow(df) != 0) {
    
    df <- df %>% 
      mutate(date = threadr::parse_unix_time(date), 
             date_end = threadr::parse_unix_time(date_end))
    
    # Reshape
    if (spread) {
      
      df <- df %>% 
        tidyr::spread(variable, value, convert = TRUE) %>% 
        arrange(site,
                date)
      
    }
    
  } else {
    
    df <- tibble()
    
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
#' @param sort Should the vector be sorted in alphabetical order? 
#' 
#' @return Character vector. 
#' 
#' @export
sample_registrations <- function(con, n = 1, sort = FALSE) {
  
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
  x <- databaser::db_get(con, databaser::db_wildcard_check(sql_select)) %>% 
    pull()
  
  # Alphabetical sort
  if (sort) x <- sort(x)
  
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
#' @return Tibble. 
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
  df <- left_join(df_captures, df_details, by = c("registration", "data_source"))
  
  # Final formatting
  df <- order_capture_and_details_variables(df)

  return(df)
  
}


order_capture_and_details_variables <- function(df) {
  
  if (nrow(df) != 0) {
    
    df %>% 
      select(session,
             instrument,
             data_source,
             field_campaign,
             site,
             site_name,
             site_met,
             date,
             registration,
             vin,
             make,
             model,
             model_variant_name,
             dplyr::matches("\\bbody\\b"),
             everything()) %>% 
      arrange(date)
    
  } else {
    
    df <- tibble()
    
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
  ) %>% 
    pull()
  
}


#' Function to import all distinct registrations from a vehicle emissions 
#' database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param as.logical Should the data frame contain tests indicating if the 
#' registraion exists or not? 
#' 
#' @return Tibble. 
#' 
#' @export
import_distinct_registrations <- function(con, as.logical = FALSE) {
  
  # Escaping may specific to PostgreSQL
  df <- databaser::db_get(
    con, 
    "SELECT DISTINCT 'vehicle_details' AS \"table\", 
    registration
    FROM vehicle_details
    WHERE registration IS NOT NULL
    UNION 
    SELECT DISTINCT 'vehicle_captures' AS \"table\", 
    registration
    FROM vehicle_captures
    WHERE registration IS NOT NULL
    ORDER BY \"table\", registration"
  )
  
  if (as.logical) {
    
    df_c <- df %>%
      filter(table == "vehicle_captures") %>%
      mutate(vehicle_captures = TRUE) %>%
      select(-table)

    df_d <- df %>%
      filter(table == "vehicle_details") %>%
      mutate(vehicle_details = TRUE) %>%
      select(-table)
    
    # Back to df
    df <- df_c %>%
      dplyr::full_join(df_d, by= "registration") %>%
      dplyr::mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
      arrange(registration)
    
  }
  
  return(df)

}


#' Function to return registrations for particular vehicle makes. 
#'
#' @param con Database connection to a vehicle emissions database.
#'
#' @param make A vector of vehicle makes. 
#' 
#' @section \code{\link{get_all_vehicle_makes}}
#' 
#' @return Tibble. 
#'  
#' @export
get_registrations_for_makes <- function(con, make) {
  
  # Check inputs
  databaser::db_wildcard_check(make)
  
  # Format make
  make <- stringr::str_trim(make)
  make <- stringr::str_to_upper(make)
  make <- stringr::str_c("'", make, "'", collapse = ", ")
  databaser::db_wildcard_check(make)
  
  # Query
  df <- databaser::db_get(
    con, 
    stringr::str_c(
      "SELECT DISTINCT value AS make, 
      data_source, 
      registration
      FROM vehicle_details 
      WHERE variable = 'make' 
      AND value IN (", make, ")
      ORDER BY make, registration, data_source"
    )
  )
  
  return(df)
  
}


#' Function to import vehicle odometer data from a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#'
#' @param con Database connection to a vehicle emissions database.
#'
#' @param registration A vector of vehicle registrations to filter return to.
#' 
#' @return Tibble. 
#'  
#' @export
import_vehicle_odometers <- function(con, registration = NA) {
  
  # Check inputs
  databaser::db_wildcard_check(registration)
  
  # Select statement
  sql_select <- "
    SELECT * 
    FROM vehicle_odometers ORDER BY data_source, registration, date
  "
  
  # Add where clause if needed
  if (!is.na(registration[1])) {
    
    # Parse registration
    registration <- make_sql_registration(registration)
    
    # Build clause
    sql_where <- stringr::str_c(
      "WHERE registration IN (", registration, ")"
    )
    
    # Add to select statement
    sql_select <- stringr::str_replace(
      sql_select, 
      "vehicle_odometers", 
      stringr::str_c("vehicle_odometers ", sql_where)
    )
    
  }
  
  # Query database, no wildcard check here
  df <- databaser::db_get(con, sql_select)
  
  # Parse dates
  if (nrow(df) != 0)
    df <- mutate(df, date = threadr::parse_unix_time(date))
  
  return(df)
  
}


#' Function to import vehicle emissions data by session from a vehicle emissions
#' database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param session A vector of sessions to return. If not used, data from all
#' sessions will be selected. 
#' 
#' @param parse_dates Should date variables be parsed? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Tibble. 
#' 
#' @export
import_by_session <- function(con, session = NA, parse_dates = TRUE, 
                              verbose = FALSE) {
  
  if (length(session) == 0) stop("At least one `session` needs to be supplied...")
  
  if (verbose) 
    message(threadr::str_date_formatted(), ": Importing vehicle capture data...")
  
  # Verbose is a sql printer argument in this function, not needed
  df_captures <- import_by_session_captures(
    con, 
    session = session, 
    verbose = FALSE
  )
  
  if (verbose) 
    message(threadr::str_date_formatted(), ": Importing vehicle details data...")
  
  df_details <- import_vehicle_details(
    con, 
    registration = sort(unique(df_captures$registration)), 
    spread = TRUE,
    parse_dates = parse_dates
  )
  
  # Join
  if (verbose) message(threadr::str_date_formatted(), ": Joining data...")
  
  df <- left_join(df_captures, df_details, by = c("data_source", "registration"))
  
  # Final formatting
  df <- order_capture_and_details_variables(df)
  
  return(df)
  
}


import_by_session_captures <- function(con, session, verbose) {
  
  # Check input
  databaser::db_wildcard_check(session)
  
  # Create select statement
  sql_select <- vehicle_captures_select_statement()
  
  # Add a where clause for session
  if (!is.na(session[1])) {
    
    # Parse session
    session <- stringr::str_c(session, collapse = ",")
    
    # Build where clause, watch space
    sql_where <- stringr::str_c(
      " WHERE vehicle_captures.session IN (", session, ")"
    )
    
    # Add where clause
    sql_select <- stringr::str_c(sql_select, sql_where)
    
  }
  
  # Clean statement
  sql_select <- stringr::str_squish(sql_select)
  
  if (verbose) message(sql_select)
  
  # Query database
  df <- databaser::db_get(con, sql_select) %>% 
    mutate(date = threadr::parse_unix_time(date))
  
  # A test for co2, it is also in `vehicle_details`
  df <- mutate(df, variable = ifelse(variable == "co2", "co2_capture", variable))
  
  # Reshape
  df <- spread_vehicle_captures_table(df)
  
  return(df)
  
}


#' Function to import vehicle emissions data by site from a vehicle emissions
#' database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param site A vector of sites to return. If not used, data from all sites
#' will be selected. 
#' 
#' @param parse_dates Should date variables be parsed? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Tibble. 
#' 
#' @export
import_by_site <- function(con, site = NA, parse_dates = TRUE, verbose = FALSE) {
  
  # Check input
  databaser::db_wildcard_check(site)
  
  if (!is.na(site[1])) {
    
    # Format site for sql
    if (verbose) message(threadr::str_date_formatted(), ": Determining sessions...")
    
    site <- unique(site)
    site <- stringr::str_c(site, collapse = ",")
    
    # Get session keys for sites
    session <- databaser::db_get(
      con, 
      stringr::str_c(
        "SELECT session
      FROM sessions
      WHERE site IN (", site, ")"
      )
    ) %>% 
      pull()
    
  } else {
    
    # All sessions will be selected
    session <- NA
    
  }
  
  if (length(session) != 0) {
    
    # Use lower level function
    df <- import_by_session(
      con, 
      session = session,
      parse_dates = parse_dates, 
      verbose = verbose
    )
    
  } else {
    
    warning("No data found for `site` used...", call. = FALSE)
    
    # Return an empty tibble
    df <- tibble()
    
  }
  
  return(df)
  
}

