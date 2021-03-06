#' Function to insert vehicle capture data into \code{`vehicle_captures`}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param df Input data frame to insert into \code{`vehicle_captures`}. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Invisible \code{con}.
#' 
#' @export
insert_vehicle_captures <- function(con, df, verbose = FALSE) {
  
  if (verbose) message(threadr::str_date_formatted(), ": Checking input...")
  
  # Test many variables
  vector_test <- c(
    anyNA(df$session), 
    anyNA(df$date),
    anyNA(df$variable),
    !is.numeric(df$value)
  )
  
  if (any(vector_test)) {
    stop("There is missing or empty data in critical variables..", call. = FALSE)
  }
  
  # Check session
  sessions_db <- databaser::db_get(
    con, 
    "SELECT DISTINCT session 
    FROM sessions"
  ) %>% 
    pull()
  
  if (!all(unique(df$session) %in% sessions_db)) {
    stop(
      "There are sessions in input not within the `sessions` table...", 
      call. = FALSE
    )
  }
  
  # Check variables
  variables_allowed <- allowed_vehicle_captures_variables()
  
  # Test
  variables_not_allowed <- setdiff(unique(df$variable), variables_allowed)
  
  if (!length(variables_not_allowed) == 0) {
    
    # Format for message
    variables_not_allowed <- variables_not_allowed %>% 
      stringr::str_c("`", ., "`") %>% 
      stringr::str_c(collapse = ", ")
    
    stop(
      "Input contains non-allowed variables (", 
      variables_not_allowed, 
      ")...", 
      call. = FALSE
    )
    
  }
  
  # Insert
  if (verbose) message(threadr::str_date_formatted(), ": Inserting...")
  databaser::db_insert(con, "vehicle_captures", df)
  
  return(invisible(con))
  
}


#' Function to insert vehicle capture data into \code{`vehicle_details`}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param df Input data frame to insert into \code{`vehicle_details`}. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Invisible \code{con}.
#' 
#' @export
insert_vehicle_details <- function(con, df, verbose = FALSE) {
  
  if (verbose) message(threadr::str_date_formatted(), ": Checking input...")
  
  # Check input
  if (anyNA(df)) stop("Missing data are not allowed...", call. = FALSE)
  
  # na.rm for vin variable
  if (any(df == "", na.rm = TRUE)) {
    stop("Empty strings are not allowed...", call. = FALSE)
  }
  
  # Get data source
  data_source <- unique(df$data_source)
  
  if (length(data_source) != 1) {
    stop("Only one data source is allowed...", call. = FALSE)
  }
  
  data_sources_db <- databaser::db_get(
    con, 
    "SELECT DISTINCT data_source 
    FROM vehicle_details_data_sources"
  ) %>% 
    pull()
  
  if (!data_source %in% data_sources_db) {
    stop(
      "`data_source` is not in `vehicle_details_data_sources` table...", 
      call. = FALSE
    )
  }

  # Check registrations
  sql_select <- stringr::str_c(
    "SELECT DISTINCT registration 
    FROM vehicle_details
    WHERE data_source=", data_source
  ) %>% 
    stringr::str_squish()
  
  df_registrations_db <- databaser::db_get(con, sql_select)
  
  if (nrow(df_registrations_db) != 0) {
    
    if (any(unique(df$registration) %in% pull(df_registrations_db))) {
      stop("`registrations` are already in database...", call. = FALSE)
    }
    
  }
  
  # Check variables
  variables_allowed <- allowed_vehicle_details_variables()
  
  # Test
  variables_not_allowed <- setdiff(unique(df$variable), variables_allowed)
  
  if (!length(variables_not_allowed) == 0) {
    
    # Format for message
    variables_not_allowed <- variables_not_allowed %>% 
      stringr::str_c("`", ., "`") %>% 
      stringr::str_c(collapse = ", ")
    
    stop(
      "Input contains non-allowed variables (", 
      variables_not_allowed, 
      ")...", 
      call. = FALSE
    )
    
  }
  
  # Do the insert
  if (verbose) message(threadr::str_date_formatted(), ": Inserting...")
  databaser::db_insert(con, "vehicle_details", df)
  
  return(invisible(con))
  
}


#' Function to return allowed variables for the \code{`vehicle_captures`} table
#' in a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @export
allowed_vehicle_captures_variables <- function() {
  
  c(
    "absolute_humidity", "acceleration", "air_temp", "atmospheric_pressure", 
    "co", "co2", "hc", "ir_hc", "ir_smoke", "nh3", "no", "no2", "nox", 
    "percent_co2", "ratio_co_co2", "ratio_hc_co2", "ratio_nh3_co2", 
    "ratio_no_co2", "ratio_no2_co2", "rh", "so2", "speed", "uv_hc", 
    "uv_smoke", "uv_water", "valid_plume_points", "vehicle_specific_power"
  )
  
}


#' Function to return allowed variables for the \code{`vehicle_details`} table
#' in a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @export
allowed_vehicle_details_variables <- function() {
  
  c(
    "acceleration_kph", "acceleration_mph", "axle_count", "bhp_count", 
    "body", "body_description", "body_shape_roof_height", "bore", 
    "cab_type", "cc", "co2", "colour", "combined_lkm", "combined_mpg", 
    "cylinder_arrangement", "cylinder_count", "door_count", "drive_axle", 
    "drive_type", "engine_description", "engine_location", "engine_make", 
    "engine_size", "euro_status", "euro_status_old", "extra_urban_lkm", 
    "extra_urban_mpg", "first_reg_date", "fuel_delivery", "fuel_type", 
    "gears_count", "gross_combined_weight", "gross_train_weight", 
    "keeper_previous_keepers", "kerb_weight", "load_length", "make", 
    "manufactured_date", "max_speed_kph", "max_speed_mph", "model", 
    "model_variant_name", "mvris_code", "payload_volume", "payload_weight", 
    "power_delivery", "power_kw", "primary_fuel", "registration_date", 
    "rigid_artic", "rpm_power", "rpm_torque", "seat_count", "setup_date", 
    "stroke", "termination_date", "torque_lb", "torque_nm", "transmission", 
    "type_approval_category", "unladen_weight", "urban_cold_lkm", 
    "urban_cold_mpg", "valve_count", "valve_gear", "vehicle_description", 
    "vehicle_gross_weight", "vehicle_height", "vehicle_length", "vehicle_origin", 
    "vehicle_series", "vehicle_width", "visibility_date", 
    "wheelbase_length", "wheelplan", "engine_number", "noise_drive_by", 
    "noise_engine", "noise_stationary", "power_to_weight", 
    "smmt_market_sector_line", "trailer_braked", "vin", "colour_secondary", 
    "engine_code", "chassis_code", "assembly_type", "vehicle_equipment_class"
  )
  
}


#' Function to insert session data into \code{`sessions`}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @param df Input data frame to insert into \code{`sessions`}. 
#' 
#' @return Invisible \code{con}.
#' 
#' @export
insert_sessions <- function(con, df) {
  
  # Check session
  if (anyDuplicated(df$session) != 0) {
    stop("`session` is not unique....", call. = FALSE)
  }
  
  if (any(df$session %in% get_sessions(con))) {
    stop("There are duplicated sessions...", call. = FALSE)
  }
  
  # Check sites
  if (!any(unique(df$site) %in% get_sites(con))) {
    stop("sites do not exist in `sites`...", call. = FALSE)
  }
  
  # Insert into database
  databaser::db_insert(con, "sessions", df)
  
  return(invisible(con))
  
}


#' Function to return session values in the \code{`sessions`} table in a vehicle
#' emissions database. 
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @export
get_sessions <- function(con) {
  
  df <- databaser::db_get(
    con, 
    "SELECT DISTINCT session 
    FROM sessions 
    ORDER BY session"
  )
  
  if (nrow(df) == 0) {
    
    x <- NA
    
  } else {
    
    x <- pull(df)
    
  }
  
  return(x)
  
}


#' Function to return site values in the \code{`sites`} table in a vehicle
#' emissions database. 
#' 
#' @param con Database connection to a vehicle emissions database. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @export
get_sites <- function(con) {
  
  databaser::db_get(
    con, 
    "SELECT DISTINCT site 
    FROM sites 
    ORDER BY site"
  ) %>% 
    pull()
  
}
