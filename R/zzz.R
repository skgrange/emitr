#' Function to squash R check's global variable notes. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "date_end", "variable", "value", "day", "date_start", "session", "site",
    "site_name", "instrument", "make", "model", "validity", "data_source", 
    "model_variant_name", "vin", "region", "site_met", "registration", 
    "field_campaign", "VDRDateTime", "mot_results_mot_test_odometer_value", 
    "row_number", "variable_clean", "test_number", "unit", "vdr_date_time",
    "ambient_temperature", "sdm_temperature", "file_image"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}
