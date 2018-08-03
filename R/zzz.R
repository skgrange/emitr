#' Function to squash R check's global variable notes. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "date_end", "variable", "value", "day", "date_start", "session", "site",
    "site_name", "instrument", "make", "model", "validity", "data_source", 
    "model_variant_name", "vin", "region", "site_met", "registration", 
    "field_campaign"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}
