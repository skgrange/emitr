#' Functions to help calculate fuel specific emissions for various pollutants
#' using their ratio to CO[2].
#' 
#' @param ratio_pollutant Ratio of pollutant to CO[2].
#' 
#' @param pollutant In \code{pollutant_ratio_to_fuel_specific_emission}, a 
#' string representing which polluant is being transformed, e.g. \code{"no"}. 
#' \code{pollutant_ratio_to_fuel_specific_emission} uses this string to find the
#' correct molar mass. In \code{pollutant_to_co2_ratio}, it is the variable 
#' which is to be transformed. 
#' 
#' @param ratio_co Name of CO/CO[2] ratio variable.
#' 
#' @param ratio_hc Name of HC/CO[2] ratio variable.
#' 
#' @param co2 Name of CO[2] ratio variable.
#' 
#' @param no Name of NO ratio variable.
#' 
#' @param no2 Name of NO[2] ratio variable.
#' 
#' @return Numeric vector or for \code{read_molar_mass_table}, a data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
pollutant_ratio_to_fuel_specific_emission <- function(ratio_pollutant, 
                                                      pollutant,
                                                      ratio_co,
                                                      ratio_hc) {
  
  # Parse input
  stopifnot(!missing(pollutant))
  pollutant <- stringr::str_to_lower(pollutant)
  pollutant <- stringr::str_trim(pollutant)
  
  # Get molar mass from look up table
  index <- which(read_molar_mass_table()$variable == pollutant)
  if (length(index) == 0) stop("Pollutant not recognised...", call. = FALSE)
  
  # Get mass from table
  molar_mass <- read_molar_mass_table()$molar_mass[index]
  
  # The fuel specific calculation
  x <- (1 / 0.014) * molar_mass * ratio_pollutant / (1 + ratio_co + 3 * ratio_hc)
  
  return(x)
  
} 


#' @rdname pollutant_ratio_to_fuel_specific_emission
#' @export
read_molar_mass_table <- function() {

  dplyr::tribble(
    ~variable, ~molar_mass, ~notes,
    "no", 30, NA,
    "no2", 46, NA,
    "nox", 46, "use no2's mass",
    "hc", 88, NA,
    "nh3", 17, NA,
    "co", 28, NA,
    "co2", 44, NA,
    "so2", 64.066, NA
  )  
  
}


#' @rdname pollutant_ratio_to_fuel_specific_emission
#' @export
pollutant_to_co2_ratio <- function(pollutant, co2) pollutant / co2


#' @rdname pollutant_ratio_to_fuel_specific_emission
#' @export
nox_as_no2_equivalent <- function(no, no2) {
  
  # Get constants
  df <- read_molar_mass_table()
  mass_no <- df[df$variable == "no", "molar_mass", drop = TRUE]
  mass_no2 <- df[df$variable == "no2", "molar_mass", drop = TRUE]
  
  # Calculate
  x <- no2 + no * mass_no2 / mass_no
  
  return(x)
  
}

# emi <- mutate(emi, 
#               Q_NO =  X.NO / X.CO2,
#               Q_NO2 =  X.NO2 / X.CO2,
#               Q_HC =  X.HC / X.CO2,
#               Q_NH3 =  X.NH3 / X.CO2,
#               Q_CO =  X.CO / X.CO2,
#               NO_gpkg = (1 / 0.014) * 30 * Q_NO / (1 + Q_CO + 3 * Q_HC),
#               NO2_gpkg = (1 / 0.014) * 46 * Q_NO2 / (1 + Q_CO + 3 * Q_HC),
#               HC_gpkg = (1 / 0.014) * 88 * Q_HC / (1 + Q_CO + 3 * Q_HC),
#               NH3_gpkg = (1 / 0.014) * 17 * Q_NH3 / (1 + Q_CO + 3 * Q_HC),
#               CO_gpkg = (1 / 0.014) * 28 * Q_CO / (1 + Q_CO + 3 * Q_HC),
#               NOx_gpkg = NO2_gpkg + NO_gpkg * 46 / 30)
