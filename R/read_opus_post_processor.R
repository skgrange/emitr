#' Function to read Opus's RSD post-processsor data files. 
#' 
#' @param file Vector of file names. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
read_opus_post_processor <- function(file, verbose = FALSE) {
  
  # Read all files
  file %>% 
    setNames(basename(file)) %>% 
    purrr::map_dfr(
      ~read_opus_post_processor_output(
        .x, 
        verbose = verbose
      ),
      .id = "file"
    )
  
}


read_opus_post_processor_output <- function(file, verbose) {
  
  # Load data
  if (verbose) message(threadr::date_message(), file)
  df <- readr::read_csv(file, col_types = readr::cols(), progress = FALSE)
  
  # Parse dates
  df <- df %>% 
    mutate(VDRDateTime = lubridate::mdy_hms(VDRDateTime, tz = "UTC"))
  
  return(df)
  
}


#' Function to clean Opus's RSD post-processsor tibbles (tables) after reading
#' with \code{\link{read_opus_post_processor}}. 
#' 
#' @param df Tibble/data frame from \code{\link{read_opus_post_processor}}.
#' 
#' @param df_images An additional tibble from 
#' \code{\link{read_opus_post_processor}} to join vehicle registrations to the 
#' table. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{read_opus_post_processor}}
#' 
#' @export
clean_opus_post_processor <- function(df, df_images = NA) {
  
  # Clean names and table a bit
  df <- df %>% 
    setNames(threadr::str_to_underscore(names(.))) %>% 
    mutate(vdr_day = lubridate::floor_date(vdr_date_time, "day"),
           ambient_temperature = ambient_temperature - 273.15,
           sdm_temperature = sdm_temperature - 273.15)
  
  # Join registraions if supplied
  if (is.data.frame(df_images) && !is.na(df_images)) {
    df <- left_join(df, df_images, by = c("std_image_name" = "file_image"))
  }
  
  return(df)
  
}


#' Function to transform Opus's character-based RSD post-processsor validity 
#' codes to a binary validity. 
#' 
#' @author Stuart K. Grange. 
#' 
#' @param x Opus's character-based RSD post-processsor validity vector.
#' 
#' @return Logical vector. 
#' 
#' @export
character_validity_to_binary <- function(x) {
  
  x <- stringr::str_to_lower(x)
  x <- if_else(x == "v", TRUE, FALSE)
  return(x)
  
}
