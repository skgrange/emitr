#' Function to read files containing registration and image file data which act
#' as the linkage between the vehicle capture and vehicile details observational
#' units. 
#' 
#' @param file Vector of file names. 
#' 
#' @param distinct Should duplicated registration-image observations be removed? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
read_registration_images_table <- function(file, distinct = TRUE) {
  
  df <- file %>% 
    setNames(basename(file)) %>% 
    purrr::map_dfr(read_registration_images_table_worker, .id = "file")
  
  if (distinct) {
    
    # Store observation count
    n_rows_pre_distinct <- nrow(df)
    
    # Do
    df <- df %>% 
      distinct(registration,
               file_image,
               .keep_all = TRUE)
    
    n_rows_lost <- n_rows_pre_distinct - nrow(df) 
    
    if (n_rows_lost != 0) {
      warning(n_rows_lost, " duplicated observation(s) removed...", call. = FALSE)
    }
    
  }
  
  return(df)
  
}


read_registration_images_table_worker <- function(file) {
  
  readr::read_csv(
    file, 
    col_names = c("registration", "file_image"), 
    col_types = "cc"
  )
  
}
