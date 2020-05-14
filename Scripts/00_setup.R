## PROJECT:  find your beach
## AUTHOR:   T.Essam | USAID
## LICENSE:  MIT
## PURPOSE:  structure project folders


# FUNCTION ----------------------------------------------------------------


folder_setup <- function(folder_list = list("Data", "Images", "Scripts", 
                                            "Dataout", "GIS", "Documents", "Graphics", "markdown")) {
  if(!is.list(folder_list))
    stop("Please provide a list of directories to create for the project.")
  print("The following directories will be created:")
  print(glue::glue(crayon::green('{folder_list}')))
  purrr::map(folder_list, ~dir.create(.))
  
}

# Set global shortucts ----------------------------------------------------

folder_setup()
