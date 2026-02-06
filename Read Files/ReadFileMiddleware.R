read_file <- function(file) {
  
  ext <- tolower(tools::file_ext(file))
  
  if (ext == "xpt") {
    haven::read_xpt(file)
    
  } else if (ext == "rds") {
    readRDS(file)
    
  } else if (ext == "rda") {
    env <- new.env()
    load(file, envir = env)
    env[[ls(env)[1]]]
    
  } else if (ext == "csv") {
    read.csv(file, stringsAsFactors = FALSE)
    
  }else if (ext == "sas7bdat") {
    haven::read_sas(file)
  } else {
    stop("Unsupported ADaM file type")
  }
}