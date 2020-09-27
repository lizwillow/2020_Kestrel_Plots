## functions I'd like in a package


# working with files ------------------------------------------------------

# create data, code, and plot folders if they do not already exist
make_folders <- function(v = "010") {
  ifelse(!dir.exists(here::here(paste0(v, "_data"))), 
         dir.create(here::here(paste0(v, "_data"))), 
         FALSE)
  ifelse(!dir.exists(here::here(paste0(v, "_plots"))), 
         dir.create(here::here(paste0(v, "_plots"))), 
         FALSE)
  ifelse(!dir.exists(here::here(paste0(v, "_code"))), 
         dir.create(here::here(paste0(v, "_code"))), 
         FALSE)
}

# make new folders within data and plot folders which are labeled 
# with the current script name
new_rfiles <- function(rfile, v) {
  ifelse(!dir.exists(here::here(paste0(v, "_data"), rfile)), 
         dir.create(here::here(paste0(v, "_data"), rfile)), 
         FALSE)
  ifelse(!dir.exists(here::here(paste0(v, "_plots"), rfile)), 
         dir.create(here::here(paste0(v, "_plots"), rfile)), 
         FALSE)
}

# use ggsave() to save plots to the rfile folder within the plots file
save_ggplot <- function(name = "tmp.png", rfile, v, ...) {
  if (rlang::is_missing(rfile)) {
    library(stringi)    
    rfile = basename(rstudioapi::getSourceEditorContext()$path) %>%
      gsub(".*[0-9]_", "", .) %>%
      gsub(".R", "", .)
  }
  ggsave(name, path = here::here(paste0(v, "_plots"), rfile), ...)
}

save_rda <- function(object, ..., v, rfile) {
  if (rlang::is_missing(rfile)) {
    library(stringi)    
    rfile = basename(rstudioapi::getSourceEditorContext()$path) %>%
      gsub(".*[0-9]_", "", .) %>%
      gsub(".R", "", .)
  }
  save(object, ...,
       file = here::here(paste0(v, "_data"), rfile,
                         paste0(deparse(substitute(object)), ".rda")))
}

save_rds <- function(object, v, rfile) {
  library(stringi)    
  if (rlang::is_missing(rfile)) {
    library(stringi)
    rfile = basename(rstudioapi::getSourceEditorContext()$path) %>%
      gsub(".*[0-9]_", "", .) %>%
      gsub(".R", "", .)
  }
  saveRDS(object,
       file = here::here(paste0(v, "_data"), rfile,
                         paste0(deparse(substitute(object)), ".rds")))
}








