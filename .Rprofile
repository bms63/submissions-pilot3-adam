source("renv/activate.R")
if (Sys.getenv("GITHUB_ACTIONS") == "") {
  envsetup::rprofile(config::get(file = "_envsetup.yml", config = "prod"))
  source("inst/startup.R")
}
Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "libcurl")
