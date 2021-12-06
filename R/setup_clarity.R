setup_clarity <- function(username, password, ini_path = file.path("inst", "auth", paste0("Clarity.ini"))) {
  ini_path
  if (!dir.exists(dirname(ini_path)))
    dir.create(dirname(ini_path), recursive = TRUE)
  write(paste0("username='",username,"'\npassword='",password,"'"), ini_path)
  .ignore <- paste0(dirname(ini_path),"/*")
  usethis::use_git_ignore(.ignore)
  usethis::use_build_ignore(.ignore)
}
