ca_dir <- "../clarity.automation"
devtools::load_all(ca_dir)

cl_driver <- clarity$new(ini_path = file.path(ca_dir, "inst", "auth", "Clarity.ini"))
cl_driver$get_hud_export()
cl_driver$debug_int()
