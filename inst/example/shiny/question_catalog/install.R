if (!dir.exists('./.lib')) dir.create('./.lib')

new_libs <- c("./.lib", .libPaths())
new_libs <- new_libs[!grepl("^/home", new_libs)]
.libPaths(new_libs)

installed_packages <- rownames(installed.packages())

devtools::install_github("dgkf/shinyAce",
  ref = "dev")

devtools::install_github("kelkhofd/riddlr",
  host = "github.roche.com/api/v3",
  ref = "shiny_rewrite")

if (!"shiny" %in% installed_packages || packageVersion("shiny") < package_version("1.2"))
  install.packages("shiny")

if (!"shinydashboard" %in% installed_packages || packageVersion("shinydashboard") < package_version("0.7"))
  install.packages("shinydashboard")

if (!"shinycssloaders" %in% installed_packages || packageVersion("shinycssloaders") < package_version("0.2"))
  install.packages("shinycssloaders")

if (!"markdown" %in% installed_packages || packageVersion("markdown") < package_version("0.9"))
  install.packages("markdown")

if (!"dplyr" %in% installed_packages || packageVersion("dplyr") < package_version("0.8"))
  install.packages("dplyr")

if (!"tibble" %in% installed_packages || packageVersion("tibble") < package_version("2.0"))
  install.packages("tibble")

if (!"purrr" %in% installed_packages || packageVersion("purrr") < package_version("0.3"))
  install.packages("purrr")

if (!"tidyr" %in% installed_packages || packageVersion("tidyr") < package_version("0.8"))
  install.packages("tidyr")

if (!"DT" %in% installed_packages || packageVersion("DT") < package_version("0.5"))
  install.packages("DT")

if (!"purrr" %in% installed_packages || packageVersion("purrr") < package_version("0.3"))
  install.packages("purrr")
