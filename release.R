library(magrittr)
library(glue)

packages = setdiff(c("tidyr", "testthat", "devtools", "DT", "git2r",
                     "devtools", "spelling", "rhub"),
                   installed.packages())
install.packages(packages)


covr = covr::package_coverage()
covr::report(covr)

# check here: https://cran.rstudio.com//web/checks/check_results_ezplot.html
# update version number:
v = "0.7.3"
readLines("DESCRIPTION") %>%
  stringr::str_replace("^Version: [0-9\\.]*$", paste0("Version: ", v)) %>%
  writeLines("DESCRIPTION")

devtools::spell_check()
devtools::check() # R CMD check
rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")
devtools::check_win_devel()
devtools::check_rhub()

## update cran-comments

git2r::commit(all = TRUE, message = paste0("CRAN commit v", v))
system("git push")

devtools::release()

tag = paste0("v", v)
git2r::tag(name = tag, message = "CRAN")
system(glue("git push origin {tag}"))
