library(magrittr)
library(glue)

packages = setdiff(c("tidyr", "testthat", "devtools", "DT", "git2r",
                     "devtools", "spelling", "rhub", "patchwork",
                     "tsibble", "tsibbledata", 'covr', 'e1071',
                     "ggrepel", "ROCR"),
                   row.names(installed.packages()))
install.packages(packages)
old.packages()
update.packages(ask = FALSE)

covr = covr::package_coverage()
covr::report(covr)

# check here: https://cran.rstudio.com//web/checks/check_results_ezplot.html
# update version number:
v = "0.8.2"
readLines("DESCRIPTION") %>%
  stringr::str_replace("^Version: [0-9\\.]*$", paste0("Version: ", v)) %>%
  writeLines("DESCRIPTION")

devtools::spell_check()
getOption("browser")("https://cran.rstudio.com//web/checks/check_results_ezplot.html")
devtools::check() # R CMD check
rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")
devtools::check_win_devel()
devtools::check_rhub()

## update cran-comments
unlink("CRAN-SUBMISSION")
git2r::commit(all = TRUE, message = paste0("CRAN commit v", v))
system("git push")

devtools::release()

tag = paste0("v", v)
git2r::tag(name = tag, message = "CRAN")
system(glue("git push origin {tag}"))
getOption("browser")("https://github.com/wkostelecki/ezplot/tags")
