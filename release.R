covr = covr::package_coverage()
covr::report(covr)

devtools::spell_check()
devtools::check() # R CMD check
rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")
devtools::check_win_devel()
devtools::check_rhub()

## update cran-comments

git2r::commit(all = TRUE, message = "CRAN commit v0.6.6")
git2r::push(credentials = git2r::cred_ssh_key())

devtools::release()

git2r::tag(name = "v0.6.6", message = "CRAN")
