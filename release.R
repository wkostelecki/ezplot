covr = covr::package_coverage()
covr::report(covr)

devtools::spell_check()
devtools::check() # R CMD check
devtools::check_win_devel()
devtools::check_rhub()

## update cran-comments

git2r::commit(all = TRUE, message = "CRAN commit")
git2r::tag(name = "v0.6.3", message = "CRAN")
git2r::push(credentials = git2r::cred_ssh_key())

devtools::release()
