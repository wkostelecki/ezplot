covr = covr::package_coverage()
covr::report(covr)

devtools::spell_check()
devtools::check() # R CMD check
devtools::check_win_devel()
devtools::check_rhub()
devtools::release()

git2r::add()
git2r::commit()
git2r::tag(name = "v0.6.0", message = "CRAN")
git2r::push()
