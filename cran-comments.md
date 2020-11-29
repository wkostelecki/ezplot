
fixed: https://cran.rstudio.com//web/checks/check_results_ezplot.html

Tested on:
- winbuilder: R Under development (unstable) (2020-11-24 r79490)
- Windows Server 2008 R2 SP1, R-devel, 32/64 bit
- Ubuntu Linux 16.04 LTS, R-release, GCC
- Fedora Linux, R-devel, clang, gfortran

#### devtools::check_rhub()
√  checking for file 'C:\Users\wkost\Documents\GitHub\ezplot/DESCRIPTION' (514ms)
-  preparing 'ezplot': (18.2s)
√  checking DESCRIPTION meta-information ... 
-  installing the package to build vignettes (401ms)
√  creating vignettes (17.8s)
-  checking for LF line-endings in source and make files and shell scripts (1.4s)
-  checking for empty or unneeded directories
   Removed empty directory 'ezplot/tests/testthat/_snaps'
-  building 'ezplot_0.6.6.tar.gz'
   
-  Uploading package
-  Preparing build, see status at
   https://builder.r-hub.io/status/ezplot_0.6.6.tar.gz-832051993e7f4e08b8703d507ba73cff
   https://builder.r-hub.io/status/ezplot_0.6.6.tar.gz-1ac35cb60ebc48338b877eacc79cd0f4
   https://builder.r-hub.io/status/ezplot_0.6.6.tar.gz-3be53bf5a5f042c08180fcd6292944a4
-  Build started
-  Creating new user
-  Downloading and unpacking package file
-  Querying package dependencies
-  Installing package dependencies
-  Running R CMD check
   setting _R_CHECK_FORCE_SUGGESTS_ to false
   setting R_COMPILE_AND_INSTALL_PACKAGES to never
   setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
   setting R_REMOTES_STANDALONE to true
   setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
   setting _R_CHECK_FORCE_SUGGESTS_ to true
   setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
-  using log directory 'C:/Users/USERQcWpZEMiju/ezplot.Rcheck' (809ms)
-  using R Under development (unstable) (2020-11-17 r79439)
-  using platform: x86_64-w64-mingw32 (64-bit)
-  using session charset: ISO8859-1
-  using option '--as-cran' (815ms)
√  checking for file 'ezplot/DESCRIPTION'
-  checking extension type ... Package
-  this is package 'ezplot' version '0.6.6'
-  package encoding: UTF-8 (1.6s)
-  checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
   Maintainer: 'Wojtek Kostelecki <wojtek.kostelecki@gmail.com>'
√  checking package namespace information
√  checking package dependencies (802ms)
√  checking if this is a source package
√  checking if there is a namespace
√  checking for executable files
√  checking for hidden files and directories (802ms)
√  checking for portable file names
√  checking serialization versions
√  checking whether package 'ezplot' can be installed
√  checking installed package size (799ms)
√  checking package directory
√  checking for future file timestamps
√  checking 'build' directory
√  checking DESCRIPTION meta-information (1.6s)
√  checking top-level files
√  checking for left-over files
√  checking index information
√  checking package subdirectories (807ms)
√  checking R files for non-ASCII characters
√  checking R files for syntax errors
√  checking whether the package can be loaded
√  checking whether the package can be loaded with stated dependencies (797ms)
√  checking whether the package can be unloaded cleanly
√  checking whether the namespace can be loaded with stated dependencies
√  checking whether the namespace can be unloaded cleanly
√  checking loading without being on the library search path (796ms)
√  checking use of S3 registration
√  checking dependencies in R code
√  checking S3 generic/method consistency
√  checking replacement functions (1.6s)
√  checking foreign function calls
√  checking R code for possible problems
√  checking Rd files
√  checking Rd metadata (797ms)
√  checking Rd line widths
√  checking Rd cross-references
√  checking for missing documentation entries
√  checking for code/documentation mismatches (802ms)
√  checking Rd \usage sections
√  checking Rd contents
√  checking for unstated dependencies in examples
√  checking installed files from 'inst/doc' (801ms)
√  checking files in 'vignettes'
√  checking examples
√  checking examples with --run-donttest
√  checking for unstated dependencies in 'tests' (802ms)
-  checking tests (828ms)
√  Running 'testthat.R'
√  checking for unstated dependencies in vignettes (813ms)
√  checking package vignettes in 'inst/doc'
√  checking re-building of vignette outputs
√  checking PDF version of manual (39s)
√  checking for non-standard things in the check directory
√  checking for detritus in the temp directory
   
-  Done with R CMD check
-  Cleaning up files and user
    

-- ezplot 0.6.6: OK

  Build ID:   ezplot_0.6.6.tar.gz-832051993e7f4e08b8703d507ba73cff
  Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  Submitted:  6m 37.1s ago
  Build time: 6m 31s

0 errors √ | 0 warnings √ | 0 notes √
