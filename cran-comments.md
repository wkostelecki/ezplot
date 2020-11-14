
fixed: https://cran.rstudio.com//web/checks/check_results_ezplot.html

Tested on:
- winbuilder: R Under development (unstable) (2020-11-13 r79429)
- Windows Server 2008 R2 SP1, R-devel, 32/64 bit
- Ubuntu Linux 16.04 LTS, R-release, GCC
- Fedora Linux, R-devel, clang, gfortran

#### devtools::check_rhub()
√  checking for file 'C:\Users\wkost\Documents\GitHub\ezplot/DESCRIPTION' ...
-  preparing 'ezplot': (15.9s)
√  checking DESCRIPTION meta-information ...
-  installing the package to build vignettes (670ms)
√  creating vignettes (26.5s)
-  checking for LF line-endings in source and make files and shell scripts (1.2s)
-  checking for empty or unneeded directories
   Removed empty directory 'ezplot/tests/testthat/_snaps'
-  building 'ezplot_0.6.3.tar.gz'
   
-  Uploading package
-  Preparing build, see status at
   https://builder.r-hub.io/status/ezplot_0.6.3.tar.gz-cf27140ca30d4b9a86edc71fc62acfd4
   https://builder.r-hub.io/status/ezplot_0.6.3.tar.gz-49460ba4a1ab495c9f23058f1054a890
   https://builder.r-hub.io/status/ezplot_0.6.3.tar.gz-96857f4cea254a7ebbda35ab9b9cc44a
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
-  using log directory 'C:/Users/USERlxZMBwDHiX/ezplot.Rcheck'
-  using R Under development (unstable) (2020-10-09 r79317)
-  using platform: x86_64-w64-mingw32 (64-bit) (821ms)
-  using session charset: ISO8859-1
-  using option '--as-cran'
√  checking for file 'ezplot/DESCRIPTION'
-  checking extension type ... Package (907ms)
-  this is package 'ezplot' version '0.6.3'
-  package encoding: UTF-8
-  checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
   Maintainer: 'Wojtek Kostelecki <wojtek.kostelecki@gmail.com>'
√  checking package namespace information
√  checking package dependencies
√  checking if this is a source package
√  checking if there is a namespace (910ms)
√  checking for executable files
√  checking for hidden files and directories
√  checking for portable file names
√  checking serialization versions (904ms)
√  checking whether package 'ezplot' can be installed
√  checking installed package size
√  checking package directory
√  checking for future file timestamps (1.7s)
√  checking 'build' directory
√  checking DESCRIPTION meta-information
√  checking top-level files
√  checking for left-over files (909ms)
√  checking index information
√  checking package subdirectories
√  checking R files for non-ASCII characters
√  checking R files for syntax errors (899ms)
√  checking whether the package can be loaded
√  checking whether the package can be loaded with stated dependencies
√  checking whether the package can be unloaded cleanly
√  checking whether the namespace can be loaded with stated dependencies (905ms)
√  checking whether the namespace can be unloaded cleanly
√  checking loading without being on the library search path
√  checking use of S3 registration
√  checking dependencies in R code (911ms)
√  checking S3 generic/method consistency
√  checking replacement functions
√  checking foreign function calls
√  checking R code for possible problems (896ms)
√  checking Rd files
√  checking Rd metadata
√  checking Rd line widths
√  checking Rd cross-references (859ms)
√  checking for missing documentation entries
√  checking for code/documentation mismatches
√  checking Rd \usage sections
√  checking Rd contents (847ms)
√  checking for unstated dependencies in examples
√  checking installed files from 'inst/doc'
√  checking files in 'vignettes'
√  checking examples (851ms)
√  checking examples with --run-donttest
√  checking for unstated dependencies in 'tests'
-  checking tests
√  Running 'testthat.R' (1.8s)
√  checking for unstated dependencies in vignettes (1.8s)
√  checking package vignettes in 'inst/doc'
√  checking re-building of vignette outputs (903ms)
√  checking PDF version of manual (2.8s)
√  checking for non-standard things in the check directory
√  checking for detritus in the temp directory
   
-  Done with R CMD check
-  Cleaning up files and user
    

-- ezplot 0.6.3: OK

  Build ID:   ezplot_0.6.3.tar.gz-cf27140ca30d4b9a86edc71fc62acfd4
  Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  Submitted:  6m 35.2s ago
  Build time: 6m 24.6s

0 errors √ | 0 warnings √ | 0 notes √
