
fixed: https://cran.rstudio.com//web/checks/check_results_ezplot.html

Tested on: 
- Fedora Linux, R-devel, clang, gfortran
- Windows Server 2008 R2 SP1, R-devel, 32/64 bit
- Ubuntu Linux 16.04 LTS, R-release, GCC
- Windows: R Under development (unstable) (2020-08-10 r79000)




#### devtools::check_rhub()
√  checking for file 'C:\Users\wkost\Documents\GitHub\ezplot/DESCRIPTION' (414ms)
-  preparing 'ezplot': (17.1s)
√  checking DESCRIPTION meta-information ... 
-  installing the package to build vignettes (446ms)
√  creating vignettes (18s)
-  checking for LF line-endings in source and make files and shell scripts (1.4s)
-  checking for empty or unneeded directories
   Removed empty directory 'ezplot/tests/testthat/_snaps'
-  building 'ezplot_0.6.3.tar.gz'
   
-  Uploading package
-  Preparing build, see status at
   https://builder.r-hub.io/status/ezplot_0.6.3.tar.gz-b7146903fbc240d99d9b04ae9a47ea42
   https://builder.r-hub.io/status/ezplot_0.6.3.tar.gz-d32a64e0f01c4fd18cd1ae908f9d22a0
   https://builder.r-hub.io/status/ezplot_0.6.3.tar.gz-f7633cbb467a4127bc5fbd54c33ec894
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
-  using log directory 'C:/Users/USERhvPVfWIleq/ezplot.Rcheck'
-  using R Under development (unstable) (2020-10-09 r79317)
-  using platform: x86_64-w64-mingw32 (64-bit) (1.3s)
-  using session charset: ISO8859-1
-  using option '--as-cran'
√  checking for file 'ezplot/DESCRIPTION'
-  checking extension type ... Package
-  this is package 'ezplot' version '0.6.3'
-  package encoding: UTF-8
-  checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
   Maintainer: 'Wojtek Kostelecki <wojtek.kostelecki@gmail.com>'
√  checking package namespace information
√  checking package dependencies
√  checking if this is a source package
√  checking if there is a namespace (1.4s)
√  checking for executable files
√  checking for hidden files and directories
√  checking for portable file names
√  checking serialization versions
√  checking whether package 'ezplot' can be installed
√  checking installed package size
√  checking package directory
√  checking for future file timestamps (1.5s)
√  checking 'build' directory
√  checking DESCRIPTION meta-information
√  checking top-level files
√  checking for left-over files (1.4s)
√  checking index information
√  checking package subdirectories
√  checking R files for non-ASCII characters
√  checking R files for syntax errors (1.3s)
√  checking whether the package can be loaded
√  checking whether the package can be loaded with stated dependencies
√  checking whether the package can be unloaded cleanly
√  checking whether the namespace can be loaded with stated dependencies
√  checking whether the namespace can be unloaded cleanly
√  checking loading without being on the library search path
√  checking use of S3 registration
√  checking dependencies in R code (1.4s)
√  checking S3 generic/method consistency
√  checking replacement functions
√  checking foreign function calls
√  checking R code for possible problems (1.3s)
√  checking Rd files
√  checking Rd metadata
√  checking Rd line widths
√  checking Rd cross-references
√  checking for missing documentation entries
√  checking for code/documentation mismatches
√  checking Rd \usage sections
√  checking Rd contents (1.4s)
√  checking for unstated dependencies in examples
√  checking installed files from 'inst/doc'
√  checking files in 'vignettes'
√  checking examples (1.6s)
√  checking examples with --run-donttest
√  checking for unstated dependencies in 'tests'
-  checking tests
√  Running 'testthat.R'
√  checking for unstated dependencies in vignettes
√  checking package vignettes in 'inst/doc'
√  checking re-building of vignette outputs (1.6s)
√  checking PDF version of manual (3.5s)
√  checking for non-standard things in the check directory
√  checking for detritus in the temp directory
   
-  Done with R CMD check
-  Cleaning up files and user
    

-- ezplot 0.6.3: OK

  Build ID:   ezplot_0.6.3.tar.gz-b7146903fbc240d99d9b04ae9a47ea42
  Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  Submitted:  6m 50.9s ago
  Build time: 6m 37.4s

0 errors √ | 0 warnings √ | 0 notes √
