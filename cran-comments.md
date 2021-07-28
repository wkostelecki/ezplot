
Tested on:
- winbuilder: R Under development (unstable) (2021-07-25 r80663)
- Windows Server 2008 R2 SP1, R-devel, 32/64 bit
- Ubuntu Linux 20.04.1 LTS, R-release, GCC
- Fedora Linux, R-devel, clang, gfortran

#### devtools::check_rhub()
√  checking for file 'C:\Users\wkost\Documents\GitHub\ezplot/DESCRIPTION' ...
-  preparing 'ezplot': (2.7s)
√  checking DESCRIPTION meta-information ... 
-  installing the package to build vignettes
√  creating vignettes (19.5s)
-  checking for LF line-endings in source and make files and shell scripts (1.3s)
-  checking for empty or unneeded directories
   Omitted 'LazyData' from DESCRIPTION
-  building 'ezplot_0.7.1.tar.gz'
   
-  Uploading package
-  Preparing build, see status at
   https://builder.r-hub.io/status/ezplot_0.7.1.tar.gz-3f80bbeb57cc434aa7d56960c5e2ece5
   https://builder.r-hub.io/status/ezplot_0.7.1.tar.gz-ca695334561641c89f75e024b8b85feb
   https://builder.r-hub.io/status/ezplot_0.7.1.tar.gz-fe996104403c4aadad307789ff667b92
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
   Error : Bioconductor does not yet build and check packages for R version 4.2; see
     https://bioconductor.org/install
-  using log directory 'C:/Users/USEREQSseanjZW/ezplot.Rcheck'
-  using R Under development (unstable) (2021-07-03 r80596)
-  using platform: x86_64-w64-mingw32 (64-bit)
-  using session charset: ISO8859-1 (2.7s)
-  using option '--as-cran'
√  checking for file 'ezplot/DESCRIPTION'
-  checking extension type ... Package
-  this is package 'ezplot' version '0.7.1'
-  package encoding: UTF-8
-  checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
   Maintainer: 'Wojtek Kostelecki <wojtek.kostelecki@gmail.com>'
√  checking package namespace information
√  checking package dependencies
√  checking if this is a source package
√  checking if there is a namespace
√  checking for executable files (2.3s)
√  checking for hidden files and directories
√  checking for portable file names
√  checking serialization versions
√  checking whether package 'ezplot' can be installed
√  checking installed package size
√  checking package directory
√  checking for future file timestamps
√  checking 'build' directory (1.3s)
√  checking DESCRIPTION meta-information
√  checking top-level files
√  checking for left-over files
√  checking index information
√  checking package subdirectories
√  checking R files for non-ASCII characters
√  checking R files for syntax errors
√  checking whether the package can be loaded (1.2s)
√  checking whether the package can be loaded with stated dependencies
√  checking whether the package can be unloaded cleanly
√  checking whether the namespace can be loaded with stated dependencies
√  checking whether the namespace can be unloaded cleanly (997ms)
√  checking loading without being on the library search path
√  checking use of S3 registration
√  checking dependencies in R code
√  checking S3 generic/method consistency (1.2s)
√  checking replacement functions
√  checking foreign function calls
√  checking R code for possible problems
√  checking Rd files (919ms)
√  checking Rd metadata
√  checking Rd line widths
√  checking Rd cross-references
√  checking for missing documentation entries (897ms)
√  checking for code/documentation mismatches
√  checking Rd \usage sections
√  checking Rd contents
√  checking for unstated dependencies in examples (963ms)
√  checking installed files from 'inst/doc'
√  checking files in 'vignettes'
√  checking examples
√  checking examples with --run-donttest (16.6s)
√  checking for unstated dependencies in 'tests'
-  checking tests
√  Running 'testthat.R' (6.1s)
√  checking for unstated dependencies in vignettes (6.1s)
√  checking package vignettes in 'inst/doc'
√  checking re-building of vignette outputs (14.5s)
√  checking PDF version of manual (46.9s)
√  checking for non-standard things in the check directory
√  checking for detritus in the temp directory
   
-  Done with R CMD check
-  Cleaning up files and user
    

-- ezplot 0.7.1: OK

  Build ID:   ezplot_0.7.1.tar.gz-3f80bbeb57cc434aa7d56960c5e2ece5
  Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  Submitted:  6m 47.8s ago
  Build time: 6m 39s

0 errors √ | 0 warnings √ | 0 notes √
