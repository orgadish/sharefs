
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sharefs

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/orgadish/sharefs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/orgadish/sharefs/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/sharefs)](https://cran.r-project.org/package=sharefs)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/sharefs)](https://cran.r-project.org/package=sharefs)
<!-- badges: end -->

sharefs lists and copies files with minimal network calls – useful when
reading from a network share (a mapped drive, a UNC path, an SMB/CIFS
mount), where listing a directory and `stat()`-ing each file separately
normally costs one network round trip per file. `sfs_dir_info()` uses a
single `Get-ChildItem` call instead, and `sfs_stage_local()` copies
files to local disk (via `sfs_robocopy()`) before you read them.

sharefs is Windows-only (`OS_type: windows`). `sfs_dir_info()` requires
PowerShell (see `sfs_powershell_available()`) and errors if it isn’t
available.

## Installation

``` r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("orgadish/sharefs")
```

## Usage

``` r
library(sharefs)

# A UNC path or mapped network drive in practice; a local temp
# directory here so this example actually runs.
network_location <- tempfile()
dir.create(network_location)
write.csv(data.frame(x = 1:2), file.path(network_location, "a.csv"), row.names = FALSE)
write.csv(data.frame(x = 3:4), file.path(network_location, "b.csv"), row.names = FALSE)

sfs_dir_info(network_location, glob = "*.csv")
#> # A tibble: 2 × 6
#>   path                       type   size modification_time   access_time        
#>   <fs::path>                 <fct> <fs:> <dttm>              <dttm>             
#> 1 …5qS/file7dc4cbf4dff/a.csv file     11 2026-07-16 19:12:39 2026-07-16 19:12:39
#> 2 …5qS/file7dc4cbf4dff/b.csv file     11 2026-07-16 19:12:39 2026-07-16 19:12:39
#> # ℹ 1 more variable: birth_time <dttm>
sfs_dir_ls(network_location, glob = "*.csv")
#> C:/Users/or.gadish/AppData/Local/Temp/Rtmp0oD5qS/file7dc4cbf4dff/a.csv
#> C:/Users/or.gadish/AppData/Local/Temp/Rtmp0oD5qS/file7dc4cbf4dff/b.csv
```

`sfs_dir_info()`’s columns are a narrower set than `fs::dir_info()`’s –
see `?sfs_dir_info` for which ones and why.

``` r
staged <- sfs_dir_info(network_location, glob = "*.csv") |>
  sfs_stage_local()
staged
#> # A tibble: 2 × 4
#>   path                                      local_path  size modification_time  
#>   <fs::path>                                <chr>      <dbl> <dttm>             
#> 1 …al/Temp/Rtmp0oD5qS/file7dc4cbf4dff/a.csv "C:\\User…    11 2026-07-16 19:12:39
#> 2 …al/Temp/Rtmp0oD5qS/file7dc4cbf4dff/b.csv "C:\\User…    11 2026-07-16 19:12:39

lapply(staged$local_path, read.csv)
#> [[1]]
#>   x
#> 1 1
#> 2 2
#> 
#> [[2]]
#>   x
#> 1 3
#> 2 4

sfs_stage_cleanup(staged)
unlink(network_location, recursive = TRUE)
```
