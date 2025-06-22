
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mNIRS

<!-- # mNIRS <img src='man/figures/logo.png' align="right" height="240" /> -->

<!-- badges: start -->

<!-- badges: end -->

`mNIRS` is a package to allow for importing, processing, and analysing
data from muscle near-infrared spectroscopy (mNIRS) devices.

## Installation

You can install the development version of `mNIRS` from
[GitHub](https://github.com/jemarnold/mNIRS) with:

``` r
# install.packages("remotes")
devtools::install_github("jemarnold/mNIRS")
```

## Citation

…

## Usage

### Read data from file

``` r
library(mNIRS)

## {mNIRS} includes some sample files from Moxy and Train.Red
file_path <- system.file("moxy_ramp_example.xlsx", package = "mNIRS")

## rename columns in the format "new_name" = "original_name"
## where "original_name" should match the file column name exactly
data_raw <- read_data(file_path = file_path,
                      nirs_columns = c(smo2_left = "smo2_left_VL",
                                       smo2_right = "smo2_right_VL"),
                      sample_column = c(time = "Time"),
                      event_column = c(event = "Event"),
                      numeric_time = TRUE,
                      .keep_all = TRUE)
#> Warning: "time" has non-sequential or repeating values. Consider investigating at "time"
#> = 1952, 1952, 1952, 2924.01, and 2924.01.
#> ℹ Estimated sample rate is 2 Hz. Overwrite this by re-running with `sample_rate = X`

plot(data_raw)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r

data_raw
#> # A tibble: 2,203 × 4
#>     time event smo2_left smo2_right
#>    <dbl> <chr>     <dbl>      <dbl>
#>  1 1740. <NA>       67.6       54  
#>  2 1740. <NA>       67.6       54  
#>  3 1741. <NA>       67.6       54  
#>  4 1742. <NA>       66.3       53.5
#>  5 1742. <NA>       66.3       53.5
#>  6 1743. <NA>       66.3       53.5
#>  7 1743. <NA>       66.3       53.5
#>  8 1744. <NA>       67.2       57.1
#>  9 1744. <NA>       67.2       57.1
#> 10 1745. <NA>       67.2       57.1
#> # ℹ 2,193 more rows
```

### Replace missing data, outliers, and fixed values

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

## metadata are stored in dataframe attributes
nirs_columns <- attributes(data_raw)$nirs_columns
sample_rate <- attributes(data_raw)$sample_rate

data_cleaned <- data_raw |> 
    mutate(
        across(any_of(nirs_columns), 
               \(.x) replace_outliers(x = .x,
                                      k = 20 * sample_rate, ## 20 sec median window
                                      t0 = 3,
                                      na.rm = TRUE,
                                      return = "median")$y
        ),
        across(any_of(nirs_columns), 
               \(.x) replace_missing_values(x = .x,
                                            method = "linear",
                                            na.rm = FALSE,
                                            maxgap = Inf)$y
        ),
        across(any_of(nirs_columns), 
               \(.x) replace_fixed_values(x = .x,
                                          fixed_values = c(0, 100),
                                          k = 20 * sample_rate,
                                          return = "median")$y
        )
    )

plot(data_cleaned)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r

data_cleaned
#> # A tibble: 2,203 × 4
#>     time event smo2_left smo2_right
#>    <dbl> <chr>     <dbl>      <dbl>
#>  1 1740. <NA>       67.6       54  
#>  2 1740. <NA>       67.6       54  
#>  3 1741. <NA>       67.6       54  
#>  4 1742. <NA>       66.3       53.5
#>  5 1742. <NA>       66.3       53.5
#>  6 1743. <NA>       66.3       53.5
#>  7 1743. <NA>       66.3       53.5
#>  8 1744. <NA>       67.2       57.1
#>  9 1744. <NA>       67.2       57.1
#> 10 1745. <NA>       67.2       57.1
#> # ℹ 2,193 more rows
```

### Resample data

``` r

sample_column <- attributes(data_cleaned)$sample_column

data_resampled <- data_cleaned |> 
    resample_dataframe(sample_column = sample_column,
                       sample_rate = sample_rate,
                       resample_rate = 1
                       ) ## resample to 1 Hz
#> ℹ Estimated sample rate is 2 Hz. Overwrite this by re-running with `sample_rate = X`

data_resampled
#> # A tibble: 2,189 × 4
#>     time smo2_left smo2_right event
#>    <dbl>     <dbl>      <dbl> <chr>
#>  1  3480      67.6       54   <NA> 
#>  2  3481      67.6       54   <NA> 
#>  3  3482      67.6       54   <NA> 
#>  4  3483      66.3       53.5 <NA> 
#>  5  3484      66.3       53.5 <NA> 
#>  6  3485      66.3       53.5 <NA> 
#>  7  3486      66.3       53.5 <NA> 
#>  8  3487      67.2       57.1 <NA> 
#>  9  3489      67.2       57.1 <NA> 
#> 10  3490      67.2       57.1 <NA> 
#> # ℹ 2,179 more rows
```

### Filter (smooth) data

``` r
data_filtered <- data_resampled |> 
    mutate(
        across(any_of(nirs_columns),
               \(.x) filter_data(x = .x,
                                 method = "moving-average",
                                 width = 15)
        )
    )
#> ℹ Moving-average: width = 15.
#> ℹ Moving-average: width = 15.

plot(data_filtered)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r

data_filtered
#> # A tibble: 2,189 × 4
#>     time smo2_left smo2_right event
#>    <dbl>     <dbl>      <dbl> <chr>
#>  1  3480      66.9       54.1 <NA> 
#>  2  3481      66.9       54.5 <NA> 
#>  3  3482      67.0       54.7 <NA> 
#>  4  3483      67.0       54.9 <NA> 
#>  5  3484      67.1       54.8 <NA> 
#>  6  3485      67.1       54.7 <NA> 
#>  7  3486      67.2       54.6 <NA> 
#>  8  3487      67.1       54.4 <NA> 
#>  9  3489      67.0       54.3 <NA> 
#> 10  3490      66.9       54.2 <NA> 
#> # ℹ 2,179 more rows
```

### Shift and normalise data

``` r
data_shifted <- data_filtered |> 
    shift_dataframe(nirs_columns = list(nirs_columns), ## wrap vector in list to shift all columns together
                    shift_to = 0,
                    position = "first",
                    mean_samples = 30) ## shift the mean first 30 sec equal to zero

plot(data_shifted)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r

data_shifted
#> # A tibble: 2,189 × 4
#>     time smo2_left smo2_right event
#>    <dbl>     <dbl>      <dbl> <chr>
#>  1  3480      6.63      -6.13 <NA> 
#>  2  3481      6.66      -5.80 <NA> 
#>  3  3482      6.69      -5.54 <NA> 
#>  4  3483      6.71      -5.32 <NA> 
#>  5  3484      6.80      -5.47 <NA> 
#>  6  3485      6.87      -5.59 <NA> 
#>  7  3486      6.93      -5.70 <NA> 
#>  8  3487      6.86      -5.85 <NA> 
#>  9  3489      6.77      -5.96 <NA> 
#> 10  3490      6.68      -6.08 <NA> 
#> # ℹ 2,179 more rows

data_normalised <- data_filtered |> 
    normalise_dataframe(nirs_columns = as.list(nirs_columns), ## convert vector to list to shift each column separately
                        normalise_range = c(0, 100))

plot(data_normalised)
```

<img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />

``` r

data_normalised
#> # A tibble: 2,189 × 4
#>     time smo2_left smo2_right event
#>    <dbl>     <dbl>      <dbl> <chr>
#>  1  3480      76.1       64.8 <NA> 
#>  2  3481      76.2       65.3 <NA> 
#>  3  3482      76.2       65.7 <NA> 
#>  4  3483      76.2       66.0 <NA> 
#>  5  3484      76.3       65.8 <NA> 
#>  6  3485      76.4       65.6 <NA> 
#>  7  3486      76.5       65.4 <NA> 
#>  8  3487      76.4       65.2 <NA> 
#>  9  3489      76.3       65.0 <NA> 
#> 10  3490      76.2       64.9 <NA> 
#> # ℹ 2,179 more rows
```

### Process kinetics

> under development

## mNIRS Device Compatibility

This package is designed to recognise mNIRS data exported as *.xlsx*,
*.xls*, or *.csv* files. It should be flexible for use with many
different mNIRS devices, and compatibility will improve with continued
development.

This package have been tested successfully with the following mNIRS
devices:

- [Moxy](https://www.moxymonitor.com/) 5 and 3
- [Train.Red](https://train.red/) FYER and Plus
- [Artinis](https://www.artinis.com/nirs-devices) Portamon and Oxymon

This package have been tested successfully with mNIRS data exported from
the following devices and apps:

- [Moxy](https://www.moxymonitor.com/) onboard export (.csv)
- [Train.Red](https://train.red/) app (.csv)
- [Artinis Oxysoft](https://www.artinis.com/oxysoft) software (.csv and
  .xlsx)
- [VO2 Master Manager](https://vo2master.com/features/) app (.xlsx)
- [PerfPro](https://perfprostudio.com/) software (.xlsx)
