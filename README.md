
<!-- README.md is generated from README.Rmd. Please edit that file -->
*Package is work in progress!*

Introduction
============

This package parses a git repository history to collect comprehensive information about the activity in the repo. The parsed data is made available to the user in a tabular format. To parse the git repo history,use `get_log_regex`. For each commit, the function also information on which files were changed in a nested tibble.

``` r
tbl <- get_log_regex() %>%
  select(short_hash, message, total_files_changed, nested)
tbl 
#> # A tibble: 11 × 4
#>    short_hash               message total_files_changed           nested
#>         <chr>                 <chr>               <int>           <list>
#> 1        243f        initial commit                  NA <tibble [7 × 6]>
#> 2        <NA>  add log example data                   1 <tibble [1 × 6]>
#> 3        <NA>           add parents                   3 <tibble [3 × 6]>
#> 4        <NA>          intermediate                   1 <tibble [1 × 6]>
#> 5        <NA>           add licence                  NA <tibble [1 × 6]>
#> 6        <NA>            add readme                  NA <tibble [2 × 6]>
#> 7        <NA>     document log data                  NA <tibble [1 × 6]>
#> 8        <NA>         add helpfiles                  10 <tibble [9 × 6]>
#> 9        <NA> update infrastructure                   3 <tibble [3 × 6]>
#> 10       <NA>        remove garbage                   6 <tibble [5 × 6]>
#> 11       <NA>        add md anyways                   5 <tibble [3 × 6]>
```

``` r
tbl$nested[[3]]
#> # A tibble: 3 × 6
#>   changed_file edits deletions insertions deletions_symbol
#>          <chr> <int>     <int>      <int>            <chr>
#> 1  DESCRIPTION     6         1          5                -
#> 2    NAMESPACE     3         1          2                -
#> 3  R/get_log.R    19         8         11         --------
#> # ... with 1 more variables: insertions_symbol <chr>
```

Since the data has such a high resolution, various graphs, tables ect. can be produced from it to give insights into a git history.

Examples
========

Since the output of `get_log_regex()` is a nested tibble, you can work on it as you work on any other tibble. Let us first have a look who comitted to this repository:

``` r
log <- get_log_regex()
log %>%
group_by(author_name) %>%
  count()
#> # A tibble: 1 × 2
#>       author_name     n
#>             <chr> <int>
#> 1 Lorenz Walthert    11
```

Next, we want to see which files were contained in most commits.

``` r
log %>%
  unnest(nested) %>% # unnest the tibble
  group_by(changed_file) %>%
  count() %>%
  na.omit() %>%
  ggplot(aes(x = changed_file, y = n)) + geom_col() + coord_flip()
```

![](README-ggplot1-1.png)
