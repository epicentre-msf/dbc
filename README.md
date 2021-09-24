
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbc: Dictionary-based cleaning

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/epicentre-msf/dbc/workflows/R-CMD-check/badge.svg)](https://github.com/epicentre-msf/dbc/actions)
<!-- badges: end -->

Tools for creating and applying dictionaries of value-replacement pairs,
to clean non-valid values of numeric, categorical, or date-type
variables within a dataset.

### Installation

Install from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("epicentre-msf/dbc")
```

### Example usage

#### Example dataset and dictionary

``` r
library(dbc)
data(ll1)          # example messy dataset
data(dict_categ1)  # example dictionary of categorical vars and allowed values

ll1
#> # A tibble: 7 × 10
#>   id    age   age_unit sex   status     contacts date_onset date_admit date_exit  exit_status
#>   <chr> <chr> <chr>    <chr> <chr>      <chr>    <chr>      <chr>      <chr>      <chr>      
#> 1 M143  14    Years    M     Suspected  22       43920      2020-04-01 2021.04.02 <NA>       
#> 2 M345  8     months   F     ?          ten      43924      43926      43940      SENT HOME  
#> 3 M104  29    <NA>     -     confirmed  15       <NA>       03_04_2020 43932      Died       
#> 4 M623  91    Year     -     Confirmed  <NA>     2020-04-10 2020-04-12 2020-04-30 Cure       
#> 5 M685  ?     Days     F     suspect    Not sure <NA>       <NA>       43918      <NA>       
#> 6 M550  39..  Ans      Homme Probable   31       43946      43951      43964      <NA>       
#> 7 M190  66    Years    M     Not a case 17       24/04/2020 43952      43941      Sent home
```

#### Check and clean numeric variables

##### 1. Produce an initial dictionary of non-valid numeric values

``` r
dict_clean_numeric <- check_numeric(
  ll1,
  vars = c("age", "contacts"), # cols that should be numeric
  fn = as.integer              # values not coercible by `fn` are non-valid
)

dict_clean_numeric
#> # A tibble: 4 × 4
#>   variable value    replacement new  
#>   <chr>    <chr>    <chr>       <lgl>
#> 1 age      ?        <NA>        TRUE 
#> 2 age      39..     <NA>        TRUE 
#> 3 contacts ten      <NA>        TRUE 
#> 4 contacts Not sure <NA>        TRUE
```

##### 2. Manually review non-valid values and give appropriate replacements, or use keyword “.na” to indicate that the value has been reviewed and cannot be corrected.

Normally one would do this step in a spreadsheet but we’ll do it in R
here for simplicity.

``` r
dict_clean_numeric$replacement <- c(".na", "39", "10", ".na")
```

##### 3. Use the cleaning dictionary to clean numeric variables

``` r
clean_numeric(
  ll1,
  vars = c("age", "contacts"),
  dict_clean = dict_clean_numeric,
  fn = as.integer
)
#> # A tibble: 7 × 10
#>   id      age age_unit sex   status     contacts date_onset date_admit date_exit  exit_status
#>   <chr> <int> <chr>    <chr> <chr>         <int> <chr>      <chr>      <chr>      <chr>      
#> 1 M143     14 Years    M     Suspected        22 43920      2020-04-01 2021.04.02 <NA>       
#> 2 M345      8 months   F     ?                10 43924      43926      43940      SENT HOME  
#> 3 M104     29 <NA>     -     confirmed        15 <NA>       03_04_2020 43932      Died       
#> 4 M623     91 Year     -     Confirmed        NA 2020-04-10 2020-04-12 2020-04-30 Cure       
#> 5 M685     NA Days     F     suspect          NA <NA>       <NA>       43918      <NA>       
#> 6 M550     39 Ans      Homme Probable         31 43946      43951      43964      <NA>       
#> 7 M190     66 Years    M     Not a case       17 24/04/2020 43952      43941      Sent home
```

##### 4. If the original dataset is updated, we can repeat the cleaning steps while retaining the corrections already made in the previous cleaning dictionary.

*Check for new non-valid numeric values, after incorporating previous
cleaning*

``` r
dict_clean_numeric_update <- check_numeric(
  ll2,                             # same as ll1 but with 3 additional entries
  vars = c("age", "contacts"),
  dict_clean = dict_clean_numeric, # incorporate previous cleaning before checking
  fn = as.integer,
  return_all = TRUE                # return original cleaning dict + new entries
)

dict_clean_numeric_update
#> # A tibble: 5 × 4
#>   variable value    replacement new  
#>   <chr>    <chr>    <chr>       <lgl>
#> 1 age      ?        .na         NA   
#> 2 age      39..     39          NA   
#> 3 contacts ten      10          NA   
#> 4 contacts Not sure .na         NA   
#> 5 age      6 years  <NA>        TRUE
```

*Manually specify replacement for new non-valid entry*

``` r
dict_clean_numeric_update$replacement[5] <- "6"
```

*Apply updated cleaning dictionary to updated dataset*

``` r
clean_numeric(
  ll2,
  vars = c("age", "contacts"),
  dict_clean = dict_clean_numeric_update,
  fn = as.integer
)
#> # A tibble: 10 × 10
#>    id      age age_unit sex   status     contacts date_onset date_admit date_exit  exit_status
#>    <chr> <int> <chr>    <chr> <chr>         <int> <chr>      <chr>      <chr>      <chr>      
#>  1 M143     14 Years    M     Suspected        22 43920      2020-04-01 2021.04.02 <NA>       
#>  2 M345      8 months   F     ?                10 43924      43926      43940      SENT HOME  
#>  3 M104     29 <NA>     -     confirmed        15 <NA>       03_04_2020 43932      Died       
#>  4 M623     91 Year     -     Confirmed        NA 2020-04-10 2020-04-12 2020-04-30 Cure       
#>  5 M685     NA Days     F     suspect          NA <NA>       <NA>       43918      <NA>       
#>  6 M550     39 Ans      Homme Probable         31 43946      43951      43964      <NA>       
#>  7 M190     66 Years    M     Not a case       17 24/04/2020 43952      43941      Sent home  
#>  8 M443     10 Months   F     Confirmed        26 <NA>       43900      43926      Cured      
#>  9 M206      6 Years    f     Conf.             7 43921      43923      109691     dead       
#> 10 M701     56 Years    M     Suspected        39 17-03-2020 20-03-2020 05-04-2020 <NA>
```

#### Check and clean categorical variables

##### 1. Produce an initial dictionary of non-valid categorical values

``` r
dict_clean_categ <- check_categorical(
  ll1,
  dict_allowed = dict_categ1 # dictionary of categorical vars and their allowed values
)

dict_clean_categ
#> # A tibble: 7 × 4
#>   variable    value   replacement new  
#>   <chr>       <chr>   <chr>       <lgl>
#> 1 age_unit    year    <NA>        TRUE 
#> 2 age_unit    ans     <NA>        TRUE 
#> 3 exit_status cure    <NA>        TRUE 
#> 4 sex         -       <NA>        TRUE 
#> 5 sex         homme   <NA>        TRUE 
#> 6 status      ?       <NA>        TRUE 
#> 7 status      suspect <NA>        TRUE
```

##### 2. Manually review non-valid values and give appropriate replacements, or use keyword “.na” to indicate that the value has been reviewed and cannot be corrected.

Again, we would normally do this step in a spreadsheet but we do it in R
here for simplicity.

``` r
dict_clean_categ$replacement <- c(
  "Years",
  "Years",
  "Cured",
  ".na",
  "M",
  ".na",
  "Suspected"
)
```

##### 3. Use the cleaning dictionary to clean categorical variables

``` r
clean_categorical(
  ll1,
  dict_allowed = dict_categ1,
  dict_clean = dict_clean_categ
)
#> # A tibble: 7 × 10
#>   id    age   age_unit sex   status     contacts date_onset date_admit date_exit  exit_status
#>   <chr> <chr> <chr>    <chr> <chr>      <chr>    <chr>      <chr>      <chr>      <chr>      
#> 1 M143  14    Years    M     Suspected  22       43920      2020-04-01 2021.04.02 <NA>       
#> 2 M345  8     Months   F     <NA>       ten      43924      43926      43940      Sent home  
#> 3 M104  29    <NA>     <NA>  Confirmed  15       <NA>       03_04_2020 43932      Died       
#> 4 M623  91    Years    <NA>  Confirmed  <NA>     2020-04-10 2020-04-12 2020-04-30 Cured      
#> 5 M685  ?     Days     F     Suspected  Not sure <NA>       <NA>       43918      <NA>       
#> 6 M550  39..  Years    M     Probable   31       43946      43951      43964      <NA>       
#> 7 M190  66    Years    M     Not a case 17       24/04/2020 43952      43941      Sent home
```

##### 4. If the original dataset is updated, we can repeat the cleaning steps while retaining the corrections already made in the previous cleaning dictionary, as in the example with numeric variables above.
