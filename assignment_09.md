P8180 - SQL Assignment \#9
================
Jon Brock
2/20/2020

``` r
library(tidyverse)
library(sqldf)
```

``` r
demo <- read.csv("./NHANES_Demographics.csv") %>% janitor::clean_names()
tri <- read.csv("./NHANES_Triglycerides.csv") %>% janitor::clean_names()
```

``` r
demo_df <- as_tibble(demo)
tri_df <- as_tibble(tri)
```

``` r
demo_clean <- mutate(demo_df,
                seqn = respondent_sequence_number,
                age = age_in_years_at_screening,
                race = as_factor(race_hispanic_origin_w_nh_asian),
                pregnant = as_factor(pregnancy_status_at_exam),
                household_income = annual_household_income,
                family_income = annual_family_income,
                gender = as_factor(gender)) %>% 
            select(seqn, age, race, pregnant, household_income, family_income, gender)

tri_clean <- mutate(tri_df,
                seqn = respondent_sequence_number,
                ldl_mmol = ldl_cholesterol_mmol_l,
                ldl_mgdl = ldl_cholesterol_mg_d_l,
                tri_mmol = triglyceride_mmol_l,
                tri_mgdl = triglyceride_mg_d_l) %>% 
            select(seqn, ldl_mmol, ldl_mgdl, tri_mmol, tri_mgdl)

head(demo_clean)
```

    ## # A tibble: 6 x 7
    ##    seqn   age race  pregnant household_income family_income gender
    ##   <int> <int> <fct> <fct>               <int>         <int> <fct> 
    ## 1 62161    22 3     <NA>                   14            14 1     
    ## 2 62162     3 1     <NA>                    4             4 2     
    ## 3 62163    14 6     <NA>                   15            15 1     
    ## 4 62164    44 3     2                       8             8 2     
    ## 5 62165    14 4     <NA>                    4             4 2     
    ## 6 62166     9 3     <NA>                   77            77 1

``` r
head(tri_clean)
```

    ## # A tibble: 6 x 5
    ##    seqn ldl_mmol ldl_mgdl tri_mmol tri_mgdl
    ##   <int>    <dbl>    <int>    <dbl>    <int>
    ## 1 62161     2.84      110    0.948       84
    ## 2 62164     3.90      151    0.632       56
    ## 3 62165     2.17       84    0.802       71
    ## 4 62169     1.89       73    0.881       78
    ## 5 62170     1.99       77    0.497       44
    ## 6 62171     1.76       68    0.429       38

1.  Write a query that would allow you to fill out table 1 and assign
    the results to an object called table1.

<!-- end list -->

``` r
table1 <- sqldf(
            "SELECT race, COUNT(race) AS freq, ROUND(AVG(age), 1) AS mean_age
             FROM demo_clean
             GROUP BY race")
table1
```

    ##   race freq mean_age
    ## 1    1 1355     22.5
    ## 2    2 1076     30.4
    ## 3    3 2973     37.9
    ## 4    4 2683     30.5
    ## 5    6 1282     31.5
    ## 6    7  387     21.8

2.  Show the distribution of race by gender and display the race/gender
    combinations from highest to lowest frequency. Note: when using SQL
    in R, you *can* refer to column aliases outside of the SELECT
    clause.

<!-- end list -->

``` r
table2 <- sqldf(
            "SELECT gender, race, COUNT(race) AS race_counts
             FROM demo_clean
             GROUP BY gender, race
             ORDER BY race_counts DESC")
table2
```

    ##    gender race race_counts
    ## 1       1    3        1508
    ## 2       2    3        1465
    ## 3       2    4        1372
    ## 4       1    4        1311
    ## 5       1    1         694
    ## 6       2    1         661
    ## 7       2    6         648
    ## 8       1    6         634
    ## 9       2    2         557
    ## 10      1    2         519
    ## 11      2    7         197
    ## 12      1    7         190

3.  Count the number of women who were pregnant at the time of
    screening. Use the column alias preg\_at\_screen.

<!-- end list -->

``` r
table3 <- sqldf(
            "SELECT COUNT(pregnant) AS preg_at_screen
             FROM demo_clean
             GROUP BY gender
             HAVING gender = 1")
table3
```

    ##   preg_at_screen
    ## 1              0

4.  How many men refused to provide annual household income?

<!-- end list -->

``` r
table4 <- sqldf(
            "SELECT COUNT(gender) AS male_count
             FROM demo_clean
             GROUP BY household_income
             HAVING household_income = 77")
table4
```

    ##   male_count
    ## 1        252

5.  What is the mean LDL level (mg/dL) for men and women? Use column
    alias mean\_ldl and round results to 1 decimal place.

<!-- end list -->

``` r
table5 <- sqldf(
            "SELECT gender, ROUND(AVG(ldl_mgdl), 1) AS mean_ldl
             FROM demo_clean AS d
             LEFT JOIN tri_clean AS t
             ON d.seqn = t.seqn
             GROUP BY gender")
table5
```

    ##   gender mean_ldl
    ## 1      1    107.8
    ## 2      2    111.1

6.  Display the minimum and maximum triglyceride levels (mmol/L) for
    each race. Use column aliases min\_tri and max\_tri.

<!-- end list -->

``` r
table6 <- sqldf(
            "SELECT race, MIN(tri_mmol) AS min_tri, MAX(tri_mmol) AS max_tri
             FROM demo_clean AS d
             LEFT JOIN tri_clean AS t
             ON d.seqn = t.seqn
             GROUP BY race
             HAVING tri_mmol IS NOT NULL OR tri_mgdl IS NOT NULL")
table6
```

    ##   race min_tri max_tri
    ## 1    1   0.339  11.053
    ## 2    2   0.339   7.768
    ## 3    3   0.361  17.635
    ## 4    4   0.203   6.040
    ## 5    6   0.237   6.593
    ## 6    7   0.294   4.301

7.  Create a new data frame that can be used for future analyses that
    combines all demographic data and any matching triglyceride data.
    Call it demo\_tri.

<!-- end list -->

``` r
demo_tri <- sqldf(
        "SELECT d.*, t.tri_mmol, t.tri_mgdl
         FROM demo_clean AS d
         LEFT JOIN tri_clean AS t
         ON d.seqn = t.seqn
         WHERE tri_mmol IS NOT NULL OR tri_mgdl IS NOT NULL") %>% 
    as_tibble()
demo_tri
```

    ## # A tibble: 2,990 x 9
    ##     seqn   age race  pregnant household_income family_income gender tri_mmol
    ##    <int> <int> <fct> <chr>               <int>         <int> <fct>     <dbl>
    ##  1 62161    22 3     <NA>                   14            14 1         0.948
    ##  2 62164    44 3     2                       8             8 2         0.632
    ##  3 62165    14 4     <NA>                    4             4 2         0.802
    ##  4 62169    21 6     <NA>                    2             2 1         0.881
    ##  5 62170    15 7     <NA>                   15            15 1         0.497
    ##  6 62171    14 1     <NA>                    9             9 1         0.429
    ##  7 62172    43 4     2                       5             5 2         1.59 
    ##  8 62177    51 6     <NA>                   99            77 1         1.42 
    ##  9 62178    80 3     <NA>                    1             1 1         1.13 
    ## 10 62184    26 4     <NA>                   15            15 1         0.452
    ##    tri_mgdl
    ##       <int>
    ##  1       84
    ##  2       56
    ##  3       71
    ##  4       78
    ##  5       44
    ##  6       38
    ##  7      141
    ##  8      126
    ##  9      100
    ## 10       40
    ## # … with 2,980 more rows
