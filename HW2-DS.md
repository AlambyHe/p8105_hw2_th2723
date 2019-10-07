p8105\_hw2\_th2723
================
Tianhui He

Initial setup:

### problem 1

Basic tidy work:

``` r
Mr.TW = 
  readxl::read_excel("Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = 1, range = "A2:N408") %>%
  janitor::clean_names() %>%
  drop_na(dumpster)%>%
  mutate(
    sports_balls = as.integer(sports_balls)
  )
```

Read and clean data for 2017 and 2018: 2018:

``` r
Mr.TW_years18 = 
  readxl::read_excel("Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = 5, range = "A2:B14") %>%
  janitor::clean_names()%>%
  drop_na(total)%>%
   mutate(year = "2018"
  )
```

2017:

``` r
Mr.TW_years17 = 
  readxl::read_excel("Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = 6, range = "A2:B14") %>%
  janitor::clean_names()%>%
  drop_na(total)%>%
   mutate(year = "2017"
  )
```

Now combine 2017 and 2018, and convert month to a character variable:

``` r
Mr.TW_years = rbind(Mr.TW_years17,Mr.TW_years18)
Mr.TW_years %>%
mutate(
       month = month.name[as.numeric(month)]
 )
```

    ## # A tibble: 24 x 3
    ##    month     total year 
    ##    <chr>     <dbl> <chr>
    ##  1 January    2.34 2017 
    ##  2 February   1.46 2017 
    ##  3 March      3.57 2017 
    ##  4 April      3.99 2017 
    ##  5 May        5.64 2017 
    ##  6 June       1.4  2017 
    ##  7 July       7.09 2017 
    ##  8 August     4.44 2017 
    ##  9 September  1.95 2017 
    ## 10 October    0    2017 
    ## # … with 14 more rows

``` r
select(Mr.TW_years, year, month, total)
```

    ## # A tibble: 24 x 3
    ##    year  month total
    ##    <chr> <dbl> <dbl>
    ##  1 2017      1  2.34
    ##  2 2017      2  1.46
    ##  3 2017      3  3.57
    ##  4 2017      4  3.99
    ##  5 2017      5  5.64
    ##  6 2017      6  1.4 
    ##  7 2017      7  7.09
    ##  8 2017      8  4.44
    ##  9 2017      9  1.95
    ## 10 2017     10  0   
    ## # … with 14 more rows

~Total precipitation in 2018 is 70.33 ~The number of observation in 2017 is 12 and the number of observation is als 12. ~The median number of ports balls in a dumpster in 2017 is 8 ~some key variables includes year, month and total

### problem 2

``` r
pols = 
  read_csv("fivethirtyeight_datasets/pols-month.csv")%>%
  janitor::clean_names()%>%
  separate(col= mon, sep="-", into = c("year", "month", "day"))%>%
  mutate(
         president = ifelse(prez_gop !=0, "gop","dem"),
         month = month.name[as.numeric(month)],
         year = as.integer(year),
         day = as.integer(day)
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   mon = col_date(format = ""),
    ##   prez_gop = col_integer(),
    ##   gov_gop = col_integer(),
    ##   sen_gop = col_integer(),
    ##   rep_gop = col_integer(),
    ##   prez_dem = col_integer(),
    ##   gov_dem = col_integer(),
    ##   sen_dem = col_integer(),
    ##   rep_dem = col_integer()
    ## )

``` r
pols_tidy_data = select(pols, year, month, everything(), -prez_dem, -prez_gop, -day)
```

snp data:

``` r
snp = 
  read_csv("fivethirtyeight_datasets/snp.csv") %>%
  janitor::clean_names()%>%
  separate(col= date, sep="/", into = c("day", "month", "year")) %>%
  mutate(
     month = month.name[as.numeric(month)],
     year = as.integer(year),
     day = as.integer(day)
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   close = col_double()
    ## )

``` r
snp_tidy_data = select(snp, year, month, close, -day)
```

unemployment data:

``` r
Um = 
  read_csv("fivethirtyeight_datasets/unemployment.csv")%>%
  janitor::clean_names() %>%
  rename(January = "jan", Feburary = "feb", March = "mar", April = "apr", May = "may", June = "jun", July="jul", August = "aug", September = "sep", October = "oct", November = "nov", December ="dec" )
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_integer(),
    ##   Jan = col_double(),
    ##   Feb = col_double(),
    ##   Mar = col_double(),
    ##   Apr = col_double(),
    ##   May = col_double(),
    ##   Jun = col_double(),
    ##   Jul = col_double(),
    ##   Aug = col_double(),
    ##   Sep = col_double(),
    ##   Oct = col_double(),
    ##   Nov = col_double(),
    ##   Dec = col_double()
    ## )

``` r
Um_tidy_data = 
    pivot_longer(
        Um, 
        January:December,
        names_to = "month", 
        values_to = "Unemployment Rate") %>%
  mutate(
    year = as.integer(year)
  )
```

Merge datasets: merge snp into pols:

``` r
p_s = left_join(pols_tidy_data,snp_tidy_data,by = c("year", "month"))
```

merge unemployment into p\_s:

``` r
all = left_join(p_s, Um_tidy_data,by = c("year", "month"))
```

In this question, I cleared data for pols, snp and unemployment rate. For pols, its clean data has 822 observation and 9 variables. The key variable "year" ranges from 1947 to 2015. Some key variables include president, a new variable we create for this homework. The untidy data use 1 or 0 to predict republican-dominance and democrat-dominance. However, I changed it to "dem" to indicate its dominance. For snp, it has 787 observations and 3 variables. The key varaibles "year" ranges from 1950 to 2015. I found that "close" variable has an increasing trending over the years which indicates a general economic growth. For unemployment rate, it has 816 observations and 3 variables. They key variable "year" ranges from 1948 to 2015. I discovered that the unemployment rate has a trend of decreasing since the Great recession, which means that the economy in United States is recovering. \#\#\# problem 3 Basic data cleaning:

``` r
Names = 
  read_csv("Popular_Baby_Names.csv")%>%
  janitor::clean_names()%>%
  distinct()%>%
  mutate(
  ethnicity = recode(ethnicity, 'ASIAN AND PACI' = "ASIAN AND PACIFIC ISLANDER", 'WHITE NON HISP'= "WHITE NON HISPANIC", 'BLACK NON HISP' = "BLACK NON HISPANIC"  )  #Use unique(pull(Names,ethnicity)) to figure out how many expressions of the same ethnicity
  ) %>%
  mutate(
    childs_first_name = str_to_title(childs_first_name)
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   `Year of Birth` = col_integer(),
    ##   Gender = col_character(),
    ##   Ethnicity = col_character(),
    ##   `Child's First Name` = col_character(),
    ##   Count = col_integer(),
    ##   Rank = col_integer()
    ## )

rank in popularity of the name "Olivia"

``` r
Olivia = 
  filter(Names, childs_first_name == "Olivia") %>%
  select(year_of_birth, ethnicity,rank) %>%
  pivot_wider(
    names_from = year_of_birth,
    values_from = rank
  )
Olivia
```

    ## # A tibble: 4 x 7
    ##   ethnicity                  `2016` `2015` `2014` `2013` `2012` `2011`
    ##   <chr>                       <int>  <int>  <int>  <int>  <int>  <int>
    ## 1 ASIAN AND PACIFIC ISLANDER      1      1      1      3      3      4
    ## 2 BLACK NON HISPANIC              8      4      8      6      8     10
    ## 3 HISPANIC                       13     16     16     22     22     18
    ## 4 WHITE NON HISPANIC              1      1      1      1      4      2

Most popular name among male children:

``` r
Male = 
  filter(Names, gender == "MALE", rank == "1") %>%
  select(year_of_birth,ethnicity,childs_first_name)%>%
  pivot_wider(
    names_from = year_of_birth,
    values_from = childs_first_name
  )
```

scatter plot:

``` r
Names %>% 
  filter(gender == "MALE") %>%
  filter(ethnicity == "WHITE NON HISPANIC")%>%
  filter(year_of_birth == "2016")%>% 
  ggplot(aes(x = rank, y = count)) + 
  geom_point(aes(), alpha = .5) +
labs(
    title = "Names for White Non-hispanic Children in 2016",
    x = "rank of name",
    y = "count of name"
)
```

![](HW2-DS_files/figure-markdown_github/unnamed-chunk-13-1.png)
