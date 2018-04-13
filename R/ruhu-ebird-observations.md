
Where are the Rufus Hummingbirds?
=================================

<img src = "../images/vnhs_ruhu_tweet.png" width = "500"></img>

When I saw the above analysis by my friend [Dave Fraser](https://twitter.com/DavidFFraser), I thought 💭:

-   cool analysis!
-   citizen science + [eBird](https://ebird.org/science/download-ebird-data-products) is awesome!
-   have I seen a Rufus Hummingbird lately? 🐦

*and then I thought ...*

-   maybe I could do that in R? 💡

And then I did.

Getting the [eBird](https://ebird.org/science/download-ebird-data-products) Data
--------------------------------------------------------------------------------

You can get up-to-date eBird data—once you have created an accout and logged in—via the [eBird website GUI](https://ebird.org/explore), but I was hoping to include more years and didn't want to click that often. The [full eBird data set is also available for download](https://ebird.org/science/download-ebird-data-products), but it is updated quarterly—so is unlikely to have the very recent records I am after. Sigh 😞.

And then I talked to [Andy Teucher](https://github.com/ateucher) about my data woes. He introduced me to the [rOpenSci](https://ropensci.org/) [R](https://www.r-project.org/) package 📦 [`rebird`](https://cran.r-project.org/web/packages/rebird/index.html)—he is a coauthor! 👍

The [`rebird`](https://cran.r-project.org/web/packages/rebird/index.html) package 📦 gets the eBird data from the web and provides it in a tidy format. 🎁

So let's start by pulling *A LOT* of eBird data—50 years seems like a lot?

``` r
#load R packages we will need 
# library(rebird) #get eBird data from the web using ebirdfreq()
# library(dplyr) #data munging
# library(purrr) #map() for looping
# 
# #function to get eBird data for many states/provinces using rebird R package
# map_state <-  function(state) {
#   map_dfr(1968:2018, ~ {
#     ebirdfreq("states", state, .x, .x, 1, 5) %>%
#       filter(comName == "Rufous Hummingbird") %>%
#       mutate(year = .x, state = state)
#   })
# }
# 
# #get eBird data for west coast state/provinces
# ruhu_raw <- map_dfr(c("CA-BC","US-CA","US-WA", "US-OR"), ~ {
#       map_state(.x)
# })
# 
# #save data object in tmp folder
# if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
# save(ruhu_raw, file = "tmp/ruhu_raw.RData")
```

``` r
# temp chunk to load saved raw data file
if (!exists("ruhu_raw")) load("tmp/ruhu_raw.RData")
```

Some Data Cleaning
------------------

``` r
head(ruhu_raw)
```

    ## # A tibble: 6 x 6
    ##   comName            monthQt    frequency sampleSize  year state
    ##   <chr>              <chr>          <dbl>      <dbl> <int> <chr>
    ## 1 Rufous Hummingbird January-1         0.        14.  1968 CA-BC
    ## 2 Rufous Hummingbird January-2         0.         9.  1968 CA-BC
    ## 3 Rufous Hummingbird January-3         0.        11.  1968 CA-BC
    ## 4 Rufous Hummingbird January-4         0.        14.  1968 CA-BC
    ## 5 Rufous Hummingbird February-1        0.        15.  1968 CA-BC
    ## 6 Rufous Hummingbird February-2        0.        14.  1968 CA-BC

The eBird data provided by [`rebird`](https://cran.r-project.org/web/packages/rebird/index.html) is indeed very tidy, perfect! 🎁 Let's take a moment to cheers 🎉 the authors of this open source R package. OK, now let's keep going...

We need to do something with the `monthQt` variable. This is our time variable, which we need for plotting, and it needs to be a recognized as a date. And you might notice some zeros in the 2018 weeks that we have not yet lived—or birded. Let's filter those zeros, that should be `NA`s, out of the data frame.

``` r
#library(dplyr) #data munging & already loaded
library(stringr) #clean up variables
library(lubridate) #make date data dates
library(tidyr) #separate month and quarter

ruhu_clean <- ruhu_raw %>% 
  separate(monthQt, c("month", "week"), sep = "-") %>%
   mutate(day = case_when(
    week == 1 ~ 1,
    week == 2 ~ 8,
    week == 3 ~ 15,
    week == 4 ~ 23
  )) %>%
  mutate(date = ymd(paste0(year, "-",month,"-",day))) %>% 
  mutate(week = lubridate::week(date)) %>% 
  filter(sampleSize != 0) #filter out any zero that is actually an NA

head(ruhu_clean)
```

    ## # A tibble: 6 x 9
    ##   comName    month  week frequency sampleSize  year state   day date      
    ##   <chr>      <chr> <dbl>     <dbl>      <dbl> <int> <chr> <dbl> <date>    
    ## 1 Rufous Hu… Janu…    1.        0.        14.  1968 CA-BC    1. 1968-01-01
    ## 2 Rufous Hu… Janu…    2.        0.         9.  1968 CA-BC    8. 1968-01-08
    ## 3 Rufous Hu… Janu…    3.        0.        11.  1968 CA-BC   15. 1968-01-15
    ## 4 Rufous Hu… Janu…    4.        0.        14.  1968 CA-BC   23. 1968-01-23
    ## 5 Rufous Hu… Febr…    5.        0.        15.  1968 CA-BC    1. 1968-02-01
    ## 6 Rufous Hu… Febr…    6.        0.        14.  1968 CA-BC    8. 1968-02-08

That looks 👀 better!

Some Data Visualizing
---------------------

Now let's have a quick, exploratory look 👀 at what data we have, starting just with British Columbia.

``` r
library(ggplot2) #plotting

ruhu_clean %>% 
  filter(state == "CA-BC") %>%  #just B.C.
ggplot(aes(x = week, y = frequency, group = year)) +
  geom_line(colour = "grey") +
  theme_minimal()
```

<img src="ruhu-ebird-observations_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

There seems to be A LOT of very high frequency values in the data set (frequency = 1.0), which is a bit puzzling? Let's colour the lines for each year to see if we learn something.

``` r
ruhu_clean %>% 
  filter(state == "CA-BC") %>%  #just B.C.
ggplot(aes(x = week, y = frequency, group = year)) +
  geom_line(aes(colour = year)) +
  theme_minimal()
```

<img src="ruhu-ebird-observations_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

Hmmmmm.. 👀 This shows that most of that puzzling data is OLDER, pre-2000s. Andy—a birder and frequent data provider to eBird—mentioned that the older records are being entered by birders going through old checklists & field notebooks. Perhaps this historical data is not as complete as data since 2002— the year eBird launched and birders started entering sightings via smartphones? Filtering out the pre-2002 frequency data seems like a reasonable approach to me.

``` r
ruhu_since_2002 <- ruhu_clean %>% 
  filter(year > 2001) 

ruhu_since_2002 %>% 
  filter(state == "CA-BC") %>%  #just B.C.
ggplot(aes(x = week, y = frequency, group = year)) +
  geom_line(aes(colour = year)) +
  theme_minimal()
```

<img src="ruhu-ebird-observations_files/figure-markdown_github/filter-1.png" style="display: block; margin: auto;" />

Looks 👀 better!

How Does 2018 🐦 Observation Frequencies Compare?
------------------------------------------------

We want to compare 2018 observation frequency against previous years.
