
Where are the Rufus Hummingbirds?
=================================

<img src = "../images/vnhs_ruhu_tweet.png" width = "500"></img>

When I saw the above analysis by my friend [Dave Fraser](https://twitter.com/DavidFFraser), I thought 💭:

-   cool analysis!
-   citizen science + [eBird](https://ebird.org/science/download-ebird-data-products) is awesome!
-   have I seen a Rufus Hummingbirds lately? 🐦

*and then I thought*

-   maybe I could do that in R? 💡

And then I did.

Getting the [eBird](https://ebird.org/science/download-ebird-data-products) data
--------------------------------------------------------------------------------

After reading up on options for getting eBird data I almost stopped. You can get up-to-date stuff via the web GUI, but I was hoping to include more years and didn't want to click that often. The full data object is available for download, but is updated quarterly, so unlikely to have the very records I am after. Sigh 😞.

And then I told Andy my 💡. He introduced me to the [rOpenSci](https://ropensci.org/) [R](https://www.r-project.org/) 📦 [`rebird`](https://cran.r-project.org/web/packages/rebird/index.html)—he is a coauthor! 👍

``` r
library(rebird) #get eBird data from the web
library(dplyr) #data munging
library(purrr) #map() for looping
library(tidyr) #separate month & quarter
library(stringr) #clean variable names
library(lubridate) #make date variable a date
library(ggplot2) #plotting
library(rphylopic) #for image, package from GitHub
library(curl) #required by rphylopic function
```
