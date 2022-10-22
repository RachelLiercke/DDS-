Rachel and Santi Case Study
================
Rachel Liercke & Santiago Gutierrez
2022-10-21

# Introduction

We have been asked by Budweiser to run an analysis on their beers and
breweries data that they have collected and answer a few questions.
\#Below we have completed our Exploratory Data Analysis over the
datasets provided. \#We will discuss the amount of breweries per state,
median values of ABV and IBU, missing values, and predictions of type of
beer. \#If there are any questions, please reach ask or reach out to our
emails.

# Create a heatmap to show how many breweries are in each state. Then print the head and tail of the combined breweries and beers dataset.

``` r
#Install and add to library all of the packages below to be used in the 
# analysis later.
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(stringi)
library(ggplot2)
library(twitteR)
```

    ## 
    ## Attaching package: 'twitteR'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     id, location

``` r
library(httr)
library(jsonlite)
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ tibble  3.1.8     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ✔ purrr   0.3.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter()     masks stats::filter()
    ## ✖ purrr::flatten()    masks jsonlite::flatten()
    ## ✖ twitteR::id()       masks dplyr::id()
    ## ✖ dplyr::lag()        masks stats::lag()
    ## ✖ twitteR::location() masks dplyr::location()

``` r
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(mvtnorm)
library(class)
library(caret)
```

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift
    ## 
    ## The following object is masked from 'package:httr':
    ## 
    ##     progress

``` r
library(e1071)

#Import Breweries.csv
#Create table for states and state abbreviations from MAPS package to create a dataset to match our states

Breweries <- read_csv("C:/Users/rache/Documents/DDS/Unit 8&9/Breweries.csv")
```

    ## Rows: 558 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Name, City, State
    ## dbl (1): Brew_ID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
lookup = data.frame(abb=state.abb, State = state.name)
#Pull only the State and City information out of the dataset to create less clutter.
#Change the name of State to "abb" to create a foreign key.
#

brews = Breweries[,c(3:4)]
colnames(brews)[2] = "abb"


#Merge the lookup table and our state tables to create a master copy. This will
#allow us to create our heatmap later.
#Change name to State to be able to count the total number of breweries per State.

brews2 = merge(brews,lookup,"abb")

#Use the count function to create a sum total of number of breweries per State.
#Print the overall dataset brewery map to see if it added correctly.
brewerymaps = count(brews2,State)
brewerymaps
```

    ##             State  n
    ## 1         Alabama  3
    ## 2          Alaska  7
    ## 3         Arizona 11
    ## 4        Arkansas  2
    ## 5      California 39
    ## 6        Colorado 47
    ## 7     Connecticut  8
    ## 8        Delaware  2
    ## 9         Florida 15
    ## 10        Georgia  7
    ## 11         Hawaii  4
    ## 12          Idaho  5
    ## 13       Illinois 18
    ## 14        Indiana 22
    ## 15           Iowa  5
    ## 16         Kansas  3
    ## 17       Kentucky  4
    ## 18      Louisiana  5
    ## 19          Maine  9
    ## 20       Maryland  7
    ## 21  Massachusetts 23
    ## 22       Michigan 32
    ## 23      Minnesota 12
    ## 24    Mississippi  2
    ## 25       Missouri  9
    ## 26        Montana  9
    ## 27       Nebraska  5
    ## 28         Nevada  2
    ## 29  New Hampshire  3
    ## 30     New Jersey  3
    ## 31     New Mexico  4
    ## 32       New York 16
    ## 33 North Carolina 19
    ## 34   North Dakota  1
    ## 35           Ohio 15
    ## 36       Oklahoma  6
    ## 37         Oregon 29
    ## 38   Pennsylvania 25
    ## 39   Rhode Island  5
    ## 40 South Carolina  4
    ## 41   South Dakota  1
    ## 42      Tennessee  3
    ## 43          Texas 28
    ## 44           Utah  4
    ## 45        Vermont 10
    ## 46       Virginia 16
    ## 47     Washington 23
    ## 48  West Virginia  1
    ## 49      Wisconsin 20
    ## 50        Wyoming  4

``` r
#Change brewery to Breweries name
colnames(brewerymaps)[2] = "Breweries"

#Create a region column that is all lowercase to match all states
brewerymaps$region <-tolower(brewerymaps$State)


#Remove the first column so we only have state and Region
brewerymaps = brewerymaps[-1]

#Create a state map based on States.
states<- map_data("state")

#Merge dataframes brewerymaps and States in order to create 
# a heat map for breweries by state
map.df<- merge(states, brewerymaps, by = "region", all.x=T)
map.df<- map.df[order(map.df$order),]

p<-ggplot(map.df,aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=Breweries))+
  geom_path() + xlab("Latitude of State") + ylab("Longitude of State") +
  scale_fill_gradientn(colours = rev(heat.colors(10)), na.value ="grey90") +
  ggtitle("Breweries by State") + coord_map() 
p
```

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
#Import Beers dataset
#Change brewery name in Beers to Brew_ID to create a foreign key
#Then combine both beers and breweries csv files

Beers <- read_csv("C:/Users/rache/Documents/DDS/Unit 8&9/Beers.csv")
```

    ## Rows: 2410 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Name, Style
    ## dbl (5): Beer_ID, ABV, IBU, Brewery_id, Ounces
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
colnames(Beers)[5] = "Brew_ID"
combined <- merge(Beers,Breweries, by ="Brew_ID", all = TRUE)
colnames(combined)[2] = "Beer_Name"
colnames(combined)[8] = "Brewery_Name"
```

``` r
#Print the first 6 and last 6 rows.

head(combined)
```

    ##   Brew_ID     Beer_Name Beer_ID   ABV IBU                               Style
    ## 1       1  Get Together    2692 0.045  50                        American IPA
    ## 2       1 Maggie's Leap    2691 0.049  26                  Milk / Sweet Stout
    ## 3       1    Wall's End    2690 0.048  19                   English Brown Ale
    ## 4       1       Pumpion    2689 0.060  38                         Pumpkin Ale
    ## 5       1    Stronghold    2688 0.060  25                     American Porter
    ## 6       1   Parapet ESB    2687 0.056  47 Extra Special / Strong Bitter (ESB)
    ##   Ounces      Brewery_Name        City State
    ## 1     16 NorthGate Brewing Minneapolis    MN
    ## 2     16 NorthGate Brewing Minneapolis    MN
    ## 3     16 NorthGate Brewing Minneapolis    MN
    ## 4     16 NorthGate Brewing Minneapolis    MN
    ## 5     16 NorthGate Brewing Minneapolis    MN
    ## 6     16 NorthGate Brewing Minneapolis    MN

``` r
tail(combined)
```

    ##      Brew_ID                 Beer_Name Beer_ID   ABV IBU
    ## 2405     556             Pilsner Ukiah      98 0.055  NA
    ## 2406     557  Heinnieweisse Weissebier      52 0.049  NA
    ## 2407     557           Snapperhead IPA      51 0.068  NA
    ## 2408     557         Moo Thunder Stout      50 0.049  NA
    ## 2409     557         Porkslap Pale Ale      49 0.043  NA
    ## 2410     558 Urban Wilderness Pale Ale      30 0.049  NA
    ##                        Style Ounces                  Brewery_Name          City
    ## 2405         German Pilsener     12         Ukiah Brewing Company         Ukiah
    ## 2406              Hefeweizen     12       Butternuts Beer and Ale Garrattsville
    ## 2407            American IPA     12       Butternuts Beer and Ale Garrattsville
    ## 2408      Milk / Sweet Stout     12       Butternuts Beer and Ale Garrattsville
    ## 2409 American Pale Ale (APA)     12       Butternuts Beer and Ale Garrattsville
    ## 2410        English Pale Ale     12 Sleeping Lady Brewing Company     Anchorage
    ##      State
    ## 2405    CA
    ## 2406    NY
    ## 2407    NY
    ## 2408    NY
    ## 2409    NY
    ## 2410    AK

\#How many NA values in ABV and IBU? \#There are 62 missing NA values
for ABV and 1005 missing values \#for IBU. The ABV values won’t have a
huge effect on the data but \#almost half (42%) of IBU is missing.
Removing these will be neccessary \#but will also make our data less
accurate.

``` r
#Sum the amount of NAs in the ABV and IBU columns.
sum(is.na(combined$ABV))
```

    ## [1] 62

``` r
sum(is.na(combined$IBU))
```

    ## [1] 1005

\#Compute the median ABV by state and plot a bar chart to comapare \# We
removed all NA values from each column to do this analysis. Kentucky has
the highest median ABV, followed by DC, Kentucky, West Viriginia and New
Mexico. There weren’t many more states that were above the average of
ABV. The median IBU has Maine with the highest one. There are a lot more
states that are above the average of the IBU than ABV.

``` r
#Filter the data for non-NA ABV values. Group by state and create
#a median for every state. Then graph these medians together without a legend.

combined <- combined[complete.cases(combined$ABV),]
combinedABV <- combined[order(combined$ABV),]


combined <- combined[complete.cases(combined$IBU),]
combinedIBU <- combined[order(combined$IBU),]

median_ABV_plot = combinedABV %>%
  filter(!is.na(ABV)) %>%
  group_by(State) %>%
  summarize(median=median(ABV),count=n())%>%
  ggplot(aes(x=State, y = median, fill = factor(if_else(State == "KY","Highest", "All Others")))) + 
  geom_bar(stat="identity", show.legend = FALSE) + 
  scale_fill_manual(name = "State", values=c("grey50","red")) + 
  ggtitle("Median Alcohol by Volume of Beer by State") + ylab("Median ABV") + 
  geom_hline(yintercept = mean(combinedABV$ABV, na.rm = TRUE), color="blue")
  median_ABV_plot
```

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#Filter the data for non-NA IBU values. Group by state and create
#a median for every state. Then graph these medians together without a legend.
median_IBU_plot = combined %>%
    filter(!is.na(IBU)) %>%
    group_by(State) %>%
    summarize(median=median(IBU),count=n())%>%
    ggplot(aes(x=State, y = median, fill = factor(if_else(State == "ME","Highest", "All Others")))) + 
    geom_bar(stat="identity", show.legend = FALSE) + scale_fill_manual(name = "State", values=c("grey50","red")) + 
    ggtitle("Median International Bitterness Units of Beer by State") +
    ylab("Median IBU") + 
    geom_hline(yintercept = mean(combined$IBU, na.rm = TRUE), color="blue")
  median_IBU_plot
```

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

\#Which state has the beer with the maximum ABV? Colorado at 12.8%
\#Which state has the most bitter beer (IBU)? Oregon with 138 IBU

``` r
# Remove any NAs in the ABV column with complete cases, order ABV by descending and return the bottom 6 rows to see the highest ABV

combined <- combined[complete.cases(combined$ABV),]
combinedABV <- combined[order(combined$ABV),]
tail(combinedABV)
```

    ##      Brew_ID                       Beer_Name Beer_ID   ABV IBU
    ## 2039     424    Ex Umbris Rye Imperial Stout     904 0.099  85
    ## 2108     445 Forest Fire Imperial Smoked Rye    1256 0.099  85
    ## 2275     504  Ten Fidy Imperial Stout (2007)     803 0.099  98
    ## 336       47                          4Beans    2574 0.100  52
    ## 144       18                            Csar    2621 0.120  90
    ## 8          2                  London Balling    2685 0.125  80
    ##                                 Style Ounces              Brewery_Name
    ## 2039 American Double / Imperial Stout     16      Hess Brewing Company
    ## 2108                         Rye Beer     16          Big Wood Brewery
    ## 2275           Russian Imperial Stout     12       Oskar Blues Brewery
    ## 336                     Baltic Porter     12       Sixpoint Craft Ales
    ## 144            Russian Imperial Stout     16   Tin Man Brewing Company
    ## 8                  English Barleywine     16 Against the Grain Brewery
    ##                 City State
    ## 2039       San Diego    CA
    ## 2108 Vadnais Heights    MN
    ## 2275           Lyons    CO
    ## 336         Brooklyn    NY
    ## 144       Evansville    IN
    ## 8         Louisville    KY

``` r
#Remove any NAS in the IBU column, order IBU by descending and return the bottom 6 rows to see the highest IBU

combined <- combined[complete.cases(combined$IBU),]
combinedIBU <- combined[order(combined$IBU),]
tail(combinedIBU)
```

    ##      Brew_ID                       Beer_Name Beer_ID   ABV IBU
    ## 1452     273                    Heady Topper    1111 0.080 120
    ## 1453     273                    Heady Topper     379 0.080 120
    ## 625      100 Bay of Bengal Double IPA (2014)    2440 0.089 126
    ## 1305     231                   Dead-Eye DIPA    2067 0.090 130
    ## 1719     345              Troopers Alley IPA    1676 0.059 135
    ## 1857     375       Bitter Bitch Imperial IPA     980 0.082 138
    ##                               Style Ounces                       Brewery_Name
    ## 1452 American Double / Imperial IPA     16                      The Alchemist
    ## 1453 American Double / Imperial IPA     16                      The Alchemist
    ## 625  American Double / Imperial IPA     12 Christian Moerlein Brewing Company
    ## 1305 American Double / Imperial IPA     16           Cape Ann Brewing Company
    ## 1719                   American IPA     12         Wolf Hills Brewing Company
    ## 1857 American Double / Imperial IPA     12            Astoria Brewing Company
    ##            City State
    ## 1452  Waterbury    VT
    ## 1453  Waterbury    VT
    ## 625  Cincinnati    OH
    ## 1305 Gloucester    MA
    ## 1719   Abingdon    VA
    ## 1857    Astoria    OR

\#There are 62 missing values or 2.5% of the data data missing ABV
values. \#The ABV has a median of 5.6% and a mean of 5.9% which is
pretty average for beer. The beer with the highest was 12.8% in Colorado
and the lowest ABV was at 0.1% (which is extremely low). It is
interesting that Colorado had the highest ABV because altitude makes you
feel the effects of beer more. The data is right skewed because most of
the values are centered around 5% but we have a lot of higher
perecentages and not a lot of lower ones.

``` r
#Make histogram of ABV values to show distribution
combined %>% ggplot(aes(x= ABV)) + geom_histogram(color = "Black") +
  ggtitle("Distribution of Alcohol by Volume in Beers")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

\#Is there a relationship between ABV and IBU? \#Yes, there is a
correlation between these values. There is a positive correlation shown
in the coefficient being 0.671. This is a moderate correlation. An
increase in IBU is associated with an increase (but not caused by) an
increase in ABV (and vice versa).

``` r
#Create a scatterplot with a fitted regession line to show the relationship between ABV and IBU
combined %>%filter(!is.na(IBU))%>%
  filter(!is.na(ABV))%>%
  ggplot(aes(x=ABV, y = IBU)) + geom_point() +geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

\#Can we predict if a beer is an IPA or an Ale by the ABV and IBU
values? \##Yes we can. \###The statistical evidence shows an 86%
accuracy when fitting a knn model. We use this model by taking a data
point in a training set of ABV and IBU values and looking at the 5
closest data points. If the majority of these 5 points are labeled as
IPA, then this one will be (and vice versa). The model does this for all
data points. We then use a \#### We can see using the multiple loops
that our k factor of 5 is the most accurate model to predict, which is
why we used k=5 to run our model.

``` r
#which k level is best?
## Create a test and train dataset based on the combined dataset with no missing values
### Loop for many k and one training / test partition


#Create a dataset with column called Style. If IPA is in name, label IPA
# If not an IPA but still an Ale, label Ale, if neither of these, label Other
#Then filter out others.

totalAle = combined %>%mutate(Style = if_else(str_detect(combined$Style, "IPA"), "IPA", if_else(str_detect(combined$Style, "Ale"), "Ale", "Other"))) %>%
  filter(!is.na(ABV)) %>%
  filter(!is.na(IBU)) %>%
  filter(Style == "IPA" | Style == "Ale") %>%
  select(ABV, IBU, State, Style)

splitPerc = .7
trainIndices = sample(1:dim(totalAle)[1],round(splitPerc*dim(totalAle)[1]))
train = totalAle[trainIndices,]
test = totalAle[-trainIndices,]
train
```

    ##       ABV IBU State Style
    ## 726 0.062  17    NC   Ale
    ## 140 0.083 100    MN   IPA
    ## 589 0.053  22    VA   Ale
    ## 724 0.051  31    NC   Ale
    ## 43  0.055  40    IN   Ale
    ## 87  0.052  35    CA   Ale
    ## 399 0.065  65    CO   Ale
    ## 856 0.046  20    WY   Ale
    ## 871 0.060  29    IA   Ale
    ## 662 0.058  39    ID   Ale
    ## 214 0.054  30    IL   Ale
    ## 830 0.062  65    VA   IPA
    ## 695 0.050  40    MT   Ale
    ## 123 0.045  47    CA   IPA
    ## 868 0.047  42    GA   Ale
    ## 886 0.056  21    IA   Ale
    ## 333 0.075  70    FL   IPA
    ## 39  0.055  64    IN   Ale
    ## 529 0.055  30    MA   Ale
    ## 205 0.052  18    CO   Ale
    ## 881 0.059  75    WA   IPA
    ## 593 0.068  90    MN   IPA
    ## 778 0.059  60    VA   Ale
    ## 368 0.067  71    WV   Ale
    ## 539 0.044  45    VA   Ale
    ## 457 0.056  55    WI   IPA
    ## 872 0.050  45    IA   Ale
    ## 239 0.052  24    RI   Ale
    ## 826 0.050  45    MO   Ale
    ## 784 0.085  86    OR   IPA
    ## 212 0.063  65    MO   IPA
    ## 60  0.054  23    IN   Ale
    ## 444 0.052  27    WA   Ale
    ## 793 0.062  68    CO   IPA
    ## 219 0.043  18    OH   Ale
    ## 358 0.061  94    OR   IPA
    ## 317 0.050  20    MS   Ale
    ## 569 0.080  80    IL   Ale
    ## 157 0.059  70    TX   IPA
    ## 555 0.052  50    TX   Ale
    ## 604 0.087  80    WI   IPA
    ## 27  0.050  40    MI   Ale
    ## 35  0.085 115    IN   IPA
    ## 647 0.042  35    ID   Ale
    ## 586 0.053  40    AK   Ale
    ## 773 0.070  80    OK   IPA
    ## 225 0.055  42    OH   Ale
    ## 809 0.088  85    CT   IPA
    ## 700 0.048  15    NE   Ale
    ## 416 0.058  15    CA   Ale
    ## 591 0.055  45    VT   IPA
    ## 389 0.042  26    IN   Ale
    ## 557 0.055  20    TX   Ale
    ## 413 0.056   4    CA   Ale
    ## 227 0.043  45    OH   Ale
    ## 503 0.064  66    ID   IPA
    ## 686 0.067  70    ND   IPA
    ## 745 0.053  48    IA   Ale
    ## 850 0.050  19    CA   Ale
    ## 473 0.075  53    OR   Ale
    ## 510 0.099 100    NJ   Ale
    ## 430 0.063  35    OK   Ale
    ## 678 0.046  45    VT   IPA
    ## 318 0.058  60    MS   Ale
    ## 649 0.048  35    ID   Ale
    ## 228 0.072  75    OH   IPA
    ## 869 0.095  19    IA   Ale
    ## 769 0.068  68    HI   IPA
    ## 566 0.060  60    AZ   IPA
    ## 666 0.055  20    MO   Ale
    ## 159 0.051  17    MD   Ale
    ## 522 0.065  80    DC   IPA
    ## 16  0.063  30    CA   Ale
    ## 188 0.039  20    OR   Ale
    ## 687 0.062  65    MT   IPA
    ## 525 0.050  26    WI   Ale
    ## 294 0.085 100    TX   IPA
    ## 496 0.060  40    TX   Ale
    ## 456 0.065  80    WI   Ale
    ## 620 0.071  83    CO   Ale
    ## 263 0.058  44    CA   Ale
    ## 105 0.050  20    KS   Ale
    ## 229 0.048  25    OH   Ale
    ## 827 0.045  30    MO   IPA
    ## 824 0.055  40    CO   Ale
    ## 241 0.060  70    AK   IPA
    ## 144 0.073  69    MN   IPA
    ## 99  0.065  70    IL   IPA
    ## 790 0.065  65    NC   Ale
    ## 536 0.048  10    MA   Ale
    ## 862 0.065  45    WA   Ale
    ## 768 0.060  24    HI   Ale
    ## 644 0.090  75    UT   IPA
    ## 842 0.073  50    FL   IPA
    ## 400 0.065  65    CO   Ale
    ## 640 0.053   7    MA   Ale
    ## 504 0.070  80    MT   IPA
    ## 426 0.068  21    VA   Ale
    ## 942 0.062  17    MI   Ale
    ## 854 0.058  46    AK   Ale
    ## 265 0.058  35    VA   Ale
    ## 431 0.068 100    OK   IPA
    ## 565 0.055  30    IN   Ale
    ## 429 0.053  25    OK   Ale
    ## 418 0.070  80    CA   IPA
    ## 549 0.059  60    CA   IPA
    ## 237 0.089 126    OH   IPA
    ## 141 0.045  18    MO   Ale
    ## 675 0.069  51    NC   IPA
    ## 900 0.050  15    AK   Ale
    ## 443 0.048  48    WA   IPA
    ## 860 0.050  15    AK   Ale
    ## 839 0.053  43    MN   Ale
    ## 602 0.045  44    KS   IPA
    ## 805 0.050  11    MI   Ale
    ## 274 0.072  40    TX   Ale
    ## 251 0.070  94    CO   IPA
    ## 659 0.063 100    ID   IPA
    ## 731 0.082 100    IL   IPA
    ## 572 0.055  40    TX   IPA
    ## 90  0.038  40    CA   IPA
    ## 126 0.070  70    VA   IPA
    ## 935 0.060  69    NC   IPA
    ## 419 0.058  15    CA   Ale
    ## 283 0.045   6    MI   Ale
    ## 487 0.067  75    CA   Ale
    ## 462 0.078  60    OR   Ale
    ## 766 0.060  35    OR   Ale
    ## 747 0.053  10    OK   Ale
    ## 381 0.060  70    PA   IPA
    ## 579 0.060  30    NM   Ale
    ## 151 0.082  68    KY   IPA
    ## 471 0.060  50    OR   Ale
    ## 194 0.060  60    OR   IPA
    ## 119 0.091 103    NY   IPA
    ## 433 0.053  25    OK   Ale
    ## 120 0.060  48    NY   Ale
    ## 138 0.049  22    IL   Ale
    ## 367 0.057  44    WV   Ale
    ## 175 0.060  34    MN   Ale
    ## 672 0.075  31    PA   Ale
    ## 799 0.053  16    FL   Ale
    ## 408 0.065  65    CO   Ale
    ## 211 0.085  69    RI   IPA
    ## 266 0.062  33    VA   Ale
    ## 470 0.069  69    OR   IPA
    ## 65  0.053  20    IN   Ale
    ## 614 0.052  40    CA   Ale
    ## 26  0.065  65    MI   IPA
    ## 729 0.082 100    IL   IPA
    ## 836 0.052  23    CO   Ale
    ## 575 0.055  55    VT   Ale
    ## 578 0.062  72    NM   IPA
    ## 570 0.050  22    IL   Ale
    ## 810 0.088  85    CT   IPA
    ## 752 0.070  70    CA   IPA
    ## 920 0.065  65    MT   IPA
    ## 499 0.063  23    TX   Ale
    ## 631 0.042  25    PA   Ale
    ## 603 0.050  16    KS   Ale
    ## 196 0.073  70    OR   Ale
    ## 684 0.050  32    ND   Ale
    ## 903 0.070 105    OR   IPA
    ## 526 0.099  43    MA   Ale
    ## 511 0.078  80    NJ   IPA
    ## 567 0.089  88    CA   IPA
    ## 926 0.056  36    CA   Ale
    ## 535 0.059  42    MA   IPA
    ## 440 0.052  21    MO   Ale
    ## 303 0.052   9    WI   Ale
    ## 139 0.047  55    MN   Ale
    ## 312 0.075  72    MS   IPA
    ## 855 0.050  40    OR   Ale
    ## 857 0.070  75    WY   IPA
    ## 899 0.074  12    CA   Ale
    ## 621 0.078  34    CO   Ale
    ## 475 0.072  75    HI   IPA
    ## 179 0.039  20    OR   Ale
    ## 40  0.054  37    IN   Ale
    ## 874 0.072  55    GA   Ale
    ## 623 0.040  37    MA   IPA
    ## 823 0.067  45    CO   IPA
    ## 328 0.070  60    FL   IPA
    ## 624 0.060  55    MA   IPA
    ## 314 0.058  60    MS   Ale
    ## 690 0.051  26    MT   Ale
    ## 86  0.062  80    MO   IPA
    ## 223 0.060  30    OH   Ale
    ## 132 0.044  44    MN   IPA
    ## 145 0.075  90    MN   IPA
    ## 276 0.099 100    TX   IPA
    ## 777 0.046  35    NE   Ale
    ## 308 0.054  32    WI   Ale
    ## 883 0.054  30    WA   Ale
    ## 348 0.067  70    OR   IPA
    ## 794 0.054  19    CO   Ale
    ## 919 0.065  11    MT   Ale
    ## 296 0.052  23    TX   Ale
    ## 680 0.057  46    NY   Ale
    ## 377 0.067  47    AZ   IPA
    ## 356 0.056  55    OR   Ale
    ## 200 0.052  18    CO   Ale
    ## 821 0.063  30    CA   Ale
    ## 446 0.062  70    WA   IPA
    ## 587 0.080 120    VT   IPA
    ## 592 0.056  28    NY   Ale
    ## 199 0.065  70    CO   IPA
    ## 527 0.090 130    MA   IPA
    ## 897 0.054  27    CA   Ale
    ## 844 0.059  18    FL   Ale
    ## 252 0.053  45    CO   Ale
    ## 476 0.072  75    HI   IPA
    ## 357 0.054  24    OR   Ale
    ## 721 0.045  21    PA   Ale
    ## 345 0.061  30    NC   Ale
    ## 540 0.052  26    VA   Ale
    ## 629 0.052  20    MA   Ale
    ## 560 0.051  31    MS   Ale
    ## 117 0.052  11    NY   Ale
    ## 162 0.047  19    NC   Ale
    ## 538 0.070  65    VA   IPA
    ## 908 0.080  35    CO   Ale
    ## 232 0.075  93    PA   Ale
    ## 286 0.050  35    MI   Ale
    ## 295 0.048  25    TX   Ale
    ## 610 0.060  43    AL   Ale
    ## 52  0.053  23    IN   Ale
    ## 480 0.065  77    IN   IPA
    ## 519 0.080  69    NY   Ale
    ## 461 0.049  20    OR   Ale
    ## 753 0.070  70    CA   IPA
    ## 181 0.039  20    OR   Ale
    ## 392 0.049  35    CO   IPA
    ## 319 0.059  14    CO   Ale
    ## 550 0.055  20    CA   Ale
    ## 743 0.070  70    IA   IPA
    ## 19  0.063  42    CA   IPA
    ## 334 0.055  25    FL   Ale
    ## 738 0.070  22    IA   Ale
    ## 677 0.047  14    MO   Ale
    ## 143 0.099  85    MN   Ale
    ## 482 0.074  97    OR   IPA
    ## 848 0.050  44    CA   IPA
    ## 537 0.059  42    MA   IPA
    ## 256 0.070 100    CO   IPA
    ## 250 0.075  22    CO   Ale
    ## 711 0.099  93    CA   IPA
    ## 542 0.062  61    TN   IPA
    ## 775 0.060  54    RI   Ale
    ## 346 0.067  70    OR   IPA
    ## 393 0.065  65    CO   Ale
    ## 182 0.039  20    OR   Ale
    ## 832 0.070  24    NC   Ale
    ## 155 0.052  25    CO   Ale
    ## 453 0.050  22    WY   Ale
    ## 608 0.051  24    WI   Ale
    ## 864 0.065  80    WA   IPA
    ## 513 0.063  61    AK   IPA
    ## 243 0.068  85    MA   IPA
    ## 929 0.071  60    MD   IPA
    ## 635 0.065  75    PA   IPA
    ## 727 0.048  23    NC   Ale
    ## 713 0.060  25    CA   Ale
    ## 730 0.052  27    IL   Ale
    ## 191 0.058  60    OR   Ale
    ## 422 0.082  80    VT   IPA
    ## 245 0.050  28    OH   Ale
    ## 779 0.059  60    VA   Ale
    ## 149 0.067  33    MN   Ale
    ## 544 0.074  74    CA   IPA
    ## 202 0.065  70    CO   IPA
    ## 702 0.065  69    MA   IPA
    ## 622 0.066  44    CO   Ale
    ## 481 0.062  62    IN   IPA
    ## 491 0.071  60    WI   IPA
    ## 361 0.056  55    OR   Ale
    ## 657 0.046  12    ID   Ale
    ## 668 0.035  45    MA   Ale
    ## 262 0.075  35    CA   IPA
    ## 407 0.087  85    CO   IPA
    ## 829 0.070  45    CO   IPA
    ## 822 0.080  86    CA   IPA
    ## 270 0.065  45    TX   Ale
    ## 811 0.072  70    AL   IPA
    ## 885 0.068  75    IA   IPA
    ## 792 0.045  15    CO   Ale
    ## 436 0.077  40    OH   Ale
    ## 330 0.070  60    FL   IPA
    ## 386 0.053  22    IN   Ale
    ## 898 0.071  85    CA   IPA
    ## 353 0.040  18    OR   Ale
    ## 911 0.065  65    CO   Ale
    ## 847 0.050  44    CA   IPA
    ## 905 0.058  49    CO   Ale
    ## 208 0.048  26    CA   Ale
    ## 183 0.039  20    OR   Ale
    ## 171 0.051  36    IL   Ale
    ## 568 0.069  69    MI   IPA
    ## 369 0.061  60    AZ   IPA
    ## 4   0.080  68    KY   IPA
    ## 489 0.047  46    CA   Ale
    ## 97  0.050 100    PA   IPA
    ## 884 0.084  82    WA   IPA
    ## 146 0.051  45    MN   Ale
    ## 546 0.094  92    CA   IPA
    ## 639 0.065  45    MA   IPA
    ## 740 0.054  30    IA   Ale
    ## 758 0.083  35    CA   Ale
    ## 249 0.066  72    CO   IPA
    ## 645 0.080  80    MA   IPA
    ## 870 0.065  26    IA   Ale
    ## 834 0.068  47    NY   IPA
    ## 712 0.080  88    CA   IPA
    ## 340 0.050  22    RI   Ale
    ## 750 0.044  42    CA   Ale
    ## 288 0.055  40    TX   Ale
    ## 741 0.053  48    IA   Ale
    ## 415 0.058  15    CA   Ale
    ## 521 0.092 115    DC   IPA
    ## 887 0.068  70    WA   IPA
    ## 634 0.057  42    PA   Ale
    ## 904 0.066 100    CO   IPA
    ## 628 0.040  37    MA   IPA
    ## 305 0.052   7    WI   Ale
    ## 235 0.048  44    CA   Ale
    ## 817 0.060  75    OR   Ale
    ## 840 0.098  76    MN   IPA
    ## 467 0.049  20    OR   Ale
    ## 913 0.065  65    CO   Ale
    ## 281 0.057  58    MI   IPA
    ## 130 0.099  90    CO   IPA
    ## 739 0.050  30    IA   Ale
    ## 6   0.066  21    KY   Ale
    ## 31  0.072  65    MI   IPA
    ## 344 0.057  47    NC   Ale
    ## 187 0.039  20    OR   Ale
    ## 351 0.055  37    LA   Ale
    ## 786 0.087  85    NC   IPA
    ## 323 0.048  32    TX   Ale
    ## 364 0.056  55    OR   Ale
    ## 759 0.070  65    CA   IPA
    ## 615 0.069  67    WA   IPA
    ## 166 0.045   8    MI   Ale
    ## 394 0.065  65    CO   Ale
    ## 2   0.048  19    MN   Ale
    ## 269 0.082 103    OR   IPA
    ## 258 0.050  25    CO   Ale
    ## 650 0.055  25    ID   Ale
    ## 378 0.060  21    AZ   Ale
    ## 80  0.070  85    AZ   IPA
    ## 697 0.051  26    MT   Ale
    ## 814 0.049  35    CO   Ale
    ## 347 0.054  24    OR   Ale
    ## 722 0.050  18    PA   Ale
    ## 12  0.080 100    CA   IPA
    ## 72  0.052  18    IN   Ale
    ## 109 0.065  62    NY   IPA
    ## 192 0.066  75    OR   IPA
    ## 501 0.069  65    NJ   IPA
    ## 783 0.069  65    ME   IPA
    ## 679 0.052  16    NY   Ale
    ## 240 0.052  24    RI   Ale
    ## 625 0.056  50    MA   IPA
    ## 412 0.062  80    WA   IPA
    ## 663 0.071  92    ID   IPA
    ## 681 0.063  50    NY   Ale
    ## 137 0.096  85    IL   IPA
    ## 825 0.051  20    TX   Ale
    ## 384 0.053  22    IN   Ale
    ## 716 0.072  85    CA   IPA
    ## 325 0.062  65    FL   IPA
    ## 437 0.060  53    OH   Ale
    ## 596 0.054  45    MN   Ale
    ## 744 0.048  25    IA   Ale
    ## 757 0.068  65    CA   Ale
    ## 845 0.037  53    CA   Ale
    ## 316 0.080  80    MS   Ale
    ## 165 0.075 115    MI   IPA
    ## 44  0.099 115    IN   IPA
    ## 943 0.053  22    NY   Ale
    ## 670 0.050  55    OH   IPA
    ## 937 0.062  42    NV   Ale
    ## 797 0.053  30    FL   Ale
    ## 253 0.066  72    CO   IPA
    ## 717 0.095  99    CA   IPA
    ## 843 0.056  35    FL   Ale
    ## 221 0.069  70    OH   IPA
    ## 32  0.047  42    MI   IPA
    ## 284 0.050  25    MI   Ale
    ## 327 0.070  60    FL   IPA
    ## 846 0.080  95    CA   IPA
    ## 451 0.048  48    WA   IPA
    ## 890 0.051  31    WI   Ale
    ## 701 0.057  29    NE   Ale
    ## 852 0.056  46    AK   Ale
    ## 751 0.070  70    CA   IPA
    ## 665 0.045  18    MO   Ale
    ## 878 0.060  64    AL   IPA
    ## 767 0.051  20    HI   Ale
    ## 380 0.050  35    TX   Ale
    ## 838 0.061  66    MO   IPA
    ## 803 0.073  55    CA   Ale
    ## 683 0.072 100    NM   IPA
    ## 76  0.050  18    TX   Ale
    ## 313 0.077  65    MS   IPA
    ## 311 0.075  89    MO   IPA
    ## 15  0.080 100    CA   IPA
    ## 78  0.050  15    AZ   Ale
    ## 479 0.054  42    HI   Ale
    ## 156 0.065  33    TX   Ale
    ## 785 0.085  86    OR   IPA
    ## 234 0.068  66    CA   IPA
    ## 154 0.075  25    CO   Ale
    ## 118 0.049  35    NY   Ale
    ## 636 0.065  60    NY   IPA
    ## 528 0.055  64    MA   IPA
    ## 366 0.061  94    OR   IPA
    ## 574 0.040  39    AR   Ale
    ## 299 0.051  21    TX   Ale
    ## 153 0.060  46    CO   Ale
    ## 616 0.073  87    CO   IPA
    ## 236 0.092  75    CA   IPA
    ## 600 0.088  77    CA   IPA
    ## 465 0.065  90    OR   IPA
    ## 91  0.070  75    CA   IPA
    ## 1   0.045  50    MN   IPA
    ## 509 0.058  38    FL   Ale
    ## 217 0.042  18    IL   Ale
    ## 115 0.064  62    NY   IPA
    ## 655 0.063 100    ID   IPA
    ## 81  0.055  45    AZ   Ale
    ## 158 0.070  82    FL   IPA
    ## 307 0.050   7    WI   Ale
    ## 406 0.080  35    CO   Ale
    ## 939 0.045  18    NY   Ale
    ## 563 0.070  40    CT   IPA
    ## 646 0.088 108    VA   IPA
    ## 341 0.042  24    RI   Ale
    ## 917 0.040   9    OK   Ale
    ## 923 0.065  65    MT   IPA
    ## 271 0.085 110    TX   IPA
    ## 23  0.070  51    MI   IPA
    ## 30  0.085  50    MI   Ale
    ## 387 0.045  36    IN   Ale
    ## 763 0.070 113    PA   IPA
    ## 795 0.055  30    NM   Ale
    ## 930 0.056  27    CO   Ale
    ## 756 0.044  42    CA   Ale
    ## 924 0.065  11    MT   Ale
    ## 124 0.075  75    CA   IPA
    ## 338 0.048  18    MN   Ale
    ## 732 0.082 100    IL   IPA
    ## 454 0.052  32    WY   Ale
    ## 490 0.065 115    CA   Ale
    ## 167 0.050  62    MI   IPA
    ## 554 0.056  30    TX   Ale
    ## 64  0.054  23    IN   Ale
    ## 892 0.065  44    WA   IPA
    ## 421 0.055  28    CA   Ale
    ## 287 0.040  55    MI   IPA
    ## 432 0.063  35    OK   Ale
    ## 762 0.084  90    PA   IPA
    ## 901 0.052  17    AK   Ale
    ## 514 0.063  61    AK   IPA
    ## 204 0.065  70    CO   IPA
    ## 455 0.068  60    WY   IPA
    ## 324 0.055  42    TX   Ale
    ## 309 0.050   7    WI   Ale
    ## 88  0.069  34    CA   Ale
    ## 735 0.049  20    WA   Ale
    ## 915 0.065  65    CO   Ale
    ## 42  0.058  58    IN   Ale
    ## 59  0.056  50    IN   Ale
    ## 48  0.058  20    IN   Ale
    ## 851 0.063  42    WA   Ale
    ## 515 0.050  24    AK   Ale
    ## 335 0.055  25    FL   Ale
    ## 272 0.092 100    TX   IPA
    ## 789 0.065  65    NC   Ale
    ## 21  0.067  74    MI   IPA
    ## 599 0.076  65    CA   IPA
    ## 641 0.065  65    PA   Ale
    ## 135 0.049  22    IL   Ale
    ## 478 0.052  23    HI   Ale
    ## 62  0.090  30    IN   Ale
    ## 520 0.050  15    DC   Ale
    ## 598 0.053  20    MA   Ale
    ## 275 0.050  20    TX   Ale
    ## 894 0.060  40    NY   IPA
    ## 82  0.055  45    AZ   Ale
    ## 17  0.047  19    CA   Ale
    ## 11  0.050  20    KY   Ale
    ## 733 0.056  41    IL   Ale
    ## 776 0.049  22    RI   Ale
    ## 174 0.062  68    MN   IPA
    ## 704 0.060  64    NY   IPA
    ## 13  0.075  85    CA   IPA
    ## 450 0.062  70    WA   IPA
    ## 656 0.052  32    ID   Ale
    ## 667 0.070  70    MO   IPA
    ## 51  0.090  30    IN   Ale
    ## 734 0.045  32    WA   Ale
    ## 925 0.077  71    CO   Ale
    ## 89  0.099 101    CA   IPA
    ## 142 0.050  28    MO   Ale
    ## 720 0.070  75    CO   IPA
    ## 428 0.051  45    VA   Ale
    ## 218 0.042  20    IL   Ale
    ## 122 0.064  62    NY   IPA
    ## 370 0.050  10    UT   Ale
    ## 302 0.054  33    WI   Ale
    ## 841 0.066  50    MN   IPA
    ## 585 0.052  49    LA   Ale
    ## 551 0.080  70    CA   IPA
    ## 571 0.080  72    IL   Ale
    ## 654 0.063 100    ID   IPA
    ## 772 0.076  78    OK   Ale
    ## 508 0.069  81    FL   IPA
    ## 837 0.053  48    CO   Ale
    ## 715 0.066  44    CA   Ale
    ## 866 0.060  18    OR   Ale
    ## 774 0.060  54    RI   Ale
    ## 725 0.072  80    NC   IPA
    ## 594 0.046  27    MN   Ale
    ## 414 0.070  80    CA   IPA
    ## 121 0.052  34    NY   Ale
    ## 310 0.054  33    WI   Ale
    ## 147 0.051  45    MN   Ale
    ## 696 0.050  35    MT   Ale
    ## 764 0.051  40    PA   Ale
    ## 895 0.044  16    NY   Ale
    ## 102 0.068 110    KS   IPA
    ## 933 0.068  55    FL   IPA
    ## 502 0.050  12    ID   Ale
    ## 169 0.058  43    IL   Ale
    ## 222 0.070  32    OH   Ale
    ## 390 0.062  60    IN   IPA
    ## 343 0.087  29    CO   Ale
    ## 197 0.066  75    OR   IPA
    ## 468 0.069  69    OR   IPA
    ## 669 0.067  60    CA   IPA
    ## 315 0.050  20    MS   Ale
    ## 273 0.085 110    TX   IPA
    ## 438 0.070  68    OH   IPA
    ## 558 0.060  30    MN   Ale
    ## 906 0.044  22    KS   Ale
    ## 342 0.092  72    CO   Ale
    ## 260 0.052  21    CA   Ale
    ## 584 0.046  30    VT   Ale
    ## 642 0.046  20    PA   Ale
    ## 835 0.068  47    NY   IPA
    ## 626 0.085  85    MA   IPA
    ## 264 0.070  88    VA   IPA
    ## 936 0.073  85    NV   Ale
    ## 819 0.058  20    CO   Ale
    ## 698 0.061  11    NE   Ale
    ## 816 0.049  35    CO   Ale
    ## 618 0.049  28    ME   Ale
    ## 941 0.060  55    MI   Ale
    ## 802 0.073  55    CA   Ale
    ## 798 0.071  62    FL   IPA
    ## 517 0.063  55    OH   Ale
    ## 891 0.065  44    WA   IPA
    ## 849 0.050  19    CA   Ale
    ## 33  0.050  27    IN   Ale
    ## 37  0.051  22    IN   Ale
    ## 907 0.092  85    CO   IPA
    ## 863 0.056  30    WA   Ale
    ## 94  0.055  17    CO   Ale
    ## 518 0.070  80    OH   IPA
    ## 927 0.060  55    TN   IPA
    ## 425 0.070  73    NC   IPA
    ## 55  0.064  75    IN   IPA
    ## 267 0.067  60    OR   IPA
    ## 611 0.076  73    TX   IPA
    ## 104 0.050  20    KS   Ale
    ## 391 0.053  22    IN   Ale
    ## 676 0.057  31    NC   Ale
    ## 66  0.056  50    IN   Ale
    ## 613 0.068  66    CA   IPA
    ## 833 0.055  65    OR   IPA
    ## 601 0.042  30    NJ   IPA
    ## 714 0.095  49    CA   Ale
    ## 477 0.042  22    HI   Ale
    ## 363 0.056  55    OR   Ale
    ## 352 0.045  28    LA   Ale
    ## 405 0.099 100    CO   IPA
    ## 278 0.050  20    TX   Ale
    ## 46  0.072  86    IN   IPA
    ## 128 0.065  33    CO   IPA
    ## 694 0.062  65    MT   IPA
    ## 458 0.062  70    WI   IPA
    ## 442 0.041  41    WA   IPA
    ## 689 0.050  35    MT   Ale
    ## 464 0.049  20    OR   Ale
    ## 172 0.070  68    IL   Ale
    ## 818 0.044  13    OR   Ale
    ## 53  0.064  75    IN   IPA
    ## 336 0.072  75    FL   Ale
    ## 22  0.048  47    MI   IPA
    ## 922 0.055  50    MT   Ale
    ## 545 0.080  70    CA   IPA
    ## 50  0.099  36    IN   Ale
    ## 161 0.071  75    NC   IPA
    ## 488 0.055  30    CA   Ale
    ## 385 0.062  60    IN   IPA
    ## 280 0.065  65    CO   IPA
    ## 648 0.062  65    ID   IPA
    ## 582 0.049  10    MA   Ale
    ## 127 0.068  24    CO   Ale
    ## 932 0.045  20    FL   Ale
    ## 134 0.054  45    IL   Ale
    ## 226 0.060  60    OH   Ale
    ## 349 0.070  60    LA   IPA
    ## 543 0.058  35    CA   Ale
    ## 216 0.042  18    IL   Ale
    ## 213 0.048  11    MO   Ale
    ## 206 0.072  65    CA   IPA
    ## 474 0.045  50    IN   IPA
    ## 760 0.078  74    CO   IPA
    ## 18  0.056  16    CA   Ale
    ## 651 0.072  60    FL   IPA
    ## 658 0.084  90    ID   Ale
    ## 203 0.052  18    CO   Ale
    ## 248 0.067  70    ME   IPA
    ## 483 0.052  40    TX   Ale
    ## 914 0.065  65    CO   Ale
    ## 398 0.087  85    CO   IPA
    ## 383 0.056  40    PA   Ale
    ## 36  0.058  36    IN   IPA
    ## 796 0.071  95    NM   Ale
    ## 495 0.055  35    TX   Ale
    ## 207 0.056  37    CA   Ale
    ## 255 0.050  40    CO   Ale
    ## 633 0.058  45    PA   IPA
    ## 79  0.055  20    AZ   Ale
    ## 853 0.065  71    AK   IPA
    ## 111 0.099 111    NY   IPA
    ## 931 0.070  67    CO   IPA
    ## 125 0.047  20    CA   Ale
    ## 469 0.077  30    OR   IPA
    ## 301 0.051  21    TX   Ale
    ## 285 0.065  25    MI   Ale
    ## 184 0.039  20    OR   Ale
    ## 524 0.049  15    WI   Ale
    ## 372 0.073  83    UT   IPA
    ## 112 0.070  70    NY   Ale
    ## 573 0.067  70    MN   IPA
    ## 110 0.047  50    NY   Ale
    ## 85  0.055  20    AZ   Ale
    ## 780 0.065  52    VA   IPA
    ## 813 0.062  55    GA   Ale
    ## 709 0.064  95    PA   IPA
    ## 434 0.077  40    OH   Ale
    ## 395 0.065  65    CO   Ale
    ## 806 0.062  43    WA   IPA
    ## 448 0.054  20    WA   Ale
    ## 401 0.087  85    CO   IPA
    ## 771 0.062  65    HI   IPA
    ## 505 0.081  17    CO   Ale
    ## 652 0.050  21    FL   Ale

``` r
test
```

    ##       ABV IBU State Style
    ## 3   0.060  38    MN   Ale
    ## 5   0.042  42    KY   Ale
    ## 7   0.040  13    KY   Ale
    ## 8   0.076  68    KY   IPA
    ## 9   0.051  38    KY   Ale
    ## 10  0.060  65    KY   IPA
    ## 14  0.060  24    CA   Ale
    ## 20  0.059  25    MI   Ale
    ## 24  0.055  10    MI   Ale
    ## 25  0.048  15    MI   Ale
    ## 28  0.050  35    MI   IPA
    ## 29  0.058  72    MI   IPA
    ## 34  0.060 104    IN   IPA
    ## 38  0.070  46    IN   IPA
    ## 41  0.064  90    IN   IPA
    ## 45  0.068  16    IN   Ale
    ## 47  0.055  40    IN   Ale
    ## 49  0.083  23    IN   Ale
    ## 54  0.063  75    IN   IPA
    ## 56  0.064  75    IN   IPA
    ## 57  0.090  30    IN   Ale
    ## 58  0.075  77    IN   IPA
    ## 61  0.091  91    IN   IPA
    ## 63  0.075  77    IN   IPA
    ## 67  0.049  23    IN   Ale
    ## 68  0.070  61    IN   IPA
    ## 69  0.050  45    IN   IPA
    ## 70  0.069  65    IN   IPA
    ## 71  0.090  50    IN   Ale
    ## 73  0.085  90    TX   IPA
    ## 74  0.068  70    TX   IPA
    ## 75  0.051  36    TX   Ale
    ## 77  0.091  99    AZ   IPA
    ## 83  0.070  85    AZ   IPA
    ## 84  0.055  45    AZ   Ale
    ## 92  0.070  70    CA   IPA
    ## 93  0.063  69    CO   IPA
    ## 95  0.075  63    TX   IPA
    ## 96  0.075  60    IL   IPA
    ## 98  0.055  40    IL   Ale
    ## 100 0.048  20    KS   Ale
    ## 101 0.056  20    KS   Ale
    ## 103 0.044  12    KS   Ale
    ## 106 0.072  93    KS   IPA
    ## 107 0.044  22    KS   Ale
    ## 108 0.063  60    KS   IPA
    ## 113 0.067  74    NY   IPA
    ## 114 0.052  34    NY   Ale
    ## 116 0.059  47    NY   Ale
    ## 129 0.075  30    CO   Ale
    ## 131 0.075  30    CO   Ale
    ## 133 0.057  27    MN   Ale
    ## 136 0.063  76    IL   IPA
    ## 148 0.097 120    MN   IPA
    ## 150 0.062  99    MN   IPA
    ## 152 0.050  20    KY   Ale
    ## 160 0.055  45    MD   IPA
    ## 163 0.055  45    NC   Ale
    ## 164 0.085  90    MD   IPA
    ## 168 0.063  69    IL   IPA
    ## 170 0.058  43    IL   Ale
    ## 173 0.072  87    MN   IPA
    ## 176 0.047  35    CA   IPA
    ## 177 0.048  16    WY   Ale
    ## 178 0.046  20    WY   Ale
    ## 180 0.039  20    OR   Ale
    ## 185 0.039  20    OR   Ale
    ## 186 0.039  20    OR   Ale
    ## 189 0.039  20    OR   Ale
    ## 190 0.039  20    OR   Ale
    ## 193 0.073  70    OR   Ale
    ## 195 0.058  60    OR   Ale
    ## 198 0.045  40    CO   IPA
    ## 201 0.052  18    CO   Ale
    ## 209 0.072  65    CA   IPA
    ## 210 0.056  37    CA   Ale
    ## 215 0.054  30    IL   Ale
    ## 220 0.050  40    OH   Ale
    ## 224 0.057  42    OH   Ale
    ## 230 0.052  67    OH   IPA
    ## 231 0.054  20    OH   Ale
    ## 233 0.075  85    PA   IPA
    ## 238 0.065  75    RI   IPA
    ## 242 0.058  45    MA   Ale
    ## 244 0.057  40    CO   Ale
    ## 246 0.057  36    OH   Ale
    ## 247 0.070  70    OH   IPA
    ## 254 0.065  82    NH   IPA
    ## 257 0.099 100    CO   IPA
    ## 259 0.070 100    CO   IPA
    ## 261 0.045  16    CA   Ale
    ## 268 0.053  36    OR   Ale
    ## 277 0.065  45    TX   Ale
    ## 279 0.065  65    CO   Ale
    ## 282 0.052  42    VA   Ale
    ## 289 0.066  20    TX   Ale
    ## 290 0.095  85    TX   IPA
    ## 291 0.066  70    TX   IPA
    ## 292 0.060  75    TX   IPA
    ## 293 0.075  33    TX   Ale
    ## 297 0.070  70    TX   IPA
    ## 298 0.099  85    TX   Ale
    ## 300 0.060  50    TX   Ale
    ## 304 0.063  64    WI   IPA
    ## 306 0.050   7    WI   Ale
    ## 320 0.057  68    CO   Ale
    ## 321 0.059  14    CO   Ale
    ## 322 0.053  49    MN   Ale
    ## 326 0.082  65    FL   Ale
    ## 329 0.070  60    FL   IPA
    ## 331 0.070  60    FL   IPA
    ## 332 0.075  70    FL   IPA
    ## 337 0.075  70    FL   IPA
    ## 339 0.042  24    RI   Ale
    ## 350 0.045  18    LA   Ale
    ## 354 0.062  82    OR   IPA
    ## 355 0.068 100    OR   IPA
    ## 359 0.061  94    OR   IPA
    ## 360 0.056  55    OR   Ale
    ## 362 0.056  55    OR   Ale
    ## 365 0.054  24    OR   Ale
    ## 371 0.040  42    UT   IPA
    ## 373 0.040  17    UT   Ale
    ## 374 0.073  82    UT   IPA
    ## 375 0.040  34    UT   Ale
    ## 376 0.040  29    UT   Ale
    ## 379 0.050  20    TX   Ale
    ## 382 0.071  36    PA   Ale
    ## 388 0.060  25    IN   Ale
    ## 396 0.065  65    CO   Ale
    ## 397 0.065  65    CO   Ale
    ## 402 0.080  70    CO   Ale
    ## 403 0.090  60    CO   IPA
    ## 404 0.087  85    CO   IPA
    ## 409 0.070  68    CO   Ale
    ## 410 0.049  25    ID   Ale
    ## 411 0.043  60    WA   IPA
    ## 417 0.056   4    CA   Ale
    ## 420 0.056   4    CA   Ale
    ## 423 0.058  60    TX   Ale
    ## 424 0.061  60    IN   Ale
    ## 427 0.057  20    VA   Ale
    ## 435 0.053  20    OH   Ale
    ## 439 0.048  35    TX   Ale
    ## 441 0.070  70    MO   IPA
    ## 445 0.062  70    WA   IPA
    ## 447 0.057  36    WA   Ale
    ## 449 0.052  27    WA   Ale
    ## 452 0.060  55    WY   Ale
    ## 459 0.055  40    OR   Ale
    ## 460 0.059  55    IL   IPA
    ## 463 0.063  70    OR   IPA
    ## 466 0.065  90    OR   IPA
    ## 472 0.075  53    OR   Ale
    ## 484 0.075  85    CA   IPA
    ## 485 0.068  75    CA   IPA
    ## 486 0.052  50    CA   Ale
    ## 492 0.048  38    WI   IPA
    ## 493 0.043  21    TX   Ale
    ## 494 0.049  21    TX   Ale
    ## 497 0.090 118    TX   IPA
    ## 498 0.055  35    TX   Ale
    ## 500 0.071  69    TX   IPA
    ## 506 0.095 104    CO   IPA
    ## 507 0.067  85    CO   Ale
    ## 512 0.042  35    NJ   IPA
    ## 516 0.057  70    AK   IPA
    ## 523 0.072  50    WI   IPA
    ## 530 0.050  50    NV   Ale
    ## 531 0.093  90    NV   IPA
    ## 532 0.062  45    MA   IPA
    ## 533 0.059  42    MA   IPA
    ## 534 0.059  20    MA   Ale
    ## 541 0.042  35    NY   Ale
    ## 547 0.059  60    CA   IPA
    ## 548 0.068  65    CA   IPA
    ## 552 0.080  70    CA   IPA
    ## 553 0.059  60    CA   IPA
    ## 556 0.070  75    TX   IPA
    ## 559 0.038  18    MS   Ale
    ## 561 0.045  35    SC   Ale
    ## 562 0.065  65    GA   IPA
    ## 564 0.052  18    CT   Ale
    ## 576 0.068  65    CO   IPA
    ## 577 0.068  65    CO   IPA
    ## 580 0.050  61    ME   Ale
    ## 581 0.055  35    MA   Ale
    ## 583 0.059  42    VT   IPA
    ## 588 0.080 120    VT   IPA
    ## 590 0.060  50    VA   IPA
    ## 595 0.065  80    MN   Ale
    ## 597 0.055  40    MA   IPA
    ## 605 0.051  24    WI   Ale
    ## 606 0.075  51    WI   IPA
    ## 607 0.065  20    WI   Ale
    ## 609 0.078  16    OH   Ale
    ## 612 0.052  20    TX   Ale
    ## 617 0.055  35    ME   Ale
    ## 619 0.069  69    ME   IPA
    ## 627 0.051  17    MA   Ale
    ## 630 0.060  55    MA   IPA
    ## 632 0.047  28    PA   Ale
    ## 637 0.065  60    NY   IPA
    ## 638 0.050  12    NY   Ale
    ## 643 0.090  75    UT   IPA
    ## 653 0.055  28    FL   Ale
    ## 660 0.065  72    WA   IPA
    ## 661 0.055  37    TN   Ale
    ## 664 0.045  18    MO   Ale
    ## 671 0.055  60    OH   Ale
    ## 673 0.092  25    PA   Ale
    ## 674 0.073  85    PA   IPA
    ## 682 0.063  37    OK   Ale
    ## 685 0.045  19    ND   Ale
    ## 688 0.050  40    MT   Ale
    ## 691 0.072  60    MT   Ale
    ## 692 0.051  26    MT   Ale
    ## 693 0.050  35    MT   Ale
    ## 699 0.065  65    NE   IPA
    ## 703 0.061  64    FL   IPA
    ## 705 0.075  70    VA   IPA
    ## 706 0.044  18    VA   Ale
    ## 707 0.056  55    VA   Ale
    ## 708 0.059 135    VA   IPA
    ## 710 0.056  70    PA   IPA
    ## 718 0.075  77    CA   Ale
    ## 719 0.058  55    MA   IPA
    ## 723 0.085  85    PA   IPA
    ## 728 0.061  41    NC   Ale
    ## 736 0.055  60    WA   Ale
    ## 737 0.066  30    WA   Ale
    ## 742 0.070  22    IA   Ale
    ## 746 0.054  30    IA   Ale
    ## 748 0.053  14    OK   Ale
    ## 749 0.050  35    WA   Ale
    ## 754 0.085  52    CA   Ale
    ## 755 0.097  94    CA   IPA
    ## 761 0.078  74    CO   IPA
    ## 765 0.082 138    OR   IPA
    ## 770 0.055  15    HI   Ale
    ## 781 0.065  52    VA   IPA
    ## 782 0.068  65    SC   IPA
    ## 787 0.080  85    NC   IPA
    ## 788 0.080  35    NC   Ale
    ## 791 0.042   9    CO   Ale
    ## 800 0.060  50    TX   Ale
    ## 801 0.056  35    CA   Ale
    ## 804 0.056  35    CA   Ale
    ## 807 0.050  20    CO   Ale
    ## 808 0.070  70    CO   IPA
    ## 812 0.047  17    GA   Ale
    ## 815 0.057  35    CO   Ale
    ## 820 0.068  65    CO   Ale
    ## 828 0.050  30    MO   Ale
    ## 831 0.074  60    CA   IPA
    ## 858 0.048  16    WY   Ale
    ## 859 0.059  15    WY   Ale
    ## 861 0.065  35    WA   Ale
    ## 865 0.070  70    OR   IPA
    ## 867 0.050  30    FL   Ale
    ## 873 0.060  31    CO   Ale
    ## 875 0.062  55    GA   IPA
    ## 876 0.056  36    AL   Ale
    ## 877 0.093 103    AL   IPA
    ## 879 0.058  36    AL   Ale
    ## 880 0.072  45    MD   IPA
    ## 882 0.054  30    WA   Ale
    ## 888 0.044  38    WA   Ale
    ## 889 0.051  32    OR   Ale
    ## 893 0.058  27    WA   Ale
    ## 896 0.070  80    FL   IPA
    ## 902 0.061  64    AK   IPA
    ## 909 0.080  35    CO   Ale
    ## 910 0.080  35    CO   Ale
    ## 912 0.065  65    CO   Ale
    ## 916 0.087  60    CO   IPA
    ## 918 0.074  83    WA   IPA
    ## 921 0.065  65    MT   IPA
    ## 928 0.062  65    CO   IPA
    ## 934 0.053  28    FL   Ale
    ## 938 0.065  52    LA   IPA
    ## 940 0.055  52    DE   Ale
    ## 944 0.065  70    NY   IPA

``` r
accs = data.frame(accuracy = numeric(30), k = numeric(30))
for(i in 1:30)
{
  classifications = knn(train[,c(1:2)],test[,c(1:2)],train$Style, prob = TRUE, k = i)
  table(test$Style,classifications)
  CM = confusionMatrix(table(test$Style,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k")
```

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
#loop for many ks and the average of many training / test 
iterations = 500
numks = 30
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(totalAle)[1],round(splitPerc * dim(totalAle)[1]))
  train = totalAle[trainIndices,]
  test = totalAle[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,c(1:2)],test[,c(1:2)],train$Style, prob = TRUE, k = i)
    table(classifications,test$Style)
    CM = confusionMatrix(table(classifications,test$Style))
    masterAcc[j,i] = CM$overall[1]
  }
}

#plot the accuracies of all the models to see which one is best.
MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")
```

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
#Create a confusion matrix based on this model and then plot the results
#This gives us our accuracy.
classifications = knn(train[,c(1:2)],test[,c(1:2)], train$Style,
                      prob = TRUE, k =5)
table(classifications,test$Style)
```

    ##                
    ## classifications Ale IPA
    ##             Ale 134  18
    ##             IPA  20 111

``` r
CM = confusionMatrix(table(classifications,test$Style))

fourfoldplot(CM$table,color = c("cyan", "pink"),
             conf.level = 0, margin = 1, main = "Predicition of IPAs and Ales")
```

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

# We compared this to a Naive-Bayes model using ABV and IBU. The results were less accurate than the Knn model, which is why we have reported our findings from the knn.

``` r
#Create a Naive-Bayes model and then create a confusion matrix to compare the accuracy to the knn.
model = naiveBayes(Style~ABV+IBU, data = train)
table(predict(model,test[,c(1:2)]), test$Style)
```

    ##      
    ##       Ale IPA
    ##   Ale 131  20
    ##   IPA  23 109

``` r
CM = confusionMatrix(table(predict(model,test[,c(1:2)]), test$Style))
CM
```

    ## Confusion Matrix and Statistics
    ## 
    ##      
    ##       Ale IPA
    ##   Ale 131  20
    ##   IPA  23 109
    ##                                           
    ##                Accuracy : 0.8481          
    ##                  95% CI : (0.8008, 0.8878)
    ##     No Information Rate : 0.5442          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.6943          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.7604          
    ##                                           
    ##             Sensitivity : 0.8506          
    ##             Specificity : 0.8450          
    ##          Pos Pred Value : 0.8675          
    ##          Neg Pred Value : 0.8258          
    ##              Prevalence : 0.5442          
    ##          Detection Rate : 0.4629          
    ##    Detection Prevalence : 0.5336          
    ##       Balanced Accuracy : 0.8478          
    ##                                           
    ##        'Positive' Class : Ale             
    ## 

\#After looking to see if we could predict the IPA vs. Ales we decided
to do a little research into a few states and what beers are popular
there. \## We looked at Missouri (HQ of Budweiser), Texas, California,
and Colorado. These last 3 had the highest breweries of all the states.
\### We looked first by our styles which were IPA, Ale, and Other and
then looked at the percentages to evenly compare them. We can see that
IPAs and Ales make up a majority of California and Colorado but Other
seems to be more popualar in Missouri and Texas. \### We also broke down
Ale into a few more categories (including IPAs) to see if a certain type
was more popular. We can see in both the distribution and percentage
graphs that IPA is the most popular choice out of all the ales looked
at. If you wanted to focus in on marketing a type of ale, IPAs would be
it.

``` r
#Create a dataset that includes just the states Missouri, Texas, California, and Colorado.
#Then plot the distribution of the beer styles in each state and the percentages of beer styles in each state.
Oktoberfest = combined %>%mutate(Style = if_else(str_detect(combined$Style, "IPA"), "IPA", if_else(str_detect(combined$Style, "Ale"), "Ale", "Other"))) %>%
  filter(State == 'MO' | State == "TX" | State == "CO" | State == "CA") %>%
  filter(!is.na(Style)) %>%
  select(State,Style)

#Distribution
Oktoberfest %>% ggplot(aes(x = State, fill = Style)) +
  geom_bar(position = "dodge") + ggtitle("Distribution of Beer Types per Specific States")+ ylab("Count of Beer Type")
```

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
#Percentages
Oktoberfest %>% ggplot(aes(x = State, fill = Style)) +
  geom_bar(position = "fill") + ggtitle("Percentage of Beer Types per Specific States") + ylab("Percentage of Beer Type")
```

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
#Create a dataset to label the different types of ales and then filter by our four states.
#Then plot the distribution of the beer styles in each state and the percentages of ale types in each state.
Oktoberfest3 = combined %>%
  mutate(Styles = if_else(str_detect(combined$Style, "American Amber / Red Ale"), "Red Ale", if_else(str_detect(combined$Style, "American Blonde Ale"), "Blonde Ale", if_else(str_detect(combined$Style, "American Brown Ale"), "Brown Ale", if_else(str_detect(combined$Style, "American Double / Imperial IPA"), "IPA", if_else(str_detect(combined$Style, "American IPA"), "IPA", if_else(str_detect(combined$Style, "APA"), "APA", if_else(str_detect(combined$Style, "American Pale Wheat Ale"), "Pale Wheat Ale", if_else(str_detect(combined$Style, "American Porter"), "American Porter", if_else(str_detect(combined$Style, "Fruit / Vegetable Beer"), "Fruit / Vegetable Beer", "Other" )))))))))) %>%
  filter(!is.na(Styles)) %>%
  filter(Styles != "Other") %>%
  filter(State == "MO" | State == "TX" | State == "CO" | State == "CA") %>%
  select(Styles, State)

#Distribution
Oktoberfest3 %>% ggplot(aes(x = State, fill = Styles)) +
  geom_bar(position = "dodge") + ggtitle("Distribution of Ale Types per Specific States")+ ylab("Count of Beer Type")
```

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
#Percentages
Oktoberfest3 %>% ggplot(aes(x = State, fill = Styles)) +
  geom_bar(position = "fill") + ggtitle("Percentage of Ale Types per Specific States") + ylab("Percentage of Beer Type")
```

![](Rachel-and-Santi--Rmarkdown-file_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

\#Conclusion \#We were able to provide a lot of useful data that
Budweiser will be able to use in further research and marketing
campaigns. \#Overall we can see based on ABV and IBU (of the data points
that had these values) we are able to predict if a beer is an IPA vs an
Ale. We also took a deeper dive into all of the ales and can say that
IPAs are the most popular style of Ale (at least in the 4 states being
looked at).
