PECO Calculations
================
Aaron Graybill
2/10/2021

## Introduction

The impediments to fairly dividing the bills are two-fold.

1.  Everyone should be paying for the baseline energy consumed daily
    regardless of household inhabitants.
2.  Not everyone is at the house on the same days.

## Procedure

To address this problem the following cost calculation procedure is
conducted:

### Step 1: Dataset

A dataset was created consisting of each person’s days at the house, the
total cost per kiloWatt hour for each day (because there’s a per unit
fee for distribution, generation, and transmission), and the amount of
kiloWatt hours used.

The beginning of the dataset looks like the following:

``` r
d <- read.csv("data/PECOdata.csv") %>% 
  filter(.,complete.cases(.))
head(d)
```

    ##       Date Aaron Kayleen Gabe Kian  Eli Mateo Usage Bill per.unit
    ## 1  11/5/20  TRUE    TRUE TRUE TRUE TRUE  TRUE    31    1  0.12987
    ## 2  11/6/20  TRUE    TRUE TRUE TRUE TRUE  TRUE    31    1  0.12987
    ## 3  11/7/20  TRUE    TRUE TRUE TRUE TRUE  TRUE    30    1  0.12987
    ## 4  11/8/20  TRUE    TRUE TRUE TRUE TRUE  TRUE    30    1  0.12987
    ## 5  11/9/20  TRUE    TRUE TRUE TRUE TRUE  TRUE    33    1  0.12987
    ## 6 11/10/20  TRUE    TRUE TRUE TRUE TRUE  TRUE    30    1  0.12987

### Step 2: Estimate Personal kWh Consumption

We look for days when there is no one at the house to calculate the
uninhabited median kiloWatt hour consumption. Call this \(\mu\). I
compute mu in the following way:

``` r
mu_data <-
  d %>%
  filter(Aaron == F &
           Kayleen == F &
           Gabe == F &
           Kian == F &
           Mateo == F) 
mu <- 
  mu_data %>% 
  summarise(mu=median(Usage)) %>%
  pull()

ggplot(mu_data)+
  geom_density(aes(x=Usage))+
  geom_vline(aes(xintercept=mu,col="mu"))
```

![](PECO_files/figure-gfm/compute%20mu-1.png)<!-- --> With \(\mu=6\), we
then say that each person consumes \(\frac{\mu}{6}\) kWh per period.
Each persons total consumption per period is usage by:

In the above formula \(i_t^{name}\) is a indicator variable that takes
\(1\) if the person is at the house at time \(t\), and \(0\) otherwise.
Note that if there is no one at the house we simply divide usage by 6.

This usage formula has the desirable feature that the sum of each
person’s usage is always equal to the total usage. The reason why this
function is piecewise is because in the cases when household consumption
is less than \(\mu\), which happens, it is possible for the people at
the house to pay less than those not there, not the right result.

Now let’s compute this usage:

``` r
d_long <- 
  d %>% 
  pivot_longer(cols=-c(Date,Usage,Bill,per.unit))
d_long <- 
  d_long %>% 
  group_by(Date) %>% 
  mutate(
    usage_est=
      case_when(
      (Usage>=mu) & (sum(value)>0) ~mu/6+(value/sum(value))*(Usage-mu),
      TRUE~Usage/6
    )
  )
```

Note that a random sample of the total usage minus the total estimated
usage are zero

``` r
set.seed(1234)
  d_long %>% 
  group_by(Date) %>% 
  summarise(difference=Usage-sum(usage_est)) %>%
  ungroup() %>% 
  sample_n(6) %>% 
  head()
```

    ## `summarise()` regrouping output by 'Date' (override with `.groups` argument)

    ## # A tibble: 6 x 2
    ##   Date     difference
    ##   <chr>         <dbl>
    ## 1 11/26/20          0
    ## 2 1/24/21           0
    ## 3 12/18/20          0
    ## 4 1/24/21           0
    ## 5 1/25/21           0
    ## 6 11/7/20           0

## Step 3: Compute Personal Daily Contribution

We have now given each person a kWh contribution, and since we have a
per period price, the only step left is multiplying these two to compute
each persons daily electric bill.

We can do that in the following way:

``` r
final_data <- 
  d_long %>% 
  mutate(daily=usage_est*per.unit) %>% 
  group_by(name) %>% 
  mutate(total=sum(daily))
```

``` r
knitr::kable(final_data %>% select(name,total) %>% unique(),format = 'latex')
```

## Including Plots

You can also embed plots, for example:

![](PECO_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
