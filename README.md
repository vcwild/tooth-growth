Tooth Growth Analysis
================

In this project, we are going to explore the ToothGrowth R dataset and
do inferential data analysis aiming to answer the following questions:

1 - Is there a relation between tooth length and supplement used?

2 - Does changing the supplement dosage interfere with the tooth length?

The project is set in literate programming, allowing full research
reproducibility.

## Tools Used

  - R language compiler
  - R base graphic devices
  - Tidyverse library packages
  - RMarkdown library package
  - Knitr library
    package

## Files

  - **[CODEBOOK](https://github.com/vcwild/tooth-growth/blob/master/analysis.pdf)**:
    step-by-step book explaining the code
    processing.
  - **[Figures](https://github.com/vcwild/tooth-growth/tree/master/analysis_files/figure-gfm)**:
    the plotted
    images
  - **[Infsim.Rmd](https://github.com/vcwild/tooth-growth/blob/master/analysis.Rmd)**:
    the script to compile the project from source

# CODEBOOK

## Setup

``` r
knitr::opts_chunk$set(echo = TRUE, cache = TRUE, warning = FALSE)
```

### Provide a basic summary of the data

Loading required packages and data
    set

``` r
require(tidyverse)
```

    ## Loading required package: tidyverse

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
data <- ToothGrowth
```

## Exploratory Data Analysis

### Summary of the data

``` r
means <- data %>% 
    group_by(supp, dose) %>% 
    summarize(tooth.length = mean(len))

means
```

    ## # A tibble: 6 x 3
    ## # Groups:   supp [2]
    ##   supp   dose tooth.length
    ##   <fct> <dbl>        <dbl>
    ## 1 OJ      0.5        13.2 
    ## 2 OJ      1          22.7 
    ## 3 OJ      2          26.1 
    ## 4 VC      0.5         7.98
    ## 5 VC      1          16.8 
    ## 6 VC      2          26.1

``` r
# Classes
nclass = round(sqrt(length(data$len)))
hist(data$len, col = "slateblue1", main = "Histogram of Tooth Length", xlab = "Tooth Length", nclass = nclass)
```

![](analysis_files/figure-gfm/summary-1.png)<!-- -->

``` r
# OJ - orange juice
# VC - vitamin C
```

## Inferential Analysis

#### Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose

### Hypotesis 1 - Relation between tooth length and the supplement used

``` r
# Separate samples for analysis
supp = data$supp

len_by_OJ = data$len[supp == "OJ"] #sample_b
len_by_VC = data$len[supp == "VC"] #sample_a

delta = abs(mean(len_by_OJ) - mean(len_by_VC))
pooledsd = (sd(len_by_OJ) + sd(len_by_VC))/2
t1_error = 0.05

# Statistical Power

power.t.test(n = length(len_by_VC), sig.level = t1_error, delta = delta, sd = pooledsd, alternative = "one.sided", type = "two.sample")$power
```

    ## [1] 0.6024765

The statistical power for the t-test is too low\!\! (below .80)

The outcome of the t-test can result in a false positive\!

``` r
# The Null Hypothesis
# H0: µa >= µb
# The Alternative Hypothesis
# Ha: µa < µb

# T-test: non-paired, variance based on approx of df, µa < H0
t.test(len_by_VC, len_by_OJ, conf.level = .95, var.equal = TRUE, paired = FALSE, alternative = "less")$p.value
```

    ## [1] 0.03019669

The p-value shows strong evidence to reject the null hypothesis (pval \<
0.05), indicating high significance for the alternative hypothesis
between the tooth length and the supplement used.

### Hypothesis Conclusion

We can imply that the p-value for this observation is too high in
relation to the threshold, and the statistical power of the analysis is
too low, meaning that the alternative hypothesis may be a false
positive.

### Hypothesis 2 - Comparing tooth growth by dose

``` r
# Separate samples for analysis
half = data$len[data$dose == .5] # sample_a
one = data$len[data$dose == 1] # sample_b
two = data$len[data$dose == 2] # sample_c

length(half) == length(one)
```

    ## [1] TRUE

``` r
delta = abs(mean(half) - mean(one))
pooledsd = (sd(half) + sd(one))/2
t1_error = 0.05

# Statistical Power

power.t.test(n = length(half), sig.level = t1_error, delta = delta, sd = pooledsd, alternative = "one.sided", type = "two.sample")$power
```

    ## [1] 0.9999988

Very high Statistical Power, meaning the outcome of the t-test is very
accurate\!\!

``` r
# The Null Hypothesis
# H0: µa >= µb
# The Alternative Hypothesis
# Ha: µa < µb

# T-test, non-paired, var by approx of df, µa < µb
t.test(half, one, alternative = "less", paired = FALSE, var.equal = TRUE, conf.level = .95)$p.value
```

    ## [1] 6.331485e-08

The p-value shows strong evidence to reject the null hypothesis (pval \<
0.05), indicating proof for the alternative hypothesis between lower
supplement doses and tooth length.

``` r
length(two) == length(one)
```

    ## [1] TRUE

``` r
delta = abs(mean(two) - mean(one))
pooledsd = (sd(two) + sd(one))/2
t1_error = 0.05

# Statistical Power

power.t.test(n = length(two), sig.level = t1_error, delta = delta, sd = pooledsd, alternative = "one.sided", type = "two.sample")$power
```

    ## [1] 0.9992657

Very high Statistical Power, meaning the outcome of the t-test is very
accurate\!\!

``` r
# The Null Hypothesis
# H0: µc <= µb
# The Alternative Hypothesis
# Ha: µc > µb

# T-test, non-paired, var by approx of df, µc > µb
t.test(two, one, alternative = "greater", paired = FALSE, var.equal = TRUE, conf.level = .95)$p.value
```

    ## [1] 9.054143e-06

The p-value shows strong evidence to reject the null hypothesis (pval \<
0.05), indicating proof for the alternative hypothesis between higher
supplement doses and tooth length.

### Hypothesis Conclusion

We can confirm with a 95% confidence interval that, for the true
population distribution, the supplement dosage level interfere with
tooth growth.

We can visualize this statement in the following plot:

``` r
ggplot(data, aes(dose, len, group = supp, fill = supp)) +
    geom_bar(stat = "identity", position = "dodge", color = "white") +
    labs(
        x = "Dose",
        y = "Tooth Length",
        fill = "Supplement"
    ) +
    coord_flip() +
    theme_minimal()
```

![](analysis_files/figure-gfm/concplot-1.png)<!-- -->
