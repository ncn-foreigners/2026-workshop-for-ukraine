# 2026 Workshop For Ukraine

WORKSHOPS FOR UKRAINE -- Inference for non-probability samples with nonprobsvy package in R

-   **Date**: Thursday, January 8th, 18:00 - 20:00 CET (Rome, Berlin, Paris timezone)

-   **Speaker**: Maciej Beręsewicz, an R enthusiast, co-organiser of several R conferences including European R Users Meeting 2016, and an R developer. Currently employed as an assistant professor in the Department of Statistics, Poznan University of Economics and Business, and the head of the Centre for the Methodology of Population Studies at the Statistical Office in Poznan. Main research topics: non-probability samples, administrative data, and population size estimation.

-   **Description**: During the workshop, the following topics will be covered:

    1)  basic theory of inference for non-probability samples,
    2)  how to use population-level information as well as probability samples to correct selection bias using various ways (inverse probability weighting, mass imputation, and double robust approach),
    3)  how to use the nonprobsvy package to estimate population mean through case studies,
    4)  how to estimate uncertainty and report results in R.

    -   For more details, please see the following working paper: Chrostowski, Chlebicki & Beręsewicz, (2025). nonprobsvy--An R package for modern methods for non-probability surveys. arXiv preprint arXiv:2504.04255 (accepted to the Journal of Statistical Software).

-   **Minimal registration fee**: 20 euro (or 20 USD or 800 UAH)

# Prerequests

Install the CRAN version (0.2.3) of the {nonprobsvy} package using the following code

``` r
install.packages("nonprobsvy")
```

Repository has the following structure:

-   `codes/` -- scripts for the workshop:
    -   `1-practice.R` -- introduction to the package
    -   `2-practice.R` -- script with exercises
-   `slides/` -- presentation for workshop \| [slides are here](slides/2026-slides-for-ukraine.pdf)

# Literature

-   Chrostowski, Ł., Chlebicki, P., & Beręsewicz, M. (2025). nonprobsvy--An R package for modern methods for non-probability surveys. arXiv preprint [arXiv:2504.04255](https://arxiv.org/abs/2504.04255). Forthcoming to the Journal of Statistical Software

# Acknowledgements

-   The authors’ work has been financed by the National Science Centre in Poland, OPUS 20, grant no. 2020/39/B/HS4/00941.
-   Łukasz Chrostowski was the main developer and maintainer of the package up to version 0.1.0. Parts of this paper are based on Łukasz’s Master’s thesis (available at https://github.com/ ncn-foreigners/graduation-theses).