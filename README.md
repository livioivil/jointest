<!-- README.md is generated from README.Rmd. Please edit that file -->

# Welcome to the dev-version of jointest R package

<!-- #[library flipscores on CRAN](http://cran.r-project.org/web/packages/flipscores/index.html) -->

------------------------------------------------------------------------

## Set up

To **install** this github version type (in R):

    ##if devtools is not installed yet: 
    ## install.packages("devtools") 
    library(devtools)
    install_github("livioivil/jointest")

<!-- ## Some examples -->

## A (minimal) example

``` r
n=20
set.seed(123)
D=data.frame(X=rnorm(n),Z1=rnorm(n),Z2=rnorm(n))
D$Y=D$Z1+D$X+rnorm(n)
mod1=glm(Y~X+Z1+Z2,data=D)
mod2=glm(Y~X+poly(Z1,2)+Z2,data=D)
mod3=glm(Y~X+poly(Z1,2)+poly(Z2,2),data=D)
mod4=glm(Y~X+Z1+poly(Z2,2),data=D)
mods=list(mod1=mod1,mod2=mod2,mod3=mod3,mod4=mod4)
for(i in 1:length(mods))
 mods[[i]]$call$data=eval(D)
library(jointest)
res=join_flipscores(mods,n_flips = 5000,        seed = 1, tested_coeffs = "X")
summary(res)
#>   Model Coeff  Estimate    Score Std. Error  z value eff_size   Pr(>|t|)
#> 1  mod1     X 1.1254046 19.78375   6.309305 3.135646 3.401085 0.01439712
#> 2  mod2     X 0.9552644 15.38036   5.053879 3.043278 3.402488 0.01419716
#> 3  mod3     X 1.0121921 14.31866   4.836669 2.960437 3.418419 0.01699660
#> 4  mod4     X 1.1696906 17.62979   5.875380 3.000621 3.354796 0.01279744
summary(combine(res))
#>   Coeff Stat nMods        S          p
#> 1     X maxT     4 19.78375 0.01619676
res=p.adjust.fwer(res)
summary(res)
#>   Model Coeff  Estimate    Score Std. Error  z value eff_size   Pr(>|t|)
#> 1  mod1     X 1.1254046 19.78375   6.309305 3.135646 3.401085 0.01439712
#> 2  mod2     X 0.9552644 15.38036   5.053879 3.043278 3.402488 0.01419716
#> 3  mod3     X 1.0121921 14.31866   4.836669 2.960437 3.418419 0.01699660
#> 4  mod4     X 1.1696906 17.62979   5.875380 3.000621 3.354796 0.01279744
#>        p.adj
#> 1 0.01619676
#> 2 0.01799640
#> 3 0.01799640
#> 4 0.01619676
```

## References

J Hemerik, JJ Goeman and L Finos (2019) Robust testing in generalized
linear models by sign-flipping score contributions. Journal of the Royal
Statistical Society Series B: Statistical Methodology, Volume 82, Issue
3, July 2020, Pages 841–864.  
<https://doi.org/10.1111/rssb.12369>

R De Santis, J Goeman, J Hemerik, L Finos (2022) Inference in
generalized linear models with robustness to misspecified variances
arXiv: 2209.13918.  
<https://arxiv.org/abs/2209.13918>

P Girardi, A Vesely, D Lakens, G Altoè, M Pastore, A Calcagnì, L Finos
(2022) Post-selection Inference in Multiverse Analysis (PIMA): an
inferential framework based on the sign flipping score test. arxiv:
2210.02794.  
<https://arxiv.org/abs/2210.02794>

## Bug reports

If you encounter a bug, please file a
[reprex](https://github.com/tidyverse/reprex) (minimal reproducible
example) on [github](https://github.com/livioivil/jointest/issues).
