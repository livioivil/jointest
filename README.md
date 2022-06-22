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
res=join_flipscores(mods,n_flips = 5000, score_type = "effective" ,
       seed = 1, tested_coeffs = "X")
summary(res)
#>   Model Coeff  Estimate    Score Std. Error   z value  eff_size Pr(>|t|)
#> 1  mod1     X 1.1254046 1.120607   1.895638 0.5911502 0.1926470   0.0046
#> 2  mod2     X 0.9552644 1.621551   1.404619 1.1544417 0.3587243   0.0032
#> 3  mod3     X 1.0121921 2.048937   1.215666 1.6854434 0.4891608   0.0028
#> 4  mod4     X 1.1696906 1.587393   1.505141 1.0546473 0.3020672   0.0040
summary(combine(res))
#>   Coeff Stat nMods        S     p
#> 1     X maxT     4 2.048937 0.007
res=p.adjust.fwer(res)
summary(res)
#>   Model Coeff  Estimate    Score Std. Error   z value  eff_size Pr(>|t|)  p.adj
#> 1  mod1     X 1.1254046 1.120607   1.895638 0.5911502 0.1926470   0.0046 0.0114
#> 2  mod2     X 0.9552644 1.621551   1.404619 1.1544417 0.3587243   0.0032 0.0114
#> 3  mod3     X 1.0121921 2.048937   1.215666 1.6854434 0.4891608   0.0028 0.0070
#> 4  mod4     X 1.1696906 1.587393   1.505141 1.0546473 0.3020672   0.0040 0.0114
```

## References

J Hemerik, JJ Goeman and L Finos (2019) Robust testing in generalized
linear models by sign-flipping score contributions, Submitted, arXiv:
1909.03796

<https://arxiv.org/abs/1909.03796>

## Bug reports

If you encounter a bug, please file a
[reprex](https://github.com/tidyverse/reprex) (minimal reproducible
example) on [github](https://github.com/livioivil/jointest/issues).
