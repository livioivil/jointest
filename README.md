<!-- README.md is generated from README.Rmd. Please edit that file -->

# jointest R package

A package devoted to multivariate resampling-based tests. By resampling jointly on all univariate tests (e.g., sign-flip score tests by Hemerik, Goeman and Finos (2020) <doi.org/10.1111/rssb.12369>) it allows for multivariate and selective inference – i.e.,, weak and strong control of the familywise error rate or confidence bounds for true discovery proportion.

<!-- #[library flipscores on CRAN](http://cran.r-project.org/web/packages/flipscores/index.html) -->

------------------------------------------------------------------------

## Set up

To **install** this GitHub version type (in `R`):

``` r
##if devtools is not installed yet: 
## install.packages("devtools") 
library(devtools)
install_github("livioivil/jointest")
library(jointest)
```

## A (minimal) example of the `join_flipscores` function

Simulate data:

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
```

Testing $X$ for each model:

``` r
res=join_flipscores(mods,n_flips = 5000, seed = 1, tested_coeffs = "X")
summary(res)
   Model Coeff  Estimate    Score Std. Error  z value Part. Cor          p
 1  mod1     X 1.1254046 19.78375   6.309305 3.135646 0.7605058 0.01499700
 2  mod2     X 0.9552644 15.38036   5.053879 3.043278 0.7608195 0.01339732
 3  mod3     X 1.0121921 14.31866   4.836669 2.960437 0.7643816 0.01279744
 4  mod4     X 1.1696906 17.62979   5.875380 3.000621 0.7501552 0.00979804
```

add adjusted $p$-values:
``` r
res=p.adjust(res)
summary(res)
   Model Coeff  Estimate    Score Std. Error  z value Part. Cor          p
 1  mod1     X 1.1254046 19.78375   6.309305 3.135646 0.7605058 0.01499700
 2  mod2     X 0.9552644 15.38036   5.053879 3.043278 0.7608195 0.01339732
 3  mod3     X 1.0121921 14.31866   4.836669 2.960437 0.7643816 0.01279744
 4  mod4     X 1.1696906 17.62979   5.875380 3.000621 0.7501552 0.00979804
      p.adj
 1 0.019996
 2 0.019996
 3 0.019996
 4 0.019996
```

look at the combined results:

``` r
summary(combine(res))
     Model Coeff Stat nMods        S          p
 1 Overall     X maxT     4 19.78375 0.00739852
```

plot the results:

``` r
plot(res, p.values = "adjusted")
```

## A (minimal) example of the `flip2sss` function

Simulate data:

``` r
N=20
n=rpois(N,20)
reff=rep(rnorm(N),n)

D=data.frame(X1=rnorm(length(reff)),
             X2=rep(rnorm(N),n),
             Grp=factor(rep(rep(LETTERS[1:3],length.out=N),n)),
             SOGG=rep(1:N,n))
D$Y=rbinom(n=nrow(D),prob=plogis( 2*D$X1 * (D$Grp=="B") +  2*D$X2+reff),size=1)
```

Define model of interest:
``` r
formula <- Y ~ Grp * X1 + X2
```
Define clusters structure:
``` r
cluster <- factor(D$SOGG)
```

Define the summary statistics (here we propose the `glm` with firth correction from the `logistf` package)

``` r
summstats_within <- 'logistf::logistf(Y ~ X1, family = binomial(link = "logit"), control=logistf::logistf.control(maxit=100))'
```
however also the classic `glm` function can be used:


``` r
#summstats_within <- 'glm(Y ~ X1, family = binomial(link = "logit"))'
```

Then, compute the $2$-Stage Summary Statistics approach specifying the summary statistics:

``` r
res <- flip2sss(formula, D, cluster, summstats_within=summstats_within)
```
or the family:

``` r
res <- flip2sss(formula, D, cluster, family = "binomial")
```

Look at the results:

``` r
res <- flip2sss(formula, D, cluster, family="binomial")
summary(res)
        Model       Coeff   Estimate       Score Std. Error     z value   Part. Cor          p
1 .Intercept. (Intercept)  4.3128538  29.0261330  24.152830  1.20176943  0.29147190 0.16116777
2 .Intercept.        GrpB -0.3824975  -1.1599283  15.511347 -0.07477934 -0.01813665 0.94741052
3 .Intercept.        GrpC -1.7121985  -5.5300048  16.061350 -0.34430510 -0.08350625 0.68066387
4 .Intercept.          X2  9.5446012 151.9594953  51.196709  2.96814969  0.71988204 0.00579884
5          X1          X1  0.2882803   2.0179621   2.658795  0.75897611  0.17889239 0.18636273
6          X1     GrpB:X1  1.1367411   3.9785937   2.073856  1.91845180  0.45218343 0.03879224
7          X1     GrpC:X1  0.1228905   0.3970308   1.779620  0.22309867  0.05258486 0.78544291
```

add the adjusted p.values:

``` r
res<-p.adjust(res)
summary(res)
        Model       Coeff   Estimate       Score Std. Error     z value   Part. Cor          p    p.adj
1 .Intercept. (Intercept)  4.3128538  29.0261330  24.152830  1.20176943  0.29147190 0.16116777 0.199960
2 .Intercept.        GrpB -0.3824975  -1.1599283  15.511347 -0.07477934 -0.01813665 0.94741052 1.000000
3 .Intercept.        GrpC -1.7121985  -5.5300048  16.061350 -0.34430510 -0.08350625 0.68066387 1.000000
4 .Intercept.          X2  9.5446012 151.9594953  51.196709  2.96814969  0.71988204 0.00579884 0.009998
5          X1          X1  0.2882803   2.0179621   2.658795  0.75897611  0.17889239 0.18636273 1.000000
6          X1     GrpB:X1  1.1367411   3.9785937   2.073856  1.91845180  0.45218343 0.03879224 1.000000
7          X1     GrpC:X1  0.1228905   0.3970308   1.779620  0.22309867  0.05258486 0.78544291 1.000000
```

plot the results:

``` r
plot(res, p.values = "adjusted")
```

look at the combined results:

``` r
summary(combine_contrasts(res))
          Model       Coeff Stat nMods          S          p
1 .Intercept..0 (Intercept) maxT     1  29.026133 0.08198360
2 .Intercept..1         Grp maxT     2  -1.159928 0.72145571
3 .Intercept..2          X2 maxT     1 151.959495 0.00359928
4          X1.0          X1 maxT     1   2.017962 0.09078184
5          X1.1      Grp:X1 maxT     2   3.978594 0.01959608
```

## References

Hemerik, J., Goeman, J. J., & Finos, L. (2020). Robust testing in generalized linear models by sign flipping score contributions. Journal of the Royal Statistical Society Series B: Statistical Methodology, 82(3), 841-864. <https://doi.org/10.1111/rssb.12369>

De Santis, R., Goeman, J. J., Hemerik, J., Davenport, S., & Finos, L. (2022). Inference in generalized linear models with robustness to misspecified variances. arXiv preprint arXiv:2209.13918. <https://arxiv.org/abs/2209.13918>

Girardi, P., Vesely, A., Lakens, D., Altoè, G., Pastore, M., Calcagnì, A., & Finos, L. (2024). Post-selection Inference in Multiverse Analysis (PIMA): An Inferential Framework Based on the Sign Flipping Score Test. Psychometrika, 1-27. <https://doi.org/10.1007/s11336-024-09973-6> 

## Bug reports

If you encounter a bug, please file a
[reprex](https://github.com/tidyverse/reprex) (minimal reproducible
example) on [github](https://github.com/livioivil/jointest/issues).
