## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(5118)

## ----data, message=F----------------------------------------------------------
library(conformalbayes)
library(rstanarm)
library(ggplot2)

sim_data = function(n=50) {
    x = rnorm(n)
    y = 3 - 2*x + rt(n, df=2)
    data.frame(x=x, y=y)
}

d_fit = sim_data()

ggplot(d_fit, aes(x, y)) +
    geom_point() +
    geom_smooth(method=lm, formula=y~x)

## ----cover-model--------------------------------------------------------------
# fit the model
m = stan_glm(y ~ x, data=d_fit, chains=1, refresh=0)

d_test = sim_data(2000)

interv_model = predictive_interval(m, newdata=d_test, prob=0.50)

# are the points covered
covered_model = with(d_test, interv_model[, 1] <= y & y <= interv_model[, 2])

ggplot(d_test, aes(x, y, color=covered_model, group=1)) +
    geom_point(size=0.4) +
    geom_linerange(aes(ymin=interv_model[, 1],
                       ymax=interv_model[, 2]), alpha=0.4) +
    labs(color="Covered?") +
    geom_smooth(method=lm, formula=y~x, color="black")

## ---- warning=FALSE-----------------------------------------------------------
m = loo_conformal(m)
print(m)

## ----cover-jack---------------------------------------------------------------
interv_jack = predictive_interval(m, newdata=d_test, prob=0.50)

# are the points covered
covered_jack = with(d_test, interv_jack[, 1] <= y & y <= interv_jack[, 2])

ggplot(d_test, aes(x, y, color=covered_jack, group=1)) +
    geom_point(size=0.4) +
    geom_linerange(aes(ymin=interv_jack[, 1],
                       ymax=interv_jack[, 2]), alpha=0.4) +
    labs(color="Covered?") +
    geom_smooth(method=lm, formula=y~x, color="black")

