# Loading packages and functions -----------------------------------------------

library(gamlss)
source("ZI_UQChen.R") # zero inflated unit quantile chen functions

# ZI UChen in gamlss.family ----------------------------------------------------

# derivatives of log.UQChen
UQC <- expression(
  log(log(tau) * sigma / (1 - exp((-log(mu))^sigma))) - log(y) + (sigma - 1) * log(-log(y)) +
    (-log(y))^sigma + log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(y))^sigma))
)
m1 <- D(UQC, "mu")
s1 <- D(UQC, "sigma")
ms2 <- D(m1, "sigma")

# Gamlss family function
ZIUQC <- function(mu.link = "logit", sigma.link = "log", nu.link = "logit") {
  mstats <- checklink(
    "mu.link", "ZIUQC", substitute(mu.link),
    c("logit", "probit", "cloglog", "log", "own")
  )
  dstats <- checklink(
    "sigma.link", "ZIUQC", substitute(sigma.link),
    c("inverse", "log", "identity")
  )
  vstats <- checklink(
    "nu.link", "ZIUQC", substitute(nu.link),
    c("logit", "probit", "cloglog", "log", "own", "identity")
  )
  structure(
    list(
      family = c("ZIUQC", "Zero Inflated Unit Quantile Chen"),
      parameters = list(mu = TRUE, sigma = TRUE, nu = TRUE),
      nopar = 3, type = "Mixed",
      mu.link = as.character(substitute(mu.link)),
      sigma.link = as.character(substitute(sigma.link)),
      nu.link = as.character(substitute(nu.link)),
      mu.linkfun = mstats$linkfun, sigma.linkfun = dstats$linkfun, nu.linkfun = vstats$linkfun,
      mu.linkinv = mstats$linkinv, sigma.linkinv = dstats$linkinv, nu.linkinv = vstats$linkinv,
      mu.dr = mstats$mu.eta, sigma.dr = dstats$mu.eta, nu.dr = vstats$mu.eta,
      dldm = function(y, mu, sigma) {
        tau <- .5
        dldm <- ifelse((y == 0), 0, eval(m1))
        dldm
      }, d2ldm2 = function(y, mu, sigma) {
        tau <- .5
        dldm <- eval(m1)
        d2ldm2 <- ifelse((y == 0), 0, -dldm * dldm)
        d2ldm2
      }, dldd = function(y, mu, sigma) {
        tau <- .5
        dldd <- ifelse((y == 0), 0, eval(s1))
        dldd
      }, d2ldd2 = function(y, mu, sigma) {
        tau <- .5
        dldd <- ifelse(is.nan(eval(s1)), 0, eval(s1))
        d2ldd2 <- ifelse((y == 0), 0, -(dldd * dldd))
        d2ldd2
      }, dldv = function(y, nu) {
        dldv <- ifelse(y == 0, 1 / nu, -1 / (1 - nu))
        dldv
      }, d2ldv2 = function(nu) {
        d2ldv2 <- -1 / (nu * (1 - nu))
        d2ldv2
      }, d2ldmdd = function(y, mu, sigma) {
        tau <- .5
        dldm <- eval(m1)
        dldd <- ifelse(is.nan(eval(s1)), 0, eval(s1))
        d2ldmdd <- ifelse((y == 0), 0, -(dldm * dldd))
        d2ldmdd
      }, d2ldmdv = function(y) {
        d2ldmdv <- rep(0, length(y))
        d2ldmdv
      }, d2ldddv = function(y) {
        d2ldddv <- rep(0, length(y))
        d2ldddv
      }, G.dev.incr = function(y, mu, sigma, nu, ...) {
        -2 * dZIUQC(y, mu, sigma, nu, log = TRUE)
      }, rqres = expression({
        uval <- ifelse(y == 0, nu * runif(length(y), 0, 1),
          (1 - nu) * pZIUQC(y, mu, sigma, nu)
        )
        rqres <- qnorm(uval)
      }),
      mu.initial = expression(mu <- rep(median(y), length(y))),
      sigma.initial = expression(sigma <- rep(1, length(y))),
      nu.initial = expression(nu <- rep(0.3, length(y))),
      mu.valid = function(mu) all(mu > 0 & mu < 1),
      sigma.valid = function(sigma) all(sigma > 0),
      nu.valid = function(nu) all(nu > 0 & nu < 1),
      y.valid = function(y) all(y >= 0 & y < 1)
    ),
    class = c("gamlss.family", "family")
  )
}