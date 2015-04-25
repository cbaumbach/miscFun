model_funs <- list(
    linear            = lm,
    logistic          = function(...) glm(..., family = binomial),
    poisson           = function(...) glm(..., family = poisson),
    quasipoisson      = function(...) glm(..., family = quasipoisson),
    negative_binomial = glm.nb)

m3reg <- function(outcome, exposure, adjustment = NULL, data, type)
{
    stopifnot(!any(duplicated(outcome)))

    ## Avoid the special case where `adjustment' is NULL by specifying
    ## an explicit intercept instead.
    if (is.null(adjustment))
        adjustment <- list("1")

    if (length(type) == 1L)
        type <- rep_len(type, length(outcome))

    names(type) <- outcome

    stopifnot(all(type %in% names(model_funs)))

    olen <- length(outcome)
    elen <- length(exposure)
    alen <- length(adjustment)

    models <- vector("list", olen * elen * alen)

    ## Fit and collect models.
    i <- 1L
    for (o in outcome) {
        for (e in exposure) {
            for (a in adjustment) {

                ## Create formula string.
                covars <- paste(c(e, a), collapse = " + ")
                model <- paste0(o, " ~ ", covars)
                fmla <- as.formula(model, env = list2env(data))

                ## Select function for fitting the model.
                f <- model_funs[[type[o]]]

                ## Fit model and save.
                m <- f(fmla)
                models[[i]] <- m
                i <- i + 1L
            }
        }
    }

    names(models) <- vapply(models, model2string, character(1))

    structure(models,
              dim = c(olen, elen, alen),
              dimnames = list(outcome, exposure, adjustment),
              class = c("m3reg", class(models)))
}

summary.m3reg <- function(x)
{
    do.call(rbind, lapply(names(x), function(m)
    {
        y <- summary(x[[m]])$coefficients
        pval <- y[2L, 4L]
        stars <- ifelse(pval <= .001, "***",
                 ifelse(pval <= .01, "**",
                 ifelse(pval <= .05, "*", " ")))
        data.frame(model = m,
                   beta  = sprintf("%.2f", y[2L, 1L]),
                   se    = sprintf("%.2f", y[2L, 2L]),
                   pval  = sprintf("%.2e", pval),
                   " "   = stars,
                   check.names = FALSE)
    }))
}

model2string <- function(m)
{
    f <- function(acc, x)
    {
        x <- sub("^ \\+", "", x)        # trim leading spaces
        x <- sub(" \\+$", "", x)        # trim trailing spaces
        x <- gsub(" \\+", " ", x)       # collapse multiple spaces
        paste(acc, x)
    }

    Reduce(f, deparse(formula(m)))
}
