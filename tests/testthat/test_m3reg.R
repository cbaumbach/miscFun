context("Regressions for multiple outcomes, exposures, and adjustments.")

test_that("find a better description", {
    set.seed(12345L)
    n <- nrow(MASS::quine)
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    x3 <- rpois(n, 20)
    f1 <- cut(rnorm(n), c(-Inf, -1, 0, 1, Inf), labels = FALSE)
    f2 <- cut(rnorm(n), c(-Inf, 0, Inf), labels = FALSE)
    y1 <- 1 + 2 * x1 + 3 * x2 + 4 * f1 + 5 * f2
    y2 <- x1 < 0
    flipped <- sample.int(n, n/20)
    y2[flipped] <- 1L - y2[flipped]

    fm <- glm.nb(Days ~ ., data = MASS::quine)
    y3 <- rnegbin(fitted(fm), n, theta = 4.5)

    d <- data.frame(x1 = x1, x2 = x2, f1 = f1, f2 = f2,
                    y1 = y1, y2 = y2, y3 = y3)

    x <- m3reg(outcome = c("y1", "y2", "y3"),
               exposure = c("x1", "f1"),
               adjustment = list("1", "x2", "f2", c("x2", "f2")),
               data = d,
               type = c("linear", "logistic", "negative_binomial"))


    test_that(TRUE, is_false())
})
