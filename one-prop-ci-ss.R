one.prop.ci.ss <- function (precision, p = 0.5, conf = 0.95, cc = FALSE, N = NULL, two.sided = TRUE) {
    if (precision == 0) stop("bad input value for precision")
    if (precision < 0 | precision > 1) stop("input value for precision not between 0 and 1")
    if (p < 0 | p > 1) stop("input value for p not between 0 and 1")
    if (conf < 0 | conf > 1) stop("input value for conf not between 0 and 1")
    if (!is.null(N)) {
        is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) abs(x - round(x)) < tol
        if (!is.wholenumber(N)) stop("input value for N not an integer")
        if (N < 0) stop("input value for N not positive")
    }
    d <- precision
    z <- qnorm((1 + conf) / 2)
    if (!two.sided) z <- qnorm(conf)
    if (is.null(N)) {
        N <- "Infinite"
        if (!cc) res <- ceiling(p * (1 - p) * (z / d) ^ 2)
        else res <- ceiling((z ^ 2 * p * (1 - p) + sqrt((d + z ^ 2* p * (1 - p)) ^ 2 - d ^ 2) + d) / (2 * d ^ 2))
    }
    else if (!cc) res <- ceiling(p * (1 - p) * z ^ 2 * N / (p * (1 - p) * z ^ 2 + d ^ 2 * (N - 1)))
    else res <- ceiling((N * (d + p * (1 - p) * z ^ 2) + sqrt(N * p * (1 - p) * z ^ 2 * (2 * N * d + N * p * (1 - p) * z ^ 2 - 1))) / (2 * ( d ^ 2 * N + p * (1 - p) * z ^ 2)))
    header <- "Results for Two-Sided Confidence Interval for One Proportion"
    if (!two.sided) header <- "Results for One-Sided Confidence Interval for One Proportion"
    cat("\n", header, "\n", "\n")
    cat(" confidence level =", conf, " sample size =", res, " p =", p, " N =", N, "\n")
}