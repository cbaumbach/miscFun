## miscFun.R

pr <- function(...)
{
    cat(..., "\n", sep = "")
}

pr1 <- function(...)
{
    cat(..., sep = "")
}

exit <- function(status = 0L, ...)
{
    quit(save = "no", status = status, ...)
}

flip <- function(f)
{
    args <- formals(f)

    if (is.null(args) || length(args) == 1L && names(args) != "...")
        stop("Function must take at least 2 arguments.")

    if ("..." %in% names(formals(f))[1:2])
        stop("Function must not have \"...\" as 1st or 2nd argument.")

    ## Swap 1st and 2nd argument.
    names(formals(f))[1:2] <- names(formals(f))[2:1]
    f
}

nuniq <- function(x)
{
    length(unique(x))
}

flatten_csv <- function(x, sep = ",", fixed = TRUE)
{
    unlist(strsplit(x, split = sep, fixed = fixed, perl = !fixed),
           use.names = FALSE)
}

nfields <- function(file, sep = "\t")
{
    length(scan(file, what = character(), sep = sep, nlines = 1L,
                quiet = TRUE))
}

colClasses <- function(fmt)
{
    fmt <- gsub(" ", "", fmt)
    if (typeof(fmt) != "character" || is.na(fmt) ||
            length(fmt) != 1L || nchar(fmt) == 0L) {
        stop("fmt must be a non-empty, non-NA character vector of length 1")
    }
    v <- NULL
    map <- c(N = "NULL",
             c = "character",
             i = "integer",
             n = "numeric",
             l = "logical",
             r = "raw",
             x = "complex")
    lst <- flatten_csv(fmt, sep = "(?<=[[:alpha:]])", fixed = FALSE)
    for (elt in lst) {
        if (nchar(elt) > 1L) {
            n <- as.integer(substr(elt, 1L, nchar(elt) - 1L))
            what <- substr(elt, nchar(elt), nchar(elt))
        }
        else {
            n <- 1L
            what <- elt
        }
        v <- c(rep(map[what], n), v)
    }
    unname(rev(v))
}

ave2 <- function (x, factors, f, ...)
{
    g <- interaction(factors)
    split(x, g) <- lapply(split(x, g), f, ...)
    x
}

rename <- function(x, old2new)
{
    old <- names(x)
    idx <- old %in% names(old2new)
    names(x)[idx] <- old2new[ old[idx] ]
    x
}

## | PARAMETER  | MEANING                                       |
## |------------+-----------------------------------------------|
## | POS        | positions (integer vector)                    |
## | START      | interval start positions (integer vector)     |
## | END        | interval end positions (integer vector)       |
## | ID         | interval ids                                  |
## | BATCH.SIZE | number of positions to process simultaneously |
##
## For every position POS[i], return a comma-separated string of all
## intervals ID[j] such that START[j] <= POS[i] <= END[j].  Process
## BATCH.SIZE elements of POS at a time.
match_intervals <- function(pos, start, end, id, batch.size = 1000L, quiet = FALSE)
{
    stopifnot(length(start) > 0L)
    stopifnot(length(end) == length(start))
    stopifnot(length(id) == length(start))
    stopifnot(length(pos) > 0L)

    ## Remove intervals with missings.
    is_missing <- is.na(start) | is.na(end) | is.na(id)
    start <- start[!is_missing]
    end   <- end  [!is_missing]
    id    <- id   [!is_missing]
    if (length(start) == 0L)
        return(rep("", length(pos)))

    fun <- function(pos)
    {
        ## Create a matrix with length(START) rows and length(POS)
        ## columns, where every row is equal to POS.  For example,
        ## If pos = c(1, 2, 3, 4), and length(start) = 3, then
        ##       |1 2 3 4|
        ## mat = |1 2 3 4|
        ##       |1 2 3 4|
        mat <- matrix(pos, ncol = length(pos), nrow = length(start),
                      byrow = TRUE)
        ## Compare every column with interval start and end positions.
        ## The result of the comparison is a matrix of the same
        ## dimensions as MAT.
        value <- start <= mat & mat <= end
        ## For every column of VALUE, find the TRUE indexes.
        idx_list <- lapply(split(value, col(value)), which)
        ## For every position in POS, collapse the names of intervals
        ## overlapping that positions into a comma-separated string.
        sapply(idx_list,
               function(x)
                   paste(sort(unique(id[x])), collapse = ","))
    }
    result <- NULL
    n <- length(pos)
    if (n < batch.size) batch.size <- n
    nbatch <- (n - 1L) %/% batch.size + 1L
    last_batch_size <- n - (nbatch - 1L) * batch.size
    if (!quiet)
        pr("Number of batches: ", nbatch)
    for (i in 1:nbatch) {
        if (!quiet)
            cat(i, "")
        first <- 1L + (i - 1L) * batch.size
        last <- if (i == nbatch) n else first + batch.size - 1L
        x <- pos[seq(first, last)]
        result <- c(list(fun(x)), result)
    }
    if (!quiet)
        cat("\n\n")
    v <- unname(do.call(c, rev(result)))
    stopifnot(length(v) == length(pos))
    v[is.na(pos)] <- NA
    v
}

find_duplicates <- function(d, cols, select = colnames(d), sep = "\t")
{
    if (nrow(d) == 0L) return(d[,select])

    if (any(! cols %in% colnames(d)))
        stop("nonexistent variables in COLS")

    if (any(! select %in% colnames(d)))
        stop("nonexistent variables in SELECT")

    if (length(cols) == 1L) {
        x <- d[,cols]
        all_na <- is.na(d[,cols])
    }
    else {
        x <- unname(apply(d[,cols], 1L, paste, collapse = sep))
        all_na <- apply(d[,cols], 1L, function(x) all(is.na(x)))
    }
    d[x %in% x[!all_na & duplicated(x)], select, drop = FALSE]
}

rev_map <- function(map)
{
    values <- unlist(map, use.names = FALSE)
    times <- sapply(map, length)
    names(values) <- rep(names(map), times = times)
    lapply(split(names(values), values), sort)
}

one_per_row <- function(d, cols, sep = ",")
{
    if (length(cols) == 0L)
        stop("COLS must be of length >= 1")
    the_col <- cols[1L]
    x <- strsplit(d[,the_col], sep)
    times <- sapply(x, length)
    d <- d[rep(seq_len(nrow(d)), times = times),]
    d[the_col] <- unname(unlist(x))
    rownames(d) <- NULL
    if (length(cols) > 1)
        return(one_per_row(d, cols = cols[-1], sep = sep))
    d
}

one_per_element <- function(x, sep = ",", perl = FALSE)
{
    unname(unlist(strsplit(x, split = sep, fixed = !perl, perl = perl)))
}

all_neighbors <- function(f, ...)
{
    force(f)

    lists <- list(...)
    if (length(lists) == 1L)
        x <- lists[[1L]]
    else
        x <- do.call(c, lists)

    ## If we get through the call to `Reduce' without an error being
    ## thrown, all direct neighbors in `x', when compared by `f',
    ## yield TRUE, that is, the predicate `f' holds throughout `x' and
    ## we return TRUE.  Otherwise one pair of direct neighbors
    ## compared FALSE, that is, the predicate `f' does not hold
    ## throughout all of `x' and we return FALSE.
    tryCatch({
        Reduce(function(x,y) if (f(x,y)) y else stop(), x)
        TRUE
    }, error = function(e) FALSE)
}

double_quote <- function(x)
{
    x <- gsub("\"", "\\\"", x, fixed = TRUE)
    paste0("\"", x, "\"")
}

single_quote <- function(x)
{
    x <- gsub("'", "\\\'", x, fixed = TRUE)
    paste0("'", x, "'")
}

noop <- function(...)
{
    invisible(NULL)
}

make_observer <- function()
{
    e <- new.env()

    function (x, show = FALSE)
    {
        if (show)
            return(ls(envir = e))

        if (length(x) != 1L
            || length(class(x)) != 1L
            || ! class(x) %in% c("character", "numeric", "integer"))
            stop("Argument must be of length 1 and ",
                 "class \"character\", \"numeric\", or \"integer\".")

        if (! class(x) == "character")
            x <- as.character(x)

        if (exists(x, e, inherits = FALSE))
            return(TRUE)

        assign(x, TRUE, envir = e, inherits = FALSE)
        return(FALSE)
    }
}

group_words <- function(x, n, split = "\\s+", perl = TRUE,
                        fixed = !perl, sep = " ", special = NULL,
                        special_sep = "", trim_leading = TRUE,
                        trim_trailing = trim_leading, tol = 0L,
                        punct = TRUE)
{
    if (n < 1L)
        stop("N must be an integer >= 1.")

    if (!fixed && !perl)
        stop("Either FIXED or PERL must be TRUE.")

    if (fixed && perl) {
        fixed <- FALSE
        warning("FIXED conflicts with PERL: setting FIXED to FALSE.")
    }

    f <- function(xs)
    {
        g <- function(acc, word)
        {
            ## Check if current line ends in a special way.
            is_special <- FALSE
            if (!is.null(special))
                is_special <- grepl(special, acc[1L])

            ## Current chunk has enough space for word.
            if (nchar(acc[1L]) + nchar(word) + nchar(sep) <= n + tol)
                ## Add word to current chunk.
                acc[1L] <- paste(acc[1L], word,
                                 sep = if (is_special) special_sep
                                       else sep)
            ## Add punctuation sign to previous chunk.
            else if (grepl("^[[:punct:]]$", word))
                acc[1L] <- paste(acc[1L], word, sep = "")
            ## Start new chunk.
            else {
                if (trim_leading)
                    word <- sub("^\\s+", "", word, perl = TRUE)
                acc <- c(word, acc)
            }

            acc
        }
        xs <- rev(Reduce(g, xs))

        ## Trim whitespace.
        if (trim_trailing)
            xs <- sub("\\s+$", "", xs, perl = TRUE)

        xs
    }

    lapply(strsplit(x, split, fixed, perl), f)
}

wrap_lines <- function(x, n, sep = "\n", max_lines = Inf, dots = "...",
                       hard = FALSE, tol = if (hard) 0L else 3L)
{
    if (nchar(dots) >= n)
        stop("DOTS must have < N characters.")

    ## Split into lines.
    if (hard)
        y <- group_words(x, n, split = NULL, sep = "", tol = tol)
    else
        y <- group_words(x, n, split = "\\s+|(?<=-)", special = "-$",
                         tol = tol)

    ## Truncate to at most `max_lines' lines and paste together using
    ## `sep' as line separator.
    f <- function(lines)
    {
        if (length(lines) <= 1L) return(lines)

        if (length(lines) > max_lines) { # too many lines
            ## Keep only the first max_lines lines.
            lines <- lines[seq_len(max_lines)]
            ## Insert dots in last line.
            last <- lines[max_lines]
            substr(last, n - nchar(dots) + 1L, n) <- dots
            lines[max_lines] <- substr(last, 1L, n)
        }
        paste(lines, collapse = sep)
    }

    sapply(y, f)
}

is.directory <- function(fs)
{
    file.info(fs)$isdir
}
