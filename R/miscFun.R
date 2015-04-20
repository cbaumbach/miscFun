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
    if (missing(factors))
        stop("`factors' must not be missing.")
    else if (is.null(factors))
        stop("`factors' must not be `NULL'.")
    else if (length(factors) == 0L)
        stop("`factors' must not be of length 0.")

    g <- interaction(factors)
    split(x, g) <- lapply(split(x, g), f, ...)
    x
}

rename <- function(x, old2new, warn = TRUE)
{
    old_names <- new_names <- names(x)

    if (is.null(names(old2new)))
        return(old_names)

    ## Warn about name tags in `old2new' that don't match any element
    ## of `names(x)'.
    if (warn) {
        matching <- names(old2new) %in% old_names
        if (any(!matching)) {
            non_matching <- paste(vapply(names(old2new)[!matching],
                                         double_quote, character(1L)),
                                  collapse = ", ")
            warning("The following name tags from `old2new' don't ",
                    "match any element of `names(x)': ", non_matching)
        }
    }

    idx <- old_names %in% names(old2new)
    new_names[idx] <- old2new[ old_names[idx] ]
    new_names
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

read.tables <- function(files, sep = NULL, ncore = 1L, header = TRUE,
                        colClasses = "character", verbose = TRUE,
                        check.names = FALSE, comment.char = "#", ...)
{
    ## Drop empty files.
    files <- Filter(function(x) file.info(x)$size > 0L, files)

    stopifnot(length(files) > 0L,
              all(vapply(files, file.exists, logical(1L))))

    ## =================================================================
    ## Determine field separator from one of the files.
    ## =================================================================
    if (is.null(sep)) {

        is_comment <- function(x)
        {
            grepl(paste0("^", comment.char), x)
        }

        ## Find a non-comment, non-empty line.
        for (i in seq_along(files)) {

            con <- file(files[i], "r")
            tryCatch({
                line <- readLines(con, 1L) # read 1st line
                while (length(line) && nchar(line) && is_comment(line))
                    line <- readLines(con, n = 1L) # read another line
            }, finally = close(con))

            if (length(line) && nchar(line))
                break            # found a non-comment, non-empty line
        }

        sep <- match1of(c("\t", ",", ";", " "), # candidate seps
                        line, fixed = TRUE)

        if (is.na(sep))
            stop("Unable to figure out field separator in ", files[i])
    }

    ## =================================================================
    ## Read and concatenate tables.
    ## =================================================================
    f <- function(filename)
    {
        if (verbose)
            pr("Reading ", filename)

        read.table(filename, sep = sep, colClasses = colClasses,
                   header = header, comment.char = comment.char, ...)
    }

    do.call(rbind, parallel::mclapply(
        files, f, mc.preschedule = FALSE, mc.cores = ncore,
        mc.silent = FALSE))
}

wait_until <- function(date_string)
{
    deadline <- strptime(date_string, format = "%d-%m-%y %H:%M")

    if (is.na(deadline))
        stop("Unable to convert to date: ", date_string)

    ## Check whether deadline has arrived.
    past_deadline <- function()
    {
        difftime(Sys.time(), deadline) > 0
    }

    if (past_deadline())
        stop("We're already past the deadline.")

    cat("Waiting until", date_string, "")

    ## Wait until deadline.
    while (!past_deadline()) {
        Sys.sleep(60)
        cat(".")
    }
    pr()
}

match1of <- function(candidates, x, ...)
{
    if (length(candidates) == 0L || !is.character(candidates))
        stop("`candidates' must be a character vector of length >= 1.")

    if (length(x) == 0L || !is.character(x))
        stop("`x' must be a character vector of length >= 1.")

    ## If length(x) > 1, create a length(x)-by-length(candidates)
    ## matrix such that the entry in row i and column j is TRUE iff
    ## candidate[j] matches in x[i].  If length(x) == 1, return a
    ## logical vector of the same length as `candidates'.
    hit_matrix <- vapply(candidates, grepl, logical(length(x)), x, ...)

    ## Convert `hit_matrix' to matrix, if necessary.
    if (length(x) == 1L)
        hit_matrix <- t(as.matrix(hit_matrix))

    apply(hit_matrix, 1L, function(hits)
    {
        if (!any(hits)) NA_character_
        else            candidates[which.max(hits)]
    })
}

nsubexp <- function(pattern)
{
    ## Remove any character classes since parentheses in character
    ## classes don't create submatches.
    y <- gsub("\\[.*?\\]", "", pattern)

    ## Remove any escaped opening parentheses since those also cannot
    ## create submatches.
    y <- gsub("\\\\\\(", "", y)

    ## Count open parentheses.
    nchar(gsub("[^(]", "", y))
}

submatch <- function(pattern, xs, drop = FALSE)
{
    nsubmatch <- nsubexp(pattern)
    if (nsubmatch == 0L)
        stop("`pattern' must contain a parenthesized subexpression.")

    ## =================================================================
    ## Extract submatches.
    ## =================================================================
    matches <- regexec(pattern, xs)
    lengths <- lapply(matches, attr, "match.length")

    extract1 <- function(s, pos, len)
    {
        vapply(seq_along(pos)[-1L],
               function(i) substr(s, pos[i], pos[i] + len[i] - 1L),
               character(1L))
    }

    z <- unname(Map(extract1, xs, matches, lengths))

    ## =================================================================
    ## Fill non-matches with `nsubmatch' NAs.
    ## =================================================================
    z <- lapply(z,
                function(x)
                {
                    if (length(x) == 0L)
                        rep(NA_character_, nsubmatch)
                    else
                        x
                })
    ## =================================================================
    ## Convert to matrix.
    ## =================================================================
    do.call(rbind, z)[, , drop = drop]
}

mkdir <- function(dirs, recursive = TRUE, verbose = TRUE)
{
    f <- function(d)
    {
        if (!file.exists(d)) {
            if (!dir.create(d, recursive = recursive))
                stop("Failed to create directory: ", d)
            if (verbose)
                pr("Created directory: ", d)
        }
        else if (!is.directory(d))
            stop("Can't create directory: ", d,
                 "\nThere is already a file of the same name.")
    }
    invisible(lapply(dirs, f))
}

maybe <- function(f, x, y = x)
{
    ## We don't want to catch errors or warnings resulting from
    ## evaluating the expression that yields the value of `x'.  The
    ## concern of `maybe' is to catch warnings and errors caused by
    ## applying `f' to `x', not errors due to evaluating `x'.
    force(x)

    ## If evaluating `f(x)' gives an error or a warning, return `y'.
    alternative <- function(ignored) y

    tryCatch(f(x), warning = alternative, error = alternative)
}

find_first_match <- function(candidates, x, ...)
{
    if (length(x) == 0L)
        stop("`x' must be a character vector of length >= 1.")

    if (length(candidates) == 0L)
        stop("`candidates' must be a character vector of length >= 1.")

    choices <- vapply(lapply(candidates, grepl, x, ...),
                      function(ys) if (any(ys)) which.max(ys) else NA,
                      integer(1L))

    if (all(is.na(choices)))
        return(NA_integer_)

    min(choices, na.rm = TRUE)
}

gzhead <- function(files, n = 1L, ncore = 1L, simplify = TRUE)
{
    not_there <- ! file.exists(files)
    if (any(not_there))
        stop("Some `files' don't exist:\n",
             paste(files[not_there], collapse = ", "))

    ## Find path to perl script for parsing obo file.
    package_dir <- find.package("miscFun")
    perl_script <- file.path(package_dir, "perl", "gzhead.pl")

    cmd <- paste(perl_script, "-n", n, files)
    x <- parallel::mclapply(cmd, system, intern = TRUE)

    if (simplify && n == 1L)
        do.call(c, x)
    else
        unname(x)
}
