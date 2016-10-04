#' Print message
#'
#' @param \dots Any number of arguments that could also be handed to
#'      \code{\link[base]{cat}}
#'
#' @return \code{pr} prints its arguments pasted together followed by
#' a newline.
#'
#' @seealso \code{\link[base]{cat}}, \code{\link[base]{writeLines}}
#'
#' @examples
#' pr("Finished iteration ", 3, " out of ", 10)
#'
#' @export
pr <- function(...) {
    cat(..., "\n", sep = "")
}

#' @rdname pr
#'
#' @return \code{pr1} prints its arguments pasted together but doesn't
#' add a trailing newline.
#'
#' @examples
#' pr1("Progress: ")
#' for (i in 1:10) pr1(".")
#'
#' @export
pr1 <- function(...) {
    cat(..., sep = "")
}

#' Exit from running R job
#'
#' @param status Exit status
#' @param \dots Other arguments that could also be passed to \code{\link[base]{quit}}
#'
#' @seealso \code{\link[base]{quit}}
#'
#' @export
exit <- function(status = 0L, ...) {
    quit(save = "no", status = status, ...)
}

#' Flip 1st and 2nd argument of a function
#'
#' @param f Function whose 1st and 2nd argument should be flipped
#' @return Same function with its 1st and 2nd argument flipped.
#'
#' @examples
#' f <- function(x, y, z) {
#'     x + 2*y + 3*z
#' }
#' g <- flip(f)
#' f(1, 2, 3) == 14
#' g(1, 2, 3) == 13
#' g(z = 3, 1, 2) == 13
#'
#' @export
flip <- function(f) {
    args <- formals(f)
    if (is.null(args) || length(args) == 1L && names(args) != "...")
        stop("Function must take at least 2 arguments.")
    if ("..." %in% names(formals(f))[1:2])
        stop("Function must not have \"...\" as 1st or 2nd argument.")
    # Swap 1st and 2nd argument.
    names(formals(f))[1:2] <- names(formals(f))[2:1]
    f
}

#' Return number of unique elements
#'
#' @param x List or vector
#'
#' @export
nuniq <- function(x) {
    length(unique(x))
}

#' Flatten list of strings of comma-separated elements
#'
#' @param x Character vector of strings of separated elements
#' @param sep Separator used to separate elements
#' @param fixed If TRUE (default), separator is interpreted as a
#'      constant string, otherwise it is considered a regular
#'      expression in perl flavor
#'
#' @details
#' The separator can also be an arbitrary perl regular expression in
#' which case `fixed' should be set to \code{FALSE}.
#'
#' @examples
#' stopifnot(all(1:5 == flatten_csv(c("1,2", "3,4,5"))))
#' stopifnot(all(c("a", "b", "c", "d") == flatten_csv(c("a1b", "c23d"), sep = "\\d+", fixed = FALSE)))
#'
#' @export
flatten_csv <- function(x, sep = ",", fixed = TRUE) {
    if (length(x) == 0)
        return(character())
    unlist(strsplit(x, split = sep, fixed = fixed, perl = !fixed), use.names = FALSE)
}

#' Return number of columns in file
#'
#' @param con File or connection for which columns should be counted
#' @param sep Separator used to separate columns in \code{file}
#'
#' @examples
#' \dontrun{
#' nfields("data.csv", sep = ",")
#' }
#'
#' @export
nfields <- function(con, sep = "\t") {
    if (is.character(file)) {
        con <- file(con)
        on.exit(close(con))
    }
    length(scan(con, what = character(), sep = sep, nlines = 1L, quiet = TRUE))
}

#' Expand template into \code{\link[utils]{read.table}}s \code{colClasses} argument
#'
#' @param fmt Format string to be expanded and then used as the
#'      \code{colClasses} argument to \code{\link[utils]{read.table}}.
#'
#' @details The following format letters are available:
#'     \tabular{ll}{
#'       c\tab "character"\cr
#'       i\tab "integer"\cr
#'       l\tab "logical"\cr
#'       n\tab "numeric"\cr
#'       N\tab "NULL"\cr
#'       r\tab "raw"\cr
#'       x\tab "complex"\cr
#'     }
#'     The digits say how many times the expansion of the character
#'     should be included in the result.
#'
#' @return A vector that can be used as the \code{colClasses} argument
#'     to \code{\link[utils]{read.table}}.
#'
#' @examples
#' stopifnot(colClasses(c("N 2 c i n")) ==
#'     c("NULL", "character", "character", "integer", "numeric"))
#'
#' @export
colClasses <- function(fmt) {
    if (!is.character(fmt) || is.na(fmt) || length(fmt) != 1 || grepl("^\\s*$", fmt))
        stop("fmt must be a non-empty, non-NA character vector of length 1")
    translate <- function(x) {
        type <- switch(letter <- substr(x, nchar(x), nchar(x)),
            N = "NULL", c = "character", i = "integer", n = "numeric",
            l = "logical", r = "raw", x = "complex",
            stop("unknown letter in format: ", letter))
        if (nchar(x) == 1)
            return(type)
        rep(type, as.integer(substr(x, 1, nchar(x) - 1)))
    }
    groups <- unlist(strsplit(fmt, "(?<=[[:alpha:]])", perl = TRUE), use.names = FALSE)
    unlist(lapply(trimws(groups), translate), use.names = FALSE)
}

#' Apply function over level combinations of factors
#'
#' @description Group a vector into subsets according to level
#'     combinations of one or several factors and apply a function
#'     separately to every subset.  This function differs from
#'     \code{\link[stats]{ave}} in three respects.  Firstly, factors
#'     have to be supplied in an explicit list.  Secondly, the
#'     function is a positional parameter.  Thirdly and most
#'     importantly, there is now a way to supply any number of
#'     additional arguments to the function via \code{\dots} as in the
#'     apply-family of functions.
#'
#' @param x Atomic vector
#' @param factors List of factors of the same length as \code{x}.
#' @param f Function
#' @param \dots Further arguments to \code{f}
#' @return A vector of the same length as \code{x}.  Subsets of the
#'     result vector equal the result of applying \code{f} to the
#'     corresponding subsets of \code{x}.
#'
#' @seealso \code{\link[stats]{ave}}
#'
#' @examples
#' stopifnot(identical(
#'     ave2(c(1, 2, 3, 4), list(c(1, 1, 2, 2)),
#'         function(x, sep) {
#'             paste(x, rev(x), sep = sep)
#'         }, "-"),
#'     c("1-2", "2-1", "3-4", "4-3")))
#' @export
ave2 <- function (x, factors, f, ...) {
    if (is.null(factors))
        stop("Argument \"factors\" must not be NULL.")
    if (length(factors) == 0)
        stop("Argument \"factors\" must not be of length 0.")
    grouping <- interaction(factors)
    if (length(grouping) != length(x))
        stop("Vectors in argument \"factors\" must have same length as argument \"x\".")
    split(x, grouping) <- lapply(split(x, grouping), f, ...)
    x
}

#' Return modified names of object
#'
#' @param x Object whose \code{names} should be modified
#' @param mapping Named character vector whose \"names\" attribute
#'     matches all, some, or none of the names of \code{x} and whose
#'     elements will be used to make up the new names of \code{x}.
#'
#' @return Return a character vector that can be used with
#'     \code{\link[base]{names<-}} to change the names of \code{x}.
#'
#' @details The \code{rename} function does not modify \code{x} and
#'     thereby avoids potentially expensive copies due to R's
#'     copy-on-modify semantics.  Use \code{\link[base]{names<-}} with
#'     the result of \code{rename} to change the names of \code{x} in
#'     a non-expensive way.
#'
#'     Note that \code{mapping} does not have to contain new values
#'     for all elements of \code{names(x)}.  If an element of
#'     \code{names(x)} is not among \code{names(mapping)}, it will be
#'     unchanged.  In particular, if \code{mapping} and \code{x} have
#'     no names in common, \code{rename} will return \code{names(x)}.
#'     It is an error for \code{names(mapping)} to contain duplicate
#'     names.  It is okay for \code{mapping} to contain a mixture of
#'     named and unnamed elements.  Unnamed elements of \code{x} will
#'     never be renamed.
#'
#' @examples
#' rename(c(a = 1, b = 2, c = 3, 4, `9` = 9),
#'     c(a = "A", b = NA, "foo", g = "G", `9` = "nine"))
#'
#' @export
rename <- function(x, mapping) {
    if (is.null(names(x)))
        stop("x must have names")
    if (is.null(names(mapping)))
        stop("mapping must have names")
    if (anyDuplicated(names(mapping)))
        stop("mapping must not have duplicate names")
    old_names <- new_names <- names(x)
    idx <- old_names %in% names(mapping)
    new_names[idx] <- mapping[old_names[idx]]
    new_names[names(x) == ""] <- ""
    new_names
}

#' Match positions to intervals
#'
#' @param pos Integer vector with positions
#' @param start Integer vector with interval start positions
#' @param end Integer vector with interval end positions
#' @param id Vector with interval ids
#' @param batch.size Number of positions in `pos' that are processed at a time
#' @param quiet Report progress if FALSE (default)
#'
#' @return A character vector of the same length as `pos' where every
#' entry is a comma-separated string containing the ids of all
#' intervals the associated position in `pos' falls into.  If a
#' position does not fall into any of the intervals, the empty string
#' is returned.
#'
#' @examples
#' pos   <- c(1L,2L,3L)
#' start <- c(0L,1L,4L)
#' end   <- c(1L,2L,5L)
#' id    <- c(1L,2L,3L)
#' match_intervals(pos, start, end, id)
#'
#' @export
match_intervals <- function(pos, start, end, id, batch.size = 1000L, quiet = FALSE) {
    stopifnot(length(start) > 0L)
    stopifnot(length(end) == length(start))
    stopifnot(length(id) == length(start))
    stopifnot(length(pos) > 0L)
    # Remove intervals with missings.
    is_missing <- is.na(start) | is.na(end) | is.na(id)
    start <- start[!is_missing]
    end <- end[!is_missing]
    id <- id[!is_missing]
    if (length(start) == 0L)
        return(rep("", length(pos)))
    fun <- function(pos) {
        # Create a matrix with length(START) rows and length(POS)
        # columns, where every row is equal to POS.  For example,
        # if pos = c(1, 2, 3, 4), and length(start) = 3, then
        #       |1 2 3 4|
        # mat = |1 2 3 4|
        #       |1 2 3 4|
        mat <- matrix(pos, ncol = length(pos), nrow = length(start), byrow = TRUE)
        # Compare every column with interval start and end positions.
        # The result of the comparison is a matrix of the same
        # dimensions as MAT.
        value <- start <= mat & mat <= end
        # For every column of VALUE, find the TRUE indexes.
        idx_list <- lapply(split(value, col(value)), which)
        # For every position in POS, collapse the names of intervals
        # overlapping that positions into a comma-separated string.
        sapply(idx_list, function(x) {
            paste(sort(unique(id[x])), collapse = ",")
        })
    }
    result <- NULL
    n <- length(pos)
    if (n < batch.size)
        batch.size <- n
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

#' Extract duplicated rows from data frame.
#'
#' @description
#' Extract all rows from a data frame that are duplicates with respect
#' to the specified columns.  Two rows are duplicates with respect to
#' the selected columns if they agree element-wise in all of the
#' selected columns.
#'
#' @param data Data frame
#' @param columns Columns in \code{data} to be checked for duplicates.
#'     Can be a character vector of column names or a numeric vector
#'     giving the column indexes.
#' @param select Columns to include in resulting data frame
#' @param sep Separator used for row-wise pasting of values in
#'     \code{cols} columns
#'
#' @return A data frame containing all rows of \code{data} that had
#'     duplicates with respect to the selected columns.
#'
#' @seealso \code{\link[base]{duplicated}}
#'
#' @examples
#' d <- read.table(text = "
#' x y z
#' 1 2 3
#' 1 3 4
#' 1 2 4
#' ", header = TRUE)
#'
#' find_duplicates(d, "x")
#' find_duplicates(d, "x", c("y", "z"))
#' find_duplicates(d, c("x", "y"))
#' find_duplicates(d, c("x", "z"))
#'
#' @export
find_duplicates <- function(data, columns, select = NULL, sep = "\t") {
    if (is.null(select))
        select <- colnames(data)
    if (nrow(data) == 0L)
        return(data[, select])
    if (any(! columns %in% colnames(data)))
        stop("nonexistent variables in COLUMNS")
    if (any(! select %in% colnames(data)))
        stop("nonexistent variables in SELECT")
    key <- do.call(function(...) paste(..., sep = "\r"), data[columns])
    data[key %in% key[duplicated(key)], select, drop = FALSE]
}

#' Invert mapping
#'
#' @description
#' Given a mapping implemented as a named list, return the inverse
#' mapping, again implemented as a list.
#'
#' @param map A named list representing a mapping
#'
#' @details NA values are dropped during the inversion.
#'
#' @return A list representing the inverse of mapping of `map'
#'
#' @examples
#' map <- list(a = c("A","C"), b = c("B","C",NA))
#' rev_map(map)
#'
#' @export
rev_map <- function(map) {
    values <- unlist(map, use.names = FALSE)
    times <- sapply(map, length)
    names(values) <- rep(names(map), times = times)
    lapply(split(names(values), values), sort)
}

#' One value per row
#'
#' @description
#' In a data frame with a column or columns where entries can consist
#' of a comma-separated list of values, split entries with multiple
#' values and put them into separate rows.  Eventually there will be
#' only "one value per row" in the specified column(s).
#'
#' @param d Data frame
#' @param cols Columns of data frame in which to spread multi-value
#'      entries across multiple rows
#' @param sep Separator used in multi-value entries
#'
#' @return A data frame with the same columns as `d' but where there
#' is only one value per row in the columns `cols'.
#'
#' @examples
#' d <- read.table(text = "
#' x y z
#' 1 a,c A,C
#' 2 b,d B,D
#' 3 e E
#' ", header = TRUE, stringsAsFactors = FALSE)
#' one_per_row(d, c("y","z"))
#'
#' @export
one_per_row <- function(d, cols, sep = ",") {
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

#' One value per vector element
#'
#' @description
#' Split comma-separated values in a character vector across multiple
#' indexes such that eventually there is "one value per element".
#'
#' @param x Character vector
#' @param sep Separator used in multi-value vector elements
#' @param perl Consider `sep' as a perl regular expression.  If FALSE
#'      (default), `sep' is considered a literal (fixed) string.
#'
#' @return Returns a character vector where every element contains
#' exactly one string.  Empty strings in `x' will not be part of the
#' return vector.
#'
#' @examples
#' x <- c("1,2", "3,4,5", "6", "", "7,8")
#' y <- c("1---2", "3--4----5", "6-7")
#' one_per_element(x)
#' one_per_element(y, sep = "-+", perl = TRUE)
#'
#' @export
one_per_element <- function(x, sep = ",", perl = FALSE) {
    unname(unlist(strsplit(x, split = sep, fixed = !perl, perl = perl)))
}

#' Test whether a binary predicate holds for all direct neighbors in a list
#'
#' @param f Binary function used for comparing neighboring elements of
#'      `x'
#' @param \dots One or more lists or vectors (holding elements of the
#'      same type)
#'
#' @return TRUE if all neighboring elements (even across lists)
#' compare TRUE, otherwise FALSE.
#'
#' @examples
#' all_neighbors(identical, c(1,1,1))
#' all_neighbors(identical, c(1,2,1))
#' all_neighbors(identical, c(NA,NA,NA))
#' all_neighbors(identical, c(1,NA,1))
#' all_neighbors(function(x,y) TRUE, 1:5)
#' all_neighbors(function(x,y) FALSE, c(1,1,1))
#' all_neighbors(function(x,y) y - x == 1L, 1:10)
#' all_neighbors(`<`, 1:10, 100:200)
#' all_neighbors(`<=`, list(a = 1, b = 3), list(c = 3, d = 5))
#'
#' @export
all_neighbors <- function(f, ...) {
    force(f)
    lists <- list(...)
    if (length(lists) == 1L)
        x <- lists[[1L]]
    else
        x <- do.call(c, lists)
    # If we get through the call to `Reduce' without an error being
    # thrown, all direct neighbors in `x', when compared by `f', yield
    # TRUE, that is, the predicate `f' holds throughout `x' and we
    # return TRUE.  Otherwise one pair of direct neighbors compared
    # FALSE, that is, the predicate `f' does not hold throughout all
    # of `x' and we return FALSE.
    tryCatch({
        Reduce(function(x,y) if (f(x,y)) y else stop(), x)
        TRUE
    }, error = function(e) FALSE)
}

#' Surround string(s) with double/single quotes
#'
#' @param x Character vector
#'
#' @return Surrounds every element of `x' with double/single quotes,
#' escaping nested double quotes as needed.
#'
#' @aliases single_quote
#'
#' @export
double_quote <- function(x) {
    x <- gsub("\"", "\\\"", x, fixed = TRUE)
    paste0("\"", x, "\"")
}

#' @rdname double_quote
single_quote <- function(x) {
    x <- gsub("'", "\\\'", x, fixed = TRUE)
    paste0("'", x, "'")
}

#' Do nothing
#'
#' @param \dots Ignored
#'
#' @return None.
#'
#' @examples
#' noop(ignore_whatever_is_in_here)
#'
#' @export
noop <- function(...) {
    invisible(NULL)
}

#' Create function for keeping track of values
#'
#' @return A function with signature `function(x, show = FALSE)'.  If
#' `show' is TRUE, the function returns a character vector with all
#' values (coerced to "character") that it has `seen' so far.  If
#' `show' is FALSE and argument `x' was `seen' before, the function
#' returns "TRUE", otherwise it returns "FALSE".  The argument `x'
#' must be a of length 1 and class "character", "numeric", or
#' "integer".  If the argument is of class "numeric" or "integer",
#' conversion to class "character" is done via
#' \code{\link[base]{as.character}}.  The order of element in the list
#' returned if `show' is TRUE is not guaranteed to be the same order
#' in which the values were `seen'.
#'
#' @examples
#' seen <- make_observer()
#' for (i in c(1:5, 1:5)) pr("Seen ", i, " -> ", seen(i))
#' seen(show = TRUE)
#'
#' @export
make_observer <- function() {
    e <- new.env()
    function (x, show = FALSE) {
        if (show)
            return(ls(envir = e))
        if (length(x) != 1L || length(class(x)) != 1L
                || ! class(x) %in% c("character", "numeric", "integer"))
            stop("Argument must be of length 1 and class must be ",
                "\"character\", \"numeric\", or \"integer\".")
        if (! class(x) == "character")
            x <- as.character(x)
        if (exists(x, e, inherits = FALSE))
            return(TRUE)
        assign(x, TRUE, envir = e, inherits = FALSE)
        return(FALSE)
    }
}

#' Group words in chunks of given length
#'
#' @param x Vector of strings to be split and regrouped
#' @param n Number of characters per chunk of regrouped words
#' @param split Pattern on which to split strings in `x' into words
#' @param perl TRUE if `split' and `special_sep' should be interpreted
#'      as perl-style regular expressions.  Takes precedence over
#'      `fixed'
#' @param fixed TRUE if `split' and `special_sep' should be
#'      interpreted as fixed strings
#' @param sep Separator used when combining words into chunks
#' @param special Pattern identifying words that should be combined in
#'      a special way (see Details)
#' @param special_sep Separator used to combine two words if the first
#'      matches `special'
#' @param trim_leading If TRUE, leading whitespace will be removed
#'      from chunks
#' @param trim_trailing If TRUE, trailing whitespace will be removed
#'      from chunks
#' @param tol Chunks will be allowed to be `tol' characters longer
#'      than `n' if that makes their lengths more even in overall
#' @param punct If TRUE, words consisting of a single punctuation mark
#'      will always be merged to the previous chunk
#'
#' @details
#' The meaning of `word' depends on the value of `split'.  By default,
#' strings are split on whitespace resulting in a list of words in the
#' normal sense (maybe including some punctuation characters and the
#' like).  Clever choice of `split', `sep', `special', and
#' `special_sep' let's you obtain a variety of effects (see
#' \code{wrap_lines}).
#'
#' @return A list of character vectors where every character vector
#' contains one or more chunks of words.  Every chunk contains from 1
#' to `n + tol' characters unless it contains a single word of length
#' > `n' in which case its length is the length of the word.
#'
#' @seealso \code{\link{wrap_lines}}
#'
#' @examples
#' s <- "The quick yellow-orange-brown fox jumps over the 3-year-old, lazy dog."
#' group_words(s, 10, split = "\\\\s+|(?<=-)", special = "-$")
#'
#' @export
group_words <- function(x, n, split = "\\s+", perl = TRUE,
        fixed = !perl, sep = " ", special = NULL, special_sep = "",
        trim_leading = TRUE, trim_trailing = trim_leading, tol = 0L,
        punct = TRUE) {
    if (n < 1L)
        stop("N must be an integer >= 1.")
    if (!fixed && !perl)
        stop("Either FIXED or PERL must be TRUE.")
    if (fixed && perl) {
        fixed <- FALSE
        warning("FIXED conflicts with PERL: setting FIXED to FALSE.")
    }
    f <- function(xs) {
        g <- function(acc, word) {
            # Check if current line ends in a special way.
            is_special <- FALSE
            if (!is.null(special))
                is_special <- grepl(special, acc[1L])
            # Current chunk has enough space for word.
            if (nchar(acc[1L]) + nchar(word) + nchar(sep) <= n + tol)
                # Add word to current chunk.
                acc[1L] <- paste(acc[1L], word,
                    sep = if (is_special) special_sep else sep)
            # Add punctuation sign to previous chunk.
            else if (grepl("^[[:punct:]]$", word))
                acc[1L] <- paste(acc[1L], word, sep = "")
            # Start new chunk.
            else {
                if (trim_leading)
                    word <- sub("^\\s+", "", word, perl = TRUE)
                acc <- c(word, acc)
            }
            acc
        }
        xs <- rev(Reduce(g, xs))
        # Trim whitespace.
        if (trim_trailing)
            xs <- sub("\\s+$", "", xs, perl = TRUE)
        xs
    }
    lapply(strsplit(x, split, fixed, perl), f)
}

#' Wrap strings across lines
#'
#' @param x Strings to be wrapped
#' @param n Number of characters per line after wrapping
#' @param sep Separator used for combining lines
#' @param max_lines Wrap every string to at most `max_lines' lines
#' @param dots String to be inserted to indicate that a string was
#'      truncated
#' @param hard TRUE if lines should have a fixed length of `n' even if
#'      that means line breaks in the middle of a word
#' @param tol Lines will be allowed to be `tol' characters longer than
#'      `n' if that makes the line lengths more even overall
#'
#' @return A list of strings each of which spans at most `max_lines'
#' physical lines of length 1 to `n + tol'.
#'
#' @seealso \code{\link{group_words}}
#'
#' @examples
#' s <- "The quick yellow-orange-red-brown fox jumps over a 3-year-old, lazy dog."
#' pr(wrap_lines(s, 12))
#' pr(wrap_lines(s, 12, hard = TRUE))
#'
#' @export
wrap_lines <- function(x, n, sep = "\n", max_lines = Inf,
        dots = "...", hard = FALSE, tol = if (hard) 0L else 3L) {
    if (nchar(dots) >= n)
        stop("DOTS must have < N characters.")
    # Split into lines.
    if (hard)
        y <- group_words(x, n, split = NULL, sep = "", tol = tol)
    else
        y <- group_words(x, n, split = "\\s+|(?<=-)", special = "-$", tol = tol)
    # Truncate to at most `max_lines' lines and paste together using
    # `sep' as line separator.
    f <- function(lines) {
        if (length(lines) <= 1L)
            return(lines)
        if (length(lines) > max_lines) { # too many lines
            # Keep only the first max_lines lines.
            lines <- lines[seq_len(max_lines)]
            # Insert dots in last line.
            last <- lines[max_lines]
            substr(last, n - nchar(dots) + 1L, n) <- dots
            lines[max_lines] <- substr(last, 1L, n)
        }
        paste(lines, collapse = sep)
    }
    sapply(y, f)
}

#' Test for directory
#'
#' @param fs Filenames to be tested for directory status
#'
#' @return TRUE for directories, otherwise FALSE.
#'
#' @export
is.directory <- function(fs) {
    file.info(fs)$isdir
}

#' Read and concatenate tables
#'
#' @param files Files with tables to be read and concatenated
#' @param sep Field separator in input tables
#' @param ncore Number of cores to use in parallel
#' @param header See documentation of \code{\link[utils]{read.table}}
#' @param colClasses See documentation of \code{\link[utils]{read.table}}
#' @param verbose If TRUE, report progress while running
#' @param check.names See documentation of \code{\link[utils]{read.table}}
#' @param comment.char See documentation of \code{\link[utils]{read.table}}
#' @param \dots Passed on to \code{\link[utils]{read.table}}
#'
#' @details
#' If not explicitly specified, the field separator will be inferred
#' from the first non-comment, non-empty line of one of the input
#' files.  Possible candidates are tab, comma, semicolon, single
#' space.  They will be tested in the above order.  The first
#' candidate found will become the field separator. The field
#' separator must be the same for all input files.
#'
#' @return A data frame that results from concatenating all input
#' tables.
#'
#' @examples
#' \dontrun{
#' read.tables(paste0("table", 1:100, ".txt"), ncore = 10L)
#' }
#'
#' @export
read.tables <- function(files, sep = NULL, ncore = 1L, header = TRUE,
        colClasses = "character", verbose = TRUE, check.names = FALSE,
        comment.char = "#", ...) {
    # Drop empty files.
    files <- Filter(function(x) file.info(x)$size > 0L, files)
    stopifnot(length(files) > 0L)
    stopifnot(all(vapply(files, file.exists, logical(1L))))
    # Determine field separator from one of the files.
    if (is.null(sep)) {
        is_comment <- function(x) {
            grepl(paste0("^", comment.char), x)
        }
        # Find a non-comment, non-empty line.
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
        candidate_separators <- c("\t", ",", ";", " ")
        sep <- match1of(candidate_separators, line, fixed = TRUE)
        if (is.na(sep))
            stop("Unable to figure out field separator in ", files[i])
    }
    # Read and concatenate tables.
    f <- function(filename) {
        if (verbose)
            pr("Reading ", filename)
        utils::read.table(filename, sep = sep, colClasses = colClasses,
            header = header, comment.char = comment.char, ...)
    }
    do.call(rbind, parallel::mclapply(files, f, mc.preschedule = FALSE,
        mc.cores = ncore, mc.silent = FALSE))
}

#' Suspend execution until deadline
#'
#' @param date_string String specifying when to continue execution
#'
#' @details
#' The date string must have the format "%d-%m-%y %H:%M", basically
#' `day-month-year hour:minute'.  For more details see
#' \code{\link[base]{strptime}}.
#'
#' It is an error to specify a date string that designates a time
#' point that, at the moment \code{wait_until} is called, lies in the
#' past.
#'
#' The accuracy of \code{wait_until} is +/- 1 minute.
#'
#' @examples
#' \dontrun{
#' wait_until("26-4-34 17:03")
#' }
#'
#' @export
wait_until <- function(date_string) {
    deadline <- strptime(date_string, format = "%d-%m-%y %H:%M")
    if (is.na(deadline))
        stop("Unable to convert to date: ", date_string)
    # Check whether deadline has arrived.
    past_deadline <- function() {
        difftime(Sys.time(), deadline) > 0
    }
    if (past_deadline())
        stop("We're already past the deadline.")
    cat("Waiting until", date_string, "")
    # Wait until deadline.
    while (!past_deadline()) {
        Sys.sleep(60)
        cat(".")
    }
    pr()
}

#' Find matching candidate
#'
#' @param candidates Character vector of candidates to be matched
#'      against `x'
#' @param x Character vector
#' @param \dots Further arguments passed on to \code{\link[base]{grepl}}
#'
#' @return A character vector of the same length as `x' where element
#' i contains the first element of `candidates' that matches in
#' `x[i]', or NA if none of the candidates match.
#'
#' @examples
#' match1of(c("foo", "bar", "xxx", "baz", "quux"), c("foo", "bar", "quux"))
#'
#' @export
match1of <- function(candidates, x, ...) {
    if (length(candidates) == 0L || !is.character(candidates))
        stop("`candidates' must be a character vector of length >= 1.")
    if (length(x) == 0L || !is.character(x))
        stop("`x' must be a character vector of length >= 1.")
    # If length(x) > 1, create a length(x)-by-length(candidates)
    # matrix such that the entry in row i and column j is TRUE iff
    # candidate[j] matches in x[i].  If length(x) == 1, return a
    # logical vector of the same length as `candidates'.
    hit_matrix <- vapply(candidates, grepl, logical(length(x)), x, ...)
    # Convert `hit_matrix' to matrix, if necessary.
    if (length(x) == 1L)
        hit_matrix <- t(as.matrix(hit_matrix))
    apply(hit_matrix, 1L, function(hits) {
        if (!any(hits))
            NA_character_
        else
            candidates[which.max(hits)]
    })
}

#' Find first matching candidate
#'
#' @param candidates Character vector of candidates to be matched
#'      against `x'
#' @param x Character vector
#' @param \dots Further arguments passed on to \code{\link[base]{grepl}}
#'
#' @return #' Index of the element of `candidates' that matches the
#' element of `x' with the smallest index.  In case of ties, the
#' smallest of the tied indexes is returned.  If none of the
#' candidates match any of the elements in `x', NA is returned.
#'
#' @examples
#' find_first_match(c("foo", "bar", "baz", "quux"), c("baz", "quux", "bar"))
#'
#' @export
find_first_match <- function(candidates, x, ...) {
    if (length(x) == 0L)
        stop("`x' must be a character vector of length >= 1.")
    if (length(candidates) == 0L)
        stop("`candidates' must be a character vector of length >= 1.")
    choices <- vapply(lapply(candidates, grepl, x, ...), function(ys) {
        if (any(ys))
            which.max(ys)
        else
            NA
    }, integer(1L))
    if (all(is.na(choices)))
        return(NA_integer_)
    min(choices, na.rm = TRUE)
}

#' Count number of parenthesized subexpressions
#'
#' @param x Character vector
#'
#' @examples
#' nsubexp("a_[(string)]_with_(2)_parenthesized_(sub)expressions")
#'
#' @export
nsubexp <- function(x) {
    # Remove any character classes since parentheses in character
    # classes don't create submatches.
    y <- gsub("\\[.*?\\]", "", x)
    # Remove any escaped opening parentheses since those also cannot
    # create submatches.
    y <- gsub("\\\\\\(", "", y)
    # Count open parentheses.
    nchar(gsub("[^(]", "", y))
}

#' Extract submatches from strings
#'
#' @param pattern An extended regular expression (see
#'      \link[base]{regex}) including at least one parenthesized
#'      subexpression
#' @param xs Character vector with strings from which to extract the
#'      subexpressions matched by `pattern'
#' @param drop If TRUE instead of returning a single-column or
#'      single-row matrix a vector is returned
#'
#' @return Returns a matrix containing submatches extracted from `xs'
#' according to `pattern'.  The entry in row i and column j
#' corresponds to the jth submatch extracted from `x[i]'.  If
#' `pattern' did not match an element of `xs', the corresponding row
#' in the returned matrix will consist entirely of NA_character_
#' values.  If `drop = TRUE' single-column and single-row matrices are
#' returned as vectors.
#'
#' @examples
#' submatch(".*chr(\\\\d+)_(\\\\d+)",
#'          c("path_chr1_2/to/the_chr10_3_foo.txt",
#'            "path_chr1_2/to/the_chr7_5_bar.txt",
#'            "path_chrA_B/to/the_chrX_Y_baz.txt"))
#'
#' # Submatch an expression within parentheses.
#' submatch("[(](.*)[)]", "foo (bar) baz")
#'
#' @export
submatch <- function(pattern, xs, drop = FALSE) {
    nsubmatch <- nsubexp(pattern)
    if (nsubmatch == 0L)
        stop("`pattern' must contain a parenthesized subexpression.")
    # Extract submatches.
    matches <- regexec(pattern, xs)
    lengths <- lapply(matches, attr, "match.length")
    extract1 <- function(s, pos, len) {
        vapply(seq_along(pos)[-1L], function(i) {
            substr(s, pos[i], pos[i] + len[i] - 1L)
        }, character(1L))
    }
    z <- unname(Map(extract1, xs, matches, lengths))
    # Fill non-matches with `nsubmatch' NAs.
    z <- lapply(z, function(x) {
        if (length(x) == 0L)
            rep(NA_character_, nsubmatch)
        else
            x
    })
    # Convert to matrix.
    do.call(rbind, z)[, , drop = drop]
}

#' Create directories
#'
#' @param dirs Character vector with paths of directories to be
#'      created
#' @param recursive If TRUE then \code{mkdir} works like `mkdir -p' in
#'      unix and makes parent directories as needed
#' @param verbose If TRUE show a message for each newly created
#'      directory
#'
#' @details
#' If a path in `dirs' points to an existing directory, there is
#' nothing left to do and the directory will just be ignored.  If
#' there is already a file that has the same name as the directory to
#' be created, an error is thrown.  Therefore \code{mkdir} only
#' returns if every directory in `dirs' either already exists or was
#' successfully created.  Failing early---instead of, for example,
#' returning a logical vector indicating which of the directories in
#' `dirs' could be created---seems a desirable behavior since later
#' code depending on the existence of the directories in `dirs' would
#' fail anyway:  There is no need to start possibly long-running
#' computations just to get an error when trying to write the precious
#' results to a file in a non-existent directory.
#'
#' @export
mkdir <- function(dirs, recursive = TRUE, verbose = TRUE) {
    f <- function(d) {
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

#' Maybe apply function to value
#'
#' @param f Function to be applied to `x'
#' @param x Object to which `f' should be applied
#' @param y Alternative value to return if applying `f' to `x' leads
#'      to an error or a warning
#'
#' @details
#' Errors or warnings caused by evaluating `x' will not be caught.
#' The purpose of \code{maybe} is only to catch errors and warning
#' caused by evaluating `f(x)'.
#'
#' @return `f(x)' unless that results in an error or a warning,
#' otherwise `y'.
#'
#' @examples
#' x <- "12"
#' y <- "foo"
#' maybe(as.integer, x)
#' maybe(as.integer, y)
#' maybe(as.integer, y, 0L)
#' tryCatch(maybe(as.integer, stop(), 0L),
#'          error = function(e) pr("Evaluating `x' caused an error."))
#'
#' @export
maybe <- function(f, x, y = x) {
    # We don't want to catch errors or warnings resulting from
    # evaluating the expression that yields the value of `x'.  The
    # concern of `maybe' is to catch warnings and errors caused by
    # applying `f' to `x', not errors due to evaluating `x'.
    force(x)
    # If evaluating `f(x)' gives an error or a warning, return `y'.
    alternative <- function(ignored) y
    tryCatch(f(x), warning = alternative, error = alternative)
}

#' Output the first part of gzip-files
#'
#' @param files Character vector with paths to gzip-files
#' @param n Number of lines to print
#' @param simplify If TRUE and `n = 1L', return result as character vector
#'
#' @return In case `simplify = FALSE', returns the first `n' lines of
#' each gzip-file in `files' as a list of character vectors.  If
#' `simplify = TRUE' and `n = 1' returns the results as a character
#' vector of the same length as `files'.
#'
#' @export
gzhead <- function(files, n = 1L, simplify = TRUE) {
    not_there <- ! file.exists(files)
    if (any(not_there))
        stop("Some `files' don't exist:\n", paste(files[not_there], collapse = ", "))
    x <- lapply(files, readLines, n)
    if (simplify && n == 1L)
        do.call(c, x)
    else
        unname(x)
}

#' Group one-dimensional data into clusters of contiguous points
#'
#' @param x Numeric vector of points to be grouped into clusters
#' @param gap Size of gap that separates two neighboring clusters
#' @param frac Size of gap that separates two neighboring clusters
#'      expressed as a fraction of the range of `x'
#'
#' @return Returns an integer vector that is aligned with `x' and
#' contains the cluster for the associated element in `x'.  NA values
#' in `x' will have NA values in the resulting vector.
#'
#' @examples
#' x <- c(9, 12, 3, 4, 17, 10, 12, NA, 6, 1)
#' cluster1d(x, gap = 3)
#' cluster1d(x, frac = .15)
#'
#' @export
cluster1d <- function(x, gap, frac = NULL) {
    if ((missing(gap) && is.null(frac))
            || (!missing(gap) && !is.null(frac)))
        stop("You must supply either `gap' or `frac'.")
    if (!is.null(frac) && (frac <= 0 || 1 <= frac))
        stop("`frac' must be in (0,1).")
    ord <- order(x)
    xs <- x[ord]
    if (length(x) == 0L)
        stop("`x' must contain at least one non-NA value.")
    if (!missing(frac))
        gap <- diff(range(xs, na.rm = TRUE)) * frac
    breaks <- c(0L, which(diff(xs) >= gap), length(xs))
    cluster <- as.integer(cut(seq_along(xs), breaks))
    cluster[is.na(xs)] <- NA
    cluster[order(ord)]
}

#' Count how many points fall into each interval
#'
#' @param pos Points
#' @param start Left endpoints of interval
#' @param end Right endpoints of interval
#'
#' @return An integer vector of the same length as `start` and `end`
#' containing the number of points in `pos` that fall into each of the
#' intervals.
#'
#' @examples
#' count_points_per_interval(0:1, 2:1, 3:4)
#'
#' @export
count_points_per_interval <- function(pos, start, end) {
    if (length(start) != length(end))
        stop("START and END must have the same length.")
    pos <- as.double(pos)
    start <- as.double(start)
    end <- as.double(end)
    inf_intervals <- is.infinite(start) | is.infinite(end)
    start[inf_intervals] <- 0
    end[inf_intervals] <- 0
    inf_points <- is.infinite(pos)
    pos[inf_points] <- NA_real_
    intervals <- rcpp_find_matching_intervals(pos, start, end)$interval
    stopifnot(all(intervals %in% seq_along(start)))
    counts <- factor(intervals, levels = seq_along(start))
    n <- as.integer(table(counts, useNA = "no"))
    n[inf_intervals] <- NA
    n
}

#' Find intervals containing a set of points
#'
#' @param pos Double vector of positions
#' @param start Double vector with left endpoints of intervals
#' @param end Double vector with right endpoints of intervals
#'
#' @return A list with elements "position" and "interval".  Both are
#'     integer vectors containing indexes.  The "position" vector
#'     indexes the \code{pos} vector and the "interval" vector the
#'     \code{start} and \code{end} vectors.  The kth index in the
#'     "position" vector forms a pair with the kth index in the
#'     "interval" vector.  A pair of indexes (i,j) means that
#'     \code{pos[i]} belongs to the interval defined by
#'     \code{start[j]} and \code{end[j]}.
#'
#' @examples
#' x <- find_matching_intervals(c(2, 7), c(5, 0, 2), c(9, 1, 3))
#' stopifnot(identical(x$position, c(1L, 2L)))
#' stopifnot(identical(x$interval, c(3L, 1L)))
#' @export
find_matching_intervals <- function(pos, start, end) {
    if (length(start) != length(end))
        stop("START and END must have the same length.")
    .Call('miscFun_rcpp_find_matching_intervals', PACKAGE = 'miscFun', pos, start, end)
}

#' Order intervals in increasing order
#'
#' @param start Left endpoints of interval
#' @param end Right endpoints of interval
#'
#' @return An integer vector `x' which, when used as an index to will
#' sort `start` and `end` into increasing order just as if
#' \code{\link[miscFun]{sort_intervals}} was used.
#'
#' @seealso \code{\link[miscFun]{sort_intervals}}
#'
#' @examples
#' start <- 2:1
#' end <- 3:4
#' ord <- order_intervals(start, end)
#' start_sorted <- start[ord]
#' end_sorted <- end[ord]
#' old_order <- order(ord)
#' stopifnot(all(start_sorted[old_order] == start))
#' stopifnot(all(end_sorted[old_order] == end))
#'
#' @export
order_intervals <- function(start, end) {
    order(start > end, is.na(start), is.na(end), start, end)
}

#' Sort intervals in increasing order
#'
#' @param start Left endpoints of interval
#' @param end Right endpoints of interval
#'
#' @return A list with two vectors named "start" and "end" of the same
#' length as `start` and `end` and representing the left and right
#' endpoints of the sorted intervals.
#'
#' @examples
#' sort_intervals(2:1, 3:4)
#'
#' @export
sort_intervals <- function(start, end) {
    new_order <- order_intervals(start, end)
    list(start = start[new_order], end = end[new_order])
}

#' Test whether all arguments have the same length
#'
#' @param \dots Objects that will be compared in terms of their length
#'
#' @return TRUE if all objects have the same length, otherwise FALSE.
#'
#' @examples
#' stopifnot(
#'     same_length(NA, 0),
#'     same_length(NULL, integer()),
#'     same_length(list(), integer()),
#'     same_length(letters, letters, letters),
#'     !same_length(letters[1], letters[1:2], letters[1:3]))
#'
#' @export
same_length <- function(...) {
    all_neighbors(`==`, vapply(list(...), length, 1L))
}

#' Create all [un]ordered pairs from two vectors
#'
#' @param x,y Atomic vectors to be paired off
#'
#' @return A data frame with column names "x" and "y" where every row
#' corresponds to a pair.
#'
#' @name make_pairs
NULL

#' @rdname make_pairs
#'
#' @examples
#' make_ordered_pairs(1:3, 1:2)$x == c(1, 2, 3, 1, 2, 3)
#' make_ordered_pairs(1:3, 1:2)$y == c(1, 1, 1, 2, 2, 2)
#'
#' @export
make_ordered_pairs <- function(x, y = NULL) {
    make_pairs(x, y, ordered = TRUE)
}

#' @rdname make_pairs
#'
#' @examples
#' make_unordered_pairs(1:3, 1:2)$x == c(1, 2, 3, 2, 3)
#' make_unordered_pairs(1:3, 1:2)$y == c(1, 1, 1, 2, 2)
#'
#' @export
make_unordered_pairs <- function(x, y = NULL) {
    make_pairs(x, y, ordered = FALSE)
}

make_pairs <- function(x, y = NULL, ordered = TRUE) {
    if (is.null(y))
        y <- x
    xy <- outer(seq_along(x), seq_along(y), paste)
    if (!ordered)  # Remove duplicates.
        xy <- xy[lower.tri(xy, diag = TRUE)]
    dim(xy) <- NULL
    pairs <- submatch("(\\d+) (\\d+)", xy)
    data.frame(
        x = x[as.integer(pairs[, 1L])],
        y = y[as.integer(pairs[, 2L])],
        stringsAsFactors = FALSE)
}
