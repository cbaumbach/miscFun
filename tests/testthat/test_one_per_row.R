context("one_per_row")

test_that("the single-column case works", {

    din <- utils::read.table(text = "
    x y z
    1 a,c A,C
    2 b,d B,D
    3 e E
    ", header = TRUE, stringsAsFactors = FALSE)

    dout <- utils::read.table(text = "
    x y z
    1 a A,C
    1 c A,C
    2 b B,D
    2 d B,D
    3 e E
    ", header = TRUE, stringsAsFactors = FALSE)

    expect_that(one_per_row(din, "y"), equals(dout))
})

test_that("multiple columns can be specified", {

    din <- utils::read.table(text = "
    x y z
    1 a,c A,C
    2 b,d B,D
    3 e E
    ", header = TRUE, stringsAsFactors = FALSE)

    dout <- utils::read.table(text = "
    x y z
    1 a A
    1 a C
    1 c A
    1 c C
    2 b B
    2 b D
    2 d B
    2 d D
    3 e E
    ", header = TRUE, stringsAsFactors = FALSE)

    expect_that(one_per_row(din, c("y","z")), equals(dout))
})
