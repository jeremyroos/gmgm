gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"

test_that("compute the expectation", {
  expect_equal(expectation(gmm_1),
               matrix(c(0.6, 2.6), 1, dimnames = list(NULL, c("A", "B"))))
})

test_that("compute conditional expectations", {
  expect_equal(expectation(gmm_1, data.frame(A = c(0, 3, 6, 9))),
               matrix(c(1.773816, 5.972318, 10.499981, 15),
                      dimnames = list(NULL, "B")),
               tolerance = 0.01)
  expect_equal(expectation(gmm_1,
                           matrix(c(0, 3, 6, 9), dimnames = list(NULL, "A"))),
               matrix(c(1.773816, 5.972318, 10.499981, 15),
                      dimnames = list(NULL, "B")),
               tolerance = 0.01)
})

test_that("compute conditional expectations with row names", {
  expect_equal(expectation(gmm_1,
                           data.frame(A = c(0, 3, 6, 9),
                                      row.names = c("row_1", "row_2",
                                                    "row_3", "row_4"))),
               matrix(c(1.773816, 5.972318, 10.499981, 15),
                      dimnames = list(c("row_1", "row_2", "row_3", "row_4"),
                                      "B")),
               tolerance = 0.01)
})

test_that("compute conditional expectations with missing values", {
  expect_equal(expectation(gmm_1, data.frame(A = c(0, 3, 6, NA))),
               matrix(c(1.773816, 5.972318, 10.499981, NA),
                      dimnames = list(NULL, "B")),
               tolerance = 0.01)
})

test_that("compute conditional expectations with extra columns", {
  expect_equal(expectation(gmm_1,
                           data.frame(A = c(0, 3, 6, 9), C = c(0, 0, 0, 0))),
               matrix(c(1.773816, 5.972318, 10.499981, 15),
                      dimnames = list(NULL, "B")),
               tolerance = 0.01)
})

test_that("compute conditional expectations with no column", {
  expect_equal(expectation(gmm_1, data.frame(c(NA, NA, NA, NA))[, FALSE]),
               matrix(c(0.6, 0.6, 0.6, 0.6, 2.6, 2.6, 2.6, 2.6), 4,
                      dimnames = list(NULL, c("A", "B"))))
})

test_that("compute conditional expectations with no row", {
  expect_equal(expectation(gmm_1, data.frame(A = numeric())),
               matrix(numeric(), 0, 1, dimnames = list(NULL, "B")))
  expect_equal(expectation(gmm_1, data.frame(A = logical())),
               matrix(numeric(), 0, 1, dimnames = list(NULL, "B")))
})

test_that("compute conditional expectations with no row and no column", {
  expect_equal(expectation(gmm_1, data.frame()),
               matrix(numeric(), 0, 2, dimnames = list(NULL, c("A", "B"))))
})
