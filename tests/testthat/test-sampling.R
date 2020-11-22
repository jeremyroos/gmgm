gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"

test_that("draw samples", {
  set.seed(0)
  expect_equal(sampling(gmm_1, n = 4),
               matrix(c(- 0.3131921, 1.3297993, 1.2724293, 0.5832023, 1.026134,
                        3.744441, 1.732479, 4.07515),
                      4, dimnames = list(NULL, c("A", "B"))),
               tolerance = 0.01)
})

test_that("draw no sample", {
  set.seed(0)
  expect_equal(sampling(gmm_1, n = 0),
               matrix(numeric(), 0, 2, dimnames = list(NULL, c("A", "B"))))
})

test_that("draw conditional samples", {
  set.seed(0)
  expect_equal(sampling(gmm_1, data.frame(A = c(0, 3, 6, 9))),
               matrix(c(2.44031, 6.899743, 10.793196, 13.911091),
                      dimnames = list(NULL, "B")),
               tolerance = 0.01)
  set.seed(0)
  expect_equal(sampling(gmm_1,
                        matrix(c(0, 3, 6, 9), dimnames = list(NULL, "A"))),
               matrix(c(2.44031, 6.899743, 10.793196, 13.911091),
                      dimnames = list(NULL, "B")),
               tolerance = 0.01)
})

test_that("draw conditional samples with row names", {
  set.seed(0)
  expect_equal(sampling(gmm_1,
                        data.frame(A = c(0, 3, 6, 9),
                                   row.names = c("row_1", "row_2", "row_3",
                                                 "row_4"))),
               matrix(c(2.44031, 6.899743, 10.793196, 13.911091),
                      dimnames = list(c("row_1", "row_2", "row_3", "row_4"),
                                      "B")),
               tolerance = 0.01)
})

test_that("draw conditional samples with missing values", {
  set.seed(0)
  expect_equal(sampling(gmm_1, data.frame(A = c(0, 3, 6, NA))),
               matrix(c(2.44031, 6.899743, 10.793196, NA),
                      dimnames = list(NULL, "B")),
               tolerance = 0.01)
})

test_that("draw conditional samples with extra columns", {
  set.seed(0)
  expect_equal(sampling(gmm_1,
                        data.frame(A = c(0, 3, 6, 9), C = c(0, 0, 0, 0))),
               matrix(c(2.44031, 6.899743, 10.793196, 13.911091),
                      dimnames = list(NULL, "B")),
               tolerance = 0.01)
})

test_that("draw conditional samples with no column", {
  set.seed(0)
  expect_equal(sampling(gmm_1, data.frame(c(NA, NA, NA, NA))[, FALSE]),
               matrix(c(- 0.3131921, 1.3297993, 1.2724293, 0.5832023, 1.026134,
                        3.744441, 1.732479, 4.07515),
                      4, dimnames = list(NULL, c("A", "B"))),
               tolerance = 0.01)
})

test_that("draw conditional samples with no row", {
  expect_equal(sampling(gmm_1, data.frame(A = numeric())),
               matrix(numeric(), 0, 1, dimnames = list(NULL, "B")))
  expect_equal(sampling(gmm_1, data.frame(A = logical())),
               matrix(numeric(), 0, 1, dimnames = list(NULL, "B")))
})

test_that("draw conditional samples with no row and no column", {
  set.seed(0)
  expect_equal(sampling(gmm_1, data.frame()),
               matrix(numeric(), 0, 2, dimnames = list(NULL, c("A", "B"))))
})
