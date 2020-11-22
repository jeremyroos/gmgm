test_that("aggregate particles", {
  expect_equal(aggregation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                           c("A", "B"), col_seq = "seq"),
               tibble(seq = c(1, 2), A = c(1.5, 7.5), B = c(5.5, 3.5)))
})

test_that("aggregate initial particles", {
  expect_equal(aggregation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5)),
                           c("A", "B"), col_seq = "seq"),
               tibble(seq = c(1, 2), A = as.numeric(c(NA, NA)),
                      B = as.numeric(c(NA, NA))))
})

test_that("aggregate unordered particles", {
  expect_equal(aggregation(data.frame(seq = c(2, 1, 2, 1),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(6, 0, 9, 3), B = c(1, 4, 6, 7)),
                           c("A", "B"), col_seq = "seq"),
               tibble(seq = c(1, 2), A = c(1.5, 7.5), B = c(5.5, 3.5)))
})

test_that("aggregate particles with unordered columns", {
  expect_equal(aggregation(data.frame(B = c(4, 7, 1, 6), seq = c(1, 1, 2, 2),
                                      A = c(0, 3, 6, 9),
                                      weight = c(0.5, 0.5, 0.5, 0.5)),
                           c("A", "B"), col_seq = "seq"),
               tibble(seq = c(1, 2), A = c(1.5, 7.5), B = c(5.5, 3.5)))
})

test_that("aggregate particles with extra columns", {
  expect_equal(aggregation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 3, 6, 9), B = c(4, 7, 1, 6),
                                      C = c(0, 0, 0, 0)),
                           c("A", "B"), col_seq = "seq"),
               tibble(seq = c(1, 2), A = c(1.5, 7.5), B = c(5.5, 3.5)))
})

test_that("aggregate particles for unordered nodes", {
  expect_equal(aggregation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                           c("B", "A"), col_seq = "seq"),
               tibble(seq = c(1, 2), A = c(1.5, 7.5), B = c(5.5, 3.5)))
})

test_that("aggregate particles for duplicated nodes", {
  expect_equal(aggregation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                           c("A", "B", "B"), col_seq = "seq"),
               tibble(seq = c(1, 2), A = c(1.5, 7.5), B = c(5.5, 3.5)))
})

test_that("aggregate particles for extra nodes", {
  expect_equal(aggregation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                           c("A", "B", "C"), col_seq = "seq"),
               tibble(seq = c(1, 2), A = c(1.5, 7.5), B = c(5.5, 3.5),
                      C = as.numeric(c(NA, NA))))
})

test_that("aggregate particles with one observation sequence", {
  expect_equal(aggregation(data.frame(weight = c(0.5, 0.5), A = c(0, 3),
                                      B = c(4, 7)),
                           c("A", "B")),
               tibble(A = 1.5, B = 5.5))
  expect_equal(aggregation(data.frame(weight = c(0.5, 0.5), A = c(0, 3),
                                      B = c(4, 7)),
                           c("A", "B"), col_seq = character()),
               tibble(A = 1.5, B = 5.5))
})

test_that("aggregate particles with duplicated observation sequence column names", {
  expect_equal(aggregation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                           c("A", "B"), col_seq = c("seq", "seq")),
               tibble(seq = c(1, 2), A = c(1.5, 7.5), B = c(5.5, 3.5)))
})

test_that("aggregate particles for several time lags", {
  expect_equal(aggregation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A.1 = c(0, 3, 6, 9), B.1 = c(4, 7, 1, 6),
                                      A = c(3, 8, 5, 8), B = c(4, 9, 7, 0)),
                           c("A", "B"), col_seq = "seq", lag = c(0, 1)),
               list(lag_0 = tibble(seq = c(1, 2), A = c(5.5, 6.5),
                                   B = c(6.5, 3.5)),
                    lag_1 = tibble(seq = c(1, 2), A = c(1.5, 7.5),
                                   B = c(5.5, 3.5))))
})

test_that("aggregate particles for duplicated time lags", {
  expect_equal(aggregation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                           c("A", "B"), col_seq = "seq", lag = c(0, 0)),
               tibble(seq = c(1, 2), A = c(1.5, 7.5), B = c(5.5, 3.5)))
  expect_equal(aggregation(data.frame(seq = c(1, 1, 2, 2),
                                      weight = c(0.5, 0.5, 0.5, 0.5),
                                      A.1 = c(0, 3, 6, 9), B.1 = c(4, 7, 1, 6),
                                      A = c(3, 8, 5, 8), B = c(4, 9, 7, 0)),
                           c("A", "B"), col_seq = "seq", lag = c(0, 1, 1)),
               list(lag_0 = tibble(seq = c(1, 2), A = c(5.5, 6.5),
                                   B = c(6.5, 3.5)),
                    lag_1 = tibble(seq = c(1, 2), A = c(1.5, 7.5),
                                   B = c(5.5, 3.5))))
})
