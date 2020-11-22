gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"

test_that("compute densities", {
  expect_equal(density(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6))),
               c(0.00875928, 0.01301928, 2.22006e-20, 6.113256e-25),
               tolerance = 0.01)
  expect_equal(density(gmm_1, matrix(c(0, 3, 6, 9, 4, 7, 1, 6), 4,
                                     dimnames = list(NULL, c("A", "B")))),
               c(0.00875928, 0.01301928, 2.22006e-20, 6.113256e-25),
               tolerance = 0.01)
})

test_that("compute densities with row names", {
  expect_equal(density(gmm_1,
                       data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6),
                                  row.names = c("row_1", "row_2", "row_3",
                                                "row_4"))),
               c(row_1 = 0.00875928, row_2 = 0.01301928, row_3 = 2.22006e-20,
                 row_4 = 6.113256e-25),
               tolerance = 0.01)
})

test_that("compute densities with missing values", {
  expect_equal(density(gmm_1,
                       data.frame(A = c(0, 3, NA, NA), B = c(4, 7, 1, NA))),
               c(0.00875928, 0.01301928, NA, NA), tolerance = 0.01)
})

test_that("compute densities with extra columns", {
  expect_equal(density(gmm_1,
                       data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6),
                                  C = c(0, 0, 0, 0))),
               c(0.00875928, 0.01301928, 2.22006e-20, 6.113256e-25),
               tolerance = 0.01)
})

test_that("compute densities with no row", {
  expect_equal(density(gmm_1, data.frame(A = numeric(), B = numeric())),
               numeric())
  expect_equal(density(gmm_1, data.frame(A = logical(), B = logical())),
               numeric())
})

test_that("compute conditional densities", {
  expect_equal(density(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                       y = "B"),
               c(0.03005989, 0.2033028, 6.79447e-17, 3.209504e-17),
               tolerance = 0.01)
})

test_that("compute conditional densities with row names", {
  expect_equal(density(gmm_1,
                       data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6),
                                  row.names = c("row_1", "row_2", "row_3",
                                                "row_4")),
                       y = "B"),
               c(row_1 = 0.03005989, row_2 = 0.2033028, row_3 = 6.79447e-17,
                 row_4 = 3.209504e-17),
               tolerance = 0.01)
})

test_that("compute conditional densities with missing values", {
  expect_equal(density(gmm_1,
                       data.frame(A = c(0, 3, NA, NA), B = c(4, 7, 1, NA)),
                       y = "B"),
               c(0.03005989, 0.2033028, NA, NA), tolerance = 0.01)
})

test_that("compute conditional densities with extra columns", {
  expect_equal(density(gmm_1,
                       data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6),
                                  C = c(0, 0, 0, 0)),
                       y = "B"),
               c(0.03005989, 0.2033028, 6.79447e-17, 3.209504e-17),
               tolerance = 0.01)
})

test_that("compute conditional densities with duplicated dependent variables", {
  expect_equal(density(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                       y = c("B", "B")),
               c(0.03005989, 0.2033028, 6.79447e-17, 3.209504e-17),
               tolerance = 0.01)
})

test_that("compute conditional densities with no explanatory variable", {
  expect_equal(density(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                       y = c("A", "B")),
               c(0.00875928, 0.01301928, 2.22006e-20, 6.113256e-25),
               tolerance = 0.01)
})

test_that("compute conditional densities with no row", {
  expect_equal(density(gmm_1, data.frame(A = numeric(), B = numeric()),
                       y = "B"),
               numeric())
  expect_equal(density(gmm_1, data.frame(A = logical(), B = logical()),
                       y = "B"),
               numeric())
})

test_that("compute log-densities", {
  expect_equal(density(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                       log = TRUE),
               c(- 4.737642, - 4.341324, - 45.254168, - 55.754168),
               tolerance = 0.01)
})

test_that("compute conditional log-densities", {
  expect_equal(density(gmm_1, data.frame(A = c(0, 3, 6, 9), B = c(4, 7, 1, 6)),
                       y = "B", log = TRUE),
               c(- 3.504564, - 1.593059, - 37.227837, - 37.97783),
               tolerance = 0.01)
})
