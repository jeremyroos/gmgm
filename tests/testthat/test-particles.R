test_that("initialize particles for one observation sequence", {
  expect_equal(particles(n_part = 2), tibble(weight = c(0.5, 0.5)))
  expect_equal(particles(data.frame(NA)[, FALSE], n_part = 2),
               tibble(weight = c(0.5, 0.5)))
})

test_that("initialize particles for several observation sequences", {
  expect_equal(particles(data.frame(seq_1 = c(1, 1, 2), seq_2 = c(1, 2, 2)),
                         n_part = 2),
               tibble(seq_1 = c(1, 1, 1, 1, 2, 2), seq_2 = c(1, 1, 2, 2, 2, 2),
                      weight = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)))
})

test_that("initialize particles for duplicated observation sequences", {
  expect_equal(particles(data.frame(seq_1 = c(1, 1, 1), seq_2 = c(1, 2, 2)),
                         n_part = 2),
               tibble(seq_1 = c(1, 1, 1, 1), seq_2 = c(1, 1, 2, 2),
                      weight = c(0.5, 0.5, 0.5, 0.5)))
})
