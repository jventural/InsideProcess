ci <- rp_code(rp_example, rp_example_coding)

test_that("rp_wilson matches the closed formula", {
  w <- rp_wilson(3, 10)
  expect_equal(unname(round(w[1, ], 4)), c(0.1078, 0.6032))
  expect_equal(unname(rp_wilson(0, 10)[1, "lower"]), 0)
})

test_that("rp_code fills missing codes with the default", {
  expect_equal(sum(ci$code != "A"), nrow(rp_example_coding))
  expect_true(all(is.na(rp_code(rp_example, rp_example_coding, default = NA)$code[ci$code == "A"])))
  p_num <- transform(rp_example_coding, p = as.integer(sub("P", "", participant)))[, c("p", "n", "code")]
  expect_identical(rp_code(rp_example, p_num)$code, ci$code)
})

test_that("rp_metrics computes the indices by hand", {
  m <- rp_metrics(ci)
  k <- m[m$n == 6, ]
  x <- ci[ci$n == 6, ]
  sig <- as.integer(x$clarity) > 1 | x$code %in% c("P", "D")
  expect_equal(k$problem, mean(sig))
  expect_equal(k$comprehension, sum(x$code == "A") / sum(x$code != "E"))
  expect_equal(k$discordance, sum(as.integer(x$clarity) == 1 & x$code %in% c("P", "D")))
  expect_s3_class(m$decision, "factor")
  expect_true(all(diff(m$problem) <= 0))
  m2 <- rp_metrics(ci, cut_reformulate = .2, cut_revise = .1, lang = "en")
  expect_equal(levels(m2$decision), c("Keep", "Revise", "Reformulate"))
})

test_that("metrics work without coding", {
  expect_message(m <- rp_metrics(rp_example), "No paraphrase coding")
  expect_true(all(is.na(m$comprehension)))
})

test_that("summary, sensitivity, quality and dimension run", {
  s <- rp_summary(ci)
  expect_s3_class(s, "rp_summary")
  expect_equal(s$participants, 12)
  expect_output(print(s), "Comprehension index")
  sn <- rp_sensitivity(ci, exclude = "P9", only_changes = FALSE)
  expect_equal(nrow(sn), 8)
  q <- rp_quality(ci)
  expect_equal(nrow(q), 12)
  expect_true(q$echo[q$participant == "P9"])
  expect_equal(nrow(rp_dimension(ci)), 2)
  expect_true(nrow(rp_stages(ci)) >= 1)
})

test_that("agreement gives perfect values for identical codings", {
  a <- rp_agreement(ci$code, ci$code)
  expect_equal(a$agreement, 1)
  expect_equal(unname(a$kappa["estimate"]), 1)
  expect_output(print(a), "Cohen")
})
