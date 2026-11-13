ci <- rp_code(rp_example, rp_example_coding)

test_that("rp_wilson matches the closed formula", {
  w <- rp_wilson(3, 10)
  expect_equal(unname(round(w[1, ], 4)), c(0.1078, 0.6032))
  expect_equal(unname(rp_wilson(0, 10)[1, "lower"]), 0)
})

test_that("rp_code fills missing codes with the default in every stage", {
  expect_equal(sum(ci$code != "A"), sum(!is.na(rp_example_coding$code)))
  expect_equal(sum(ci$code_retrieval != "A"), sum(!is.na(rp_example_coding$code_retrieval)))
  expect_equal(sum(ci$code_judgment != "A"), sum(!is.na(rp_example_coding$code_judgment)))
  expect_true(all(is.na(rp_code(rp_example, rp_example_coding, default = NA)$code[ci$code == "A"])))
  p_num <- transform(rp_example_coding, p = as.integer(sub("P", "", participant)))[, c("p", "n", "code")]
  x <- rp_code(rp_example, p_num)
  expect_identical(x$code, ci$code)
  expect_true(all(x$code_judgment == "A"))
})

test_that("comprehension-only metrics are computed by hand", {
  m <- rp_metrics(ci, stages = "comprehension")
  k <- m[m$n == 6, ]
  x <- ci[ci$n == 6, ]
  sig <- as.integer(x$clarity) > 1 | x$code %in% c("P", "D")
  expect_equal(k$problem, mean(sig))
  expect_equal(k$problem_comprehension, k$problem)
  expect_equal(k$comprehension, sum(x$code == "A") / sum(x$code != "E"))
  expect_equal(k$discordance, sum(as.integer(x$clarity) == 1 & x$code %in% c("P", "D")))
  expect_s3_class(m$decision, "factor")
  expect_true(all(diff(m$problem) <= 0))
  m2 <- rp_metrics(ci, cut_reformulate = .2, cut_revise = .1, lang = "en", stages = "comprehension")
  expect_equal(levels(m2$decision), c("Keep", "Revise", "Reformulate"))
})

test_that("metrics with every stage are computed by hand", {
  m <- rp_metrics(ci)
  expect_equal(attr(m, "settings")$stages, c("comprehension", "retrieval", "judgment", "response"))
  k <- m[m$n == 8, ]
  x <- ci[ci$n == 8, ]
  s_c <- as.integer(x$clarity) > 1 | x$code %in% c("P", "D")
  s_r <- x$code_retrieval %in% c("P", "D")
  s_j <- x$code_judgment %in% c("P", "D")
  s_s <- x$response != "No, fue fácil"
  expect_equal(k$problem, mean(s_c | s_r | s_j | s_s))
  expect_equal(k$problem_judgment, mean(s_j))
  expect_equal(k$problem_response, mean(s_s))
  expect_equal(k$judgment_index, sum(x$code_judgment == "A") / sum(x$code_judgment != "E"))
  expect_true(all(m$problem >= m$problem_comprehension))
  expect_true(all(m$main_stage %in% c(unname(InsideProcess:::rp_stage_labels("es")), NA)))
  st <- rp_stages(ci)
  expect_equal(st$key, c("comprehension", "retrieval", "judgment", "response"))
  expect_equal(st$problems[st$key == "judgment"], sum(ci$code_judgment %in% c("P", "D")))
  expect_equal(nrow(rp_stages(ci, by_item = TRUE)), 8 * 4)
})

test_that("metrics work without coding", {
  expect_message(m <- rp_metrics(rp_example), "No paraphrase coding")
  expect_true(all(is.na(m$comprehension)))
  expect_equal(attr(m, "settings")$stages, c("comprehension", "response"))
})

test_that("answer options are counted", {
  lv <- c("Nunca", "A veces", "Casi siempre", "Siempre")
  o <- rp_options(ci, levels = lv)
  expect_equal(nrow(o), 8)
  expect_equal(rowSums(o[, lv]), o$answers)
  expect_error(rp_options(ci, levels = lv[1:2]), "not in")
})

test_that("summary, sensitivity, quality and dimension run", {
  s <- rp_summary(ci)
  expect_s3_class(s, "rp_summary")
  expect_equal(s$participants, 12)
  expect_output(print(s), "Comprehension index")
  expect_output(print(s), "by stage")
  sn <- rp_sensitivity(ci, exclude = "P9", only_changes = FALSE)
  expect_equal(nrow(sn), 8)
  q <- rp_quality(ci)
  expect_equal(nrow(q), 12)
  expect_true(q$echo[q$participant == "P9"])
  expect_equal(nrow(rp_dimension(ci)), 2)
  r <- rp_readings(ci, items = "ORG04")
  expect_true(all(c("code_judgment", "judgment") %in% names(r)))
})

test_that("agreement gives perfect values for identical codings", {
  a <- rp_agreement(ci$code, ci$code)
  expect_equal(a$agreement, 1)
  expect_equal(unname(a$kappa["estimate"]), 1)
  expect_output(print(a), "Cohen")
  c2 <- rp_example_coding
  i <- which(!is.na(c2$code_judgment))[1:3]
  c2$code_judgment[i] <- "A"
  b <- rp_agreement(rp_example_coding, c2, stage = "judgment")
  expect_lt(b$agreement, 1)
  expect_equal(b$stage, "judgment")
})
