test_that("rp_form writes a page with the items and probes", {
  f <- rp_form(rp_example_items, file.path(tempdir(), "f.html"), probes = c("clarity", "paraphrase", "retrieval"))
  h <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_match(h, "Organizo mi tiempo", fixed = TRUE)
  expect_match(h, "\"retrieval\"", fixed = TRUE)
  expect_false(grepl("/*__CONFIG__*/", h, fixed = TRUE))
  expect_error(suppressWarnings(rp_form(rp_example_items, tempfile(), probes = "answer")), "answer_options")
})

test_that("rp_apps_script writes the four files", {
  out <- rp_apps_script(rp_example_items, file.path(tempdir(), "app"), lang = "en")
  expect_true(all(file.exists(out)))
  gs <- paste(readLines(out[2], encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("__[A-Z_]+__", gs))
  expect_match(gs, "Responses", fixed = TRUE)
})

test_that("rp_google_form writes a script and its answers can be read", {
  f <- rp_google_form(rp_example_items, file.path(tempdir(), "g.gs"))
  expect_match(paste(readLines(f, encoding = "UTF-8"), collapse = ""), "crearFormulario", fixed = TRUE)
  wide <- data.frame(`Marca temporal` = "x", `1.1 a` = "Más o menos", `1.2 b` = "mi idea",
                     `1.3 c` = "palabra", `2.1 a` = "Sí, se entiende", `2.2 b` = "otra idea", check.names = FALSE)
  g <- rp_read_google_form(wide, items = rp_example_items)
  expect_equal(nrow(g), 2)
  expect_equal(g$item_code, c("DIS01", "ORG01"))
  expect_equal(as.integer(g$clarity), c(2L, 1L))
})

test_that("rp_read reads the csv of the offline form", {
  f <- file.path(tempdir(), "off.csv")
  d <- data.frame(id = "PX", edad = 20, n = 1:2, item = rp_example_items$item[1:2],
                  claridad = c("Sí, se entiende", "No se entiende"), parafraseo = c("a b c", "d e f"),
                  confuso = c("", "tiempo"), segundos = c(30, 40))
  con <- file(f, "w", encoding = "UTF-8"); utils::write.table(d, con, sep = ";", row.names = FALSE); close(con)
  x <- rp_read(f, items = rp_example_items)
  expect_s3_class(x, "rp_data")
  expect_equal(x$item_code, c("DIS01", "ORG01"))
  expect_equal(attr(x, "participants")$edad, 20)
  tpl <- rp_coding_template(x, file.path(tempdir(), "cod.csv"))
  expect_true(all(c("code", "note", "stage") %in% names(tpl)))
})

test_that("tables and figures are built", {
  ci <- rp_code(rp_example, rp_example_coding)
  for (t in c("metrics", "decision", "participants", "clarity", "confusing", "codes", "quality", "dimension", "stages", "matrix")) {
    expect_s3_class(rp_table(ci, t), "data.frame")
  }
  expect_s3_class(rp_table(ci, "sensitivity", exclude = "P9"), "data.frame")
  expect_s3_class(rp_plot_map(ci), "ggplot")
  expect_s3_class(rp_plot_clarity(ci), "ggplot")
  expect_s3_class(rp_plot_dimension(ci), "ggplot")
  expect_s3_class(rp_plot_problem(rp_metrics(ci)), "ggplot")
})
