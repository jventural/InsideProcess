#' InsideProcess: validity evidence based on response processes
#'
#' Standalone functions to gather and quantify validity evidence based on
#' response processes through self-administered cognitive interviews.
#'
#' - Form: [rp_form()], [rp_apps_script()], [rp_google_form()], [rp_profile()].
#' - Data: [rp_read()], [rp_read_google_form()], [rp_coding_template()], [rp_code()].
#' - Coding: [rp_scheme()], [rp_agreement()].
#' - Metrics: [rp_metrics()], [rp_wilson()], [rp_summary()], [rp_sensitivity()],
#'   [rp_quality()], [rp_dimension()], [rp_stages()], [rp_readings()].
#' - Tables and figures: [rp_table()], [rp_word()], [rp_plot_clarity()],
#'   [rp_plot_map()], [rp_plot_dimension()], [rp_plot_problem()].
#'
#' Labels of tables and figures are in Spanish by default; use
#' `options(InsideProcess.lang = "en")` for English.
#'
#' @keywords internal
"_PACKAGE"

#' Example cognitive interview (simulated)
#'
#' Simulated answers of 12 participants to 8 items of a fictitious
#' questionnaire on study engagement, gathered with the default probes of
#' [rp_form()]. They are not real data.
#'
#' @format `rp_example`: a data frame with 96 rows (participant by item) and
#'   the columns `participant`, `id`, `n`, `item_code`, `dimension`, `item`,
#'   `clarity` (ordered factor), `paraphrase`, `confusing` and `seconds`.
#' @source Simulated with `data-raw/rp_example.R`.
"rp_example"

#' Coding of the example paraphrases (simulated)
#'
#' Codes of the paraphrases of [rp_example] that are not adequate, as a coder
#' would list them; unlisted answers count as adequate with
#' `rp_code(default = "A")`.
#'
#' @format A data frame with the columns `participant`, `n`, `code`
#'   (`P`, `D` or `E`), `note` and `stage`.
#' @source Simulated with `data-raw/rp_example.R`.
"rp_example_coding"

#' Items of the example questionnaire (simulated)
#'
#' @format A data frame with 8 rows and the columns `n`, `item`, `item_code`
#'   and `dimension`.
#' @source Written for the examples of the package.
"rp_example_items"
