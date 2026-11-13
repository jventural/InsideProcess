#' InsideProcess: validity evidence based on response processes
#'
#' Standalone functions to gather and quantify validity evidence based on
#' response processes through self-administered cognitive interviews, stage
#' by stage of the survey response model: comprehension, retrieval, judgment
#' and response.
#'
#' - Form: [rp_form()], [rp_apps_script()], [rp_google_form()], [rp_profile()].
#' - Data: [rp_read()], [rp_read_google_form()], [rp_coding_template()], [rp_code()].
#' - Coding: [rp_scheme()], [rp_agreement()].
#' - Metrics: [rp_metrics()], [rp_wilson()], [rp_summary()], [rp_sensitivity()],
#'   [rp_quality()], [rp_dimension()], [rp_stages()], [rp_options()], [rp_readings()].
#' - Tables and figures: [rp_table()], [rp_word()], [rp_plot_clarity()],
#'   [rp_plot_map()], [rp_plot_dimension()], [rp_plot_problem()], [rp_plot_stages()].
#'
#' Labels of tables and figures are in Spanish by default; use
#' `options(InsideProcess.lang = "en")` for English.
#'
#' @keywords internal
"_PACKAGE"

#' Example cognitive interview (simulated)
#'
#' Simulated answers of 12 participants to 8 items of a fictitious
#' questionnaire on study engagement, gathered with every probe of
#' [rp_form()]: clarity, paraphrase, confusing part, what came to mind
#' (retrieval), the answer on the test's scale ("Nunca", "A veces", "Casi
#' siempre", "Siempre"), the reason for that answer (judgment) and the
#' difficulty to choose an option (response). They are not real data.
#'
#' @format `rp_example`: a data frame with 96 rows (participant by item) and
#'   the columns `participant`, `id`, `n`, `item_code`, `dimension`, `item`,
#'   `clarity` (ordered factor), `paraphrase`, `confusing`, `retrieval`,
#'   `answer`, `judgment`, `response` and `seconds`.
#' @source Simulated with `data-raw/rp_example.R`.
"rp_example"

#' Coding of the example answers (simulated)
#'
#' Codes of the answers of [rp_example] that are not adequate in at least one
#' stage, as a coder would list them. Empty codes, and answers not listed,
#' count as adequate with `rp_code(default = "A")`.
#'
#' @format A data frame with the columns `participant`, `n`, `code` and
#'   `note` (comprehension), `code_retrieval` and `note_retrieval`,
#'   `code_judgment` and `note_judgment`. Codes are `P`, `D`, `E` or empty;
#'   see [rp_scheme()] for their meaning in each stage.
#' @source Simulated with `data-raw/rp_example.R`.
"rp_example_coding"

#' Items of the example questionnaire (simulated)
#'
#' @format A data frame with 8 rows and the columns `n`, `item`, `item_code`
#'   and `dimension`.
#' @source Written for the examples of the package.
"rp_example_items"
