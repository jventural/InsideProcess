# Figures (ggplot2).

#' @importFrom ggplot2 .data
NULL

rp_col_clarity <- c("#2E7D32", "#F9A825", "#C62828")
rp_col_code <- c(A = "#2E7D32", P = "#F9A825", D = "#C62828", E = "#9E9E9E")
rp_col_decision <- c("#2E7D32", "#F9A825", "#C62828")

#' Items with declared difficulty
#'
#' Bar chart of the items that at least one participant marked as "somewhat"
#' or "not clear", with the number of participants of each kind. Items at the
#' top were marked by more participants.
#'
#' @param data Answers read with [rp_read()].
#' @param label `"code"` labels bars with the item code; `"both"` adds the
#'   sentence.
#' @param lang Language of the labels: `"es"` or `"en"`.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' rp_plot_clarity(rp_example)
rp_plot_clarity <- function(data, label = c("both", "code"), lang = NULL) {
  label <- match.arg(label)
  lang <- rp_lang(lang)
  d <- as.data.frame(data)
  d <- d[rp_clarity_level(d$clarity) > 0, , drop = FALSE]
  if (!nrow(d)) stop("No item was marked as not fully clear.", call. = FALSE)
  d$etq <- if (label == "both" && "item" %in% names(d)) paste0(d$item_code, ": ", d$item) else d$item_code
  d$etq <- vapply(strwrap_list(d$etq, 60), paste, character(1), collapse = "\n")
  d$clarity <- droplevels(factor(as.character(d$clarity), levels = levels(data$clarity)[2:3]))
  tot <- table(d$etq)
  d$etq <- factor(d$etq, levels = names(sort(tot)))
  cols <- stats::setNames(rp_col_clarity[2:3], levels(data$clarity)[2:3])
  ggplot2::ggplot(d, ggplot2::aes(y = .data$etq, fill = .data$clarity)) +
    ggplot2::geom_bar() +
    ggplot2::scale_fill_manual(values = cols, name = NULL, drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = function(l) seq(0, ceiling(l[2])), expand = ggplot2::expansion(mult = c(0, .05))) +
    ggplot2::labs(x = if (lang == "es") "Participantes" else "Participants", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank())
}

strwrap_list <- function(x, width) lapply(x, strwrap, width = width)

#' Map of codes by item and participant
#'
#' Tile chart with one row per item and one column per participant, coloured
#' by the code of one stage (A, P, D, E; see [rp_scheme()]). In the
#' comprehension stage a black dot marks that the participant also declared a
#' difficulty. Items with more problems are at the top, so the figure shows at
#' once which items fail and which participants concentrate the problems or
#' gave answers that cannot be assessed.
#'
#' @param data Answers coded with [rp_code()].
#' @param stage Stage whose codes are drawn: `"comprehension"` (paraphrase,
#'   column `code`), `"retrieval"` (`code_retrieval`) or `"judgment"`
#'   (`code_judgment`).
#' @param lang Language of the labels: `"es"` or `"en"`.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_plot_map(ci)
#' rp_plot_map(ci, stage = "judgment")
rp_plot_map <- function(data, stage = c("comprehension", "retrieval", "judgment"), lang = NULL) {
  stage <- match.arg(stage)
  lang <- rp_lang(lang)
  d <- as.data.frame(data)
  cc <- rp_stage_cols[[stage]]
  if (!cc %in% names(d) || all(is.na(d[[cc]]))) stop("No `", cc, "` coding: add it with rp_code().", call. = FALSE)
  d <- d[!is.na(d[[cc]]), , drop = FALSE]
  d$.code <- d[[cc]]
  d$.signal <- d$.code %in% c("P", "D")
  if (stage == "comprehension") d$.signal <- d$.signal | (!is.na(rp_clarity_level(d$clarity)) & rp_clarity_level(d$clarity) > 0)
  k <- tapply(d$.signal, d$item_code, sum)
  d$item_code <- factor(d$item_code, levels = names(sort(k)))
  d$participant <- rp_order_participants(d$participant)
  d$.code <- factor(d$.code, levels = c("A", "P", "D", "E"))
  d$.unclear <- stage == "comprehension" & !is.na(rp_clarity_level(d$clarity)) & rp_clarity_level(d$clarity) > 0
  lab <- rp_code_short(stage, lang)
  title <- unname(c(comprehension = if (lang == "es") "Parafraseo" else "Paraphrase",
                    retrieval = if (lang == "es") "Recuerdo" else "Recall",
                    judgment = if (lang == "es") "Raz\u00F3n" else "Reason")[stage])
  ggplot2::ggplot(d, ggplot2::aes(.data$participant, .data$item_code, fill = .data$.code)) +
    ggplot2::geom_tile(color = "white", linewidth = .8) +
    ggplot2::geom_point(data = d[d$.unclear, , drop = FALSE], size = 2, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = rp_col_code, labels = lab, drop = FALSE, name = title) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "top")
}

#' Problems by dimension (figure)
#'
#' Bars with the percentage of answers that show a signal of problem in each
#' dimension of the test.
#'
#' @param data Answers read with an item key and coded with [rp_code()].
#' @param lang Language of the labels: `"es"` or `"en"`.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_plot_dimension(ci)
rp_plot_dimension <- function(data, lang = NULL) {
  lang <- rp_lang(lang)
  g <- rp_dimension(data)
  g$dimension <- factor(g$dimension, levels = rev(g$dimension))
  ggplot2::ggplot(g, ggplot2::aes(.data$problem, .data$dimension)) +
    ggplot2::geom_col(fill = "#455A64", width = .6) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(100 * .data$problem), " %")), hjust = -.2, size = 3.5) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), " %"),
                                limits = c(0, min(1, max(g$problem) * 1.25 + .02))) +
    ggplot2::labs(x = if (lang == "es") "Respuestas con se\u00F1al de problema" else "Answers with a signal of problem", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank())
}

#' Problem index with its interval and the decision cut-offs
#'
#' Shows, for each item, the problem index (IP) as a point with its Wilson
#' interval, coloured by the decision, with dashed lines at the cut-offs to
#' revise and to reformulate and a dotted line at the threshold of a firm
#' problem. It makes visible which decisions are solid and which depend on
#' one or two participants.
#'
#' @param metrics Output of [rp_metrics()].
#' @param top Show only the `top` items with the highest IP (`NULL` shows all).
#' @param lang Language of the labels: `"es"` or `"en"`.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_plot_problem(rp_metrics(ci))
rp_plot_problem <- function(metrics, top = NULL, lang = NULL) {
  if (!inherits(metrics, "rp_metrics")) stop("`metrics` must come from rp_metrics().", call. = FALSE)
  s <- attr(metrics, "settings")
  lang <- if (is.null(lang)) s$lang else rp_lang(lang)
  m <- as.data.frame(metrics)
  if (!is.null(top)) m <- utils::head(m[order(-m$problem), , drop = FALSE], top)
  m$item_code <- factor(m$item_code, levels = rev(m$item_code[order(-m$problem, -m$severity, m$n)]))
  cols <- stats::setNames(rp_col_decision, levels(metrics$decision))
  ggplot2::ggplot(m, ggplot2::aes(.data$problem, .data$item_code, colour = .data$decision)) +
    ggplot2::geom_vline(xintercept = c(s$cut_revise, s$cut_reformulate), linetype = "dashed", colour = "grey55") +
    ggplot2::geom_vline(xintercept = s$firm, linetype = "dotted", colour = "grey55") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$problem_lower, xmax = .data$problem_upper), width = .3, orientation = "y") +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_colour_manual(values = cols, name = NULL, drop = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
    ggplot2::labs(x = if (lang == "es") "\u00CDndice de problema (IC 95 % de Wilson)" else "Problem index (95% Wilson interval)", y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank())
}

#' Problems by item and stage of the response process (figure)
#'
#' Heat map with one row per item and one column per stage analysed
#' (comprehension, retrieval, judgment, response), coloured and labelled by
#' the share of participants with a problem in that stage. It shows where each
#' item fails: an item misunderstood needs new wording, while an item
#' understood but answered with another criterion, or hard to place on the
#' scale, needs a different change.
#'
#' @param metrics Output of [rp_metrics()].
#' @param lang Language of the labels: `"es"` or `"en"`.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_plot_stages(rp_metrics(ci))
rp_plot_stages <- function(metrics, lang = NULL) {
  if (!inherits(metrics, "rp_metrics")) stop("`metrics` must come from rp_metrics().", call. = FALSE)
  s <- attr(metrics, "settings")
  lang <- if (is.null(lang)) s$lang else rp_lang(lang)
  m <- as.data.frame(metrics)
  st <- intersect(s$stages, sub("^problem_", "", names(m)))
  lab <- rp_stage_labels(lang)
  long <- do.call(rbind, lapply(st, function(k) {
    data.frame(item_code = m$item_code, stage = unname(lab[k]), value = m[[paste0("problem_", k)]], stringsAsFactors = FALSE)
  }))
  long$item_code <- factor(long$item_code, levels = rev(m$item_code[order(-m$problem, -m$severity, m$n)]))
  long$stage <- factor(long$stage, levels = unname(lab[st]))
  long$txt <- ifelse(is.na(long$value), "", paste0(round(100 * long$value), "%"))
  ggplot2::ggplot(long, ggplot2::aes(.data$stage, .data$item_code, fill = .data$value)) +
    ggplot2::geom_tile(colour = "white", linewidth = .8) +
    ggplot2::geom_text(ggplot2::aes(label = .data$txt), size = 3) +
    ggplot2::scale_fill_gradient(low = "#FFFFFF", high = "#C62828", limits = c(0, 1), na.value = "grey90",
                                 labels = function(x) paste0(round(100 * x), "%"),
                                 name = if (lang == "es") "Con problema" else "With a problem") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom")
}
