# Publication-ready tables.

#' Tables of a cognitive interview
#'
#' Returns, ready to publish, the tables of a cognitive interview report.
#' Each `type` is computed from the answers (coded with [rp_code()]):
#'
#' - `"participants"`: profile, items not fully clear and minutes of each participant.
#' - `"clarity"`: frequency of each clarity answer.
#' - `"confusing"`: confusing words or parts written by participants.
#' - `"codes"`: frequency of each paraphrase code.
#' - `"decision"`: signals by source (clarity, `P`, `D`, `E`) and decision per item.
#' - `"metrics"`: IC, IP with its Wilson interval, severity, discordance and decision.
#' - `"sensitivity"`: decision with everybody and without `exclude`.
#' - `"quality"`: length, time and echoes of each participant.
#' - `"dimension"`: problems and comprehension by dimension.
#' - `"stages"`: problems by stage of the response process.
#' - `"matrix"`: one row per item with decision, main problems and, if given,
#'   the proposed wording (`proposals`). It suits the supplementary material.
#'
#' @param data Answers read with [rp_read()] and coded with [rp_code()].
#' @param type Table to build (see Details).
#' @param exclude Participants to leave out in `"sensitivity"`.
#' @param proposals For `"matrix"`: data frame with `item_code`, `proposal`
#'   and, optionally, `reason` (Spanish `propuesta` and `razon` are accepted).
#' @param gt If `TRUE` and the gt package is installed, returns a formatted gt
#'   table, with the decision and the comprehension index coloured.
#' @param digits Decimals of the indices.
#' @param lang Language of the labels: `"es"` or `"en"`.
#' @param ... Arguments passed to [rp_metrics()] (cut-offs).
#'
#' @return A data frame with labelled columns or, with `gt = TRUE`, a
#'   `gt_tbl` object.
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_table(ci, "metrics")
#' rp_table(ci, "clarity")
rp_table <- function(data, type = c("metrics", "decision", "participants", "clarity", "confusing", "codes",
                                    "sensitivity", "quality", "dimension", "stages", "matrix"),
                     exclude = NULL, proposals = NULL, gt = FALSE, digits = 2, lang = NULL, ...) {
  type <- match.arg(type)
  lang <- rp_lang(lang)
  es <- lang == "es"
  L <- function(s, e) if (es) s else e
  d <- as.data.frame(data)
  r <- function(x) round(x, digits)
  note <- NULL
  out <- switch(type,
    metrics = {
      m <- rp_metrics(data, lang = lang, ...)
      s <- attr(m, "settings")
      note <- L(paste0("IC: \u00EDndice de comprensi\u00F3n. IP: \u00EDndice de problema. IC 95 %: intervalo de Wilson. Severidad: 0 a 2. Discordancia: n\u00FAmero de participantes (de ", s$N, ")."),
                paste0("CI: comprehension index. PI: problem index. 95% CI: Wilson interval. Severity: 0 to 2. Discordance: number of participants (of ", s$N, ")."))
      x <- data.frame(m$item_code, m$dimension, r(m$comprehension), r(m$problem),
                      sprintf(paste0("[%.", digits, "f, %.", digits, "f]"), m$problem_lower, m$problem_upper),
                      r(m$severity), m$discordance, as.character(m$decision))
      names(x) <- c(L("\u00CDtem", "Item"), L("Dimensi\u00F3n", "Dimension"), L("IC", "CI"), L("IP", "PI"),
                    L("IC 95 % del IP", "95% CI of PI"), L("Severidad", "Severity"), L("Discordancia", "Discordance"),
                    L("Decisi\u00F3n", "Decision"))
      x
    },
    decision = {
      m <- rp_metrics(data, lang = lang, ...)
      note <- L(paste0("Claridad: marc\u00F3 dificultad. P, D y E: c\u00F3digos del parafraseo. Se\u00F1ales: participantes con al menos una se\u00F1al (m\u00E1ximo ", attr(m, "settings")$N, ")."),
                paste0("Clarity: declared difficulty. P, D and E: paraphrase codes. Signals: participants with at least one signal (maximum ", attr(m, "settings")$N, ")."))
      x <- data.frame(m$item_code, m$dimension, m$item, m$clarity, m$P, m$D, m$E, m$signals, as.character(m$decision))
      names(x) <- c(L("\u00CDtem", "Item"), L("Dimensi\u00F3n", "Dimension"), L("Frase", "Sentence"), L("Claridad", "Clarity"),
                    "P", "D", "E", L("Se\u00F1ales", "Signals"), L("Decisi\u00F3n", "Decision"))
      x
    },
    participants = {
      q <- rp_quality(data)
      drop <- c("chars", "seconds", "A", "P", "D", "E", "brief", "fast", "echo", "n_frases", "n_no_claras", "minutos",
                "n_items", "n_unclear", "comment", "comentario")
      x <- q[, setdiff(names(q), drop), drop = FALSE]
      x <- x[, c("participant", setdiff(names(x), c("participant", "unclear", "minutes")), "unclear", "minutes"), drop = FALSE]
      x$minutes <- round(x$minutes)
      names(x)[1] <- L("Participante", "Participant")
      names(x)[names(x) == "unclear"] <- L("Frases no claras", "Items not clear")
      names(x)[names(x) == "minutes"] <- L("Minutos", "Minutes")
      note <- L("Los minutos suman el tiempo en cada frase.", "Minutes add up the time spent on each item.")
      x
    },
    clarity = {
      tb <- table(d$clarity)
      x <- data.frame(names(tb), as.integer(tb), r(100 * as.integer(tb) / sum(tb)))
      names(x) <- c(L("Respuesta", "Answer"), L("Frecuencia", "Frequency"), "%")
      x
    },
    confusing = {
      y <- d[rp_clarity_level(d$clarity) > 0, , drop = FALSE]
      y <- y[order(y$item_code, y$participant), , drop = FALSE]
      x <- data.frame(y$item_code, y$item, as.character(y$participant), as.character(y$clarity),
                      if ("confusing" %in% names(y)) y$confusing else NA)
      names(x) <- c(L("\u00CDtem", "Item"), L("Frase", "Sentence"), L("Participante", "Participant"),
                    L("Marc\u00F3", "Marked"), L("Parte confusa", "Confusing part"))
      x
    },
    codes = {
      if (!rp_has_code(d)) stop("No coding: add it with rp_code().", call. = FALSE)
      lab <- rp_code_labels(lang)
      tb <- table(factor(d$code, levels = names(lab)))
      x <- data.frame(names(lab), unname(lab), as.integer(tb), r(100 * as.integer(tb) / sum(tb)))
      names(x) <- c(L("C\u00F3digo", "Code"), L("Significado", "Meaning"), L("Frecuencia", "Frequency"), "%")
      x
    },
    sensitivity = {
      if (is.null(exclude)) stop("Give the participants to leave out in `exclude`.", call. = FALSE)
      s <- rp_sensitivity(data, exclude = exclude, lang = lang, ...)
      w <- paste(L("sin", "without"), rp_list_text(exclude, lang, conj = L("ni", "or")))
      x <- data.frame(s$item_code, r(s$problem_all), as.character(s$decision_all), r(s$problem_without), as.character(s$decision_without))
      names(x) <- c(L("\u00CDtem", "Item"), L("IP (todos)", "PI (all)"), L("Con todos", "With all"),
                    paste0(L("IP (", "PI ("), w, ")"), paste0(toupper(substr(w, 1, 1)), substring(w, 2)))
      note <- L("Solo se listan las frases que no quedan en \"Mantener\" en al menos uno de los dos c\u00E1lculos.",
                "Only items that are not kept in at least one of the two analyses are listed.")
      x
    },
    quality = {
      q <- rp_quality(data)
      x <- data.frame(q$participant, q$chars, q$E, q$A, r(q$seconds))
      names(x) <- c(L("Participante", "Participant"), L("Caracteres (mediana)", "Characters (median)"),
                    L("Ecos", "Echoes"), L("Capta la idea", "Adequate"), L("Segundos por frase (mediana)", "Seconds per item (median)"))
      x
    },
    dimension = {
      g <- rp_dimension(data)
      x <- data.frame(g$dimension, g$items, r(100 * g$problem), r(g$comprehension))
      names(x) <- c(L("Dimensi\u00F3n", "Dimension"), L("Frases", "Items"), L("% con se\u00F1al", "% with signal"), L("IC", "CI"))
      x
    },
    stages = {
      x <- rp_stages(data)
      names(x)[names(x) == "stage"] <- L("Etapa", "Stage")
      names(x)[names(x) == "problem_type"] <- L("Tipo de problema", "Problem type")
      names(x)[names(x) == "problems"] <- L("Problemas", "Problems")
      x
    },
    matrix = {
      m <- rp_metrics(data, lang = lang, ...)
      m <- m[order(m$n), , drop = FALSE]
      main <- vapply(m$n, function(k) {
        y <- d[d$n == k & rp_signal(d), , drop = FALSE]
        if (!nrow(y)) return("")
        txt <- if ("note" %in% names(y)) y$note else rep(NA, nrow(y))
        if ("confusing" %in% names(y)) txt <- ifelse(is.na(txt) | txt == "", y$confusing, txt)
        txt <- unique(stats::na.omit(txt[txt != ""]))
        paste(utils::head(txt, 3), collapse = "; ")
      }, character(1))
      x <- data.frame(m$item_code, m$dimension, m$item, r(m$comprehension), r(m$problem), as.character(m$decision), main)
      names(x) <- c(L("\u00CDtem", "Item"), L("Dimensi\u00F3n", "Dimension"), L("Frase original", "Original sentence"),
                    L("IC", "CI"), L("IP", "PI"), L("Decisi\u00F3n", "Decision"), L("Problemas observados", "Observed problems"))
      if (!is.null(proposals)) {
        pr <- rp_standardize(as.data.frame(proposals))
        names(pr)[names(pr) %in% c("propuesta")] <- "proposal"
        names(pr)[names(pr) %in% c("razon", "motivo")] <- "reason"
        names(pr)[names(pr) %in% c("codigo_item", "item")] <- "item_code"
        if (!all(c("item_code", "proposal") %in% names(pr))) stop("`proposals` needs `item_code` and `proposal`.", call. = FALSE)
        x[[L("Redacci\u00F3n propuesta", "Proposed wording")]] <- pr$proposal[match(m$item_code, pr$item_code)]
        if ("reason" %in% names(pr)) x[[L("Motivo", "Reason")]] <- pr$reason[match(m$item_code, pr$item_code)]
      }
      x
    }
  )
  rownames(out) <- NULL
  attr(out, "note") <- note
  if (!gt) return(out)
  if (!requireNamespace("gt", quietly = TRUE)) {
    warning("Install the gt package for formatted tables; a data frame is returned.", call. = FALSE)
    return(out)
  }
  rp_gt(out, note, lang)
}

rp_gt <- function(x, note, lang) {
  g <- gt::gt(x)
  dec <- intersect(c("Decisi\u00F3n", "Decision"), names(x))
  lab <- rp_decision_labels(lang)
  if (length(dec)) {
    g <- gt::data_color(g, columns = dec, method = "factor", levels = lab,
                        palette = c("#C8E6C9", "#FFF9C4", "#FFCDD2"))
  }
  ic <- intersect(c("IC", "CI"), names(x))
  if (length(ic) && is.numeric(x[[ic]])) {
    g <- gt::data_color(g, columns = ic, palette = c("#FFCDD2", "#FFF9C4", "#C8E6C9"), domain = c(0, 1))
  }
  if (!is.null(note)) g <- gt::tab_source_note(g, note)
  g
}

#' Save tables and figures in a Word document
#'
#' Writes a Word document with the tables (data frames, for example from
#' [rp_table()]) and figures (ggplot objects, for example from
#' [rp_plot_map()]) given, each under its title, in the order given.
#'
#' @param x Named list of data frames and ggplot objects. Names are used as
#'   titles.
#' @param file Path of the `.docx` file.
#' @param width,height Size of the figures in inches.
#'
#' @return The path of the file, invisibly.
#' @export
#' @examples
#' if (requireNamespace("officer", quietly = TRUE) && requireNamespace("flextable", quietly = TRUE)) {
#'   ci <- rp_code(rp_example, rp_example_coding)
#'   f <- rp_word(list("Table 1. Item metrics" = rp_table(ci, "metrics", lang = "en"),
#'                     "Figure 1. Problem index" = rp_plot_problem(rp_metrics(ci, lang = "en"))),
#'                file.path(tempdir(), "interview.docx"))
#'   unlink(f)
#' }
rp_word <- function(x, file, width = 6.5, height = 4.5) {
  if (!requireNamespace("officer", quietly = TRUE) || !requireNamespace("flextable", quietly = TRUE)) {
    stop("Install the officer and flextable packages to write Word files.", call. = FALSE)
  }
  if (!is.list(x) || is.data.frame(x) || inherits(x, "ggplot")) x <- list(x)
  if (is.null(names(x))) names(x) <- rep("", length(x))
  doc <- officer::read_docx()
  for (k in seq_along(x)) {
    obj <- x[[k]]
    if (nzchar(names(x)[k])) doc <- officer::body_add_par(doc, names(x)[k], style = "heading 2")
    if (inherits(obj, "ggplot")) {
      png <- tempfile(fileext = ".png")
      ggplot2::ggsave(png, obj, width = width, height = height, dpi = 300)
      doc <- officer::body_add_img(doc, png, width = width, height = height)
    } else if (is.data.frame(obj)) {
      ft <- flextable::flextable(as.data.frame(obj))
      ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
      ft <- flextable::fontsize(ft, size = 10, part = "all")
      ft <- flextable::autofit(ft)
      doc <- flextable::body_add_flextable(doc, ft)
      if (!is.null(attr(obj, "note"))) doc <- officer::body_add_par(doc, paste("Nota.", attr(obj, "note")))
    } else {
      stop("Element ", k, " is neither a data frame nor a ggplot.", call. = FALSE)
    }
  }
  print(doc, target = file)
  invisible(file)
}
