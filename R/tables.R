# Publication-ready tables.

#' Tables of a cognitive interview
#'
#' Returns, ready to publish, the tables of a cognitive interview report.
#' Each `type` is computed from the answers (coded with [rp_code()]):
#'
#' - `"participants"`: profile, items not fully clear and minutes of each participant.
#' - `"clarity"`: frequency of each clarity answer.
#' - `"confusing"`: confusing words or parts written by participants.
#' - `"codes"`: frequency of each code of a stage (`stage`), with its meaning.
#' - `"decision"`: signals by source (clarity, `P`, `D`, `E`) and decision per item.
#' - `"metrics"`: IC, IP with its Wilson interval, severity, discordance and
#'   decision; when more than one stage is analysed, also the share with a
#'   problem in each stage and the main stage.
#' - `"sensitivity"`: decision with everybody and without `exclude`.
#' - `"quality"`: length, time and echoes of each participant.
#' - `"dimension"`: problems and comprehension by dimension.
#' - `"stages"`: answers with a problem in each stage of the response process,
#'   with the Wilson interval.
#' - `"options"`: use of the answer options of the test in each item (see
#'   [rp_options()]).
#' - `"matrix"`: one row per item with decision, main problems and, if given,
#'   the proposed wording (`proposals`). It suits the supplementary material.
#'
#' @param data Answers read with [rp_read()] and coded with [rp_code()].
#' @param type Table to build (see Details).
#' @param stage For `"codes"`: `"comprehension"`, `"retrieval"` or
#'   `"judgment"`.
#' @param levels For `"options"`: the answer options in order.
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
#' rp_table(ci, "stages")
#' rp_table(ci, "codes", stage = "judgment")
rp_table <- function(data, type = c("metrics", "decision", "participants", "clarity", "confusing", "codes",
                                    "sensitivity", "quality", "dimension", "stages", "options", "matrix"),
                     stage = c("comprehension", "retrieval", "judgment"), levels = NULL,
                     exclude = NULL, proposals = NULL, gt = FALSE, digits = 2, lang = NULL, ...) {
  type <- match.arg(type)
  stage <- match.arg(stage)
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
      if (length(s$stages) > 1) {
        lab <- rp_stage_labels(lang)
        for (k in s$stages) x[[paste(L("IP", "PI"), tolower(lab[[k]]))]] <- r(m[[paste0("problem_", k)]])
        x[[L("Etapa principal", "Main stage")]] <- m$main_stage
        note <- paste(note, L("IP por etapa: participantes con problema en esa etapa / participantes.",
                              "PI by stage: participants with a problem in that stage / participants."))
      }
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
      cc <- rp_stage_cols[[stage]]
      if (!cc %in% names(d) || all(is.na(d[[cc]]))) stop("No `", cc, "` coding: add it with rp_code().", call. = FALSE)
      sch <- rp_scheme(lang)$codes
      lab <- stats::setNames(sch[[stage]], sch$code)
      tb <- table(factor(d[[cc]], levels = names(lab)))
      x <- data.frame(names(lab), unname(lab), as.integer(tb), r(100 * as.integer(tb) / sum(tb)))
      names(x) <- c(L("C\u00F3digo", "Code"), L("Significado", "Meaning"), L("Frecuencia", "Frequency"), "%")
      note <- paste0(L("Etapa: ", "Stage: "), tolower(rp_stage_labels(lang)[[stage]]), ".")
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
      g <- rp_stages(data, exclude = exclude, lang = lang)
      x <- data.frame(g$stage, g$assessable, g$problems, r(100 * g$proportion),
                      sprintf(paste0("[%.", digits, "f, %.", digits, "f]"), g$lower, g$upper))
      names(x) <- c(L("Etapa", "Stage"), L("Respuestas evaluables", "Assessable answers"), L("Con problema", "With a problem"),
                    "%", L("IC 95 %", "95% CI"))
      note <- L("Comprensi\u00F3n: dificultad declarada o parafraseo P o D. Recuperaci\u00F3n y juicio: c\u00F3digos P o D. Respuesta: alguna dificultad para elegir una opci\u00F3n. Los c\u00F3digos E no son evaluables. IC 95 %: intervalo de Wilson de la proporci\u00F3n.",
                "Comprehension: declared difficulty or paraphrase P or D. Retrieval and judgment: codes P or D. Response: some difficulty to choose an option. E codes are not assessable. 95% CI: Wilson interval of the proportion.")
      x
    },
    options = {
      o <- rp_options(data, levels = levels, exclude = exclude)
      x <- o[, setdiff(names(o), c("n", "mode_share", "concentrated", "mode")), drop = FALSE]
      x[[L("Opci\u00F3n m\u00E1s elegida (%)", "Most chosen option (%)")]] <- ifelse(is.na(o$mode), "", sprintf("%s (%.0f)", o$mode, 100 * o$mode_share))
      names(x)[names(x) == "item_code"] <- L("\u00CDtem", "Item")
      names(x)[names(x) == "answers"] <- L("Respuestas", "Answers")
      names(x)[names(x) == "unused"] <- L("Opciones sin uso", "Unused options")
      note <- L("Recuento de participantes que eligieron cada opci\u00F3n al responder la frase en la entrevista.",
                "Number of participants who chose each option when answering the item in the interview.")
      x
    },
    matrix = {
      m <- rp_metrics(data, lang = lang, ...)
      m <- m[order(m$n), , drop = FALSE]
      sig <- rp_signal(d, attr(m, "settings")$stages)
      main <- vapply(m$n, function(k) {
        y <- d[d$n == k & sig, , drop = FALSE]
        if (!nrow(y)) return("")
        txt <- if ("note" %in% names(y)) y$note else rep(NA, nrow(y))
        if ("confusing" %in% names(y)) txt <- ifelse(is.na(txt) | txt == "", y$confusing, txt)
        for (v in intersect(c("note_retrieval", "note_judgment"), names(y))) txt <- c(txt, y[[v]])
        txt <- unique(stats::na.omit(txt[txt != ""]))
        paste(utils::head(txt, 3), collapse = "; ")
      }, character(1))
      x <- data.frame(m$item_code, m$dimension, m$item, r(m$comprehension), r(m$problem), as.character(m$decision), main)
      names(x) <- c(L("\u00CDtem", "Item"), L("Dimensi\u00F3n", "Dimension"), L("Frase original", "Original sentence"),
                    L("IC", "CI"), L("IP", "PI"), L("Decisi\u00F3n", "Decision"), L("Problemas observados", "Observed problems"))
      if (length(attr(m, "settings")$stages) > 1) x <- cbind(x[, 1:6], stats::setNames(data.frame(m$main_stage), L("Etapa principal", "Main stage")), x[, 7, drop = FALSE])
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
