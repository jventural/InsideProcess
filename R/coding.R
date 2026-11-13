# Coding the open answers of each stage and agreement between coders.

# Code columns of each coded stage. The response stage is not coded: it comes
# from the closed `response` probe.
rp_stage_cols <- c(comprehension = "code", retrieval = "code_retrieval", judgment = "code_judgment")
rp_note_cols <- c(comprehension = "note", retrieval = "note_retrieval", judgment = "note_judgment")
rp_text_cols <- c(comprehension = "paraphrase", retrieval = "retrieval", judgment = "judgment")

rp_stage_labels <- function(lang = "es") {
  if (lang == "es") c(comprehension = "Comprensi\u00F3n", retrieval = "Recuperaci\u00F3n", judgment = "Juicio", response = "Respuesta")
  else c(comprehension = "Comprehension", retrieval = "Retrieval", judgment = "Judgment", response = "Response")
}

#' Coding scheme for the stages of the response process
#'
#' Returns the coding scheme used by the package. The open answers of each
#' coded stage take the same four letters, so that severity and agreement are
#' computed in the same way, but each letter has a meaning of its own in each
#' stage of the survey response model (Tourangeau et al., 2000):
#'
#' | Code | Comprehension (paraphrase) | Retrieval (what came to mind) | Judgment (why that answer) |
#' |---|---|---|---|
#' | `A` | Captures the intended idea | Recalls situations relevant to the construct and its context | The reason supports the answer and rests on the construct |
#' | `P` | Partial, or shifts the focus | Vague or generic recall | Rests on another criterion (what is desirable, what others do, a hypothetical case) |
#' | `D` | Gives the item another meaning | Recalls another domain or construct | The reason contradicts the chosen answer |
#' | `E` | Echo: repeats the item | Nothing assessable | Nothing assessable |
#'
#' `P` weighs 1 and `D` weighs 2 in the severity; `E` is left out of the
#' indices because it does not allow the stage to be judged. The response
#' stage is not coded: it comes from the closed `response` probe ("was it hard
#' to choose an option?") and from the use of the answer options
#' ([rp_options()]).
#'
#' @param lang `"es"` or `"en"`.
#'
#' @return A list with three data frames: `codes` (code, severity weight and
#'   meaning in each coded stage), `stages` (stage, the question it answers,
#'   the probes that document it and the column that holds its code) and
#'   `columns` (code and note columns expected in a coding table).
#' @references Tourangeau, R., Rips, L. J., & Rasinski, K. (2000). *The
#'   psychology of survey response*. Cambridge University Press.
#'   \doi{10.1017/CBO9780511819322}
#' @export
#' @examples
#' rp_scheme()$codes
rp_scheme <- function(lang = NULL) {
  lang <- rp_lang(lang)
  es <- lang == "es"
  codes <- data.frame(
    code = c("A", "P", "D", "E"),
    severity = c(0, 1, 2, NA),
    comprehension = if (es) c("Capta la idea", "Parcial o desplaza el foco", "Otro significado", "Eco (repite la frase)")
                    else c("Captures the idea", "Partial or shifted focus", "Different meaning", "Echo (repeats the item)"),
    retrieval = if (es) c("Recuerdo pertinente al constructo", "Recuerdo vago o gen\u00E9rico", "Otro \u00E1mbito u otro constructo", "Nada evaluable")
                else c("Recall relevant to the construct", "Vague or generic recall", "Another domain or construct", "Nothing assessable"),
    judgment = if (es) c("La raz\u00F3n sostiene la respuesta", "Otro criterio (deseabilidad, norma, caso hipot\u00E9tico)", "La raz\u00F3n contradice la respuesta", "Nada evaluable")
               else c("The reason supports the answer", "Another criterion (desirability, norm, hypothetical case)", "The reason contradicts the answer", "Nothing assessable"),
    stringsAsFactors = FALSE)
  stages <- data.frame(
    stage = names(rp_stage_labels(lang)),
    label = unname(rp_stage_labels(lang)),
    question = if (es) c("\u00BFEntiende lo que la frase pregunta?", "\u00BFRecuerda informaci\u00F3n pertinente?",
                         "\u00BFIntegra esa informaci\u00F3n en un juicio coherente?", "\u00BFUbica su juicio en las opciones?")
               else c("Does the person understand what is asked?", "Does the person recall relevant information?",
                      "Does the person integrate it into a coherent judgment?", "Does the person map the judgment onto the options?"),
    probes = c("clarity, paraphrase, confusing", "retrieval", "answer, judgment", "response, answer"),
    code_column = c(rp_stage_cols, response = "-"),
    stringsAsFactors = FALSE, row.names = NULL)
  columns <- data.frame(stage = names(rp_stage_cols), code = unname(rp_stage_cols), note = unname(rp_note_cols),
                        text = unname(rp_text_cols), stringsAsFactors = FALSE)
  list(codes = codes, stages = stages, columns = columns)
}

# Coded stages present in the data: those whose probe was asked.
rp_stages_in <- function(d) {
  has <- vapply(rp_text_cols, function(v) v %in% names(d) && any(!is.na(d[[v]]) & d[[v]] != ""), logical(1))
  names(rp_text_cols)[has]
}

#' Template to code the open answers
#'
#' Builds a table with one row per answer, ready to be coded by one or more
#' coders (in Excel or in R). For each coded stage present in the data
#' (comprehension when there are paraphrases, retrieval and judgment when
#' those probes were asked) it adds an empty code column and a note column:
#' `code` and `note` (comprehension), `code_retrieval` and `note_retrieval`,
#' `code_judgment` and `note_judgment`. Codes are `A`, `P`, `D` or `E`, with
#' the meaning of each stage given by [rp_scheme()]. `problem_type` is free
#' text (for example vague term, double-barrelled, implicit assumption).
#'
#' @param data Answers read with [rp_read()].
#' @param file Optional path of a csv file to write (UTF-8 with BOM, so that
#'   Excel shows accents correctly).
#' @param only_flagged If `TRUE`, keeps only the answers in which the
#'   participant declared some difficulty. Default `FALSE`: every answer.
#'
#' @return The template as a data frame (invisibly when `file` is given).
#' @seealso [rp_code()], [rp_agreement()], [rp_scheme()]
#' @export
#' @examples
#' tpl <- rp_coding_template(rp_example)
#' names(tpl)
rp_coding_template <- function(data, file = NULL, only_flagged = FALSE) {
  rp_check_data(data, c("participant", "n"))
  d <- as.data.frame(data)
  if (only_flagged && "clarity" %in% names(d)) d <- d[rp_clarity_level(d$clarity) > 0, , drop = FALSE]
  keep <- intersect(c("participant", "n", "item_code", "dimension", "item", "clarity", "paraphrase", "confusing",
                      "retrieval", "answer", "judgment", "response"), names(d))
  out <- d[, keep, drop = FALSE]
  if ("clarity" %in% names(out)) out$clarity <- as.character(out$clarity)
  out$participant <- as.character(out$participant)
  st <- rp_stages_in(d)
  if (!length(st)) st <- "comprehension"
  for (s in st) {
    out[[rp_stage_cols[[s]]]] <- ""
    out[[rp_note_cols[[s]]]] <- ""
  }
  out$problem_type <- ""
  rownames(out) <- NULL
  if (!is.null(file)) {
    con <- file(file, open = "w", encoding = "UTF-8")
    cat("\uFEFF", file = con)
    utils::write.csv(out, con, row.names = FALSE, na = "")
    close(con)
    return(invisible(out))
  }
  out
}

rp_clean_code <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "NA")] <- NA
  bad <- setdiff(stats::na.omit(unique(x)), c("A", "P", "D", "E"))
  if (length(bad)) stop("Codes must be A, P, D or E. Found: ", paste(bad, collapse = ", "), call. = FALSE)
  x
}

rp_read_coding <- function(coding) {
  if (is.character(coding) && length(coding) == 1) {
    coding <- if (tolower(tools::file_ext(coding)) %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) stop("Install the readxl package to read Excel files.", call. = FALSE)
      as.data.frame(readxl::read_excel(coding))
    } else rp_read_csv(coding)
  }
  coding <- rp_standardize(as.data.frame(coding, stringsAsFactors = FALSE))
  if (!"participant" %in% names(coding) && "p" %in% names(coding)) coding$participant <- paste0("P", coding$p)
  if (!all(c("participant", "n") %in% names(coding)) || !any(rp_stage_cols %in% names(coding))) {
    stop("The coding needs the columns `participant` (or `p`), `n` and at least one code column (",
         paste(rp_stage_cols, collapse = ", "), ").", call. = FALSE)
  }
  coding$participant <- as.character(coding$participant)
  coding$participant <- ifelse(grepl("^\\d+$", coding$participant), paste0("P", coding$participant), coding$participant)
  coding$n <- as.integer(coding$n)
  for (v in intersect(rp_stage_cols, names(coding))) coding[[v]] <- rp_clean_code(coding[[v]])
  if (anyDuplicated(coding[, c("participant", "n")])) stop("The coding has repeated participant-item pairs.", call. = FALSE)
  coding
}

#' Add the coding of the open answers to the answers
#'
#' Joins a coding table (from [rp_coding_template()], a csv or an Excel
#' file) to the answers by participant and item. It takes every code column
#' present: `code` (comprehension), `code_retrieval` and `code_judgment`, with
#' their notes. Answers that are not in the coding, or whose code is empty,
#' receive `default`, but only in the stages whose probe the participant
#' answered. This allows a quick coding in which only the problematic answers
#' are listed and the rest count as adequate (`default = "A"`).
#'
#' The coding may identify participants as `P3` (column `participant`) or by
#' their order number (column `p`). Spanish names are accepted: `codigo`,
#' `nota`, `codigo_recuperacion`, `nota_recuperacion`, `codigo_juicio`,
#' `nota_juicio` and `tipo_problema`.
#'
#' @param data Answers read with [rp_read()].
#' @param coding Data frame or path to a csv or Excel file with `participant`
#'   (or `p`), `n` and one or more code columns.
#' @param default Code for answers left without a code. Use `NA` to keep them
#'   uncoded (they are then left out of the indices).
#'
#' @return `data` with the code and note columns of every coded stage and
#'   `problem_type`.
#' @seealso [rp_coding_template()], [rp_scheme()], [rp_metrics()]
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' table(ci$code)
#' table(ci$code_judgment)
rp_code <- function(data, coding, default = "A") {
  rp_check_data(data, c("participant", "n"))
  cod <- rp_read_coding(coding)
  cls <- class(data)
  att <- attr(data, "participants")
  data <- as.data.frame(data)
  data <- data[, setdiff(names(data), c(rp_stage_cols, rp_note_cols, "stage", "problem_type")), drop = FALSE]
  key_d <- paste(as.character(data$participant), data$n)
  key_c <- paste(cod$participant, cod$n)
  miss <- setdiff(key_c, key_d)
  if (length(miss)) warning(length(miss), " coded rows do not match any answer (for example ", miss[1], ").", call. = FALSE)
  i <- match(key_d, key_c)
  st <- union("comprehension", rp_stages_in(data))
  for (s in st) {
    cc <- rp_stage_cols[[s]]
    x <- if (cc %in% names(cod)) cod[[cc]][i] else rep(NA_character_, nrow(data))
    answered <- if (rp_text_cols[[s]] %in% names(data)) !is.na(data[[rp_text_cols[[s]]]]) else rep(s == "comprehension", nrow(data))
    x[is.na(x) & answered] <- default
    data[[cc]] <- x
    nc <- rp_note_cols[[s]]
    data[[nc]] <- if (nc %in% names(cod)) as.character(cod[[nc]][i]) else NA_character_
    data[[nc]][data[[nc]] %in% ""] <- NA_character_
  }
  data$problem_type <- if ("problem_type" %in% names(cod)) as.character(cod$problem_type[i]) else NA_character_
  data$problem_type[data$problem_type %in% ""] <- NA_character_
  attr(data, "participants") <- att
  class(data) <- cls
  data
}

#' Agreement between two coders
#'
#' Compares the codes that two coders gave to the same answers in one stage
#' and reports the percentage of agreement, Cohen's kappa and Gwet's AC1, with
#' their asymptotic confidence intervals, and the cross table of codes. AC1 is
#' reported because, when almost every answer is adequate, kappa can be low
#' even with high agreement (the kappa paradox). Run it once per coded stage.
#'
#' @param coder1,coder2 Codings of the same answers: data frames or paths
#'   with `participant` (or `p`), `n` and the code column of `stage`, or two
#'   character vectors of codes of the same length.
#' @param stage Stage whose codes are compared: `"comprehension"` (column
#'   `code`), `"retrieval"` (`code_retrieval`) or `"judgment"`
#'   (`code_judgment`). Ignored when codes are given as vectors.
#' @param default Code given to answers missing from one of the codings
#'   (as in [rp_code()]). Use `NA` to drop them.
#' @param conf Confidence level of the intervals.
#'
#' @return An object of class `rp_agreement`: a list with `stage`, `n`
#'   (answers compared), `agreement` (proportion), `kappa` and `ac1` (each a
#'   vector with estimate, lower and upper limit) and `table` (cross table of
#'   codes).
#' @references Gwet, K. L. (2008). Computing inter-rater reliability and its
#'   variance in the presence of high agreement. *British Journal of
#'   Mathematical and Statistical Psychology, 61*(1), 29-48.
#'   \doi{10.1348/000711006X126600}
#' @export
#' @examples
#' c1 <- rp_example_coding
#' c2 <- c1
#' i <- which(!is.na(c2$code_judgment))[1:3]
#' c2$code_judgment[i] <- "A"
#' rp_agreement(c1, c2, stage = "judgment")
rp_agreement <- function(coder1, coder2, stage = c("comprehension", "retrieval", "judgment"),
                         default = "A", conf = .95) {
  stage <- match.arg(stage)
  if (is.atomic(coder1) && is.atomic(coder2) && !(is.character(coder1) && length(coder1) == 1 && file.exists(coder1))) {
    if (length(coder1) != length(coder2)) stop("Both vectors of codes must have the same length.", call. = FALSE)
    a <- toupper(as.character(coder1)); b <- toupper(as.character(coder2))
  } else {
    cc <- rp_stage_cols[[stage]]
    c1 <- rp_read_coding(coder1); c2 <- rp_read_coding(coder2)
    if (!cc %in% names(c1) || !cc %in% names(c2)) stop("Both codings need the column `", cc, "`.", call. = FALSE)
    keys <- union(paste(c1$participant, c1$n), paste(c2$participant, c2$n))
    a <- c1[[cc]][match(keys, paste(c1$participant, c1$n))]
    b <- c2[[cc]][match(keys, paste(c2$participant, c2$n))]
    a[is.na(a)] <- default; b[is.na(b)] <- default
  }
  ok <- !is.na(a) & !is.na(b)
  a <- a[ok]; b <- b[ok]
  n <- length(a)
  if (n < 2) stop("At least two answers are needed.", call. = FALSE)
  lev <- sort(unique(c(a, b)))
  tab <- table(coder1 = factor(a, lev), coder2 = factor(b, lev))
  p <- tab / n
  po <- sum(diag(p))
  r <- rowSums(p); k <- colSums(p)
  z <- stats::qnorm(1 - (1 - conf) / 2)
  pe_k <- sum(r * k)
  kap <- if (pe_k < 1) (po - pe_k) / (1 - pe_k) else NA_real_
  se_k <- if (pe_k < 1) sqrt(po * (1 - po) / (n * (1 - pe_k)^2)) else NA_real_
  q <- length(lev)
  pi_k <- (r + k) / 2
  pe_g <- if (q > 1) sum(pi_k * (1 - pi_k)) / (q - 1) else 0
  ac1 <- (po - pe_g) / (1 - pe_g)
  se_g <- sqrt(po * (1 - po) / (n * (1 - pe_g)^2))
  lim <- function(est, se) c(estimate = est, lower = max(-1, est - z * se), upper = min(1, est + z * se))
  out <- list(stage = stage, n = n, agreement = po, kappa = lim(kap, se_k), ac1 = lim(ac1, se_g), table = tab, conf = conf)
  class(out) <- "rp_agreement"
  out
}

#' Print the agreement between coders
#'
#' @param x Output of [rp_agreement()].
#' @param ... Not used.
#' @return `x`, invisibly. Called for its side effect: it prints the
#'   agreement, kappa, AC1 and the cross table of codes.
#' @export
print.rp_agreement <- function(x, ...) {
  cat("Agreement between coders, ", x$stage, " stage (", x$n, " answers)\n", sep = "")
  cat(sprintf("  Percent agreement: %.1f %%\n", 100 * x$agreement))
  cat(sprintf("  Cohen's kappa:     %.2f [%.2f, %.2f]\n", x$kappa[1], x$kappa[2], x$kappa[3]))
  cat(sprintf("  Gwet's AC1:        %.2f [%.2f, %.2f]\n", x$ac1[1], x$ac1[2], x$ac1[3]))
  cat("\n")
  print(x$table)
  invisible(x)
}
