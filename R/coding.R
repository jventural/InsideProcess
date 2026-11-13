# Coding the paraphrases and agreement between coders.

#' Codes for paraphrases and stages of the response process
#'
#' Returns the coding scheme used by the package: four codes for each
#' paraphrase and the four stages of the survey response model (Tourangeau et
#' al., 2000) to which a problem can be assigned.
#'
#' A paraphrase is judged against the intended meaning of the item and, when
#' there is one, the operational definition of its dimension. `A` (adequate)
#' captures the idea. `P` (partial) keeps only part of it or shifts the focus.
#' `D` (different) gives the item another meaning. `E` (echo) repeats the
#' sentence or answers it instead of explaining it, so comprehension cannot be
#' judged; echoes are left out of the comprehension index.
#'
#' @param lang `"es"` or `"en"`.
#'
#' @return A list with two data frames: `codes` (code, label, severity weight)
#'   and `stages` (stage and the question it answers).
#' @references Tourangeau, R., Rips, L. J., & Rasinski, K. (2000). *The
#'   psychology of survey response*. Cambridge University Press.
#'   \doi{10.1017/CBO9780511819322}
#' @export
#' @examples
#' rp_scheme()
rp_scheme <- function(lang = NULL) {
  lang <- rp_lang(lang)
  lab <- rp_code_labels(lang)
  codes <- data.frame(code = names(lab), label = unname(lab), severity = c(0, 1, 2, NA), row.names = NULL)
  stages <- if (lang == "es") {
    data.frame(stage = c("comprension", "recuperacion", "juicio", "respuesta"),
               question = c("\u00BFEntiende lo que la frase pregunta?",
                            "\u00BFRecuerda la informaci\u00F3n pertinente?",
                            "\u00BFIntegra esa informaci\u00F3n en un juicio?",
                            "\u00BFUbica su juicio en las opciones?"))
  } else {
    data.frame(stage = c("comprehension", "retrieval", "judgment", "response"),
               question = c("Does the person understand what is asked?",
                            "Does the person recall the relevant information?",
                            "Does the person integrate it into a judgment?",
                            "Does the person map the judgment onto the options?"))
  }
  list(codes = codes, stages = stages)
}

#' Template to code the paraphrases
#'
#' Builds a table with one row per answer, ready to be coded by one or more
#' coders (in Excel or in R). Columns `code`, `note`, `stage` and
#' `problem_type` are left empty for the coder. `code` takes `A`, `P`, `D` or
#' `E` (see [rp_scheme()]); `stage` takes a stage of the response process;
#' `problem_type` is free text (for example vague term, double-barrelled,
#' implicit assumption).
#'
#' @param data Answers read with [rp_read()].
#' @param file Optional path of a csv file to write (UTF-8 with BOM, so that
#'   Excel shows accents correctly).
#' @param only_flagged If `TRUE`, keeps only the answers in which the
#'   participant declared some difficulty. Default `FALSE`: every paraphrase.
#'
#' @return The template as a data frame (invisibly when `file` is given).
#' @seealso [rp_code()], [rp_agreement()]
#' @export
#' @examples
#' tpl <- rp_coding_template(rp_example)
#' head(tpl)
rp_coding_template <- function(data, file = NULL, only_flagged = FALSE) {
  rp_check_data(data, c("participant", "n"))
  d <- as.data.frame(data)
  if (only_flagged && "clarity" %in% names(d)) d <- d[rp_clarity_level(d$clarity) > 0, , drop = FALSE]
  keep <- intersect(c("participant", "n", "item_code", "dimension", "item", "clarity", "paraphrase", "confusing",
                      "retrieval", "answer", "judgment"), names(d))
  out <- d[, keep, drop = FALSE]
  out$clarity <- if ("clarity" %in% names(out)) as.character(out$clarity) else NULL
  out$participant <- as.character(out$participant)
  out$code <- ""
  out$note <- ""
  out$stage <- ""
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

rp_read_coding <- function(coding) {
  if (is.character(coding) && length(coding) == 1) {
    coding <- if (tolower(tools::file_ext(coding)) %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) stop("Install the readxl package to read Excel files.", call. = FALSE)
      as.data.frame(readxl::read_excel(coding))
    } else rp_read_csv(coding)
  }
  coding <- rp_standardize(as.data.frame(coding, stringsAsFactors = FALSE))
  if (!"participant" %in% names(coding) && "p" %in% names(coding)) coding$participant <- paste0("P", coding$p)
  if (!all(c("participant", "n", "code") %in% names(coding))) {
    stop("The coding needs the columns `participant` (or `p`), `n` and `code`.", call. = FALSE)
  }
  coding$participant <- as.character(coding$participant)
  coding$participant <- ifelse(grepl("^\\d+$", coding$participant), paste0("P", coding$participant), coding$participant)
  coding$n <- as.integer(coding$n)
  coding$code <- toupper(trimws(as.character(coding$code)))
  coding$code[coding$code %in% c("", "NA")] <- NA
  bad <- setdiff(stats::na.omit(unique(coding$code)), c("A", "P", "D", "E"))
  if (length(bad)) stop("Codes must be A, P, D or E. Found: ", paste(bad, collapse = ", "), call. = FALSE)
  if (anyDuplicated(coding[, c("participant", "n")])) stop("The coding has repeated participant-item pairs.", call. = FALSE)
  coding
}

#' Add the coding of the paraphrases to the answers
#'
#' Joins a coding table (from [rp_coding_template()], a csv or an Excel
#' file) to the answers by participant and item. Answers that are not in the
#' coding, or whose code is empty, receive `default`. This allows a quick
#' coding in which only the problematic paraphrases are listed and the rest
#' count as adequate (`default = "A"`).
#'
#' The coding may identify participants as `P3` (column `participant`) or by
#' their order number (column `p`). Spanish names `codigo`, `nota`, `etapa`
#' and `tipo_problema` are accepted.
#'
#' @param data Answers read with [rp_read()].
#' @param coding Data frame or path to a csv or Excel file with the columns
#'   `participant` (or `p`), `n`, `code` and, optionally, `note`, `stage` and
#'   `problem_type`.
#' @param default Code for answers left without a code. Use `NA` to keep them
#'   uncoded (they are then left out of the comprehension index).
#'
#' @return `data` with the columns `code`, `note`, `stage` and `problem_type`.
#' @seealso [rp_coding_template()], [rp_metrics()]
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' table(ci$code)
rp_code <- function(data, coding, default = "A") {
  rp_check_data(data, c("participant", "n"))
  cod <- rp_read_coding(coding)
  cls <- class(data)
  att <- attr(data, "participants")
  data <- as.data.frame(data)
  data <- data[, setdiff(names(data), c("code", "note", "stage", "problem_type")), drop = FALSE]
  key_d <- paste(as.character(data$participant), data$n)
  key_c <- paste(cod$participant, cod$n)
  miss <- setdiff(key_c, key_d)
  if (length(miss)) warning(length(miss), " coded rows do not match any answer (for example ", miss[1], ").", call. = FALSE)
  i <- match(key_d, key_c)
  data$code <- cod$code[i]
  data$code[is.na(data$code)] <- default
  for (v in c("note", "stage", "problem_type")) {
    data[[v]] <- if (v %in% names(cod)) as.character(cod[[v]][i]) else NA_character_
    data[[v]][data[[v]] %in% ""] <- NA_character_
  }
  attr(data, "participants") <- att
  class(data) <- cls
  data
}

#' Agreement between two coders
#'
#' Compares the codes that two coders gave to the same paraphrases and
#' reports the percentage of agreement, Cohen's kappa and Gwet's AC1, with
#' their asymptotic confidence intervals, and the cross table of codes.
#' AC1 is reported because, when almost every paraphrase is adequate, kappa
#' can be low even with high agreement (the kappa paradox).
#'
#' @param coder1,coder2 Codings of the same answers: data frames or paths
#'   with `participant` (or `p`), `n` and `code`, or two character vectors of
#'   codes of the same length.
#' @param default Code given to answers missing from one of the codings
#'   (as in [rp_code()]). Use `NA` to drop them.
#' @param conf Confidence level of the intervals.
#'
#' @return An object of class `rp_agreement`: a list with `n` (answers
#'   compared), `agreement` (proportion), `kappa` and `ac1` (each a vector with
#'   estimate, lower and upper limit) and `table` (cross table of codes).
#' @references Gwet, K. L. (2008). Computing inter-rater reliability and its
#'   variance in the presence of high agreement. *British Journal of
#'   Mathematical and Statistical Psychology, 61*(1), 29-48.
#'   \doi{10.1348/000711006X126600}
#' @export
#' @examples
#' set.seed(1)
#' c1 <- rp_example_coding$code
#' c2 <- c1
#' c2[sample(length(c2), 3)] <- "A"
#' rp_agreement(c1, c2)
rp_agreement <- function(coder1, coder2, default = "A", conf = .95) {
  if (is.atomic(coder1) && is.atomic(coder2) && !(is.character(coder1) && length(coder1) == 1 && file.exists(coder1))) {
    if (length(coder1) != length(coder2)) stop("Both vectors of codes must have the same length.", call. = FALSE)
    a <- toupper(as.character(coder1)); b <- toupper(as.character(coder2))
  } else {
    c1 <- rp_read_coding(coder1); c2 <- rp_read_coding(coder2)
    keys <- union(paste(c1$participant, c1$n), paste(c2$participant, c2$n))
    a <- c1$code[match(keys, paste(c1$participant, c1$n))]
    b <- c2$code[match(keys, paste(c2$participant, c2$n))]
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
  out <- list(n = n, agreement = po, kappa = lim(kap, se_k), ac1 = lim(ac1, se_g), table = tab, conf = conf)
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
  cat("Agreement between coders (", x$n, " answers)\n", sep = "")
  cat(sprintf("  Percent agreement: %.1f %%\n", 100 * x$agreement))
  cat(sprintf("  Cohen's kappa:     %.2f [%.2f, %.2f]\n", x$kappa[1], x$kappa[2], x$kappa[3]))
  cat(sprintf("  Gwet's AC1:        %.2f [%.2f, %.2f]\n", x$ac1[1], x$ac1[2], x$ac1[3]))
  cat("\n")
  print(x$table)
  invisible(x)
}
