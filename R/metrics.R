# Metrics that quantify the decision for each item.

#' Wilson confidence interval for a proportion
#'
#' Score interval of Wilson (1927), which behaves well with the small samples
#' of cognitive interviews and when the proportion is 0 or 1.
#'
#' @param x Number of successes (for example, participants with a signal).
#' @param n Number of trials (participants).
#' @param conf Confidence level.
#'
#' @return A matrix with columns `lower` and `upper`, one row per value of `x`.
#' @references Wilson, E. B. (1927). Probable inference, the law of
#'   succession, and statistical inference. *Journal of the American
#'   Statistical Association, 22*(158), 209-212. \doi{10.1080/01621459.1927.10502953}
#' @export
#' @examples
#' rp_wilson(3, 10)
#' rp_wilson(0:3, 10)
rp_wilson <- function(x, n, conf = .95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- ifelse(n > 0, x / n, NA_real_)
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  cbind(lower = pmax(0, centre - half), upper = pmin(1, centre + half))
}

#' Metrics to quantify the decision on each item
#'
#' Computes, for each item, the metrics that turn the cognitive interview into
#' a decision: keep, revise or reformulate. A *signal* is a participant who
#' declared some difficulty (clarity "somewhat" or "not clear") or whose
#' paraphrase was partial (`P`) or gave another meaning (`D`). Echoes (`E`) are
#' not signals, because they do not allow comprehension to be judged.
#'
#' | Metric | What it measures | How it is computed |
#' |---|---|---|
#' | `comprehension` (IC) | Share that understood the item as intended | `A` paraphrases / assessable paraphrases (echoes excluded) |
#' | `problem` (IP) | Share with some signal | Participants with a signal / participants |
#' | `problem_lower`, `problem_upper` | How much IP could vary in another sample | Wilson interval |
#' | `severity` | How serious the problems were (0 to 2) | Mean of the larger of: `D` = 2, `P` = 1, "not clear" = 2, "somewhat" = 1 |
#' | `discordance` | Said it was clear but did not understand | Clarity "yes" with paraphrase `P` or `D` |
#' | `firm` | The problem is unlikely to be chance | Lower limit of IP above `firm` |
#'
#' The decision uses proportions so that it does not depend on the number of
#' participants: reformulate when IP reaches `cut_reformulate`, revise when it
#' reaches `cut_revise`, keep otherwise. The cut-offs are tentative and should
#' be declared in the method.
#'
#' @param data Answers read with [rp_read()] and, ideally, coded with
#'   [rp_code()]. Without coding, only declared clarity is used.
#' @param cut_reformulate,cut_revise Cut-offs of the problem index.
#' @param firm Threshold for the lower limit of the Wilson interval.
#' @param conf Confidence level of the interval.
#' @param exclude Participants to leave out (for example `c("P2", "P6")`).
#' @param lang Language of the decision labels: `"es"` or `"en"`.
#'
#' @return A data frame of class `rp_metrics`, one row per item ordered from
#'   the most to the least problematic, with `n`, `item_code`, `dimension`,
#'   `item`, `participants`, `clarity` (participants who declared difficulty),
#'   `A`, `P`, `D`, `E`, `assessable`, `signals`, `comprehension`, `problem`,
#'   `problem_lower`, `problem_upper`, `severity`, `discordance`, `seconds`
#'   (median, when timed), `firm` and `decision`.
#' @references Padilla, J.-L., & Benitez, I. (2014). Validity evidence based on
#'   response processes. *Psicothema, 26*(1), 136-144.
#'   \doi{10.7334/psicothema2013.259}
#' @seealso [rp_summary()], [rp_sensitivity()], [rp_table()], [rp_plot_problem()]
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' m <- rp_metrics(ci)
#' m[, c("item_code", "comprehension", "problem", "decision")]
rp_metrics <- function(data, cut_reformulate = .50, cut_revise = .30, firm = .20,
                       conf = .95, exclude = NULL, lang = NULL) {
  lang <- rp_lang(lang)
  rp_check_data(data, c("participant", "n", "clarity"))
  if (cut_revise >= cut_reformulate) stop("`cut_revise` must be lower than `cut_reformulate`.", call. = FALSE)
  d <- as.data.frame(data)
  if (!is.null(exclude)) d <- d[!as.character(d$participant) %in% exclude, , drop = FALSE]
  coded <- rp_has_code(d)
  if (!coded) message("No paraphrase coding found: metrics use declared clarity only. Add it with rp_code().")
  d$.signal <- rp_signal(d)
  d$.sev <- rp_severity_row(d)
  cl <- rp_clarity_level(d$clarity)
  d$.unclear <- !is.na(cl) & cl > 0
  d$.disc <- if (coded) !is.na(cl) & cl == 0 & d$code %in% c("P", "D") else NA
  if (!"item_code" %in% names(d)) d$item_code <- paste0("I", d$n)
  if (!"dimension" %in% names(d)) d$dimension <- NA_character_
  if (!"item" %in% names(d)) d$item <- NA_character_
  sp <- split(d, d$n)
  rows <- lapply(sp, function(x) {
    code <- if (coded) x$code else rep(NA_character_, nrow(x))
    np <- sum(!is.na(x$clarity))
    data.frame(n = x$n[1], item_code = x$item_code[1], dimension = x$dimension[1], item = x$item[1],
               participants = np, clarity = sum(x$.unclear),
               A = sum(code == "A", na.rm = TRUE), P = sum(code == "P", na.rm = TRUE),
               D = sum(code == "D", na.rm = TRUE), E = sum(code == "E", na.rm = TRUE),
               assessable = sum(!is.na(code) & code != "E"), signals = sum(x$.signal),
               severity = mean(x$.sev), discordance = if (coded) sum(x$.disc) else NA_integer_,
               seconds = if ("seconds" %in% names(x)) stats::median(x$seconds, na.rm = TRUE) else NA_real_,
               stringsAsFactors = FALSE)
  })
  m <- do.call(rbind, rows)
  m$comprehension <- ifelse(m$assessable > 0, m$A / m$assessable, NA_real_)
  m$problem <- m$signals / m$participants
  ci <- rp_wilson(m$signals, m$participants, conf)
  m$problem_lower <- ci[, "lower"]
  m$problem_upper <- ci[, "upper"]
  m$firm <- m$problem_lower > firm
  m$decision <- rp_decide(m$problem, cut_reformulate, cut_revise, lang)
  m <- m[order(-m$problem, -m$severity, m$n), c("n", "item_code", "dimension", "item", "participants", "clarity",
                                                 "A", "P", "D", "E", "assessable", "signals", "comprehension",
                                                 "problem", "problem_lower", "problem_upper", "severity",
                                                 "discordance", "seconds", "firm", "decision")]
  rownames(m) <- NULL
  attr(m, "settings") <- list(cut_reformulate = cut_reformulate, cut_revise = cut_revise, firm = firm,
                              conf = conf, exclude = exclude, coded = coded, lang = lang,
                              N = length(unique(d$participant)))
  class(m) <- c("rp_metrics", "data.frame")
  m
}

#' Print item metrics
#'
#' @param x Output of [rp_metrics()].
#' @param digits Decimals shown.
#' @param ... Not used.
#' @return `x`, invisibly. Called for its side effect: it prints the key
#'   metrics and the decision of each item.
#' @export
print.rp_metrics <- function(x, digits = 2, ...) {
  s <- attr(x, "settings")
  cols <- c("item_code", "comprehension", "problem", "problem_lower", "problem_upper",
            "severity", "discordance", "decision")
  if (!all(cols %in% names(x))) {
    print(as.data.frame(x), digits = digits + 1)
    return(invisible(x))
  }
  y <- as.data.frame(x)[, cols]
  for (v in c("comprehension", "problem", "problem_lower", "problem_upper", "severity")) y[[v]] <- round(y[[v]], digits)
  cat("Item metrics (N = ", s$N, "; reformulate >= ", s$cut_reformulate, ", revise >= ", s$cut_revise, ")\n", sep = "")
  print(y, row.names = FALSE)
  invisible(x)
}

#' Overall summary of a cognitive interview
#'
#' Gathers the figures needed to write the results: participants and answers,
#' declared clarity, overall comprehension index, discordance between declared
#' and shown understanding, items with a firm problem, the count of decisions
#' and the relation between time and problems (Spearman), when timed.
#'
#' @param data Answers read with [rp_read()] and coded with [rp_code()].
#' @param ... Arguments passed to [rp_metrics()] (cut-offs, `exclude`, `lang`).
#'
#' @return An object of class `rp_summary`: a list with `participants`,
#'   `answers`, `clear` (proportion that declared the item clear),
#'   `declared_problems`, `shown_problems` (paraphrases `P` or `D`),
#'   `discordant` (of the shown problems, how many said "clear"),
#'   `comprehension` (overall index), `items_ic80` (items with an index of .80
#'   or more), `items`, `items_no_signal`, `firm` (item codes), `decisions`
#'   (table), `time` (Spearman test, or `NULL`) and `metrics`.
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_summary(ci)
rp_summary <- function(data, ...) {
  m <- rp_metrics(data, ...)
  s <- attr(m, "settings")
  d <- as.data.frame(data)
  if (!is.null(s$exclude)) d <- d[!as.character(d$participant) %in% s$exclude, , drop = FALSE]
  cl <- rp_clarity_level(d$clarity)
  coded <- rp_has_code(d)
  shown <- if (coded) sum(d$code %in% c("P", "D")) else NA
  disc <- if (coded) sum(cl == 0 & d$code %in% c("P", "D"), na.rm = TRUE) else NA
  ic <- if (coded) sum(d$code == "A", na.rm = TRUE) / sum(!is.na(d$code) & d$code != "E") else NA
  time <- NULL
  if (!all(is.na(m$seconds)) && stats::sd(m$seconds, na.rm = TRUE) > 0 && stats::sd(m$problem) > 0) {
    time <- suppressWarnings(stats::cor.test(m$seconds, m$problem, method = "spearman", exact = FALSE))
  }
  out <- list(participants = s$N, answers = nrow(d), clear = mean(cl == 0, na.rm = TRUE),
              declared_problems = sum(cl > 0, na.rm = TRUE), shown_problems = shown, discordant = disc,
              comprehension = ic, items_ic80 = sum(m$comprehension >= .80, na.rm = TRUE), items = nrow(m),
              items_no_signal = sum(m$signals == 0), firm = m$item_code[m$firm],
              decisions = table(m$decision), time = time, metrics = m)
  class(out) <- "rp_summary"
  out
}

#' Print the summary of a cognitive interview
#'
#' @param x Output of [rp_summary()].
#' @param ... Not used.
#' @return `x`, invisibly. Called for its side effect: it prints the overall
#'   figures of the interview.
#' @export
print.rp_summary <- function(x, ...) {
  pct <- function(v) sprintf("%.1f %%", 100 * v)
  cat("Cognitive interview: ", x$participants, " participants, ", x$items, " items, ", x$answers, " answers\n", sep = "")
  cat("  Declared clear:          ", pct(x$clear), "\n", sep = "")
  cat("  Declared difficulties:   ", x$declared_problems, "\n", sep = "")
  if (!is.na(x$comprehension)) {
    cat(sprintf("  Comprehension index:     %.2f (items >= .80: %d of %d)\n", x$comprehension, x$items_ic80, x$items))
    cat("  Paraphrases P or D:      ", x$shown_problems, " (", x$discordant, " from participants who said it was clear)\n", sep = "")
  }
  cat("  Items with no signal:    ", x$items_no_signal, "\n", sep = "")
  cat("  Firm problems:           ", if (length(x$firm)) paste(x$firm, collapse = ", ") else "none", "\n", sep = "")
  cat("  Decisions:               ", paste(names(x$decisions), x$decisions, sep = " ", collapse = " | "), "\n", sep = "")
  if (!is.null(x$time)) cat(sprintf("  Time vs problem (Spearman): rho = %.2f, p = %.3f\n", x$time$estimate, x$time$p.value))
  invisible(x)
}

#' Does the decision change without some participants?
#'
#' Recomputes the problem index and the decision leaving out the chosen
#' participants (for example those who answered very briefly or quickly, see
#' [rp_quality()]) and compares it with the decision using everybody. Choose
#' the participants to exclude before looking at the result, and declare the
#' criterion.
#'
#' @param data Answers read with [rp_read()] and coded with [rp_code()].
#' @param exclude Participants to leave out, for example `c("P2", "P6")`.
#' @param only_changes If `TRUE` (default), keeps only the items that are not
#'   kept in at least one of the two analyses.
#' @param ... Other arguments passed to [rp_metrics()].
#'
#' @return A data frame with `item_code`, `problem_all`, `decision_all`,
#'   `problem_without`, `decision_without` and `change` (`"="`, `"up"` or
#'   `"down"`).
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_sensitivity(ci, exclude = "P3")
rp_sensitivity <- function(data, exclude, only_changes = TRUE, ...) {
  a <- rp_metrics(data, ...)
  b <- suppressMessages(rp_metrics(data, exclude = exclude, ...))
  b <- b[match(a$n, b$n), ]
  lv <- levels(a$decision)
  ch <- sign(as.integer(b$decision) - as.integer(a$decision))
  out <- data.frame(n = a$n, item_code = a$item_code, problem_all = a$problem, decision_all = a$decision,
                    problem_without = b$problem, decision_without = b$decision,
                    change = c("down", "=", "up")[ch + 2], stringsAsFactors = FALSE)
  if (only_changes) out <- out[out$decision_all != lv[1] | out$decision_without != lv[1], , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "exclude") <- exclude
  out
}

#' Quality of the answers of each participant
#'
#' A self-administered cognitive interview depends on participants writing in
#' their own words. This function shows, for each participant, how much they
#' wrote, how fast they answered and how often they only echoed the item, and
#' flags those whose answers carry little evidence. Flags are a starting
#' point for a sensitivity analysis ([rp_sensitivity()]), not a rule to
#' delete participants.
#'
#' @param data Answers read with [rp_read()], coded or not.
#' @param min_chars Median paraphrase length (characters) below which a
#'   participant is flagged as brief.
#' @param min_seconds Median seconds per item below which a participant is
#'   flagged as fast.
#' @param max_echo Share of echoes above which a participant is flagged.
#'
#' @return A data frame with one row per participant: `participant`, `chars`
#'   (median characters of the paraphrase), `seconds` (median per item),
#'   `minutes` (total), `unclear` (items declared not fully clear), `A`, `P`,
#'   `D`, `E`, the flags `brief`, `fast`, `echo` and the profile columns when
#'   available.
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_quality(ci)
rp_quality <- function(data, min_chars = 50, min_seconds = 30, max_echo = .50) {
  rp_check_data(data, c("participant", "n"))
  d <- as.data.frame(data)
  coded <- rp_has_code(d)
  sp <- split(d, d$participant, drop = TRUE)
  out <- do.call(rbind, lapply(names(sp), function(p) {
    x <- sp[[p]]
    code <- if (coded) x$code else rep(NA_character_, nrow(x))
    data.frame(participant = p,
               chars = if ("paraphrase" %in% names(x)) stats::median(nchar(x$paraphrase), na.rm = TRUE) else NA_real_,
               seconds = if ("seconds" %in% names(x)) stats::median(x$seconds, na.rm = TRUE) else NA_real_,
               minutes = if ("seconds" %in% names(x)) sum(x$seconds, na.rm = TRUE) / 60 else NA_real_,
               unclear = if ("clarity" %in% names(x)) sum(rp_clarity_level(x$clarity) > 0, na.rm = TRUE) else NA_integer_,
               A = sum(code == "A", na.rm = TRUE), P = sum(code == "P", na.rm = TRUE),
               D = sum(code == "D", na.rm = TRUE), E = sum(code == "E", na.rm = TRUE),
               items = nrow(x), stringsAsFactors = FALSE)
  }))
  out$brief <- !is.na(out$chars) & out$chars < min_chars
  out$fast <- !is.na(out$seconds) & out$seconds < min_seconds
  out$echo <- coded & out$E / out$items > max_echo
  out$items <- NULL
  out <- out[order(as.integer(rp_order_participants(out$participant))), , drop = FALSE]
  prof <- attr(data, "participants")
  if (!is.null(prof) && "participant" %in% names(prof)) {
    prof <- as.data.frame(prof)
    extra <- setdiff(names(prof), c(names(out), "id", "recibido", "inicio", "fin", "received", "start", "end"))
    out <- cbind(out, prof[match(out$participant, as.character(prof$participant)), extra, drop = FALSE])
  }
  rownames(out) <- NULL
  out
}

#' Problems by dimension
#'
#' Share of answers with a signal of problem and comprehension index for each
#' dimension of the test.
#'
#' @param data Answers read with [rp_read()] (with the item key, so that
#'   `dimension` is filled) and coded with [rp_code()].
#'
#' @return A data frame with `dimension`, `items`, `answers`, `problem`
#'   (proportion of answers with a signal) and `comprehension`, ordered from
#'   the highest problem.
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_dimension(ci)
rp_dimension <- function(data) {
  rp_check_data(data, c("participant", "n", "clarity", "dimension"))
  d <- as.data.frame(data)
  if (all(is.na(d$dimension))) stop("No dimensions: read the data with an item key (`items` in rp_read()).", call. = FALSE)
  d$.signal <- rp_signal(d)
  coded <- rp_has_code(d)
  sp <- split(d, d$dimension)
  out <- do.call(rbind, lapply(names(sp), function(k) {
    x <- sp[[k]]
    data.frame(dimension = k, items = length(unique(x$n)), answers = nrow(x), problem = mean(x$.signal),
               comprehension = if (coded) sum(x$code == "A", na.rm = TRUE) / sum(!is.na(x$code) & x$code != "E") else NA_real_,
               stringsAsFactors = FALSE)
  }))
  out <- out[order(-out$problem), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Problems by stage of the response process
#'
#' Counts the problems coded in each stage of the response model
#' (comprehension, retrieval, judgment, response) and, if coded, by type of
#' problem. It shows which part of the response process the evidence covers:
#' a comprehension-only interview documents the first stage, not the others.
#'
#' @param data Answers coded with [rp_code()] including the column `stage`
#'   (and, optionally, `problem_type`).
#' @param by_item If `TRUE`, counts by item and stage.
#'
#' @return A data frame with the counts by `stage` (and `problem_type` or
#'   `item_code`).
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_stages(ci)
rp_stages <- function(data, by_item = FALSE) {
  d <- as.data.frame(data)
  if (!"stage" %in% names(d) || all(is.na(d$stage))) {
    stop("No stage coded. Fill the column `stage` of the coding (see rp_scheme()).", call. = FALSE)
  }
  d <- d[!is.na(d$stage) & d$stage != "", , drop = FALSE]
  by <- if (by_item) c("item_code", "stage") else if ("problem_type" %in% names(d) && any(!is.na(d$problem_type))) c("stage", "problem_type") else "stage"
  d$problem_type[is.na(d$problem_type)] <- "-"
  out <- stats::aggregate(list(problems = rep(1L, nrow(d))), d[by], sum)
  out <- out[order(out[[by[1]]], -out$problems), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Readings behind each problematic item
#'
#' Lists, for the chosen items, what each participant with a signal said: the
#' code and note of the paraphrase, or the clarity marked and the confusing
#' part. It is the qualitative material needed to rewrite an item.
#'
#' @param data Answers read with [rp_read()] and coded with [rp_code()].
#' @param items Item codes or numbers to show. By default, items whose
#'   decision is not "keep".
#' @param ... Arguments passed to [rp_metrics()] when `items` is `NULL`.
#'
#' @return A data frame with `item_code`, `item`, `participant`, `clarity`,
#'   `code`, `paraphrase`, `note` and `confusing`, one row per signal.
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_readings(ci)
rp_readings <- function(data, items = NULL, ...) {
  d <- as.data.frame(data)
  if (is.null(items)) {
    m <- rp_metrics(data, ...)
    items <- m$item_code[as.integer(m$decision) > 1]
  }
  d <- d[d$item_code %in% items | d$n %in% suppressWarnings(as.integer(items)), , drop = FALSE]
  d <- d[rp_signal(d), , drop = FALSE]
  cols <- intersect(c("item_code", "item", "participant", "clarity", "code", "paraphrase", "note", "confusing"), names(d))
  d <- d[order(match(d$item_code, items), d$participant), cols, drop = FALSE]
  if ("clarity" %in% names(d)) d$clarity <- as.character(d$clarity)
  rownames(d) <- NULL
  d
}
