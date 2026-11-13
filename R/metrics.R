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
#' shows a problem in any of the stages analysed:
#'
#' - comprehension: declared difficulty (clarity "somewhat" or "not clear") or
#'   a paraphrase partial (`P`) or with another meaning (`D`);
#' - retrieval: a recall coded `P` (vague) or `D` (another domain);
#' - judgment: a reason coded `P` (another criterion) or `D` (it contradicts
#'   the answer);
#' - response: some difficulty to choose an option ("a little" or "hard").
#'
#' Echoes and answers that cannot be assessed (`E`) are not signals. See
#' [rp_scheme()] for the meaning of each code in each stage.
#'
#' | Metric | What it measures | How it is computed |
#' |---|---|---|
#' | `comprehension` (IC) | Share that understood the item as intended | `A` paraphrases / assessable paraphrases (echoes excluded) |
#' | `problem` (IP) | Share with some signal | Participants with a signal in any stage / participants |
#' | `problem_lower`, `problem_upper` | How much IP could vary in another sample | Wilson interval |
#' | `severity` | How serious the problems were (0 to 2) | Mean of the largest weight across stages: `D` = 2, `P` = 1, "not clear" or "hard" = 2, "somewhat" or "a little" = 1 |
#' | `discordance` | Said it was clear but did not understand | Clarity "yes" with paraphrase `P` or `D` |
#' | `firm` | The problem is unlikely to be chance | Lower limit of IP above `firm` |
#' | `problem_<stage>` | Share with a signal in that stage | Participants with a signal in the stage / participants |
#' | `retrieval_index`, `judgment_index` | Share of adequate recalls and reasons | `A` / assessable codes of the stage |
#' | `main_stage` | Where the item fails | Stage with the highest `problem_<stage>` |
#'
#' The decision uses proportions so that it does not depend on the number of
#' participants: reformulate when IP reaches `cut_reformulate`, revise when it
#' reaches `cut_revise`, keep otherwise. The cut-offs are tentative and should
#' be declared in the method. With only comprehension probes, the evidence
#' covers the first stage of the response process and `main_stage` is always
#' comprehension.
#'
#' @param data Answers read with [rp_read()] and, ideally, coded with
#'   [rp_code()]. Without coding, only declared clarity (and the `response`
#'   probe, if asked) is used.
#' @param cut_reformulate,cut_revise Cut-offs of the problem index.
#' @param firm Threshold for the lower limit of the Wilson interval.
#' @param conf Confidence level of the interval.
#' @param exclude Participants to leave out (for example `c("P2", "P6")`).
#' @param stages Stages that count as signals: any of `"comprehension"`,
#'   `"retrieval"`, `"judgment"` and `"response"`. By default, every stage
#'   with data (and coding, for retrieval and judgment). Use
#'   `stages = "comprehension"` to reproduce a comprehension-only analysis.
#' @param lang Language of the decision and stage labels: `"es"` or `"en"`.
#'
#' @return A data frame of class `rp_metrics`, one row per item ordered from
#'   the most to the least problematic, with `n`, `item_code`, `dimension`,
#'   `item`, `participants`, `clarity` (participants who declared difficulty),
#'   `A`, `P`, `D`, `E` (paraphrase codes), `assessable`, `signals`,
#'   `comprehension`, `problem`, `problem_lower`, `problem_upper`, `severity`,
#'   `discordance`, `seconds` (median, when timed), `firm`, `decision`, one
#'   `problem_<stage>` column per stage analysed, `retrieval_index` and
#'   `judgment_index` (when those stages are analysed) and `main_stage`.
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
                       conf = .95, exclude = NULL, stages = NULL, lang = NULL) {
  lang <- rp_lang(lang)
  rp_check_data(data, c("participant", "n"))
  if (cut_revise >= cut_reformulate) stop("`cut_revise` must be lower than `cut_reformulate`.", call. = FALSE)
  d <- as.data.frame(data)
  if (!is.null(exclude)) d <- d[!as.character(d$participant) %in% exclude, , drop = FALSE]
  if (!"clarity" %in% names(d)) d$clarity <- NA
  auto <- is.null(stages)
  stages <- rp_pick_stages(d, stages)
  if (!length(stages)) stop("Nothing to analyse: the answers have no clarity, coding or response probe.", call. = FALSE)
  coded <- rp_has_code(d)
  if (!coded && "comprehension" %in% stages) {
    message("No paraphrase coding found: comprehension uses declared clarity only. Add it with rp_code().")
  }
  for (s in c("retrieval", "judgment")) {
    if (auto && rp_text_cols[[s]] %in% names(d) && !s %in% stages && any(!is.na(d[[rp_text_cols[[s]]]]))) {
      message("The ", s, " answers are not coded: add `", rp_stage_cols[[s]], "` with rp_code() to analyse that stage.")
    }
  }
  sm <- rp_stage_matrix(d, stages)
  d$.signal <- rowSums(sm$signal) > 0
  d$.sev <- apply(sm$severity, 1, max)
  for (s in stages) {
    d[[paste0(".sig_", s)]] <- sm$signal[, s]
    d[[paste0(".ass_", s)]] <- sm$assessable[, s]
  }
  no_clarity <- all(is.na(d$clarity))
  cl <- rp_clarity_level(d$clarity)
  d$.unclear <- !is.na(cl) & cl > 0
  d$.disc <- if (coded) !is.na(cl) & cl == 0 & d$code %in% c("P", "D") else NA
  if (!"item_code" %in% names(d)) d$item_code <- paste0("I", d$n)
  if (!"dimension" %in% names(d)) d$dimension <- NA_character_
  if (!"item" %in% names(d)) d$item <- NA_character_
  idx <- function(code) { k <- sum(code %in% c("A", "P", "D")); if (k > 0) sum(code %in% "A") / k else NA_real_ }
  sp <- split(d, d$n)
  rows <- lapply(sp, function(x) {
    code <- if (coded) x$code else rep(NA_character_, nrow(x))
    np <- if (no_clarity) nrow(x) else sum(!is.na(x$clarity))
    out <- data.frame(n = x$n[1], item_code = x$item_code[1], dimension = x$dimension[1], item = x$item[1],
                      participants = np, clarity = sum(x$.unclear),
                      A = sum(code == "A", na.rm = TRUE), P = sum(code == "P", na.rm = TRUE),
                      D = sum(code == "D", na.rm = TRUE), E = sum(code == "E", na.rm = TRUE),
                      assessable = sum(!is.na(code) & code != "E"), signals = sum(x$.signal),
                      severity = mean(x$.sev), discordance = if (coded) sum(x$.disc) else NA_integer_,
                      seconds = if ("seconds" %in% names(x)) stats::median(x$seconds, na.rm = TRUE) else NA_real_,
                      stringsAsFactors = FALSE)
    for (s in stages) out[[paste0("problem_", s)]] <- if (np > 0) sum(x[[paste0(".sig_", s)]]) / np else NA_real_
    if ("retrieval" %in% stages) out$retrieval_index <- idx(x$code_retrieval)
    if ("judgment" %in% stages) out$judgment_index <- idx(x$code_judgment)
    out
  })
  m <- do.call(rbind, rows)
  m$comprehension <- ifelse(m$assessable > 0, m$A / m$assessable, NA_real_)
  m$problem <- m$signals / m$participants
  ci <- rp_wilson(m$signals, m$participants, conf)
  m$problem_lower <- ci[, "lower"]
  m$problem_upper <- ci[, "upper"]
  m$firm <- m$problem_lower > firm
  m$decision <- rp_decide(m$problem, cut_reformulate, cut_revise, lang)
  pcols <- paste0("problem_", stages)
  lab <- rp_stage_labels(lang)
  m$main_stage <- apply(as.matrix(m[, pcols, drop = FALSE]), 1, function(v) {
    if (all(is.na(v)) || max(v, na.rm = TRUE) == 0) NA_character_ else unname(lab[stages[which.max(v)]])
  })
  extra <- c(pcols, intersect(c("retrieval_index", "judgment_index"), names(m)), "main_stage")
  m <- m[order(-m$problem, -m$severity, m$n), c("n", "item_code", "dimension", "item", "participants", "clarity",
                                                 "A", "P", "D", "E", "assessable", "signals", "comprehension",
                                                 "problem", "problem_lower", "problem_upper", "severity",
                                                 "discordance", "seconds", "firm", "decision", extra)]
  rownames(m) <- NULL
  attr(m, "settings") <- list(cut_reformulate = cut_reformulate, cut_revise = cut_revise, firm = firm,
                              conf = conf, exclude = exclude, coded = coded, lang = lang, stages = stages,
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
  if (length(s$stages) > 1) cols <- c(cols, intersect(paste0("problem_", s$stages), names(x)), "main_stage")
  y <- as.data.frame(x)[, cols]
  num <- intersect(c("comprehension", "problem", "problem_lower", "problem_upper", "severity", paste0("problem_", s$stages)), names(y))
  for (v in num) y[[v]] <- round(y[[v]], digits)
  names(y) <- sub("^problem_(retrieval|judgment|response|comprehension)$", "p_\\1", names(y))
  cat("Item metrics (N = ", s$N, "; reformulate >= ", s$cut_reformulate, ", revise >= ", s$cut_revise,
      "; stages: ", paste(s$stages, collapse = ", "), ")\n", sep = "")
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
#'   (table), `time` (Spearman test, or `NULL`), `stages` (output of
#'   [rp_stages()]: answers with a problem in each stage) and `metrics`.
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
              decisions = table(m$decision), time = time,
              stages = rp_stages(d, stages = s$stages, conf = s$conf, lang = s$lang), metrics = m)
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
  if (!is.null(x$stages) && nrow(x$stages) > 1) {
    cat("  Answers with a problem, by stage:\n")
    for (k in seq_len(nrow(x$stages))) {
      cat(sprintf("    %-16s %d of %d (%s)\n", x$stages$stage[k], x$stages$problems[k], x$stages$assessable[k],
                  pct(x$stages$proportion[k])))
    }
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
#' Share of answers with a signal of problem (in any stage analysed, see
#' [rp_metrics()]) and comprehension index for each
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
#' Counts, for each stage of the response model (comprehension, retrieval,
#' judgment, response), how many answers could be assessed and how many show
#' a problem, with the Wilson interval of that proportion. It shows which part
#' of the response process the evidence covers and where the items fail: a
#' comprehension-only interview documents the first stage, not the others.
#'
#' Problems are, in comprehension, a declared difficulty or a paraphrase `P`
#' or `D`; in retrieval and judgment, the codes `P` and `D` of
#' `code_retrieval` and `code_judgment` (see [rp_scheme()]); in response, any
#' difficulty to choose an option. Answers coded `E` are not assessable.
#'
#' @param data Answers read with [rp_read()] and coded with [rp_code()].
#' @param stages Stages to count. By default, every stage with data.
#' @param by_item If `TRUE`, counts by item and stage.
#' @param exclude Participants to leave out.
#' @param conf Confidence level of the interval.
#' @param lang Language of the stage labels: `"es"` or `"en"`.
#'
#' @return A data frame with `stage` (label), `key` (stage name in English),
#'   `answers`, `assessable`, `problems`, `proportion` (problems /
#'   assessable), `lower` and `upper` (Wilson interval); with `by_item = TRUE`
#'   also `item_code`, one row per item and stage.
#' @seealso [rp_scheme()], [rp_plot_stages()]
#' @export
#' @examples
#' ci <- rp_code(rp_example, rp_example_coding)
#' rp_stages(ci)
#' head(rp_stages(ci, by_item = TRUE))
rp_stages <- function(data, stages = NULL, by_item = FALSE, exclude = NULL, conf = .95, lang = NULL) {
  lang <- rp_lang(lang)
  rp_check_data(data, c("participant", "n"))
  d <- as.data.frame(data)
  if (!is.null(exclude)) d <- d[!as.character(d$participant) %in% exclude, , drop = FALSE]
  stages <- rp_pick_stages(d, stages)
  if (!length(stages)) stop("No stage can be analysed: the answers have no clarity, coding or response probe.", call. = FALSE)
  if (!"item_code" %in% names(d)) d$item_code <- paste0("I", d$n)
  sm <- rp_stage_matrix(d, stages)
  lab <- rp_stage_labels(lang)
  one <- function(rows, s) {
    a <- sum(sm$assessable[rows, s]); p <- sum(sm$signal[rows, s] & sm$assessable[rows, s])
    ci <- rp_wilson(p, a, conf)
    data.frame(stage = unname(lab[s]), key = s, answers = length(rows), assessable = a, problems = p,
               proportion = if (a > 0) p / a else NA_real_, lower = ci[, "lower"], upper = ci[, "upper"],
               stringsAsFactors = FALSE)
  }
  if (!by_item) {
    out <- do.call(rbind, lapply(stages, function(s) one(seq_len(nrow(d)), s)))
  } else {
    items <- unique(d[order(d$n), "item_code"])
    out <- do.call(rbind, lapply(items, function(k) {
      rows <- which(d$item_code == k)
      cbind(item_code = k, do.call(rbind, lapply(stages, function(s) one(rows, s))), stringsAsFactors = FALSE)
    }))
  }
  rownames(out) <- NULL
  out
}

#' Use of the answer options
#'
#' When the form asked participants to answer each item with the test's own
#' scale (the `answer` probe), this function shows how the options were used:
#' how many chose each one, which options nobody used and whether the answers
#' concentrate in a single option. Unused options and answers piled on one
#' extreme are evidence about the response stage (Tourangeau et al., 2000):
#' the categories may not fit how people place their judgment. With the small
#' samples of a cognitive interview, read them as clues to be checked against
#' the reasons given (`judgment`) and the difficulty declared (`response`).
#'
#' @param data Answers read with [rp_read()] with the column `answer`.
#' @param levels The answer options in order. By default, the levels of
#'   `answer` if it is a factor or, otherwise, the options observed in order
#'   of appearance (pass them to also count the options nobody chose).
#' @param concentration Share of the most chosen option from which the
#'   answers are flagged as concentrated.
#' @param exclude Participants to leave out.
#'
#' @return A data frame with one row per item: `n`, `item_code`, `answers`,
#'   one column with the count of each option, `unused` (options nobody
#'   chose), `mode` (most chosen option), `mode_share` and `concentrated`.
#' @references Tourangeau, R., Rips, L. J., & Rasinski, K. (2000). *The
#'   psychology of survey response*. Cambridge University Press.
#'   \doi{10.1017/CBO9780511819322}
#' @export
#' @examples
#' rp_options(rp_example, levels = c("Nunca", "A veces", "Casi siempre", "Siempre"))
rp_options <- function(data, levels = NULL, concentration = .80, exclude = NULL) {
  rp_check_data(data, c("participant", "n", "answer"))
  d <- as.data.frame(data)
  if (!is.null(exclude)) d <- d[!as.character(d$participant) %in% exclude, , drop = FALSE]
  if (!"item_code" %in% names(d)) d$item_code <- paste0("I", d$n)
  ans <- trimws(as.character(d$answer))
  ans[ans %in% c("", "NA")] <- NA
  if (all(is.na(ans))) stop("The column `answer` is empty: the form did not ask the `answer` probe.", call. = FALSE)
  if (is.null(levels)) levels <- if (is.factor(d$answer)) base::levels(d$answer) else unique(stats::na.omit(ans))
  bad <- setdiff(stats::na.omit(unique(ans)), levels)
  if (length(bad)) stop("Answers not in `levels`: ", paste(bad, collapse = ", "), call. = FALSE)
  d$.ans <- factor(ans, levels = levels)
  items <- unique(d[order(d$n), c("n", "item_code")])
  out <- do.call(rbind, lapply(seq_len(nrow(items)), function(i) {
    x <- d$.ans[d$n == items$n[i]]
    tb <- table(x)
    k <- sum(tb)
    row <- data.frame(n = items$n[i], item_code = items$item_code[i], answers = k, stringsAsFactors = FALSE)
    row <- cbind(row, as.data.frame(as.list(stats::setNames(as.integer(tb), levels)), check.names = FALSE))
    row$unused <- paste(levels[tb == 0], collapse = "; ")
    row$mode <- if (k > 0) levels[which.max(tb)] else NA_character_
    row$mode_share <- if (k > 0) max(tb) / k else NA_real_
    row$concentrated <- !is.na(row$mode_share) & row$mode_share >= concentration
    row
  }))
  rownames(out) <- NULL
  out
}

#' Readings behind each problematic item
#'
#' Lists, for the chosen items, what each participant with a signal said: the
#' code and note of the paraphrase, or the clarity marked and the confusing
#' part and, when those probes were asked, what came to mind, the answer
#' chosen, the reason and the difficulty to choose, with their codes. It is
#' the qualitative material needed to rewrite an item.
#'
#' @param data Answers read with [rp_read()] and coded with [rp_code()].
#' @param items Item codes or numbers to show. By default, items whose
#'   decision is not "keep".
#' @param ... Arguments passed to [rp_metrics()] when `items` is `NULL`.
#'
#' @return A data frame with `item_code`, `item`, `participant`, `clarity`,
#'   `code`, `paraphrase`, `note`, `confusing` and, when present,
#'   `code_retrieval`, `retrieval`, `note_retrieval`, `answer`,
#'   `code_judgment`, `judgment`, `note_judgment` and `response`, one row per
#'   participant with a signal.
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
  st <- list(...)$stages
  d <- d[rp_signal(d, st), , drop = FALSE]
  cols <- intersect(c("item_code", "item", "participant", "clarity", "code", "paraphrase", "note", "confusing",
                      "code_retrieval", "retrieval", "note_retrieval", "answer", "code_judgment", "judgment",
                      "note_judgment", "response"), names(d))
  d <- d[order(match(d$item_code, items), d$participant), cols, drop = FALSE]
  for (v in intersect(c("clarity", "answer", "response"), names(d))) d[[v]] <- as.character(d[[v]])
  rownames(d) <- NULL
  d
}
