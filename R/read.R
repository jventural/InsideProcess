# Reading the answers of the cognitive interview.

#' Read the answers of a cognitive interview
#'
#' Reads the answers gathered with [rp_apps_script()] or [rp_form()] and
#' returns one row per participant and item, with standard column names. It
#' accepts the Excel file downloaded from the Google Sheet (tabs
#' `Participantes`/`Respuestas` or `Participants`/`Responses`), a csv file
#' downloaded by the offline form, a folder with several of those csv files, or
#' a data frame already in long format.
#'
#' Participants are labelled `P1`, `P2`, ... in the order in which their
#' answers arrived. Spanish column names (`claridad`, `parafraseo`, `confuso`,
#' `segundos`) are renamed to `clarity`, `paraphrase`, `confusing`, `seconds`.
#'
#' @param x Path to an `.xlsx` or `.csv` file, path to a folder of `.csv`
#'   files, or a data frame with one row per participant and item.
#' @param items Optional item key: a data frame or the path to `items.csv`
#'   written by [rp_apps_script()]. It adds the item code (`item_code`) and the
#'   `dimension` to each answer.
#' @param clarity_levels The three clarity options in order (clear, partly
#'   clear, not clear). By default the Spanish or English options of the form
#'   are detected.
#'
#' @return A data frame of class `rp_data` with one row per participant and
#'   item and the columns `participant`, `id`, `n`, `item_code`, `dimension`,
#'   `item`, `clarity` (ordered factor), `paraphrase`, `confusing`,
#'   `seconds` and any other probe gathered. The participant table, when
#'   present in the file, is kept in the attribute `"participants"`.
#' @seealso [rp_code()] to add the coding of paraphrases, [rp_metrics()].
#' @export
#' @examples
#' f <- file.path(tempdir(), "answers.csv")
#' utils::write.csv(rp_example, f, row.names = FALSE)
#' ci <- rp_read(f, items = rp_example_items)
#' head(ci[, c("participant", "item_code", "clarity")])
#' unlink(f)
rp_read <- function(x, items = NULL, clarity_levels = NULL) {
  part <- NULL
  if (is.data.frame(x)) {
    resp <- as.data.frame(x, stringsAsFactors = FALSE)
  } else if (is.character(x) && length(x) == 1 && dir.exists(x)) {
    files <- list.files(x, pattern = "\\.csv$", full.names = TRUE)
    if (!length(files)) stop("No csv files in ", x, call. = FALSE)
    resp <- do.call(rbind_fill, lapply(files, rp_read_csv))
  } else if (is.character(x) && length(x) == 1 && file.exists(x)) {
    ext <- tolower(tools::file_ext(x))
    if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) stop("Install the readxl package to read Excel files.", call. = FALSE)
      sh <- readxl::excel_sheets(x)
      s_resp <- intersect(c("Respuestas", "Responses"), sh)
      s_part <- intersect(c("Participantes", "Participants"), sh)
      resp <- as.data.frame(readxl::read_excel(x, if (length(s_resp)) s_resp[1] else sh[1]))
      if (length(s_part)) part <- as.data.frame(readxl::read_excel(x, s_part[1]))
    } else {
      resp <- rp_read_csv(x)
    }
  } else {
    stop("`x` must be a data frame, a file or a folder.", call. = FALSE)
  }
  resp <- rp_standardize(resp)
  if (!"id" %in% names(resp) && "participant" %in% names(resp)) resp$id <- resp$participant
  if (!all(c("id", "n") %in% names(resp))) stop("The answers need the columns `id` and `n`.", call. = FALSE)
  resp$n <- as.integer(resp$n)
  ids <- unique(as.character(resp$id))
  resp$participant <- factor(paste0("P", match(as.character(resp$id), ids)), levels = paste0("P", seq_along(ids)))

  if ("clarity" %in% names(resp)) {
    if (is.null(clarity_levels)) {
      vals <- unique(stats::na.omit(as.character(resp$clarity)))
      clarity_levels <- if (all(vals %in% rp_clarity_options("en"))) rp_clarity_options("en") else rp_clarity_options("es")
    }
    if (length(clarity_levels) != 3) stop("`clarity_levels` must have three options.", call. = FALSE)
    bad <- setdiff(unique(stats::na.omit(as.character(resp$clarity))), clarity_levels)
    if (length(bad)) stop("Clarity values not in `clarity_levels`: ", paste(bad, collapse = ", "), call. = FALSE)
    resp$clarity <- factor(as.character(resp$clarity), levels = clarity_levels, ordered = TRUE)
  }
  for (v in intersect(c("paraphrase", "confusing", "retrieval", "judgment"), names(resp))) {
    resp[[v]] <- trimws(as.character(resp[[v]]))
    resp[[v]][resp[[v]] %in% c("", "NA")] <- NA_character_
  }
  if ("seconds" %in% names(resp)) resp$seconds <- suppressWarnings(as.numeric(resp$seconds))

  if (is.character(items) && length(items) == 1 && file.exists(items)) items <- utils::read.csv(items, encoding = "UTF-8", check.names = FALSE)
  key <- rp_items_key(items)
  if (!is.null(key)) {
    for (v in c("item_code", "dimension")) resp[[v]] <- key[[v]][match(resp$n, key$n)]
    if ("item" %in% names(key) && !"item" %in% names(resp)) resp$item <- key$item[match(resp$n, key$n)]
  }
  if (!"item_code" %in% names(resp)) resp$item_code <- paste0("I", resp$n)
  if (!"dimension" %in% names(resp)) resp$dimension <- NA_character_
  first <- c("participant", "id", "n", "item_code", "dimension", "item")
  resp <- resp[order(as.integer(resp$participant), resp$n), c(intersect(first, names(resp)), setdiff(names(resp), first)), drop = FALSE]
  rownames(resp) <- NULL

  if (is.null(part)) part <- rp_participants_from_long(resp)
  if (!is.null(part)) {
    part <- rp_standardize(part)
    if ("id" %in% names(part)) {
      part$participant <- factor(paste0("P", match(as.character(part$id), ids)), levels = levels(resp$participant))
      part <- part[order(as.integer(part$participant)), c("participant", setdiff(names(part), "participant")), drop = FALSE]
      rownames(part) <- NULL
    }
  }
  attr(resp, "participants") <- part
  class(resp) <- c("rp_data", "data.frame")
  resp
}

rp_read_csv <- function(f) {
  first <- readLines(f, n = 1, encoding = "UTF-8", warn = FALSE)
  sep <- if (lengths(regmatches(first, gregexpr(";", first))) > lengths(regmatches(first, gregexpr(",", first)))) ";" else ","
  d <- utils::read.csv(f, sep = sep, encoding = "UTF-8", check.names = FALSE, stringsAsFactors = FALSE)
  names(d) <- sub("^\uFEFF", "", names(d))
  names(d) <- sub("^\u00EF\u00BB\u00BF", "", names(d))
  d
}

rbind_fill <- function(...) {
  l <- list(...)
  cols <- unique(unlist(lapply(l, names)))
  l <- lapply(l, function(d) { for (cc in setdiff(cols, names(d))) d[[cc]] <- NA; d[, cols, drop = FALSE] })
  do.call(rbind, l)
}

# Profile columns repeated in the long csv of the offline form.
rp_participants_from_long <- function(resp) {
  std <- c("participant", "id", "n", "item_code", "dimension", "item", "clarity", "paraphrase", "confusing",
           "retrieval", "answer", "judgment", "response", "seconds", "code", "note", "stage", "problem_type")
  extra <- setdiff(names(resp), std)
  if (!length(extra)) return(NULL)
  ok <- vapply(extra, function(v) all(tapply(as.character(resp[[v]]), resp$id, function(z) length(unique(z)) == 1)), logical(1))
  extra <- extra[ok]
  if (!length(extra)) return(NULL)
  p <- resp[!duplicated(resp$id), c("id", extra), drop = FALSE]
  rownames(p) <- NULL
  p
}

#' Read the answers of a native Google Form
#'
#' Turns the responses of a form built with [rp_google_form()] (downloaded
#' from Google Forms or its sheet as csv or Excel) into one row per
#' participant and item, the format the other functions use. Question columns
#' are recognised by their `item.probe` number (for example `3.2` is the
#' paraphrase of item 3).
#'
#' @param x Path to the `.csv` or `.xlsx` file, or a data frame with one row
#'   per participant.
#' @param items Item key (data frame or path to a csv), as in [rp_read()]. It
#'   gives the item text, code and dimension.
#' @param clarity_levels The three clarity options in order, as in [rp_read()].
#'
#' @return A data frame of class `rp_data`, as returned by [rp_read()]. There
#'   is no `seconds` column, because Google Forms does not time each item.
#' @seealso [rp_google_form()], [rp_read()]
#' @export
#' @examples
#' wide <- data.frame(
#'   `Marca temporal` = c("2026-11-01 10:00", "2026-11-01 10:05"),
#'   `1.1 Is it clear?` = c("Yes, it is clear", "Somewhat"),
#'   `1.2 In your words?` = c("whether I enjoy learning", "about classes"),
#'   `1.3 Which word?` = c(NA, "learning"),
#'   check.names = FALSE)
#' rp_read_google_form(wide, items = rp_example_items[1, ])
rp_read_google_form <- function(x, items = NULL, clarity_levels = NULL) {
  if (is.data.frame(x)) {
    w <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (tolower(tools::file_ext(x)) %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Install the readxl package to read Excel files.", call. = FALSE)
    w <- as.data.frame(readxl::read_excel(x), check.names = FALSE)
  } else {
    w <- rp_read_csv(x)
  }
  pat <- "^\\s*(\\d+)\\.(\\d)\\s"
  qcols <- grep(pat, names(w), value = TRUE)
  if (!length(qcols)) stop("No question columns numbered as `item.probe` (for example `3.2`) were found.", call. = FALSE)
  other <- setdiff(names(w), qcols)
  w$id <- paste0("G", seq_len(nrow(w)))
  n <- as.integer(sub(paste0(pat, ".*"), "\\1", qcols))
  probe <- rp_probe_types[as.integer(sub(paste0(pat, ".*"), "\\2", qcols))]
  long <- do.call(rbind_fill, lapply(sort(unique(n)), function(k) {
    d <- data.frame(id = w$id, n = k, stringsAsFactors = FALSE)
    for (j in which(n == k)) d[[probe[j]]] <- as.character(w[[qcols[j]]])
    d
  }))
  long <- long[order(match(long$id, w$id), long$n), , drop = FALSE]
  key <- if (is.character(items) && length(items) == 1 && file.exists(items)) utils::read.csv(items, encoding = "UTF-8") else items
  key <- rp_items_key(key)
  if (!is.null(key) && "item" %in% names(key)) long$item <- key$item[match(long$n, key$n)]
  out <- rp_read(long, items = key, clarity_levels = clarity_levels)
  part <- w[, c("id", other), drop = FALSE]
  part$participant <- factor(paste0("P", seq_len(nrow(part))), levels = levels(out$participant))
  attr(out, "participants") <- part[, c("participant", "id", other), drop = FALSE]
  out
}
