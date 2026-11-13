# Internal helpers: language, column names and small utilities.

rp_lang <- function(lang = NULL) {
  if (is.null(lang)) lang <- getOption("InsideProcess.lang", "es")
  lang <- match.arg(lang, c("es", "en"))
  lang
}

# Clarity options of the form, in order: clear, partly clear, not clear.
rp_clarity_options <- function(lang = "es") {
  if (lang == "es") c("S\u00ED, se entiende", "M\u00E1s o menos", "No se entiende")
  else c("Yes, it is clear", "Somewhat", "It is not clear")
}

# Column names written by the form (lang) and the standard names used by the package.
rp_fields <- function(lang = "es") {
  if (lang == "es") {
    c(clarity = "claridad", paraphrase = "parafraseo", confusing = "confuso",
      retrieval = "recuerdo", answer = "respuesta", judgment = "razon",
      response = "dificultad", seconds = "segundos")
  } else {
    c(clarity = "clarity", paraphrase = "paraphrase", confusing = "confusing",
      retrieval = "retrieval", answer = "answer", judgment = "judgment",
      response = "difficulty", seconds = "seconds")
  }
}

# Spanish and English aliases accepted when reading data.
rp_aliases <- c(
  claridad = "clarity", parafraseo = "paraphrase", confuso = "confusing",
  recuerdo = "retrieval", respuesta = "answer", razon = "judgment",
  dificultad = "response", difficulty = "response", segundos = "seconds",
  codigo = "code", nota = "note", etapa = "stage", tipo_problema = "problem_type",
  participante = "participant", comentario = "comment"
)

rp_standardize <- function(df) {
  nm <- tolower(trimws(names(df)))
  nm <- iconv(nm, "UTF-8", "ASCII//TRANSLIT")
  nm[is.na(nm)] <- names(df)[is.na(nm)]
  hit <- nm %in% names(rp_aliases)
  nm[hit] <- rp_aliases[nm[hit]]
  names(df) <- nm
  df
}

# Standardize an item key: n, item, item_code, dimension.
rp_items_key <- function(items) {
  if (is.null(items)) return(NULL)
  if (is.character(items)) items <- data.frame(item = items)
  if (!is.data.frame(items)) stop("`items` must be a data frame or a character vector.", call. = FALSE)
  items <- as.data.frame(items, stringsAsFactors = FALSE)
  nm <- tolower(iconv(names(items), "UTF-8", "ASCII//TRANSLIT"))
  map <- c(codigo = "item_code", codigo_item = "item_code", code = "item_code",
           code_item = "item_code", item_code = "item_code", frase = "item",
           enunciado = "item", text = "item", dimension = "dimension",
           dimensiones = "dimension", factor = "dimension", n = "n", item = "item")
  hit <- nm %in% names(map)
  nm[hit] <- map[nm[hit]]
  names(items) <- nm
  if (!"n" %in% names(items)) items$n <- seq_len(nrow(items))
  if (!"item_code" %in% names(items)) items$item_code <- paste0("I", items$n)
  if (!"dimension" %in% names(items)) items$dimension <- NA_character_
  items$n <- as.integer(items$n)
  if (anyDuplicated(items$n)) stop("Item numbers `n` must be unique.", call. = FALSE)
  keep <- intersect(c("n", "item", "item_code", "dimension"), names(items))
  items[order(items$n), keep, drop = FALSE]
}

rp_check_data <- function(data, need = c("participant", "n", "clarity")) {
  if (!is.data.frame(data)) stop("`data` must be a data frame (see rp_read()).", call. = FALSE)
  miss <- setdiff(need, names(data))
  if (length(miss)) {
    stop("Missing columns in `data`: ", paste(miss, collapse = ", "),
         ". Read the data with rp_read() and code it with rp_code().", call. = FALSE)
  }
  invisible(TRUE)
}

# Clarity level as 0 (clear), 1 (partly), 2 (not clear).
rp_clarity_level <- function(x) {
  if (is.factor(x)) return(as.integer(x) - 1L)
  lv <- c(rp_clarity_options("es"), rp_clarity_options("en"))
  out <- match(x, lv)
  ifelse(is.na(out), NA_integer_, (out - 1L) %% 3L)
}

rp_has_code <- function(data) "code" %in% names(data) && any(!is.na(data$code))

# Options of the `response` probe, in order: easy, a little, hard.
rp_response_options <- function(lang = "es") {
  if (lang == "es") c("No, fue f\u00E1cil", "Un poco", "S\u00ED, me cost\u00F3")
  else c("No, it was easy", "A little", "Yes, it was hard")
}

# Difficulty to choose an option as 0 (easy), 1 (a little), 2 (hard).
rp_response_level <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.factor(x)) return(pmin(as.integer(x) - 1L, 2L))
  if (is.numeric(x)) return(as.integer(pmin(pmax(x, 0), 2)))
  lv <- c(rp_response_options("es"), rp_response_options("en"))
  out <- match(trimws(as.character(x)), lv)
  ifelse(is.na(out), NA_integer_, (out - 1L) %% 3L)
}

rp_stage_keys <- c("comprehension", "retrieval", "judgment", "response")

# Stages that can be analysed: comprehension (clarity or paraphrase codes),
# retrieval and judgment (when coded) and response (closed probe).
rp_stages_available <- function(d) {
  st <- character(0)
  if (("clarity" %in% names(d) && any(!is.na(d$clarity))) || rp_has_code(d)) st <- "comprehension"
  if ("code_retrieval" %in% names(d) && any(!is.na(d$code_retrieval))) st <- c(st, "retrieval")
  if ("code_judgment" %in% names(d) && any(!is.na(d$code_judgment))) st <- c(st, "judgment")
  if ("response" %in% names(d) && any(!is.na(rp_response_level(d$response)))) st <- c(st, "response")
  st
}

rp_pick_stages <- function(d, stages = NULL) {
  avail <- rp_stages_available(d)
  if (is.null(stages)) return(avail)
  stages <- unique(match.arg(stages, rp_stage_keys, several.ok = TRUE))
  miss <- setdiff(stages, avail)
  if (length(miss)) warning("No data or coding for the stage(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  intersect(stages, avail)
}

# For each answer and stage: signal of problem, severity (0 to 2) and whether
# the stage could be assessed.
rp_stage_matrix <- function(d, stages = rp_stages_available(d)) {
  n <- nrow(d)
  sev_code <- function(code) { s <- unname(c(A = 0, E = 0, P = 1, D = 2)[code]); s[is.na(s)] <- 0; s }
  sig <- sev <- ass <- list()
  for (s in stages) {
    if (s == "comprehension") {
      cl <- if ("clarity" %in% names(d)) rp_clarity_level(d$clarity) else rep(NA_integer_, n)
      code <- if ("code" %in% names(d)) as.character(d$code) else rep(NA_character_, n)
      cl0 <- cl; cl0[is.na(cl0)] <- 0L
      sig[[s]] <- (!is.na(cl) & cl > 0) | (!is.na(code) & code %in% c("P", "D"))
      sev[[s]] <- pmax(sev_code(code), cl0)
      ass[[s]] <- !is.na(cl) | (!is.na(code) & code != "E")
    } else if (s == "response") {
      lv <- rp_response_level(d$response)
      sig[[s]] <- !is.na(lv) & lv > 0
      sev[[s]] <- ifelse(is.na(lv), 0, lv)
      ass[[s]] <- !is.na(lv)
    } else {
      code <- as.character(d[[c(retrieval = "code_retrieval", judgment = "code_judgment")[[s]]]])
      sig[[s]] <- !is.na(code) & code %in% c("P", "D")
      sev[[s]] <- sev_code(code)
      ass[[s]] <- !is.na(code) & code != "E"
    }
  }
  mk <- function(l) if (length(l)) do.call(cbind, l) else matrix(logical(0), n, 0)
  list(signal = mk(sig), severity = mk(sev), assessable = mk(ass))
}

# Signal of problem for each answer: a signal in any of the stages analysed.
# In comprehension: declared difficulty or paraphrase P or D.
rp_signal <- function(data, stages = NULL) {
  d <- as.data.frame(data)
  x <- rp_stage_matrix(d, rp_pick_stages(d, stages))$signal
  if (!ncol(x)) return(rep(FALSE, nrow(d)))
  rowSums(x) > 0
}

rp_severity_row <- function(data, stages = NULL) {
  d <- as.data.frame(data)
  x <- rp_stage_matrix(d, rp_pick_stages(d, stages))$severity
  if (!ncol(x)) return(rep(0, nrow(d)))
  apply(x, 1, max)
}

rp_decide <- function(p, cut_reformulate, cut_revise, lang) {
  lab <- rp_decision_labels(lang)
  out <- ifelse(p >= cut_reformulate, lab[3], ifelse(p >= cut_revise, lab[2], lab[1]))
  factor(out, levels = lab)
}

rp_decision_labels <- function(lang = "es") {
  if (lang == "es") c("Mantener", "Revisar", "Reformular") else c("Keep", "Revise", "Reformulate")
}

rp_code_labels <- function(lang = "es") {
  if (lang == "es") {
    c(A = "Capta la idea", P = "Parcial o desplaza el foco", D = "Otro significado", E = "Eco (no evaluable)")
  } else {
    c(A = "Adequate", P = "Partial or shifted focus", D = "Different meaning", E = "Echo (not assessable)")
  }
}

# Short labels of the codes of each stage, for legends.
rp_code_short <- function(stage = "comprehension", lang = "es") {
  es <- lang == "es"
  switch(stage,
    comprehension = if (es) c(A = "A capta", P = "P parcial", D = "D otro sentido", E = "E eco")
                    else c(A = "A adequate", P = "P partial", D = "D different", E = "E echo"),
    retrieval = if (es) c(A = "A pertinente", P = "P vago", D = "D otro \u00E1mbito", E = "E no evaluable")
                else c(A = "A relevant", P = "P vague", D = "D other domain", E = "E not assessable"),
    judgment = if (es) c(A = "A sostiene", P = "P otro criterio", D = "D contradice", E = "E no evaluable")
               else c(A = "A supports", P = "P other criterion", D = "D contradicts", E = "E not assessable"))
}

rp_list_text <- function(x, lang = "es", conj = NULL) {
  x <- as.character(x)
  if (length(x) < 2) return(paste(x, collapse = ""))
  paste(paste(utils::head(x, -1), collapse = ", "), if (!is.null(conj)) conj else if (lang == "es") "y" else "and", utils::tail(x, 1))
}

rp_order_participants <- function(p) {
  p <- as.character(p)
  num <- suppressWarnings(as.integer(sub("^P", "", p)))
  if (all(!is.na(num))) factor(p, levels = unique(p[order(num)])) else factor(p, levels = unique(p))
}
