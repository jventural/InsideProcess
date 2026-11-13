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

# Signal of problem for each response: declared difficulty or paraphrase P or D.
rp_signal <- function(data) {
  cl <- rp_clarity_level(data$clarity)
  sig <- !is.na(cl) & cl > 0
  if (rp_has_code(data)) sig <- sig | (!is.na(data$code) & data$code %in% c("P", "D"))
  sig
}

rp_severity_row <- function(data) {
  cl <- rp_clarity_level(data$clarity)
  cl[is.na(cl)] <- 0L
  sp <- if (rp_has_code(data)) c(A = 0, E = 0, P = 1, D = 2)[data$code] else rep(0, nrow(data))
  sp[is.na(sp)] <- 0
  pmax(sp, cl)
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
