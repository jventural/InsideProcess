# Web form for self-administered cognitive interviews (web probing).

rp_probe_types <- c("clarity", "paraphrase", "confusing", "retrieval", "answer", "judgment", "response")

rp_texts <- function(lang = "es") {
  if (lang == "es") {
    list(
      welcome_title = "Ay\u00FAdanos a revisar unas frases",
      welcome_html = paste0(
        "<p>Estamos preparando un cuestionario y, antes de usarlo, necesitamos saber si sus frases se entienden bien.</p>",
        "<p>Ver\u00E1s <b>{n} frases</b>, una por una. En cada una te preguntaremos qu\u00E9 entiendes t\u00FA al leerla. ",
        "No hay respuestas correctas ni incorrectas: solo nos interesa c\u00F3mo la entiendes.</p>",
        "<p>{minutes}Tu participaci\u00F3n es voluntaria y an\u00F3nima: no pedimos tu nombre. ",
        "Puedes dejarlo en cualquier momento; si cierras la p\u00E1gina, al volver seguir\u00E1s donde te quedaste.</p>"),
      minutes = "Te tomar\u00E1 unos {m} minutos. ",
      consent = "Le\u00ED esta informaci\u00F3n y acepto participar.",
      start = "Comenzar", back = "\u2190 Atr\u00E1s", next_label = "Siguiente", finish = "Terminar",
      profile_title = "Unos datos sobre ti",
      profile_missing = "Completa todos los datos para continuar.",
      example_title = "As\u00ED funciona",
      example_intro = "Mira este ejemplo, que no es parte del cuestionario:",
      example_item = "Frase:",
      example_confusing = "Si una frase <b>no</b> se entiende del todo, tambi\u00E9n te preguntaremos qu\u00E9 palabra o parte te pareci\u00F3 rara o dif\u00EDcil.",
      example_own_words = "Escribe con tus propias palabras, como se lo explicar\u00EDas a un compa\u00F1ero. No copies la frase.",
      go_items = "Empezar con las frases",
      counter = "FRASE {i} DE {n}",
      q_clarity = "\u00BFEsta frase se entiende con claridad?",
      q_paraphrase = "Con tus palabras, \u00BFqu\u00E9 te est\u00E1 preguntando esta frase?",
      q_confusing = "\u00BFQu\u00E9 palabra o parte de la frase te result\u00F3 rara, dif\u00EDcil o confusa?",
      q_retrieval = "\u00BFEn qu\u00E9 pensaste o qu\u00E9 situaci\u00F3n recordaste al leerla?",
      q_answer = "\u00BFQu\u00E9 responder\u00EDas a esta frase?",
      q_judgment = "\u00BFPor qu\u00E9 elegiste esa respuesta?",
      q_response = "\u00BFTe cost\u00F3 elegir una de las opciones?",
      ph_paraphrase = "Escribe lo que entiendes al leerla...",
      ph_confusing = "Por ejemplo, una palabra que no se usa entre estudiantes...",
      ph_retrieval = "Una situaci\u00F3n, una persona, un momento...",
      ph_judgment = "Lo que te hizo elegir esa opci\u00F3n...",
      ph_comment = "Tu comentario...",
      reason_partly = "Marcaste que esta frase se entiende <b>m\u00E1s o menos</b>. Ay\u00FAdanos a ubicar qu\u00E9 parte le quita claridad.",
      reason_not = "Marcaste que esta frase <b>no se entiende</b>. Ay\u00FAdanos a ubicar qu\u00E9 parte la vuelve dif\u00EDcil.",
      need_clarity = "Indica si la frase se entiende.",
      need_paraphrase = "Escribe con tus palabras qu\u00E9 entiendes (al menos unas palabras).",
      need_confusing = "Cu\u00E9ntanos qu\u00E9 parte de la frase te result\u00F3 confusa.",
      need_text = "Escribe tu respuesta con tus palabras (al menos unas palabras).",
      need_choice = "Elige una opci\u00F3n.",
      close_title = "\u00A1Terminaste las frases!",
      close_comment = "\u00BFQuieres comentar algo m\u00E1s sobre las frases o sobre el cuestionario? (opcional)",
      back_last = "\u2190 Volver a la \u00FAltima frase", send = "Enviar respuestas", sending = "Enviando...",
      retry = "Reintentar env\u00EDo",
      send_failed = "No se pudo enviar. Revisa tu conexi\u00F3n y reintenta",
      download = "Descargar mis respuestas en un archivo",
      thanks_title = "\u00A1Gracias por tu ayuda!",
      thanks_text = "Tus respuestas se registraron. Con ellas mejoraremos las frases del cuestionario.",
      code_label = "C\u00F3digo de participaci\u00F3n:",
      new_participation = "Nueva participaci\u00F3n en este equipo"
    )
  } else {
    list(
      welcome_title = "Help us review some sentences",
      welcome_html = paste0(
        "<p>We are preparing a questionnaire and, before using it, we need to know whether its sentences are clear.</p>",
        "<p>You will see <b>{n} sentences</b>, one at a time. For each one we will ask what you understand when you read it. ",
        "There are no right or wrong answers: we only want to know how you understand it.</p>",
        "<p>{minutes}Participation is voluntary and anonymous: we do not ask for your name. ",
        "You may stop at any time; if you close the page, you will continue where you left off.</p>"),
      minutes = "It will take about {m} minutes. ",
      consent = "I have read this information and agree to take part.",
      start = "Start", back = "\u2190 Back", next_label = "Next", finish = "Finish",
      profile_title = "A few details about you",
      profile_missing = "Please fill in all the details to continue.",
      example_title = "How it works",
      example_intro = "Look at this example, which is not part of the questionnaire:",
      example_item = "Sentence:",
      example_confusing = "If a sentence is <b>not</b> fully clear, we will also ask which word or part seemed odd or difficult.",
      example_own_words = "Write in your own words, as you would explain it to a classmate. Do not copy the sentence.",
      go_items = "Start with the sentences",
      counter = "SENTENCE {i} OF {n}",
      q_clarity = "Is this sentence clear?",
      q_paraphrase = "In your own words, what is this sentence asking you?",
      q_confusing = "Which word or part of the sentence seemed odd, difficult or confusing?",
      q_retrieval = "What did you think of, or what situation came to mind, when you read it?",
      q_answer = "How would you answer this sentence?",
      q_judgment = "Why did you choose that answer?",
      q_response = "Was it hard to choose one of the options?",
      ph_paraphrase = "Write what you understand...",
      ph_confusing = "For example, a word people do not usually say...",
      ph_retrieval = "A situation, a person, a moment...",
      ph_judgment = "What made you choose that option...",
      ph_comment = "Your comment...",
      reason_partly = "You marked this sentence as <b>somewhat</b> clear. Help us find the part that makes it less clear.",
      reason_not = "You marked this sentence as <b>not clear</b>. Help us find the part that makes it difficult.",
      need_clarity = "Please say whether the sentence is clear.",
      need_paraphrase = "Please write in your own words what you understand (at least a few words).",
      need_confusing = "Please tell us which part was confusing.",
      need_text = "Please answer in your own words (at least a few words).",
      need_choice = "Please choose an option.",
      close_title = "You have finished the sentences!",
      close_comment = "Would you like to add anything about the sentences or the questionnaire? (optional)",
      back_last = "\u2190 Back to the last sentence", send = "Send answers", sending = "Sending...",
      retry = "Try again",
      send_failed = "The answers could not be sent. Check your connection and try again",
      download = "Download my answers as a file",
      thanks_title = "Thank you for your help!",
      thanks_text = "Your answers were recorded. They will help us improve the questionnaire.",
      code_label = "Participation code:",
      new_participation = "New participation on this device"
    )
  }
}

#' Profile fields for the cognitive interview form
#'
#' Builds the list of participant details the form asks before the items
#' (for example age, sex, programme and year of study). Use it to change the
#' default fields of [rp_form()], [rp_apps_script()] and [rp_google_form()].
#'
#' @param ... Fields given as `id = "Label"` (a free text field). For a number
#'   or a closed list, give a list: `edad = list("Edad", "number", min = 16)` or
#'   `sexo = list("Sexo", options = c("Mujer", "Hombre"))`.
#' @param lang Language of the default fields when `...` is empty: `"es"` or `"en"`.
#'
#' @return A list of fields. Each field is a list with `id`, `label`, `type`
#'   (`"text"`, `"number"` or `"select"`), `options`, `min` and `max`.
#'   An empty list means that the form asks no profile.
#' @export
#' @examples
#' rp_profile()
#' rp_profile(edad = list("Edad", "number", min = 16, max = 70),
#'            carrera = "Carrera")
rp_profile <- function(..., lang = NULL) {
  lang <- rp_lang(lang)
  dots <- list(...)
  if (!length(dots)) {
    dots <- if (lang == "es") {
      list(edad = list("Edad (a\u00F1os)", "number", min = 16, max = 70),
           sexo = list("Sexo", options = c("Mujer", "Hombre", "Otro", "Prefiero no decirlo")),
           carrera = "Carrera",
           ciclo = list("Ciclo que cursas", "number", min = 1, max = 14))
    } else {
      list(age = list("Age (years)", "number", min = 16, max = 90),
           sex = list("Sex", options = c("Female", "Male", "Other", "Prefer not to say")),
           programme = "Programme",
           year = list("Year of study", "number", min = 1, max = 10))
    }
  }
  if (is.null(names(dots)) || any(names(dots) == "")) stop("Every field needs a name (its id).", call. = FALSE)
  out <- lapply(names(dots), function(id) {
    f <- dots[[id]]
    if (is.character(f)) f <- list(f)
    type <- if (!is.null(f$options)) "select" else if (length(f) >= 2 && is.character(f[[2]])) f[[2]] else "text"
    list(id = id, label = f[[1]], type = type,
         options = if (is.null(f$options)) list() else as.list(f$options),
         min = f$min, max = f$max)
  })
  unname(out)
}

rp_form_config <- function(items, probes, answer_options, response_options, profile,
                           minutes, example, key, texts, min_words, lang) {
  probes <- unique(match.arg(probes, rp_probe_types, several.ok = TRUE))
  if (!"paraphrase" %in% probes && !"clarity" %in% probes) {
    warning("Without `clarity` or `paraphrase` the form gathers no comprehension evidence.", call. = FALSE)
  }
  if ("answer" %in% probes && is.null(answer_options)) {
    stop("The `answer` probe needs `answer_options` (the response scale of the test).", call. = FALSE)
  }
  if ("response" %in% probes && is.null(response_options)) {
    response_options <- rp_response_options(lang)
  }
  key_items <- rp_items_key(items)
  if (!"item" %in% names(key_items)) stop("`items` needs a column with the item text (`item`).", call. = FALSE)
  t <- rp_texts(lang)
  if (length(texts)) {
    bad <- setdiff(names(texts), names(t))
    if (length(bad)) stop("Unknown text keys: ", paste(bad, collapse = ", "), ". See rp_form() for the list.", call. = FALSE)
    t[names(texts)] <- texts
  }
  mins <- if (is.null(minutes)) "" else gsub("{m}", minutes, t$minutes, fixed = TRUE)
  t$welcome_html <- gsub("{minutes}", mins, gsub("{n}", nrow(key_items), t$welcome_html, fixed = TRUE), fixed = TRUE)
  if (isTRUE(example)) {
    example <- if (lang == "es") list(item = "Llego temprano a mis clases.", paraphrase = "Si soy puntual, si llego a la hora o antes a las clases.")
               else list(item = "I arrive early to my classes.", paraphrase = "Whether I am on time, arriving at or before the start of class.")
  }
  if (isFALSE(example)) example <- NULL
  fields <- as.list(rp_fields(lang))
  list(items = lapply(seq_len(nrow(key_items)), function(i) list(n = key_items$n[i], item = key_items$item[i])),
       probes = probes, fields = fields, clarity_options = rp_clarity_options(lang),
       answer_options = if (is.null(answer_options)) list() else as.list(answer_options),
       response_options = if (is.null(response_options)) list() else as.list(response_options),
       profile = profile, example = example, key = key, t = t, min_words = min_words,
       comment_field = if (lang == "es") "comentario" else "comment",
       file_prefix = if (lang == "es") "entrevista_cognitiva_" else "cognitive_interview_",
       items_key = key_items)
}

rp_write_utf8 <- function(text, file) {
  con <- file(file, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(text, con, useBytes = FALSE)
  invisible(file)
}

rp_render_form <- function(cfg, title, accent, lang) {
  tpl <- readLines(system.file("templates", "form.html", package = "InsideProcess"), encoding = "UTF-8", warn = FALSE)
  cfg_js <- cfg
  cfg_js$items_key <- NULL
  json <- jsonlite::toJSON(cfg_js, auto_unbox = TRUE, null = "null")
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  html <- paste(tpl, collapse = "\n")
  html <- sub("/*__CONFIG__*/{}", json, html, fixed = TRUE)
  html <- gsub("__TITLE__", rp_html_escape(title), html, fixed = TRUE)
  html <- gsub("__LANG__", lang, html, fixed = TRUE)
  html <- gsub("__ACCENT__", accent, html, fixed = TRUE)
  html
}

rp_html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Write the cognitive interview form as a web page
#'
#' Writes a single, self-contained HTML page that walks each participant
#' through consent, profile, a worked example and then the items, one at a
#' time, with the chosen probes. The page records the seconds spent on each
#' item (paradata) and keeps the progress in the browser, so a participant who
#' closes the page continues where they left off.
#'
#' Opened directly in a browser, the page works offline: at the end it
#' downloads the answers as a csv file that [rp_read()] reads (one file or a
#' whole folder). Inside a Google Apps Script web app it sends the answers to a
#' Google Sheet instead; [rp_apps_script()] writes everything that needs.
#'
#' Probes and the stage of the response process (Tourangeau et al., 2000) each
#' one documents:
#'
#' | Probe | Question | Stage |
#' |---|---|---|
#' | `clarity` | Is this sentence clear? (yes, somewhat, no) | Comprehension (declared) |
#' | `paraphrase` | In your own words, what is it asking? | Comprehension (shown) |
#' | `confusing` | Which word or part was confusing? (only if not fully clear) | Comprehension |
#' | `retrieval` | What did you think of when reading it? | Retrieval |
#' | `answer` | How would you answer it? (the test's scale) | Judgment and response |
#' | `judgment` | Why did you choose that answer? | Judgment |
#' | `response` | Was it hard to choose an option? | Response (mapping) |
#'
#' Texts that can be changed through `texts`: `welcome_title`, `welcome_html`
#' (use `{n}` for the number of items and `{minutes}` for the duration
#' sentence), `minutes`, `consent`, `profile_title`, `example_intro`, `counter`,
#' `q_clarity`, `q_paraphrase`, `q_confusing`, `q_retrieval`, `q_answer`,
#' `q_judgment`, `q_response`, `close_title`, `close_comment`, `thanks_title`,
#' `thanks_text` and the remaining button and warning labels of `rp_texts()`.
#'
#' @param items Data frame with the item text in `item` and, optionally, its
#'   order `n`, its code (`item_code` or `codigo`) and its `dimension`; or a
#'   character vector of items.
#' @param file Path of the HTML file to write.
#' @param title Title of the browser tab.
#' @param probes Probes asked for each item (see Details). The default is the
#'   comprehension set: `c("clarity", "paraphrase", "confusing")`.
#' @param answer_options Response scale of the test, needed for the `answer` probe.
#' @param response_options Options of the `response` probe. A three-point
#'   difficulty scale is used when `NULL`.
#' @param profile Participant details asked before the items, built with
#'   [rp_profile()]. Use `list()` to ask none.
#' @param minutes Approximate duration shown in the welcome text, or `NULL`.
#' @param example `TRUE` shows a worked example before the items; `FALSE` skips
#'   it; a list with `item` and `paraphrase` gives your own example.
#' @param key Name under which the browser saves the progress. Use a different
#'   key for each round so that answers of two rounds do not mix.
#' @param texts Named list of texts that replace the defaults (see Details).
#' @param min_words Minimum number of words required in open answers.
#' @param accent Main colour of the page (hexadecimal).
#' @param lang Language of the form: `"es"` (default) or `"en"`. The default can
#'   be set with `options(InsideProcess.lang = "en")`.
#'
#' @return The path of the file, invisibly.
#' @references Tourangeau, R., Rips, L. J., & Rasinski, K. (2000). *The
#'   psychology of survey response*. Cambridge University Press.
#'   \doi{10.1017/CBO9780511819322}
#' @seealso [rp_apps_script()] to publish it with Google, [rp_google_form()]
#'   for a native Google Form, [rp_read()] to read the answers.
#' @export
#' @examples
#' f <- rp_form(rp_example_items, file.path(tempdir(), "form.html"))
#' file.exists(f)
#'
#' # Comprehension plus retrieval and judgment probes
#' f2 <- rp_form(rp_example_items, file.path(tempdir(), "form2.html"),
#'               probes = c("clarity", "paraphrase", "confusing", "retrieval",
#'                          "answer", "judgment"),
#'               answer_options = c("Nunca", "A veces", "Siempre"))
#' unlink(c(f, f2))
rp_form <- function(items, file, title = NULL,
                    probes = c("clarity", "paraphrase", "confusing"),
                    answer_options = NULL, response_options = NULL,
                    profile = rp_profile(lang = lang), minutes = NULL, example = TRUE,
                    key = NULL, texts = list(), min_words = 3, accent = "#2f6f62", lang = NULL) {
  lang <- rp_lang(lang)
  if (is.null(title)) title <- if (lang == "es") "Entrevista cognitiva" else "Cognitive interview"
  if (is.null(key)) key <- paste0("rp_", gsub("[^a-z0-9]", "", tolower(basename(tempfile("")))))
  cfg <- rp_form_config(items, probes, answer_options, response_options, profile,
                        minutes, example, key, texts, min_words, lang)
  rp_write_utf8(rp_render_form(cfg, title, accent, lang), file)
  invisible(file)
}

#' Publish the cognitive interview form with Google Apps Script
#'
#' Writes, in one folder, the four files needed to run the form of [rp_form()]
#' as a Google Apps Script web app bound to a Google Sheet: `Index.html` (the
#' form), `Codigo.gs` (the server code that saves each participation),
#' `items.csv` (the item key for [rp_read()]) and a step-by-step guide to
#' publish it. Answers land in two tabs of the sheet: one row per participant
#' (profile, start and end, minutes, final comment) and one row per participant
#' and item (probes and seconds on the item). Download the sheet as Excel and
#' read it with [rp_read()].
#'
#' Unlike a Google Form, the web app records the seconds spent on each item
#' and shows the confusing-part probe only when the item was not fully clear,
#' with a reminder of what the participant marked.
#'
#' @inheritParams rp_form
#' @param dir Folder where the files are written. It is created if needed.
#' @param ... Other arguments passed to [rp_form()] (`probes`, `profile`,
#'   `answer_options`, `texts`, `minutes`, `example`, `min_words`, `accent`).
#'
#' @return The paths of the written files, invisibly.
#' @seealso [rp_form()], [rp_google_form()], [rp_read()]
#' @export
#' @examples
#' out <- rp_apps_script(rp_example_items, file.path(tempdir(), "web_app"),
#'                       title = "Entrevista cognitiva", minutes = 15)
#' basename(out)
#' unlink(file.path(tempdir(), "web_app"), recursive = TRUE)
rp_apps_script <- function(items, dir, title = NULL, key = NULL, lang = NULL, ...) {
  lang <- rp_lang(lang)
  if (is.null(title)) title <- if (lang == "es") "Entrevista cognitiva" else "Cognitive interview"
  if (is.null(key)) key <- paste0("rp_", gsub("[^a-z0-9]", "_", tolower(title)), "_", format(Sys.Date(), "%Y%m%d"))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  args <- list(...)
  defaults <- list(probes = c("clarity", "paraphrase", "confusing"), answer_options = NULL,
                   response_options = NULL, profile = rp_profile(lang = lang), minutes = NULL,
                   example = TRUE, texts = list(), min_words = 3, accent = "#2f6f62")
  bad <- setdiff(names(args), names(defaults))
  if (length(bad)) stop("Unknown arguments: ", paste(bad, collapse = ", "), call. = FALSE)
  defaults[names(args)] <- args
  a <- defaults
  cfg <- rp_form_config(items, a$probes, a$answer_options, a$response_options, a$profile,
                        a$minutes, a$example, key, a$texts, a$min_words, lang)
  f_html <- file.path(dir, "Index.html")
  rp_write_utf8(rp_render_form(cfg, title, a$accent, lang), f_html)

  fl <- cfg$fields
  col_resp <- c("n", "item", unlist(fl[cfg$probes]), fl$seconds)
  col_prof <- vapply(cfg$profile, function(x) x$id, character(1))
  col_end <- if (lang == "es") c("n_frases", "n_no_claras", "minutos", "comentario") else c("n_items", "n_unclear", "minutes", "comment")
  sheets <- if (lang == "es") c("Participantes", "Respuestas") else c("Participants", "Responses")
  js_arr <- function(x) jsonlite::toJSON(unname(as.character(x)))
  gs <- paste(readLines(system.file("templates", "Code.gs", package = "InsideProcess"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  rep <- c(`__SHEET_PART__` = sheets[1], `__SHEET_RESP__` = sheets[2],
           `__COL_PROFILE__` = if (length(col_prof)) js_arr(col_prof) else "[]",
           `__COL_RESP__` = js_arr(col_resp), `__COL_END__` = js_arr(col_end),
           `__CLARITY_FIELD__` = if ("clarity" %in% cfg$probes) fl$clarity else "",
           `__CLEAR__` = cfg$clarity_options[1], `__SECONDS_FIELD__` = fl$seconds,
           `__TITLE__` = gsub("'", "\\\\'", title))
  for (k in names(rep)) gs <- gsub(k, rep[[k]], gs, fixed = TRUE)
  f_gs <- file.path(dir, "Codigo.gs")
  rp_write_utf8(gs, f_gs)

  f_items <- file.path(dir, "items.csv")
  utils::write.csv(cfg$items_key, f_items, row.names = FALSE, fileEncoding = "UTF-8")

  f_guide <- file.path(dir, if (lang == "es") "COMO_PUBLICAR.txt" else "HOW_TO_PUBLISH.txt")
  rp_write_utf8(rp_guide_apps_script(lang, sheets), f_guide)
  out <- c(f_html, f_gs, f_items, f_guide)
  message(if (lang == "es") "Archivos listos en " else "Files ready in ", normalizePath(dir, winslash = "/"),
          if (lang == "es") ". Siga los pasos de COMO_PUBLICAR.txt." else ". Follow HOW_TO_PUBLISH.txt.")
  invisible(out)
}

rp_guide_apps_script <- function(lang, sheets) {
  if (lang == "es") {
    c("C\u00D3MO PUBLICAR LA ENTREVISTA COGNITIVA CON GOOGLE (unos 10 minutos)",
      "",
      "1. En Google Drive cree una hoja de c\u00E1lculo nueva (vac\u00EDa). Ah\u00ED llegar\u00E1n las respuestas.",
      "2. En la hoja: Extensiones > Apps Script. Se abre el editor del proyecto vinculado.",
      "3. Borre el contenido de Code.gs (o C\u00F3digo.gs) y pegue todo el archivo Codigo.gs.",
      "4. Pulse + junto a Archivos > HTML, n\u00F3mbrelo exactamente Index y pegue todo el archivo Index.html.",
      "5. Guarde (Ctrl + S).",
      "6. Implementar > Nueva implementaci\u00F3n > tipo Aplicaci\u00F3n web.",
      "   Ejecutar como: Yo.  Qui\u00E9n tiene acceso: Cualquier usuario.",
      "7. Autorice los permisos (es su propia cuenta: Avanzado > Ir al proyecto).",
      "8. Copie la URL que termina en /exec: ese es el enlace para los participantes.",
      "   Si quiere un enlace corto, ac\u00F3rtelo (por ejemplo, con is.gd).",
      "9. Pruebe el enlace una vez y borre en la hoja la fila de prueba.",
      "",
      paste0("Las respuestas llegan a las pesta\u00F1as \"", sheets[1], "\" y \"", sheets[2], "\" (se crean con el primer env\u00EDo)."),
      "Para analizar: Archivo > Descargar > Microsoft Excel y luego, en R:",
      "  ec <- rp_read(\"archivo.xlsx\", items = \"items.csv\")",
      "",
      "Para cambiar una frase sin cambiar el enlace: vuelva a generar Index.html, p\u00E9guelo en el editor y",
      "use Implementar > Gestionar implementaciones > l\u00E1piz > Versi\u00F3n: nueva versi\u00F3n.",
      "Abra Index.html en su navegador para probarlo sin Google: al final descarga un csv que rp_read() tambi\u00E9n lee.")
  } else {
    c("HOW TO PUBLISH THE COGNITIVE INTERVIEW WITH GOOGLE (about 10 minutes)",
      "",
      "1. In Google Drive create a new, empty spreadsheet. Answers will arrive there.",
      "2. In the sheet: Extensions > Apps Script. The bound project editor opens.",
      "3. Delete the content of Code.gs and paste the whole file Codigo.gs.",
      "4. Click + next to Files > HTML, name it exactly Index and paste the whole file Index.html.",
      "5. Save (Ctrl + S).",
      "6. Deploy > New deployment > type Web app.",
      "   Execute as: Me.  Who has access: Anyone.",
      "7. Authorise the permissions (it is your own account: Advanced > Go to project).",
      "8. Copy the URL ending in /exec: that is the link for participants.",
      "9. Try the link once and delete the test row in the sheet.",
      "",
      paste0("Answers arrive in the tabs \"", sheets[1], "\" and \"", sheets[2], "\" (created with the first submission)."),
      "To analyse: File > Download > Microsoft Excel and then, in R:",
      "  ci <- rp_read(\"file.xlsx\", items = \"items.csv\")",
      "",
      "To change an item without changing the link: write Index.html again, paste it in the editor and use",
      "Deploy > Manage deployments > pencil > Version: new version.",
      "Open Index.html in your browser to try it without Google: at the end it downloads a csv that rp_read() also reads.")
  }
}

#' Write a script that builds the cognitive interview as a native Google Form
#'
#' Writes a Google Apps Script file that, run once from
#' [script.google.com](https://script.google.com), creates a Google Form with a
#' consent page, the profile questions and one section per item with the
#' chosen probes. The confusing-part question sits in its own section and is
#' reached only when the participant does not mark the item as clear (Google
#' Forms branching). The form is linked to a new response sheet. Use it when a
#' plain Google Form is preferred; note that Google Forms does not record the
#' seconds spent on each item, so analyses based on time are not available.
#' Read the downloaded responses with [rp_read_google_form()].
#'
#' Question titles start with `item.probe` numbers (for example `3.1` is the
#' clarity probe of item 3 and `3.2` its paraphrase), which is how
#' [rp_read_google_form()] finds them. Do not change those numbers.
#'
#' @inheritParams rp_form
#' @param file Path of the `.gs` file to write.
#'
#' @return The path of the file, invisibly.
#' @seealso [rp_read_google_form()], [rp_apps_script()]
#' @export
#' @examples
#' f <- rp_google_form(rp_example_items, file.path(tempdir(), "crear_formulario.gs"))
#' head(readLines(f), 3)
#' unlink(f)
rp_google_form <- function(items, file, title = NULL,
                           probes = c("clarity", "paraphrase", "confusing"),
                           answer_options = NULL, response_options = NULL,
                           profile = rp_profile(lang = lang), texts = list(), lang = NULL) {
  lang <- rp_lang(lang)
  if (is.null(title)) title <- if (lang == "es") "Entrevista cognitiva" else "Cognitive interview"
  cfg <- rp_form_config(items, probes, answer_options, response_options, profile,
                        minutes = NULL, example = FALSE, key = "", texts = texts, min_words = 3, lang = lang)
  t <- cfg$t
  strip <- function(x) gsub("<[^>]+>", "", x)
  intro <- strip(gsub("</p>", "\n\n", t$welcome_html, fixed = TRUE))
  js <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)
  probe_num <- stats::setNames(seq_along(rp_probe_types), rp_probe_types)
  spec <- list(title = title, intro = trimws(intro), consent = t$consent,
               profile = cfg$profile, items = cfg$items, probes = cfg$probes, probe_num = as.list(probe_num),
               q = list(clarity = t$q_clarity, paraphrase = t$q_paraphrase, confusing = t$q_confusing,
                        retrieval = t$q_retrieval, answer = t$q_answer, judgment = t$q_judgment,
                        response = t$q_response),
               clarity_options = cfg$clarity_options, answer_options = cfg$answer_options,
               response_options = cfg$response_options,
               counter = t$counter, close = strip(t$close_comment), thanks = strip(t$thanks_text),
               reason = strip(t$reason_partly))
  code <- c(
    "// Written by rp_google_form() of the R package InsideProcess.",
    "// 1) Open https://script.google.com > New project. 2) Paste this file. 3) Run crearFormulario and authorise.",
    "// 4) The log (View > Logs) prints the link to share and the response sheet.",
    paste0("const SPEC = ", js(spec), ";"),
    "",
    "function crearFormulario() {",
    "  const f = FormApp.create(SPEC.title);",
    "  f.setDescription(SPEC.intro).setProgressBar(true).setConfirmationMessage(SPEC.thanks);",
    "  f.addCheckboxItem().setTitle(SPEC.consent).setChoiceValues([SPEC.consent]).setRequired(true);",
    "  SPEC.profile.forEach(c => {",
    "    if (c.type === 'select') f.addMultipleChoiceItem().setTitle(c.label).setChoiceValues(c.options).setRequired(true);",
    "    else if (c.type === 'number') {",
    "      const v = FormApp.createTextValidation().requireNumber().build();",
    "      f.addTextItem().setTitle(c.label).setValidation(v).setRequired(true);",
    "    } else f.addTextItem().setTitle(c.label).setRequired(true);",
    "  });",
    "  const has = p => SPEC.probes.indexOf(p) >= 0;",
    "  const tit = (n, p) => n + '.' + SPEC.probe_num[p] + ' ' + SPEC.q[p];",
    "  const N = SPEC.items.length, clar = [], conf = [], pags = [];",
    "  SPEC.items.forEach((it, i) => {",
    "    const pg = f.addPageBreakItem().setTitle(SPEC.counter.replace('{i}', i + 1).replace('{n}', N)).setHelpText('\"' + it.item + '\"');",
    "    pags.push(pg);",
    "    if (has('clarity')) clar.push(f.addMultipleChoiceItem().setTitle(tit(it.n, 'clarity')).setRequired(true));",
    "    if (has('paraphrase')) f.addParagraphTextItem().setTitle(tit(it.n, 'paraphrase')).setRequired(true);",
    "    if (has('retrieval')) f.addParagraphTextItem().setTitle(tit(it.n, 'retrieval')).setRequired(true);",
    "    if (has('answer')) f.addMultipleChoiceItem().setTitle(tit(it.n, 'answer')).setChoiceValues(SPEC.answer_options).setRequired(true);",
    "    if (has('judgment')) f.addParagraphTextItem().setTitle(tit(it.n, 'judgment')).setRequired(true);",
    "    if (has('response')) f.addMultipleChoiceItem().setTitle(tit(it.n, 'response')).setChoiceValues(SPEC.response_options).setRequired(true);",
    "    if (has('confusing')) {",
    "      const pc = f.addPageBreakItem().setTitle(SPEC.counter.replace('{i}', i + 1).replace('{n}', N)).setHelpText('\"' + it.item + '\"\\n\\n' + SPEC.reason);",
    "      f.addParagraphTextItem().setTitle(tit(it.n, 'confusing')).setRequired(has('clarity'));",
    "      conf.push(pc);",
    "    }",
    "  });",
    "  const final = f.addPageBreakItem().setTitle(SPEC.close);",
    "  f.addParagraphTextItem().setTitle(SPEC.close);",
    "  if (has('clarity') && has('confusing')) {",
    "    clar.forEach((q, i) => {",
    "      const siguiente = i + 1 < N ? pags[i + 1] : final;",
    "      q.setChoices([q.createChoice(SPEC.clarity_options[0], siguiente),",
    "                    q.createChoice(SPEC.clarity_options[1], conf[i]),",
    "                    q.createChoice(SPEC.clarity_options[2], conf[i])]);",
    "    });",
    "  } else {",
    "    clar.forEach(q => q.setChoiceValues(SPEC.clarity_options));",
    "  }",
    "  const hoja = SpreadsheetApp.create(SPEC.title + ' (respuestas)');",
    "  f.setDestination(FormApp.DestinationType.SPREADSHEET, hoja.getId());",
    "  Logger.log('Enlace para compartir: ' + f.getPublishedUrl());",
    "  Logger.log('Editar el formulario: ' + f.getEditUrl());",
    "  Logger.log('Hoja de respuestas: ' + hoja.getUrl());",
    "}")
  rp_write_utf8(code, file)
  invisible(file)
}
