# Simulated example data for InsideProcess (not real participants).
# Run from the package root, after devtools::load_all(), with:
# source("data-raw/rp_example.R")
# The interview asked every probe: clarity, paraphrase, confusing (when not
# clear), retrieval, answer (with the test's scale), judgment and response.
set.seed(20261113)

rp_example_items <- data.frame(
  n = 1:8,
  item = c("Disfruto aprendiendo cosas nuevas en mis cursos.",
           "Organizo mi tiempo para cumplir con mis tareas.",
           "Me entusiasma participar en clase.",
           "Planifico con antelación mis periodos de estudio.",
           "Siento que mis estudios tienen relevancia para mí.",
           "Gestiono mis recursos académicos de manera eficiente.",
           "Aprender me resulta gratificante incluso cuando es difícil.",
           "Cumplo con los plazos sin dejar todo para el final."),
  item_code = c("DIS01", "ORG01", "DIS02", "ORG02", "DIS03", "ORG03", "DIS04", "ORG04"),
  dimension = rep(c("Disfrute", "Organización"), 4),
  stringsAsFactors = FALSE)

scale_options <- c("Nunca", "A veces", "Casi siempre", "Siempre")

# Comprehension: one adequate reading per item and some problematic ones.
ok <- c("Si me gusta aprender temas nuevos en las clases de la universidad",
        "Si reparto bien mis horas para hacer los trabajos a tiempo",
        "Si me emociona levantar la mano y opinar en las clases",
        "Si decido antes cuándo voy a estudiar y no improviso",
        "Si lo que estudio es importante para mi vida",
        "Si uso bien mis materiales, libros y tiempo para estudiar",
        "Si aprender me hace sentir bien aunque cueste",
        "Si entrego las cosas a tiempo sin esperar al último día")
partial <- c("Si me gustan los cursos", "Si hago mis tareas", "Si voy a clases",
             "Si estudio", "Si los cursos sirven para el trabajo", "Si tengo dinero para estudiar",
             "Si las cosas difíciles me gustan", "Si soy responsable")
# Retrieval: what came to mind.
recall_ok <- c("Cuando aprendí a programar en el curso de estadística",
               "La semana de parciales, cuando hice un horario",
               "Cuando expuse en el curso de metodología",
               "El domingo, cuando armo mi semana de estudio",
               "Pensé en mi carrera y en lo que quiero hacer después",
               "Cuando ordeno mis apuntes y los libros de la biblioteca",
               "Cuando un tema de cálculo me costó pero al final lo entendí",
               "La entrega del último informe del curso")
recall_other <- c("En mis clases de baile", "En mi trabajo de los fines de semana",
                  "En las reuniones de mi familia", "Cuando planifico un viaje",
                  "En mi vida en general", "En el dinero que me dan mis padres",
                  "Cuando aprendí a manejar", "En los pagos del banco")
reason_ok <- c("Porque casi nunca me pasa en mis cursos", "Porque solo me pasa en algunos cursos",
               "Porque me pasa en la mayoría de mis cursos", "Porque me pasa siempre en mis cursos")
reason_other <- c("Porque así debería ser un buen estudiante", "Porque es lo que se espera de uno",
                  "Porque si me pasara, respondería eso")

problem_c <- c(.10, .10, .15, .15, .30, .55, .40, .10)  # comprehension (P or D)
problem_r <- c(.05, .05, .10, .05, .30, .45, .15, .05)  # retrieval
problem_j <- c(.05, .10, .05, .10, .20, .10, .40, .45)  # judgment
problem_s <- c(.05, .05, .10, .05, .15, .20, .45, .10)  # response (hard to choose)
answer_p <- rep(list(c(.05, .25, .40, .30)), 8)
answer_p[[7]] <- c(.10, .40, .30, .20)
answer_p[[8]] <- c(0, .02, .08, .90)                    # piled on the top option

participants <- paste0("P", 1:12)
rows <- list(); cod <- list()
for (p in seq_along(participants)) {
  speed <- stats::runif(1, .5, 1.6)
  echoer <- p == 9
  for (k in 1:8) {
    # comprehension
    r <- stats::runif(1)
    code <- if (echoer && k %in% c(1, 2, 3, 5, 6, 8)) "E" else if (r < problem_c[k] * .4) "D" else if (r < problem_c[k]) "P" else "A"
    para <- switch(code, A = ok[k], P = partial[k], D = partial[k],
                   E = paste("Si", tolower(substr(rp_example_items$item[k], 1, 1)), substring(rp_example_items$item[k], 2)))
    clar_p <- if (code %in% c("P", "D")) .35 else .05
    if (k == 6) clar_p <- clar_p + .25
    cl <- if (stats::runif(1) < clar_p) sample(c("Más o menos", "No se entiende"), 1, prob = c(.7, .3)) else "Sí, se entiende"
    conf <- if (cl == "Sí, se entiende") NA_character_ else c("antelación", "relevancia", "recursos académicos", "gratificante")[match(k, c(4, 5, 6, 7), nomatch = 3)]
    # retrieval
    r <- stats::runif(1)
    code_r <- if (echoer && stats::runif(1) < .5) "E" else if (r < problem_r[k] * .5) "D" else if (r < problem_r[k]) "P" else "A"
    recall <- switch(code_r, A = recall_ok[k], P = "En la universidad en general", D = recall_other[k], E = "No sé")
    # answer and judgment
    a <- sample(4, 1, prob = answer_p[[k]])
    r <- stats::runif(1)
    code_j <- if (echoer && stats::runif(1) < .5) "E" else if (r < problem_j[k] * .3) "D" else if (r < problem_j[k]) "P" else "A"
    if (code_j == "D") a <- 4
    reason <- switch(code_j, A = reason_ok[a], P = sample(reason_other, 1), D = "Porque casi nunca lo hago", E = "Porque sí")
    # response
    hard <- stats::runif(1) < problem_s[k]
    resp <- if (hard) sample(c("Un poco", "Sí, me costó"), 1, prob = c(.7, .3)) else "No, fue fácil"
    rows[[length(rows) + 1]] <- data.frame(id = sprintf("PEJ%02d", p), n = k, clarity = cl, paraphrase = para,
                                           confusing = conf, retrieval = recall, answer = scale_options[a],
                                           judgment = reason, response = resp,
                                           seconds = round(stats::rlnorm(1, log(80 * speed), .4)),
                                           stringsAsFactors = FALSE)
    if (code != "A" || code_r != "A" || code_j != "A") {
      note <- switch(code, A = NA_character_, P = "Capta solo una parte de la frase", D = "Le da otro significado",
                     E = "Repite la frase")
      note_r <- switch(code_r, A = NA_character_, P = "Recuerdo genérico, sin situación concreta",
                       D = "Piensa en otro ámbito", E = "No dice qué recordó")
      note_j <- switch(code_j, A = NA_character_, P = "Responde por lo que debería ser",
                       D = "La razón contradice la opción elegida", E = "Razón sin contenido")
      cod[[length(cod) + 1]] <- data.frame(participant = participants[p], n = k,
                                           code = if (code == "A") NA_character_ else code, note = note,
                                           code_retrieval = if (code_r == "A") NA_character_ else code_r,
                                           note_retrieval = note_r,
                                           code_judgment = if (code_j == "A") NA_character_ else code_j,
                                           note_judgment = note_j, stringsAsFactors = FALSE)
    }
  }
}
raw <- do.call(rbind, rows)
rp_example <- rp_read(raw, items = rp_example_items)
attr(rp_example, "participants") <- NULL
rp_example <- as.data.frame(rp_example)
rp_example_coding <- do.call(rbind, cod)
rownames(rp_example_coding) <- NULL
save(rp_example, file = "data/rp_example.rda", compress = "xz")
save(rp_example_coding, file = "data/rp_example_coding.rda", compress = "xz")
save(rp_example_items, file = "data/rp_example_items.rda", compress = "xz")
