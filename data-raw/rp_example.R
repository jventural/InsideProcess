# Simulated example data for InsideProcess (not real participants).
# Run from the package root with: source("data-raw/rp_example.R")
set.seed(20261112)

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

# Paraphrases: one adequate reading per item and some problematic ones.
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
problem_p <- c(.10, .10, .15, .15, .30, .55, .40, .10)  # chance of a P or D reading per item
participants <- paste0("P", 1:12)
rows <- list(); cod <- list()
for (p in seq_along(participants)) {
  speed <- stats::runif(1, .5, 1.6)
  echoer <- p == 9
  for (k in 1:8) {
    r <- stats::runif(1)
    code <- if (echoer && stats::runif(1) < .7) "E" else if (r < problem_p[k] * .4) "D" else if (r < problem_p[k]) "P" else "A"
    para <- switch(code, A = ok[k], P = partial[k], D = partial[k],
                   E = paste("Si", tolower(substr(rp_example_items$item[k], 1, 1)), substring(rp_example_items$item[k], 2)))
    clar_p <- if (code %in% c("P", "D")) .35 else .05
    if (k == 6) clar_p <- clar_p + .25
    cl <- if (stats::runif(1) < clar_p) sample(c("Más o menos", "No se entiende"), 1, prob = c(.7, .3)) else "Sí, se entiende"
    conf <- if (cl == "Sí, se entiende") NA_character_ else c("antelación", "relevancia", "recursos académicos", "gratificante")[match(k, c(4, 5, 6, 7), nomatch = 3)]
    rows[[length(rows) + 1]] <- data.frame(id = sprintf("PEJ%02d", p), n = k, clarity = cl, paraphrase = para,
                                           confusing = conf, seconds = round(stats::rlnorm(1, log(45 * speed), .4)),
                                           stringsAsFactors = FALSE)
    if (code != "A") {
      note <- switch(code, P = "Capta solo una parte de la frase", D = "Le da otro significado",
                     E = "Repite la frase")
      stage <- if (code == "E") NA_character_ else "comprension"
      cod[[length(cod) + 1]] <- data.frame(participant = participants[p], n = k, code = code, note = note,
                                           stage = stage, stringsAsFactors = FALSE)
    }
  }
}
raw <- do.call(rbind, rows)
rp_example <- rp_read(raw, items = rp_example_items)
attr(rp_example, "participants") <- NULL
rp_example <- as.data.frame(rp_example)
rp_example_coding <- do.call(rbind, cod)
save(rp_example, file = "data/rp_example.rda", compress = "xz")
save(rp_example_coding, file = "data/rp_example_coding.rda", compress = "xz")
save(rp_example_items, file = "data/rp_example_items.rda", compress = "xz")
