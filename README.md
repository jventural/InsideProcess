# InsideProcess

Evidencias de validez basadas en los **procesos de respuesta** a partir de
entrevistas cognitivas autoadministradas (sondeo web). Funciones sueltas, sin un
flujo obligatorio: cada una se usa en una línea.

## Instalación

```r
# Versión en desarrollo desde GitHub
# install.packages("remotes")
remotes::install_github("jventural/InsideProcess")
```

Las etiquetas de tablas y figuras salen en español; para inglés:
`options(InsideProcess.lang = "en")`.

## Qué evidencia produce

Los *Standards* (AERA, APA y NCME, 2014) reconocen cinco fuentes de evidencia de
validez; una de ellas son los **procesos de respuesta**: si lo que la persona
hace al responder coincide con lo que el constructo supone. La **entrevista
cognitiva** es el método principal para obtener esa evidencia (Padilla y
Benítez, 2014; Castillo-Díaz y Padilla, 2013). Por eso el paquete no se queda en
"la entrevista cognitiva": la usa para documentar procesos de respuesta, etapa
por etapa del modelo de Tourangeau et al. (2000).

| Sonda (`probes`) | Etapa | Qué evidencia aporta |
|---|---|---|
| `clarity`, `paraphrase`, `confusing` | Comprensión | Si la frase se entiende como se quiso (declarado y mostrado) |
| `retrieval` | Recuperación | Qué recuerdos o situaciones activa la frase |
| `answer`, `judgment` | Juicio | Cómo llega la persona a su respuesta |
| `response` | Respuesta | Si las opciones permiten ubicar el juicio |
| segundos por frase | Paradatos | Esfuerzo y respuestas apuradas |

Con las sondas por defecto (las de comprensión) la evidencia cubre **solo la
primera etapa**. Para hablar de procesos de respuesta en sentido amplio, sume
`retrieval`, `answer`, `judgment` y `response`.

### Codificación por etapa

Las respuestas abiertas de cada etapa se codifican con las mismas cuatro
letras, cada una con su sentido en esa etapa (`rp_scheme()`):

| Código | Comprensión (`code`) | Recuperación (`code_retrieval`) | Juicio (`code_judgment`) |
|---|---|---|---|
| A | Capta la idea | Recuerdo pertinente al constructo | La razón sostiene la respuesta |
| P | Parcial o desplaza el foco | Recuerdo vago o genérico | Otro criterio (deseabilidad, norma, caso hipotético) |
| D | Otro significado | Otro ámbito u otro constructo | La razón contradice la respuesta |
| E | Eco (repite la frase) | Nada evaluable | Nada evaluable |

La etapa de respuesta no se codifica: sale de la sonda cerrada `response`
("¿te costó elegir una opción?") y del uso de las opciones (`rp_options()`).

## Funciones

| Para | Función |
|---|---|
| Crear el formulario web | `rp_form()` |
| Publicarlo con Google (hoja de respuestas) | `rp_apps_script()` |
| Crear un Google Form nativo | `rp_google_form()` |
| Leer las respuestas | `rp_read()`, `rp_read_google_form()` |
| Codificar respuestas abiertas (A, P, D, E) | `rp_coding_template()`, `rp_code()`, `rp_scheme()` |
| Acuerdo entre codificadores, por etapa | `rp_agreement()` (kappa y AC1) |
| Métricas y decisión por frase | `rp_metrics()`, `rp_wilson()` |
| Resumen para redactar | `rp_summary()` |
| Sensibilidad sin participantes dudosos | `rp_sensitivity()`, `rp_quality()` |
| Por dimensión y por etapa | `rp_dimension()`, `rp_stages()` |
| Uso de las opciones de respuesta | `rp_options()` |
| Lecturas detrás de cada problema | `rp_readings()` |
| Tablas | `rp_table()` (`gt = TRUE` para tabla con colores), `rp_word()` |
| Figuras | `rp_plot_clarity()`, `rp_plot_map()`, `rp_plot_dimension()`, `rp_plot_problem()`, `rp_plot_stages()` |

## Ejemplo

```r
library(InsideProcess)

# 1. Formulario: carpeta con Index.html, Codigo.gs, items.csv y COMO_PUBLICAR.txt
rp_apps_script(mis_items, "formulario", title = "Entrevista cognitiva", minutes = 25)

# 2. Datos: la hoja de Google bajada como Excel
ec <- rp_read("respuestas.xlsx", items = "formulario/items.csv")
rp_coding_template(ec, "codificacion.csv")      # se codifica en Excel
ec <- rp_code(ec, "codificacion.csv")

# 3. Métricas, tablas y figuras
m <- rp_metrics(ec)                              # IC, IP [Wilson], severidad, discordancia, decisión
rp_summary(ec)
rp_table(ec, "metrics", gt = TRUE)
rp_plot_problem(m)
rp_sensitivity(ec, exclude = c("P2", "P6"))

# 4. Si se preguntaron recuerdo, respuesta, razón y dificultad
rp_stages(ec)                                    # problemas por etapa con IC de Wilson
rp_plot_stages(m)                                # dónde falla cada frase
rp_options(ec, levels = c("Nunca", "A veces", "Casi siempre", "Siempre"))
rp_agreement("codificador1.csv", "codificador2.csv", stage = "judgment")
```

Con solo sondas de comprensión, `rp_metrics(ec, stages = "comprehension")`
y `rp_metrics(ec)` dan lo mismo.

## Métricas

| Métrica | Qué mide | Cómo se calcula |
|---|---|---|
| IC, índice de comprensión | Proporción que entendió la frase como se quiso | Parafraseos A / parafraseos evaluables (sin ecos) |
| IP, índice de problema | Proporción con alguna señal en cualquier etapa | Participantes con señal / participantes |
| IC 95 % del IP | Cuánto podría variar el IP en otra muestra | Intervalo de Wilson |
| Severidad | Gravedad de los problemas (0 a 2) | Mayor peso entre etapas: D = 2, P = 1, "no se entiende" o "sí, me costó" = 2, "más o menos" o "un poco" = 1 |
| Discordancia | Dijeron entender y no entendieron | "Sí, se entiende" con parafraseo P o D |
| IP por etapa | Dónde está el problema | Participantes con señal en esa etapa / participantes |
| Índices de recuperación y de juicio | Recuerdos y razones adecuados | Códigos A / códigos evaluables de la etapa |
| Etapa principal | Qué cambiar en la frase | Etapa con el mayor IP |
| Decisión | Mantener, revisar o reformular | IP ≥ .50 reformular; IP ≥ .30 revisar |

## Referencias

- Castillo-Díaz, M., y Padilla, J.-L. (2013). How cognitive interviewing can provide validity evidence of the response processes to scale items. *Social Indicators Research, 114*(3), 963-975. https://doi.org/10.1007/s11205-012-0184-8
- Padilla, J.-L., y Benítez, I. (2014). Validity evidence based on response processes. *Psicothema, 26*(1), 136-144. https://doi.org/10.7334/psicothema2013.259
- Tourangeau, R., Rips, L. J., y Rasinski, K. (2000). *The psychology of survey response*. Cambridge University Press. https://doi.org/10.1017/CBO9780511819322
- Willis, G. B. (2005). *Cognitive interviewing: A tool for improving questionnaire design*. SAGE. https://doi.org/10.4135/9781412983655
- Zumbo, B. D., y Hubley, A. M. (Eds.). (2017). *Understanding and investigating response processes in validation research*. Springer. https://doi.org/10.1007/978-3-319-56129-5
