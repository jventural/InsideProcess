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
`retrieval`, `answer` y `judgment`, y codifique la etapa de cada problema.

## Funciones

| Para | Función |
|---|---|
| Crear el formulario web | `rp_form()` |
| Publicarlo con Google (hoja de respuestas) | `rp_apps_script()` |
| Crear un Google Form nativo | `rp_google_form()` |
| Leer las respuestas | `rp_read()`, `rp_read_google_form()` |
| Codificar parafraseos (A, P, D, E) | `rp_coding_template()`, `rp_code()`, `rp_scheme()` |
| Acuerdo entre codificadores | `rp_agreement()` (kappa y AC1) |
| Métricas y decisión por frase | `rp_metrics()`, `rp_wilson()` |
| Resumen para redactar | `rp_summary()` |
| Sensibilidad sin participantes dudosos | `rp_sensitivity()`, `rp_quality()` |
| Por dimensión y por etapa | `rp_dimension()`, `rp_stages()` |
| Lecturas detrás de cada problema | `rp_readings()` |
| Tablas | `rp_table()` (`gt = TRUE` para tabla con colores), `rp_word()` |
| Figuras | `rp_plot_clarity()`, `rp_plot_map()`, `rp_plot_dimension()`, `rp_plot_problem()` |

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
```

## Métricas

| Métrica | Qué mide | Cómo se calcula |
|---|---|---|
| IC, índice de comprensión | Proporción que entendió la frase como se quiso | Parafraseos A / parafraseos evaluables (sin ecos) |
| IP, índice de problema | Proporción con alguna señal | Participantes con señal / participantes |
| IC 95 % del IP | Cuánto podría variar el IP en otra muestra | Intervalo de Wilson |
| Severidad | Gravedad de los problemas (0 a 2) | Mayor de: D = 2, P = 1, "no se entiende" = 2, "más o menos" = 1 |
| Discordancia | Dijeron entender y no entendieron | "Sí, se entiende" con parafraseo P o D |
| Decisión | Mantener, revisar o reformular | IP ≥ .50 reformular; IP ≥ .30 revisar |

## Referencias

- Castillo-Díaz, M., y Padilla, J.-L. (2013). How cognitive interviewing can provide validity evidence of the response processes to scale items. *Social Indicators Research, 114*(3), 963-975. https://doi.org/10.1007/s11205-012-0184-8
- Padilla, J.-L., y Benítez, I. (2014). Validity evidence based on response processes. *Psicothema, 26*(1), 136-144. https://doi.org/10.7334/psicothema2013.259
- Tourangeau, R., Rips, L. J., y Rasinski, K. (2000). *The psychology of survey response*. Cambridge University Press. https://doi.org/10.1017/CBO9780511819322
- Willis, G. B. (2005). *Cognitive interviewing: A tool for improving questionnaire design*. SAGE. https://doi.org/10.4135/9781412983655
- Zumbo, B. D., y Hubley, A. M. (Eds.). (2017). *Understanding and investigating response processes in validation research*. Springer. https://doi.org/10.1007/978-3-319-56129-5
