# InsideProcess 0.1.0

* First release.
* Web form for self-administered cognitive interviews: `rp_form()`,
  `rp_apps_script()` (Google Apps Script web application bound to a
  spreadsheet) and `rp_google_form()` (native Google Forms questionnaire),
  with probes for the four stages of the survey response model.
* Reading and coding: `rp_read()`, `rp_read_google_form()`,
  `rp_coding_template()`, `rp_code()`, `rp_scheme()` and `rp_agreement()`.
  The open answers of comprehension (paraphrase), retrieval (what came to
  mind) and judgment (why that answer) are coded with the same letters
  (`A`, `P`, `D`, `E`), each with a meaning of its own in its stage, and
  agreement between coders is computed for each stage.
* Item metrics and decision: `rp_metrics()` (signals, severity and problem
  index across the stages analysed, with the problem in each stage and the
  main stage of each item), `rp_wilson()`, `rp_summary()`,
  `rp_sensitivity()`, `rp_quality()`, `rp_dimension()`, `rp_stages()`
  (problems by stage with Wilson intervals), `rp_options()` (use of the
  answer options) and `rp_readings()`.
* Tables and figures: `rp_table()`, `rp_word()`, `rp_plot_clarity()`,
  `rp_plot_map()` (codes of any coded stage), `rp_plot_dimension()`,
  `rp_plot_problem()` and `rp_plot_stages()` (items by stage).
