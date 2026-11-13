// Written by rp_apps_script() of the R package InsideProcess.
// Goes inside the Google Sheet that receives the answers (Extensions > Apps Script).
// doGet() serves the form (file Index.html of the project); guardar() writes each participation in two tabs:
//   __SHEET_PART__: one row per participant (profile, times, final comment).
//   __SHEET_RESP__: one row per participant and item (probes and seconds spent on the item).

const HOJA_PART = "__SHEET_PART__";
const HOJA_RESP = "__SHEET_RESP__";
const COL_PERFIL = __COL_PROFILE__;
const COL_RESP = __COL_RESP__;
const COL_FIN = __COL_END__;
const CAMPO_CLARIDAD = "__CLARITY_FIELD__";
const CLARO = "__CLEAR__";
const CAMPO_SEGUNDOS = "__SECONDS_FIELD__";

function doGet() {
  return HtmlService.createHtmlOutputFromFile('Index')
    .setTitle('__TITLE__')
    .addMetaTag('viewport', 'width=device-width, initial-scale=1');
}

function hoja_(nombre, encabezado) {
  const libro = SpreadsheetApp.getActiveSpreadsheet();
  const h = libro.getSheetByName(nombre) || libro.insertSheet(nombre);
  if (h.getLastRow() === 0) {
    h.appendRow(encabezado);
    h.getRange(1, 1, 1, encabezado.length).setFontWeight('bold');
    h.setFrozenRows(1);
  }
  return h;
}

function guardar(d) {
  const candado = LockService.getScriptLock();
  candado.waitLock(30000);
  try {
    const recibido = new Date();
    const p = hoja_(HOJA_PART, ['id', 'recibido', 'inicio', 'fin'].concat(COL_PERFIL, COL_FIN));
    // A re-send by the same participant (for example, after a lost connection) is not duplicated.
    const ids = p.getLastRow() > 1 ? p.getRange(2, 1, p.getLastRow() - 1, 1).getValues().flat() : [];
    if (ids.indexOf(d.id) >= 0) return 'duplicado';
    const r = d.respuestas;
    const segundos = r.reduce((s, x) => s + (Number(x[CAMPO_SEGUNDOS]) || 0), 0);
    const noClaras = CAMPO_CLARIDAD ? r.filter(x => x[CAMPO_CLARIDAD] !== CLARO).length : '';
    p.appendRow([d.id, recibido, d.inicio, d.fin]
      .concat(COL_PERFIL.map(k => d.perfil[k] ?? ''))
      .concat([r.length, noClaras, Math.round(segundos / 6) / 10, d.comentario || '']));
    const h = hoja_(HOJA_RESP, ['id'].concat(COL_RESP));
    const filas = r.map(x => [d.id].concat(COL_RESP.map(k => x[k] ?? '')));
    h.getRange(h.getLastRow() + 1, 1, filas.length, filas[0].length).setValues(filas);
    return 'ok';
  } finally {
    candado.releaseLock();
  }
}
