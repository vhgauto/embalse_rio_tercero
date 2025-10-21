informe <- function(MES, AÑO) {
  MES <- if (MES <= 9) paste0("0", MES) else MES

  fecha_actual <- as.Date(paste0(AÑO, "-", MES, "-01"))
  mes_etq <- stringr::str_to_sentence(format(fecha_actual, "%B"))

  q <- readLines("informe_docx_plantilla.qmd")
  q <- gsub("X_MES_INT", MES, q)
  q <- gsub("X_AÑO", AÑO, q)
  q <- gsub("X_MES_CHR", mes_etq, q)

  writeLines(q, paste0(getwd(), "/informe_docx_", AÑO, "_", MES, ".qmd"))

  quarto::quarto_render(paste0(
    getwd(),
    "/informe_docx_",
    AÑO,
    "_",
    MES,
    ".qmd"
  ))
}

txt1 <- "Ejecutar "
txt2 <- "informe(MES, AÑO) "
txt3 <- "para crear archivos .docx"

txtn <- nchar(paste0(txt1, txt2, txt3))
esp <- 2

chr <- "×"

cat(
  crayon::green$bgBlack$bold(stringr::str_flatten(rep(chr, txtn + esp * 2))),
  "\n",
  crayon::green$bgBlack$bold(paste0(
    chr,
    stringr::str_flatten(rep(" ", txtn + esp)),
    chr
  )),
  "\n",
  crayon::green$bgBlack$bold(paste0(
    chr,
    stringr::str_flatten(rep(" ", esp - 1))
  )),
  crayon::yellow$bgBlack$bold(txt1),
  crayon::white$bgBlack$bold(txt2),
  crayon::yellow$bgBlack$bold(txt3),
  crayon::green$bgBlack$bold(paste0(
    stringr::str_flatten(rep(" ", esp - 1)),
    chr
  )),
  "\n",
  crayon::green$bgBlack$bold(paste0(
    chr,
    stringr::str_flatten(rep(" ", txtn + esp)),
    chr
  )),
  "\n",
  crayon::green$bgBlack$bold(stringr::str_flatten(rep(chr, txtn + esp * 2))),
  sep = ""
)
