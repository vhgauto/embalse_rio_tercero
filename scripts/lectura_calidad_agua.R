protocolos <- list.files(
  paste0("datos/calidad_agua_", año_actual, "_", mes_actual_chr),
  pattern = "protocolo",
  full.names = TRUE
)

sitio9 <- protocolos[str_detect(protocolos, "9")]
sitio11 <- protocolos[str_detect(protocolos, "11")]

# d9 <- f_pdf(sitio9)
# d11 <- f_pdf(sitio11)
