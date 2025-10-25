fechas_v <- as.Date(c(paste0(2025, "-", 1:9, "-01")))

purrr::walk(
  fechas_v[9],
  \(x) {
    informe_pdf <- paste0(format(x, "%Y-%m"), ".pdf")
    quarto::quarto_render(
      input = "template_informe.qmd",
      output_file = informe_pdf,
      execute_params = list(
        fecha = x
      )
    )
    file.copy(
      from = paste0("docs/", informe_pdf),
      to = paste0("informes/", informe_pdf)
    )
    unlink(paste0("docs/", informe_pdf))
  }
)
