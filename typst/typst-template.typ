#let article(
  title: "algún título",
  author: "algún autor",
  paper: "a4",
  body,
) = {
  set document(title: title, author: author)

  set text(font: "Lato")

  set page(
    paper: paper,
    margin: (bottom: 1cm, top: 6cm, left: 2cm, right: 2cm),
    numbering: "1 de 1",
    header: grid(
      columns: (1fr, 1fr, 1fr),
      gutter: 1pt,
      align: (center + horizon),
      [#figure(
        image("img/conae.png", width: 90%)
      )],
      [#text(size: 1em)[
        *2025 - "AÑO DE LA RECONSTRUCCIÓN DE LA NACIÓN ARGENTINA"* 
      ]],
      [#figure(
        image("img/unrc.png", width: 90%)
      )]
    )
  )

  page(align(left + top)[
    #body
  ])
}
