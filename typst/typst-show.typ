#show: article.with(
$if(title)$
  title: "$title$",
$endif$

$if(author)$
  author: "$author$",
$endif$

)
