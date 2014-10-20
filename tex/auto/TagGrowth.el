(TeX-add-style-hook "TagGrowth"
 (lambda ()
    (LaTeX-add-labels
     "fig:1")
    (TeX-run-style-hooks
     "natbib"
     "sort"
     "hyperref"
     "colorlinks=true"
     "geometry"
     "margin=2.54cm"
     "parskip"
     "tabularx"
     "graphicx"
     "epsfig"
     "amsmath"
     "amssymb"
     "amsfonts"
     ""
     "latex2e"
     "art11"
     "article"
     "a4paper"
     "11pt")))

