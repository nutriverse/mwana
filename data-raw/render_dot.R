# Render workflow.qmd to get graph viz figure ----------------------------------

quarto::quarto_render("data-raw/workflow.qmd")

file.copy(
  from = "data-raw/workflow_files/figure-commonmark/dot-figure-1.png",
  to = "man/figures/workflow.png"
)


