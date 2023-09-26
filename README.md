# TSD nettskjema Visualization

![plot](./image.png)

A shiny app that allows you to summarize UiO-Nettskjema data (csv-files), or other csv-files of similar format.
The 'base' software lets you visualize up to two variables together, with one grouping variable,
with different options depending on the nature of the variable (e.g. if continuous or categorical).
The resulting plot dimensions can be adjusted with sliders, and saved in a variety of image formats.

## Getting started

Install the package from GitHub

```r
devtools::install_github("LAMaglan/UiONettskjemaViz")
```

Load the package

```r
library(UiONettskjemaViz)
```

Run the Shiny app

```r
UiONettskjemaViz::run_app()
```
