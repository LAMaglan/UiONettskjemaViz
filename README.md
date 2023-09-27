# UiO nettskjema Visualization

![image](https://github.com/LAMaglan/UiONettskjemaViz/assets/29206211/cd0d2fd1-29fe-4107-bcdb-0ca8d4288b55)

A shiny app that allows you to summarize [UiO-Nettskjema](https://www.uio.no/english/services/it/adm-services/nettskjema/) data (csv-files), or other csv-files of similar format.
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
