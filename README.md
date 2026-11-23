# sfdepmap

Moran, LISA and Getis-Ord maps for the GEOG-364 spatial statistics course.

## Install

```r
install.packages("sfdepmap",
                 repos = c("https://YOUR-GITHUB-NAME.r-universe.dev",
                           "https://cloud.r-project.org"))
```

## Use

```r
library(sf)
library(sfdep)
library(sfdepmap)

# Make your neighbours and weights as normal
NeighbourMatrix <- st_contiguity(Census.sf, queen = FALSE)
WeightsMatrix   <- st_weights(NeighbourMatrix, style = "W", allow_zero = TRUE)

# Then every function is given the same things
lisa_map(data       = Census.sf,
         variable   = "percent_income_gt75",
         label      = "GEOID",
         neighbours = NeighbourMatrix,
         weights    = WeightsMatrix,
         title      = "Percentage of people making >$75K")
```

| Function | What it makes |
|---|---|
| `moran_scatterplot()` | Moran's scatterplot |
| `lisa_scatterplot()` | Moran's scatterplot with points coloured by LISA group |
| `lisa_map()` | LISA map (Local Moran's I) |
| `getisstar_map()` | Getis-Ord Gi* hot spot map (each area counted with its neighbours) |
| `getis_map()` | Getis-Ord Gi hot spot map (each area's neighbours only) |
| `autocorrelation_summary()` | One figure with four maps: your data, LISA, Gi* and Gi |

Type `?lisa_map` (or any other function name) to see all the options.

In R Markdown or Quarto, these chunk options give the maps enough room:

- one map: `{r, fig.width=7, fig.height=6}`
- the four-map figure: `{r, fig.width=9, fig.height=11}`

A full worked example (downloading census data, cleaning it, and making every
plot) is installed with the package. To find it, run:

```r
system.file("examples", "SLR_Autocorrelation_example.R", package = "sfdepmap")
```
