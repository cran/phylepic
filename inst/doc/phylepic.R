## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
suppressPackageStartupMessages({
  library(phylepic)
  library(ape)
  library(dplyr)
  library(ggplot2)
})

## -----------------------------------------------------------------------------
library(phylepic)
library(ape)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
tree <- read.tree(system.file("enteric.newick", package = "phylepic"))
plot(tree)

## -----------------------------------------------------------------------------
metadata <- read.csv(system.file("enteric_metadata.csv", package = "phylepic"))
str(metadata)

## -----------------------------------------------------------------------------
metadata <-
  metadata |>
  mutate(
    across(c(source, cluster), factor),
    collection_date = as.Date(collection_date)
  )

## -----------------------------------------------------------------------------
clade.parent <- ape::getMRCA(tree, c("NSW-0324", "NSW-0330"))
clade <- ape::extract.clade(tree, clade.parent)
plot(clade)

## -----------------------------------------------------------------------------
phylepic(clade, metadata, name, collection_date) |> plot()

## ----fig.width=11, fig.height=9-----------------------------------------------
phydata <- phylepic(tree, metadata, name, collection_date)

plot(phydata)

## -----------------------------------------------------------------------------
options(phylepic.week_start = "Monday")

## -----------------------------------------------------------------------------
date_scale <- scale_x_week(
  name = "Date of sample collection",
  limits = as.Date(c("2023-08-14", "2023-11-15")),
  date_labels = "%d %b",
  week_breaks = 4L,
  week_minor_breaks = 2L
)

## ----fig.width=11, fig.height=9-----------------------------------------------
plot(phydata, scale.date = date_scale, height.tree = 6)

## -----------------------------------------------------------------------------
cluster_scale <- scale_colour_brewer(
  # the name affects the legend title
  name = "Cluster",
  # these 3 parameters affect the colour choice
  type = "qual",
  palette = 2,
  direction = -1,
  # don't drop unused levels; we want consistency between panels
  drop = FALSE,
  # suppress the explicit NA entry in the legend; not all tips are in a cluster
  na.translate = FALSE,
  # we'll use this scale later for both fill and colour aesthetics
  aesthetics = c("fill", "colour"),
)

## -----------------------------------------------------------------------------
plot_tree(phydata) +
  # `filter = leaf` in ggraph geoms means that they only draw the tips
  ggraph::geom_node_point(aes(filter = leaf, colour = cluster), size = 2, show.legend = FALSE) +
  cluster_scale

## ----fig.width=11, fig.height=9-----------------------------------------------
plot(
  phydata,
  plot.tree = function(x) {
    # this function will be called with x = phydata
    plot_tree(x) +
      ggraph::geom_node_point(aes(filter = leaf, colour = cluster), size = 2, show.legend = FALSE) +
      cluster_scale
  },
  plot.bars = function(x) {
    plot_bars(
      x,
      # 'source' is the name of the corresponding metadata column
      source = scale_fill_hue(
        name = "Source",
        # this just changes the colours
        h.start = 30,
        # as above, we want to turn off drop and na.translate
        drop = FALSE,
        na.translate = FALSE
      ),
      # if we wanted more tile columns, we would add them here
    )
  },
  scale.date = date_scale,
  width.tree = 20,  # new: also specify the relative widths of the 4 columns:
  width.date = 12,  #
  width.legend = 4, #
  height.tree = 6
)

## ----eval=FALSE---------------------------------------------------------------
#  plot(
#    phydata,
#    plot.bars = function(x) {
#      plot_bars(
#        x,
#        source = scale_fill_hue(...)
#      )
#    }
#  )
#  
#  # equivalent to the above
#  plot(
#    phydata,
#    plot.bars = plot_bars(
#      source = scale_fill_hue(...),
#    )
#  )

## ----fig.width=11, fig.height=9-----------------------------------------------
plot(
  phydata,
  plot.tree = function(x) {
    plot_tree(x) +
      ggraph::geom_node_point(aes(filter = leaf, colour = cluster), size = 2, show.legend = FALSE) +
      cluster_scale
  },
  plot.bars = plot_bars(
    source = scale_fill_hue(
      name = "Source", h.start = 30, drop = FALSE, na.translate = FALSE
    ),
  ),
  plot.epicurve = plot_epicurve(fill = cluster),
  plot.calendar = plot_calendar(
    fill = cluster,
    labels = "%d",
  ),
  scale.date = date_scale,
  scale.fill = cluster_scale, # new: pass the scale to both panels
  width.tree = 20,
  width.date = 12,
  width.legend = 4,
  height.tree = 6
)

