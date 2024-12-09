## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggfla)

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(mpg, wt)) +
    geom_point()

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    theme_axis_shortArrow()

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    theme(
        axis.line = element_line_fixlen(
          length = unit(15, "mm"),
          arrow = arrow(type = "closed", angle = 20, length = unit(0.1, "in"))
        )
    )

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(hjust = 0),
        axis.line = element_line_fixlen(
          length = unit(15, "mm"),
          arrow = arrow(type = "closed", angle = 20, length = unit(0.1, "in"))
        )
    )

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    theme(
        # Everything else for the tidy panel
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(hjust = 0),
        # Now comes to the short arrow setting
        axis.line.x = element_line_fixlen(  # We can separately set x and y axis
            colour = "blue",                # Color of the whole arrow
            length = unit(1, "in"),         # Length of the axis arrow
            linewidth = 2,                  # Width of the arrow line, in "mm"
            linetype = 3,                   # Line type of the arrow line, 3 for dotted
            arrow = FALSE,                  # No arrow head, just a line
            lineend = "round"               # Line end style, "round", "butt" or "square"
        ),
        axis.line.y = element_line_fixlen(
            colour = "red",
            length = unit(2, "cm"),
            linewidth = 0.5,
            linetype = 1,                   # 1 for solid,
           arrow = arrow(                   # `arrow()` function wraps the arrow head setting
                angle = 20,                 # Angle of the arrow head
                length = unit(0.1, "in"),   # Length of the arrow head, with unit
                type = "closed"             # "closed" for closed head arrow
            )
        )
    )

