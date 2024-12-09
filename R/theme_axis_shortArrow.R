#' Theme for short-arrow axis like scanpy umap
#' @param axis.length Length of the axis line. Default `15`.
#' @param axis.length.unit Unit of the axis line length. Default `"mm"`.
#' @param arrow.type,arrow.angle,arrow.length Parameters for the arrow. Default
#' `"closed"`, `20`, and `unit(0.1, "in")`. See `?arrow` for more details.
#' @param ... Additional arguments passed to `element_line_fixlen`
#' @return A `theme` object to be added to a plot
#' @export
#' @seealso [element_line_fixlen()]
#' @examples
#' ggplot(mtcars, aes(mpg, wt)) +
#'     geom_point() +
#'     theme_axis_shortArrow()
theme_axis_shortArrow <- function(
        axis.length = 15,
        axis.length.unit = "mm",
        arrow.type = "closed",
        arrow.angle = 20,
        arrow.length = unit(0.1, "in"),
        ...
) {
    theme(
        axis.ticks.x.top = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.x.top = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.text.y.right = element_blank(),
        axis.text.y.left = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x.bottom = element_text(hjust = 0),
        axis.title.y.left = element_text(hjust = 0),
        axis.line = element_line_fixlen(
            length = unit(axis.length, axis.length.unit),
            arrow = arrow(
                type = arrow.type,
                angle = arrow.angle,
                length = arrow.length
            ),
            ...
        )
    )
}

