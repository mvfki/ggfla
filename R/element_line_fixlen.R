#' @title Line element with fixed length setting
#' @export
#' @description
#' Used at the place in ggplot theme `axis.line*`
#' @param colour,color Line/border colour. Color is an alias for colour.
#' @param length a [grid::unit()] object to specify length of the line.
#' @param linewidth Line/border size in mm.
#' @param linetype Line type. An integer (0:8), a name (blank, solid,
#'    dashed, dotted, dotdash, longdash, twodash), or a string with
#'    an even number (up to eight) of hexadecimal digits which give the
#'    lengths in consecutive positions in the string.
#' @param lineend Line end Line end style (round, butt, square)
#' @param arrow Arrow specification, as created by [grid::arrow()]
#' @param inherit.blank Should this element inherit the existence of an
#'   `element_blank` among its parents? If `TRUE` the existence of
#'   a blank element among its parents will cause this element to be blank as
#'   well. If `FALSE` any blank parent element will be ignored when
#'   calculating final element state.
#' @return An S3 object of class `element_line_fixlen`, `element_line`, and `element`.
#' @examples
#' ggplot(mtcars, aes(mpg, wt)) +
#'     geom_point() +
#'     theme(
#'         axis.line = element_line_fixlen(length = unit(1, "cm")),
#'         axis.ticks = element_blank(),
#'         axis.text = element_blank(),
#'         axis.title = element_text(hjust = 0)
#'     )
element_line_fixlen <- function(colour = NULL, length = NULL, linewidth = NULL, linetype = NULL,
                               lineend = NULL, color = NULL, arrow = NULL,
                               inherit.blank = FALSE) {
    colour <- color %||% colour %||% "black"
    arrow <- arrow %||% FALSE
    length <- length %||% unit(1, "npc")

    structure(
        list(colour = colour, length = length, linewidth = linewidth,
             linetype = linetype, lineend = lineend, arrow = arrow,
             inherit.blank = inherit.blank),
        class = c("element_line_fixlen", "element_line", "element")
    )
}


#' @exportS3Method ggplot2::element_grob
#' @method element_grob element_line_fixlen
#' @export
element_grob.element_line_fixlen <- function(
        element,
        x = 0:1,
        y = 0:1,
        colour = NULL,
        length = NULL,
        linewidth = NULL,
        linetype = NULL,
        lineend = NULL,
        default.units = "npc",
        id.lengths = NULL,
        ...
) {
    gp <- gpar(col = colour, fill = colour,
               lwd = len0_null(linewidth * .pt), lty = linetype,
               lineend = lineend)
    element_gp <- gpar(col = element$colour, fill = element$colour,
                       lwd = len0_null(element$linewidth * .pt),
                       lty = element$linetype, lineend = element$lineend)
    arrow <- if (is.logical(element$arrow) && !element$arrow) {
        NULL
    } else {
        element$arrow
    }

    length <- length %||% element$length %||% unit(1, "npc")
    if (
        !identical(x[1], x[2]) && identical(y[1], y[2])
    ) {
        # horizontal
        x[2] <- convertWidth(length, "cm")
    } else if (
        identical(x[1], x[2]) && !identical(y[1], y[2])
    ) {
        # vertical
        y[2] <- convertHeight(length, "cm")
    } else {
        cli::cli_abort("{.field element_line_fixlen} can only be used for axis.line* for now.")
    }

    polylineGrob(
        x, y, default.units = default.units, gp = modify_list(element_gp, gp),
        id.lengths = id.lengths, arrow = arrow, ...
    )
}

# Helper functions copied from ggplot2
len0_null <- function(x) {
    if (length(x) == 0)
        NULL
    else x
}

modify_list <- function(old, new) {
    for (i in names(new)) old[[i]] <- new[[i]]
    old
}
