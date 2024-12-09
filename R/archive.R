# README ####

# This file has no working definition
# Everything comes from developmental efforts.
# Just keeping them here for now in case of future use.


# Originally in aaa.R ####

# .onLoad <- function(libname, pkgname) {
#     register_theme_elements(
#         axis.shortArrow = element_shortArrow(colour = "black", length = unit(1, "in"), linewidth = 0.5, linetype = 1, lineend = NULL, arrow = arrow(), arrow.fill = "black"),
#         axis.shortArrow.x.top = element_blank(),
#         axis.shortArrow.y.right = element_blank(),
#         element_tree = list(
#             shortArrow = el_def("element_shortArrow", NULL),
#             axis.shortArrow = el_def("element_shortArrow", "shortArrow"),
#             axis.shortArrow.x = el_def("element_shortArrow", "axis.shortArrow"),
#             axis.shortArrow.y = el_def("element_shortArrow", "axis.shortArrow"),
#             axis.shortArrow.x.bottom = el_def("element_shortArrow", "axis.shortArrow.x"),
#             axis.shortArrow.x.top = el_def("element_shortArrow", "axis.shortArrow.x"),
#             axis.shortArrow.y.left = el_def("element_shortArrow", "axis.shortArrow.y"),
#             axis.shortArrow.y.right = el_def("element_shortArrow", "axis.shortArrow.y")
#         )
#     )
# }


# Originally in utils.R ####

# data_frame0 <- function (...) {
#     vctrs::data_frame(..., .name_repair = "minimal")
# }
#
# empty <- function (df) {
#     is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is.waive(df)
# }
#
# is.waive <- function (x) inherits(x, "waiver")
#
# .trbl <- c("top", "right", "bottom", "left")
#
# height_cm <- function (x) {
#     if (is.grob(x)) {
#         convertHeight(grobHeight(x), "cm", TRUE)
#     }
#     else if (is.unit(x)) {
#         convertHeight(x, "cm", TRUE)
#     }
#     else if (is.list(x)) {
#         vapply(x, height_cm, numeric(1))
#     }
#     else {
#         cli::cli_abort("Don't know how to get height of {.cls {class(x)}} object")
#     }
# }
#
# width_cm <- function (x) {
#     if (is.grob(x)) {
#         convertWidth(grobWidth(x), "cm", TRUE)
#     }
#     else if (is.unit(x)) {
#         convertWidth(x, "cm", TRUE)
#     }
#     else if (is.list(x)) {
#         vapply(x, width_cm, numeric(1))
#     }
#     else {
#         cli::cli_abort("Don't know how to get width of {.cls {class(x)}} object")
#     }
# }
#
# opposite_position <- function (position) {
#     switch(position, top = "bottom", bottom = "top", left = "right",
#            right = "left")
# }
#
# IS_NUMBER_oob <- 2
#
# oxford_comma <- function (chr, sep = ", ", final = "or") {
#     n <- length(chr)
#     if (n < 2) {
#         return(chr)
#     }
#     head <- chr[seq_len(n - 1)]
#     last <- chr[n]
#     head <- paste(head, collapse = sep)
#     if (n > 2) {
#         paste0(head, sep, final, " ", last)
#     }
#     else {
#         paste0(head, " ", final, " ", last)
#     }
# }
#
# stop_input_type <- function (x, what, ..., allow_na = FALSE, allow_null = FALSE,
#                              show_value = TRUE, arg = caller_arg(x), call = caller_env()) {
#     cli <- env_get_list(nms = c("format_arg", "format_code"),
#                                last = topenv(), default = function(x) sprintf("`%s`",
#                                                                               x), inherit = TRUE)
#     if (allow_na) {
#         what <- c(what, cli$format_code("NA"))
#     }
#     if (allow_null) {
#         what <- c(what, cli$format_code("NULL"))
#     }
#     if (length(what)) {
#         what <- oxford_comma(what)
#     }
#     message <- sprintf("%s must be %s, not %s.", cli$format_arg(arg),
#                        what, obj_type_friendly(x, value = show_value))
#     abort(message, ..., call = call, arg = arg)
# }
#
# .stop_not_number <- function (x, ..., exit_code, allow_decimal, min, max, allow_na,
#                               allow_null, arg, call) {
#     if (allow_decimal) {
#         what <- "a number"
#     }
#     else {
#         what <- "a whole number"
#     }
#     if (exit_code == IS_NUMBER_oob) {
#         min <- min %||% -Inf
#         max <- max %||% Inf
#         if (min > -Inf && max < Inf) {
#             what <- sprintf("%s between %s and %s", what, min,
#                             max)
#         }
#         else if (x < min) {
#             what <- sprintf("%s larger than or equal to %s",
#                             what, min)
#         }
#         else if (x > max) {
#             what <- sprintf("%s smaller than or equal to %s",
#                             what, max)
#         }
#         else {
#             abort("Unexpected state in OOB check", .internal = TRUE)
#         }
#     }
#     stop_input_type(x, what, ..., allow_na = allow_na, allow_null = allow_null,
#                     arg = arg, call = call)
# }
#
# check_number_decimal <- function(
        #         x, ..., min = NULL, max = NULL, allow_infinite = TRUE,
#         allow_na = FALSE, allow_null = FALSE, arg = caller_arg(x),
#         call = caller_env())
# {
#     if (missing(x)) {
#         exit_code <- IS_NUMBER_false
#     }
#     else if (0 == (exit_code <- .standalone_types_check_dot_call(ffi_standalone_check_number_1.0.7,
#                                                                  x, allow_decimal = TRUE, min, max, allow_infinite, allow_na,
#                                                                  allow_null))) {
#         return(invisible(NULL))
#     }
#     .stop_not_number(x, ..., exit_code = exit_code, allow_decimal = TRUE,
#                      min = min, max = max, allow_na = allow_na, allow_null = allow_null,
#                      arg = arg, call = call)
# }
#
# label_angle_heuristic <- function(element, position, angle) {
#     if (!inherits(element, "element_text")
#         || is.null(position)
#         || is.null(angle %|W|% NULL)) {
#         return(element)
#     }
#     arg_match0(position, .trbl)
#
#     check_number_decimal(angle)
#     radian <- deg2rad(angle)
#     digits <- 3
#
#     # Taking the sign of the (co)sine snaps the value to c(-1, 0, 1)
#     # Doing `x / 2 + 0.5` rescales it to c(0, 0.5, 1), which are good values for justification
#     # The rounding step ensures we can get (co)sine to exact 0 so it can become 0.5
#     # which we need for center-justifications
#     cosine <- sign(round(cos(radian), digits)) / 2 + 0.5
#     sine   <- sign(round(sin(radian), digits)) / 2 + 0.5
#
#     # Depending on position, we might need to swap or flip justification values
#     hjust <- switch(position, left = cosine, right = 1 - cosine, top = 1 - sine, sine)
#     vjust <- switch(position, left = 1 - sine, right = sine, top = 1 - cosine, cosine)
#
#     element$angle <- angle %||% element$angle
#     element$hjust <- hjust %||% element$hjust
#     element$vjust <- vjust %||% element$vjust
#     element
# }
#
# is.zero <- function(x) is.null(x) || inherits(x, "zeroGrob")
#
# waiver <- function() structure(list(), class = "waiver")
#
# absoluteGrob <- function (grob, width = NULL, height = NULL, xmin = NULL, ymin = NULL,
#           vp = NULL) {
#     gTree(children = grob, width = width, height = height, xmin = xmin,
#           ymin = ymin, vp = vp, cl = "absoluteGrob")
# }
#
# rotate_just <- function (angle, hjust, vjust) {
#     angle <- (angle %||% 0)%%360
#     if (is.character(hjust)) {
#         hjust <- match(hjust, c("left", "right")) - 1
#         hjust[is.na(hjust)] <- 0.5
#     }
#     if (is.character(vjust)) {
#         vjust <- match(vjust, c("bottom", "top")) - 1
#         vjust[is.na(vjust)] <- 0.5
#     }
#     size <- vec_size_common(angle, hjust, vjust)
#     angle <- vec_recycle(angle, size)
#     hjust <- vec_recycle(hjust, size)
#     vjust <- vec_recycle(vjust, size)
#     case <- findInterval(angle, c(0, 90, 180, 270, 360))
#     hnew <- hjust
#     vnew <- vjust
#     is_case <- which(case == 2)
#     hnew[is_case] <- 1 - vjust[is_case]
#     vnew[is_case] <- hjust[is_case]
#     is_case <- which(case == 3)
#     hnew[is_case] <- 1 - hjust[is_case]
#     vnew[is_case] <- 1 - vjust[is_case]
#     is_case <- which(case == 4)
#     hnew[is_case] <- vjust[is_case]
#     vnew[is_case] <- 1 - hjust[is_case]
#     list(hjust = hnew, vjust = vnew)
# }
#
# `%|W|%` <- function (a, b) if (!is.waive(a)) a else b
#
# validate_labels <- function (labels) {
#     if (!is.list(labels)) {
#         return(labels)
#     }
#     if (any(vapply(labels, is.language, logical(1)))) {
#         do.call(expression, labels)
#     }
#     else {
#         unlist(labels)
#     }
# }
#
# axis_label_priority_between <- function (x, y) {
#     n <- y - x + 1
#     if (n <= 2) {
#         return(numeric(0))
#     }
#     mid <- x - 1 + (n + 1)%/%2
#     c(mid, axis_label_priority_between(x, mid), axis_label_priority_between(mid,
#                                                                             y))
# }
#
# axis_label_priority <- function (n) {
#     if (n <= 0) {
#         return(numeric(0))
#     }
#     c(1, n, axis_label_priority_between(1, n))
# }
#
# draw_axis_labels <- function(break_positions, break_labels, label_element, is_vertical,
#           check.overlap = FALSE) {
#     position_dim <- if (is_vertical)
#         "y"
#     else "x"
#     label_margin_name <- if (is_vertical)
#         "margin_x"
#     else "margin_y"
#     n_breaks <- length(break_positions)
#     break_positions <- unit(break_positions, "native")
#     if (check.overlap) {
#         priority <- axis_label_priority(n_breaks)
#         break_labels <- break_labels[priority]
#         break_positions <- break_positions[priority]
#     }
#     labels_grob <- exec(element_grob, label_element, `:=`(!!position_dim,
#                                                           break_positions), `:=`(!!label_margin_name, TRUE), label = break_labels,
#                         check.overlap = check.overlap)
# }
#
# len0_null <- function(x) {
#     if (length(x) == 0)
#         NULL
#     else x
# }
#
# modify_list <- function(old, new) {
#     for (i in names(new)) old[[i]] <- new[[i]]
#     old
# }

# Originally in guide_axis_shortArrow.R ####

# GuideAxisShortArrow <- ggproto(
#     "GuideAxisShortArrow", GuideAxis,
#     # params = list(
#     #     title     = waiver(),
#     #     theme     = NULL,
#     #     name      = "axis",
#     #     hash      = character(),
#     #     position  = waiver(),
#     #     direction = NULL,
#     #     angle     = NULL,
#     #     n.dodge   = 1,
#     #     minor.ticks = FALSE,
#     #     cap       = "none",
#     #     order     = 0,
#     #     check.overlap = FALSE
#     # ),
#     #
#     # available_aes = c("x", "y"),
#     #
#     # hashables = exprs(title, key$.value, key$.label, name),
#     #
#     elements = list(
#         shortArrow = "axis.shortArrow",
#         text  = "axis.text",
#         ticks = "axis.ticks",
#         minor = "axis.minor.ticks",
#         major_length = "axis.ticks.length",
#         minor_length = "axis.minor.ticks.length"
#     ),
#     #
#     # extract_key = function(scale, aesthetic, minor.ticks = FALSE, ...) {
#     #     major <- Guide$extract_key(scale, aesthetic, ...)
#     #     if (is.null(major) && is.null(scale$scale$get_breaks())) {
#     #         major <- data_frame0()
#     #     }
#     #     if (!minor.ticks) {
#     #         return(major)
#     #     }
#     #
#     #     minor_breaks <- scale$get_breaks_minor()
#     #     minor_breaks <- setdiff(minor_breaks, major$.value)
#     #     minor_breaks <- minor_breaks[is.finite(minor_breaks)]
#     #
#     #     if (length(minor_breaks) < 1) {
#     #         return(major)
#     #     }
#     #
#     #     minor <- data_frame0(!!aesthetic := scale$map(minor_breaks))
#     #     minor$.value <- minor_breaks
#     #     minor$.type <- "minor"
#     #
#     #     if (nrow(major) > 0) {
#     #         major$.type <- "major"
#     #         if (!vec_is(minor$.value, major$.value)) {
#     #             # If we have mixed types of values, which may happen in discrete scales,
#     #             # discard minor values in favour of the major values.
#     #             minor$.value <- NULL
#     #         }
#     #         vec_rbind(major, minor)
#     #     } else {
#     #         minor
#     #     }
#     # },
#     #
#     # extract_params = function(scale, params, ...) {
#     #     params$name <- paste0(params$name, "_", params$aesthetic)
#     #     params
#     # },
#     #
#     # extract_decor = function(scale, aesthetic, position, key, cap = "none", ...) {
#     #
#     #     value <- c(-Inf, Inf)
#     #     has_key <- !(is.null(key) || nrow(key) < 1)
#     #     if (cap %in% c("both", "upper") && has_key) {
#     #         value[2] <- max(key[[aesthetic]])
#     #     }
#     #     if (cap %in% c("both", "lower") && has_key) {
#     #         value[1] <- min(key[[aesthetic]])
#     #     }
#     #
#     #     opposite <- setdiff(c("x", "y"), aesthetic)
#     #     opposite_value <- if (position %in% c("top", "right")) -Inf else Inf
#     #
#     #     vctrs::data_frame(
#     #         !!aesthetic := value,
#     #         !!opposite  := opposite_value
#     #     )
#     # },
#     #
#     # transform = function(self, params, coord, panel_params) {
#     #     key <- params$key
#     #     position <- params$position
#     #     check <- FALSE
#     #
#     #     if (!(is.null(position) || nrow(key) == 0)) {
#     #         check <- TRUE
#     #         aesthetics <- names(key)[!grepl("^\\.", names(key))]
#     #         if (!all(c("x", "y") %in% aesthetics)) {
#     #             other_aesthetic <- setdiff(c("x", "y"), aesthetics)
#     #             override_value <- if (position %in% c("bottom", "left")) -Inf else Inf
#     #             key[[other_aesthetic]] <- override_value
#     #         }
#     #         key <- coord$transform(key, panel_params)
#     #         params$key <- key
#     #     }
#     #
#     #     if (!is.null(params$decor)) {
#     #         params$decor <- coord_munch(coord, params$decor, panel_params)
#     #
#     #         if (!coord$is_linear()) {
#     #             # For non-linear coords, we hardcode the opposite position
#     #             params$decor$x <- switch(position, left = 1, right = 0, params$decor$x)
#     #             params$decor$y <- switch(position, top = 0, bottom = 1, params$decor$y)
#     #         }
#     #     }
#     #
#     #     if (!check) {
#     #         return(params)
#     #     }
#     #
#     #     # Ported over from `warn_for_position_guide`
#     #     # This is trying to catch when a user specifies a position perpendicular
#     #     # to the direction of the axis (e.g., a "y" axis on "top").
#     #     # The strategy is to check that two or more unique breaks are mapped
#     #     # to the same value along the axis.
#     #     breaks_are_unique <- !duplicated(key$.value)
#     #     if (empty(key) || sum(breaks_are_unique) == 1) {
#     #         return(params)
#     #     }
#     #
#     #     if (position %in% c("top", "bottom")) {
#     #         position_aes <- "x"
#     #     } else if (position %in% c("left", "right")) {
#     #         position_aes <- "y"
#     #     } else {
#     #         return(params)
#     #     }
#     #
#     #     if (length(unique(key[[position_aes]][breaks_are_unique])) == 1) {
#     #         cli::cli_warn(c(
#     #             "Position guide is perpendicular to the intended axis.",
#     #             "i" = "Did you mean to specify a different guide {.arg position}?"
#     #         ))
#     #     }
#     #
#     #     return(params)
#     # },
#     #
#     # merge = function(self, params, new_guide, new_params) {
#     #     if (!inherits(new_guide, "GuideNone")) {
#     #         cli::cli_warn(c(
#     #             "{.fn {snake_class(self)}}: Discarding guide on merge.",
#     #             "i" = "Do you have more than one guide with the same {.arg position}?"
#     #         ))
#     #     }
#     #     return(list(guide = self, params = params))
#     # },
#     #
#     # setup_elements = function(params, elements, theme) {
#     #     is_char <- vapply(elements, is.character, logical(1))
#     #     suffix <- params$theme_suffix %||%
#     #         paste(params$aes, params$position, sep = ".")
#     #     elements[is_char] <- vapply(
#     #         elements[is_char],
#     #         function(x) paste(x, suffix, sep = "."),
#     #         character(1)
#     #     )
#     #     Guide$setup_elements(params, elements, theme)
#     # },
#     #
#     # override_elements = function(params, elements, theme) {
#     #     elements$text <-
#     #         label_angle_heuristic(elements$text, params$position, params$angle)
#     #     if (inherits(elements$ticks, "element_blank")) {
#     #         elements$major_length <- unit(0, "cm")
#     #     }
#     #     if (inherits(elements$minor, "element_blank") || isFALSE(params$minor.ticks)) {
#     #         elements$minor_length <- unit(0, "cm")
#     #     }
#     #     return(elements)
#     # },
#     #
#     # setup_params = function(params) {
#     #     position  <- arg_match0(params$position, .trbl)
#     #     direction <- if (position %in% c("left", "right")) {
#     #         "vertical"
#     #     } else {
#     #         "horizontal"
#     #     }
#     #
#     #     new_params <- c("aes", "orth_aes", "para_sizes", "orth_size", "orth_sizes",
#     #                     "vertical", "measure_gtable", "measure_text")
#     #     if (direction == "vertical") {
#     #         params[new_params] <- list(
#     #             "y", "x", "heights", "width", "widths",
#     #             TRUE, gtable::gtable_width, width_cm
#     #         )
#     #     } else {
#     #         params[new_params] <- list(
#     #             "x", "y", "widths", "height", "heights",
#     #             FALSE, gtable::gtable_height, height_cm
#     #         )
#     #     }
#     #
#     #     new_params <- list(
#     #         opposite  = opposite_position(position),
#     #         secondary = position %in% c("top", "right"),
#     #         lab_first = position %in% c("top", "left"),
#     #         orth_side = if (position %in% c("top", "right")) 0 else 1,
#     #         direction = direction,
#     #         position  = position
#     #     )
#     #     c(params, new_params)
#     # },
#     #
#     # build_title = function(label, elements, params) {
#     #     zeroGrob()
#     # },
#
#     # The decor in the axis guide is the axis line
#     build_decor = function(decor, grobs, elements, params) {
#         if (empty(decor)) {
#             return(zeroGrob())
#         }
#         if (params$position %in% c("top", "bottom")) {
#             x <- unit(decor$x, "npc")
#             x[2] <- convertWidth(elements$shortArrow$length, "cm")
#             y <- unit(decor$y, "npc")
#         } else {
#             x <- unit(decor$x, "npc")
#             y <- unit(decor$y, "npc")
#             y[2] <- convertHeight(elements$shortArrow$length, "cm")
#         }
#
#         element_grob(
#             elements$shortArrow,
#             x = unit(decor$x, "npc"),
#             y = unit(decor$y, "npc")
#         )
#     },
#
#     # build_ticks = function(key, elements, params, position = params$opposite) {
#     #
#     #     major <- Guide$build_ticks(
#     #         vec_slice(key, (key$.type %||% "major") == "major"),
#     #         elements$ticks, params, position,
#     #         elements$major_length
#     #     )
#     #
#     #     if (!params$minor.ticks) {
#     #         return(major)
#     #     }
#     #
#     #     minor <- Guide$build_ticks(
#     #         vec_slice(key, (key$.type %||% "major") == "minor"),
#     #         elements$minor, params, position,
#     #         elements$minor_length
#     #     )
#     #     grobTree(major, minor, name = "ticks")
#     # },
#
#     # build_labels = function(key, elements, params) {
#     #
#     #     if (".type" %in% names(key)) {
#     #         key <- vec_slice(key, key$.type == "major")
#     #     }
#     #
#     #     labels   <- validate_labels(key$.label)
#     #     n_labels <- length(labels)
#     #
#     #     if (n_labels < 1) {
#     #         return(list(zeroGrob()))
#     #     }
#     #
#     #     pos <- key[[params$aes]]
#     #
#     #     dodge_pos     <- rep(seq_len(params$n.dodge %||% 1), length.out = n_labels)
#     #     dodge_indices <- unname(split(seq_len(n_labels), dodge_pos))
#     #
#     #     lapply(dodge_indices, function(indices) {
#     #         draw_axis_labels(
#     #             break_positions = pos[indices],
#     #             break_labels    = labels[indices],
#     #             label_element   = elements$text,
#     #             is_vertical     = params$vertical,
#     #             check.overlap   = params$check.overlap %||% FALSE
#     #         )
#     #     })
#     # },
#
#     # measure_grobs = function(grobs, params, elements) {
#     #
#     #     # Below, we include a spacer measurement. This measurement is used
#     #     # to offset subsequent rows/columns in the gtable in case the tick length is
#     #     # negative. This causes the text to align nicely at panel borders.
#     #     # In case tick length is positive, this will just be a 0-size empty row
#     #     # or column.
#     #
#     #     measure <- params$measure_text
#     #
#     #     # Ticks
#     #     major_cm <- convertUnit(elements$major_length, "cm", valueOnly = TRUE)
#     #     range <- range(0, major_cm)
#     #     if (params$minor.ticks && !inherits(elements$minor, "element_blank")) {
#     #         minor_cm <- convertUnit(elements$minor_length, "cm", valueOnly = TRUE)
#     #         range <- range(range, minor_cm)
#     #     }
#     #
#     #     length <- unit(range[2], "cm")
#     #     spacer <- max(unit(0, "pt"), unit(-1 * diff(range), "cm"))
#     #
#     #     # Text
#     #     labels <- unit(measure(grobs$labels), "cm")
#     #     title  <- unit(measure(grobs$title), "cm")
#     #
#     #     sizes <- grid::unit.c(length, spacer, labels, title)
#     #     if (params$lab_first) {
#     #         sizes <- rev(sizes)
#     #     }
#     #     sizes
#     # },
#     #
#     # arrange_layout = function(key, sizes, params, elements) {
#     #
#     #     layout <- seq_along(sizes)
#     #
#     #     if (params$lab_first) {
#     #         layout <- rev(layout)
#     #     }
#     #     # Set gap for spacer
#     #     layout <- layout[-2]
#     #
#     #     layout <- list(1, -1, layout, layout)
#     #     nms <- if (params$vertical) c("t", "b", "l", "r") else c("l", "r", "t", "b")
#     #     setNames(layout, nms)
#     # },
# #
# #     assemble_drawing = function(grobs, layout, sizes, params, elements) {
# #
# #         axis_line <- grobs$decor
# #
# #         # Unlist the 'label' grobs
# #         z <- if (params$position == "left") c(2, 1, 3) else 1:3
# #         z <- rep(z, c(1, length(grobs$labels), 1))
# #         has_labels <- !is.zero(grobs$labels[[1]])
# #         grobs  <- c(list(grobs$ticks), grobs$labels, list(grobs$title))
# #
# #         # Initialise empty gtable
# #         gt <- exec(
# #             gtable::gtable,
# #             !!params$orth_sizes := sizes,
# #             !!params$para_sizes := unit(1, "npc"),
# #             name = "axis"
# #         )
# #
# #         # Add grobs
# #         gt <- gtable::gtable_add_grob(
# #             gt, grobs,
# #             t = layout$t, b = layout$b, l = layout$l, r = layout$r,
# #             clip = "off", z = z
# #         )
# #
# #         # Set justification viewport
# #         vp <- exec(
# #             grid::viewport,
# #             !!params$orth_aes := unit(params$orth_side, "npc"),
# #             !!params$orth_size := max(params$measure_gtable(gt), unit(1, "npc")),
# #             just = params$opposite
# #         )
# #
# #         # Add null-unit padding to justify based on eventual gtable cell shape
# #         # rather than dimensions of this axis alone.
# #         if (has_labels && params$position %in% c("left", "right")) {
# #             where <- layout$l[-c(1, length(layout$l))]
# #             just <- with(elements$text, rotate_just(angle, hjust, vjust))$hjust %||% 0.5
# #             gt <- gtable_add_cols(gt, grid::unit(just, "null"), pos = min(where) - 1)
# #             gt <- gtable_add_cols(gt, grid::unit(1 - just, "null"), pos = max(where) + 1)
# #         }
# #         if (has_labels && params$position %in% c("top", "bottom")) {
# #             where <- layout$t[-c(1, length(layout$t))]
# #             just <- with(elements$text, rotate_just(angle, hjust, vjust))$vjust %||% 0.5
# #             gt <- gtable_add_rows(gt, grid::unit(1 - just, "null"), pos = min(where) - 1)
# #             gt <- gtable_add_rows(gt, grid::unit(just, "null"), pos = max(where) + 1)
# #         }
# #
# #         # Assemble with axis line
# #         absoluteGrob(
# #             gList(axis_line, gt),
# #             width  = gtable_width(gt),
# #             height = gtable_height(gt),
# #             vp = vp
# #         )
# #     },
# #
# #     draw_early_exit = function(self, params, elements) {
# #         line <- self$build_decor(decor = params$decor, elements = elements,
# #                                  params = params)
# #         absoluteGrob(
# #             gList(line),
# #             width  = grobWidth(line),
# #             height = grobHeight(line)
# #         )
# #     }
# )
#
#
# guide_axis_shortArrow <- function(
        #         title = waiver(),
#         theme = NULL,
#         spacing = NULL, order = 0, position = waiver(),
#         ...
# ) {
#     new_guide(
#         title = title,
#         theme = theme,
#         available_aes = c("x", "y"),
#         order = order,
#         position = position,
#         name = "axis_shortArrow",
#         super = GuideAxisShortArrow
#     )
# }
