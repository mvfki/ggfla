test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Basic ggplot for further testing
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()

test_that("element_line_fixlen works", {
    expect_s3_class(element_line_fixlen(), "element_line")
    expect_s3_class(
        element_grob(
            element_line_fixlen(length = unit(1, "cm"), arrow = arrow(), color = "blue"),
            x = c(1,1), y = c(0, 1)
        ),
        "polyline"
    )
    expect_s3_class(
        element_grob(
            element_line_fixlen(arrow = arrow(), color = "blue"),
            x = c(1,1), y = c(0, 1)
        ),
        "polyline"
    )
    expect_s3_class(
        element_grob(
            element_line_fixlen(length = unit(1, "cm"), color = "blue"),
            x = c(0,1), y = c(1, 1)
        ),
        "polyline"
    )
    expect_s3_class(
        element_grob(
            element_line_fixlen(),
            x = c(0,1), y = c(1, 1)
        ),
        "polyline"
    )
})

test_that("theme_axis_shortArrow works", {
    expect_s3_class(theme_axis_shortArrow(), "theme")
})
