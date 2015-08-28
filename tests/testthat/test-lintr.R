context("Code is high quality and lint free")
test_that("Code Lint", {
  skip_on_cran()
  skip_if_not_installed("lintr")

  my_linters <- list(
    #commented_code_linter = lintr::commented_code_linter, # 140
    #single_quotes_linter = lintr::single_quotes_linter, # 88
    #snake_case_linter = lintr::snake_case_linter, # 86
    infix_spaces_linter = lintr::infix_spaces_linter,
    trailing_whitespace_linter = lintr::trailing_whitespace_linter,
    spaces_left_parentheses_linter = lintr::spaces_left_parentheses_linter,
    trailing_blank_lines_linter = lintr::trailing_blank_lines_linter,
    absolute_paths_linter=lintr::absolute_paths_linter,
    assignment_linter=lintr::assignment_linter,
    closed_curly_linter=lintr::closed_curly_linter,
    commas_linter=lintr::commas_linter,
    line_length_linter=lintr::line_length_linter,
    no_tab_linter=lintr::no_tab_linter,
    object_usage_linter=lintr::object_usage_linter,
    multiple_dots_linter=lintr::multiple_dots_linter,
    object_length_linter=lintr::object_length_linter,
    open_curly_linter=lintr::open_curly_linter,
    spaces_inside_linter=lintr::spaces_inside_linter
  )

#   library(magrittr)
#   library(dplyr)
#   lintr::lint_package(linters = my_linters) %>%
#     as.data.frame %>%
#     group_by(linter) %>%
#     tally(sort = TRUE) %$%
#     sprintf("linters: with_defaults(\n    %s\n    NULL\n  )\n",
#             paste0(linter, " = NULL, # ", n, collapse="\n    ")) %>%
#     cat()

  lintr::expect_lint_free(linters=my_linters)
})
