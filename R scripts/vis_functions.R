
plot_tabs <- function(plots) {
  purrr::walk(
    names(plots),
    \(x) {
      cat('### ', x, '\n\n')
      print(plots[[x]])
      cat('\n\n')
    }
  )
}