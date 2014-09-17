#' Fire a signal at fixed fps.
#'
#' @param rate Number of signals to fire per second
#' @export
#' @return A reactive giving the time in seconds between the current and
#'   previous signal being fired
#' @examples
#' shinyApp(
#'   ui = fluidPage(textOutput("delta")),
#'   server = function(input, output) {
#'     tick <- fps(10)
#'     output$delta <- renderText(tick())
#'   }
#' )
fps <- function(rate) {
  cur <- proc.time()[[1]]

  reactive({
    invalidateLater(1000 / rate, NULL)
    last <- cur
    cur <<- proc.time()[[1]]

    cur - last
  })
}

#' Conditionally file a signal at fixed fps.
#'
#' @inheritParams fps
#' @param when A reactive that evaluates to TRUE or FALSE.
#' @return A reactive giving the time in seconds since the last signal.
#'   The first time after a pause is always zero - this way summing the
#'   signal gives the amount of sum the output signal has been running
#' @export
#' @examples
#' shinyApp(
#'   ui = fluidPage(textOutput("delta"), checkboxInput("pause", "pause")),
#'   server = function(input, output) {
#'     tick <- fpsWhen(10, reactive(!input$pause))
#'     output$delta <- renderText(tick())
#'   }
#' )
fpsWhen <- function(rate, when) {
  lastTime <- NA
  tick <- function(now = proc.time()[[1]]) {
    if (is.na(lastTime)) {
      lastTime <<- now
    }
    elapsed <- now - lastTime
    lastTime <<- now
    elapsed
  }

  rv <- reactiveValues(nonce = 0)

  o <- observe({
    invalidateLater(1000 / rate, NULL)
    if (isTRUE(when())) {
      rv$nonce <- isolate(rv$nonce) + 1
    } else {
      lastTime <<- NA
    }
  })

  reactive({
    rv$nonce
    tick()
  })
}


#' The current time.
#'
#' @export
#' @return A reactive \code{\link{POSIXct}}.
#' @examples
#' shinyApp(
#'   ui = fluidPage(textOutput("tick")),
#'   server = function(input, output) {
#'     tick <- every(1)
#'     output$tick <- renderText(as.character(tick()))
#'   }
#' )
every <- function(seconds) {
  reactive({
    invalidateLater(seconds * 1000, NULL)
    Sys.time()
  })
}
