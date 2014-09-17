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
#'     tick <- reactive(!input$pause) %>% fpsWhen(10)
#'     output$delta <- renderText(tick())
#'   }
#' )
fpsWhen <- function(when, rate) {
  lastTime <- NA
  tick <- function(now = proc.time()[[3]]) {
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

#' Add a timestamp to any signal.
#'
#' @param Input signal
#' @return A reactive list: the first element is a timestamp (a
#'   \code{\link{POSIXct}}), the second element is the \code{signal}.
#' @export
#' @examples
#' shinyApp(
#'   ui = fluidPage(textOutput("tick"), textOutput("time")),
#'   server = function(input, output) {
#'     tick <- fps(10) %>% timestamp()
#'     output$tick <- renderText(tick()[[1]])
#'     output$time <- renderText(tick()[[2]])
#'   }
#' )
timestamp <- function(signal) {
  reactive({
    list(Sys.time(), signal())
  })
}

#' Since last sigal?
#'
#' Has it been \code{delay} seconds since the last signal?
#'
#' @param signal A reactive.
#' @param delay Delay in seconds.
#' @return A reactive boolean.
#' @export
#' @examples
#' shinyApp(
#'   ui = fluidPage(actionButton("click", "click"), textOutput("clicked")),
#'   server = function(input, output) {
#'     clicked <- reactive(input$click) %>% since(2)
#'     output$clicked <- renderText(clicked())
#'   }
#' )
since <- function(signal, delay) {
  rv <- reactiveValues(on = FALSE, last_signal = now())

  observe({
    signal()
    isolate(rv$last_signal <- now())
    isolate(rv$on <- TRUE)
  })
  observe({
    if (now() >= rv$last_signal + delay) {
      isolate(rv$on <- FALSE)
    }
    if (rv$on) {
      invalidateLater(delay * 1000, NULL)
    }
  })

  reactive(rv$on)
}

now <- function() proc.time()[[3]]
