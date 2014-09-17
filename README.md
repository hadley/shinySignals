# shinySignals

Shiny is a reactive programming framework, but it focusses mainly on the nuts and bolts of generating interactive analysis apps. shinySignals is an attempt to port more functional programming tools for working with signals (reactives) as interesting fundamental objects in their own right. 

Currently, shinySignals only implements the [Signal](http://library.elm-lang.org/catalog/elm-lang-Elm/0.12.3/Signal) and [Time](http://library.elm-lang.org/catalog/elm-lang-Elm/0.12.3/Time) libraries from the [Elm programming language](http://elm-lang.org). It's expected that this package will grow over time to include useful techniques from other FRP frameworks, and to include uniquely R-like features.

## Installation

shinySignals is currently only available on github. Run the following code to install it:

```R
# install.packages("devtools")
devtools::install_github("hadley/shinySignals")
```

## Elm

Compared to the Elm API, the main change is that the key signal always comes first. This leads to a more natural style of composition using `%>%`:

```R
library(shinySignals)
fps(30) %>% count()

step <- reactive{(fps(30); sample(c(-1, 1), 1))}
walk <- step %>% reducePast(`+`, 0)
```
