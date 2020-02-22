# riddlr

A package for making coding challenges, largely inspired by the 
[`learnr`](https://github.com/rstudio/learnr) package.

## Installation

Install the package off of GitHub via

```r
devtools::install_github("dgkf/riddlr")
```

## Quick Start

Try out some examples:

### Building a single question shiny app

```r
library(shiny)

app_file <- system.file("example", "shiny", "single_question", "app.R", package = "riddlr")
shinyAppFile(appR)
```

### Rendering a searchable question catalog shiny app

```r
library(shiny)

app_file <- system.file("example", "shiny", "question_catalog", "app.R", package = "riddlr")
shinyAppFile(appR)
```

## Community Shout-outs

### `learnr`

A lot of this package is inspired by `learnr`, with early versions being built
directly into `learnr`'s Rmarkdown framework. The scope is slightly different,
being an evaluation tool rather than a learning tool. From this slight change in
perspective, there were a few features that `learnr` couldn't accommodate.

- easily testing code against multiple test cases, whereas `learnr` only handles
a single input
- timeout per test case (or timeout with a grace period for feedback)
- hooks for triggering database logs 
- easier addition of markdown-formatted help dialogs

Could this all be contributed back to the `learnr` package? Absolutely, but
ensuring code quality, reusability and consistency within the `learnr`
framework wasn't within scope for the proof-of-concept.

### `shinyAce`

A lot of work was put into giving a comfortable programming interface with
staple features like improved code completion, help-text popups, parsing errors
and warnings noted in the code form gutter. All of these features were first
added internally as part of the proof-of-concept, but were later [contributed
back](https://github.com/trestletech/shinyAce/pull/66) to the `shinyAce`
package.

A huge thank you goes out to @vnijs and @detule for giving great feedback on
such a large body of new features and diligently checking many edge cases that
would have gone unaddressed.
