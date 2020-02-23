Welcome to [`riddlr`](github.com/dgkf/riddlr), a framework for evaluating R code
against a set of preset solutions.

> ## Quick Start
>
> Start testing your R skills against some example questions by navigating to the
catalog page. Once you've found one you want to tackle, click the `>_` icon to
launch the riddle. 

---

#### Details

Riddles consist of a question prompt, user code block and a known correct
solution. When a user loads up a riddle, `shiny` will render a question `Rmd` 
file as a new page and provide the user with a prepopulated code block to test
their mettle. Code can be run with console output displayed to the user. When
they're satisfied with their solution, they can submit it where their solution
will be evaluated and the output will be compared against the output of the 
solution code run against the same test input. 

If a test fails, execution stops and the user gets feedback about the input that
was used, expected result and what their code produced to allow them to iterate
on their code, addressing possible edge cases or performance issues. Tests can
fail when the output doesn't match, or when a execution time limit is reached.
Users get feedback about exactly why their code failed so they can target their
development.

When successful, a random congratulatory message is displayed to the user. 

`riddlr` comes with hooks for triggering additional actions when a riddle is
submitted, gather data about what was submitted and the result of execution,
allowing for logging.

#### Applications

Aside from being a pretty cool app, this demo is intended to provide a
foundation for some community-driven tools for learning R. We all benefit from
having easy tools to learn and develop R skills.

This code is all released under the `MIT` license so that companies can use it
for interview evaluations, providing a rigorous, extendable and effective way of
providing targetted programming examples that are more akin to a real
programming environment. Unlike typical whiteboarding-style programming
assessments, this gives users the flexibility to use the packages they're
comfortable with (granted they're installed on the system), code autocompletion,
parameter tooltips, realtime syntax error feedback, access to help documentation
and the ability to iterate on their code. The submission hooks are intended to
provide an easy way of capturing multiple submissions so that a user or
interviewee's iterative coding can be evaluated in addition to their final
result.
