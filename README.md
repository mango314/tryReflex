# tryReflex
functional reactive programming (FRP) with Haskell

I thought Haskell might be more fun to learn if we tried to build a website. There is not much documentation, just a few quickreferences,
[[1](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md), [2](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md)] the source code iself and off you go.
There are lots and lots and lots of monads ([3](https://www.youtube.com/watch?v=9fohXBj2UEI), [4](https://www.youtube.com/watch?v=ZhuHCtR3xq8) )(at least not for the programmer). 

Also, the Elm [compiler](https://github.com/elm-lang/elm-compiler/tree/master/src) is written in Haskell and compiles Elm language files into JavaScript in a reliable way.

Elm is a functional programming language, but it does not use monads   And it is almost exclusively
about web programming.  Haskell can be used for web or desktop.  It can be used in front-end or back-end.  Can even chomp up 
substantial backend computations and do them in the web browser.

Here's a sample of the difference between Elm and Haskell (by design);

* Haskell seems to prefer infix operators such as `!!` and `$` while Elm prefers to put the operator in front
* `+ 2 3` vs.  `2 + 3`
* As just stated, Haskell uses monads liberally and therefore Reflex uses (several monads) to build websites and user interactions as a by-product of more abstract (monadic) computations.  Elm models the signals directly.
* `el "div"` abstractly creates a div element but also places one on the page.  `div [] []` represents the div element itself and you are passing the signals through the [Elm architecture](https://guide.elm-lang.org/architecture/).
* Reflex documenation [is](http://reflex-frp.readthedocs.io/en/latest/architecture.html#overview-of-reflex-basics) improving. The main goal would be to understand the 3 monads that reflex defines: `Event`, the `Behavior` and the `Dynamic`.

None of the applications of these are useful.  Purely test-driving the language. [Demo](https://monsieurcactus.github.io/tryReflex/candy/)
