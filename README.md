Transition Graph
================

Ver. 0.1

With this Graph library you can compose evaluable graphs of transitions over your arbitrary monadic language.

- Graph can incorporate any monadic language.
- Graph is event-driven, so this approach can be considered reactive programming.
- It seems, Graph is Turing-complete, but no one has checked this yet. ;)

One of possible applications includes step-by-step games with well-defined nodes and edges.
For example, adventure games that have locations and actions the player can do.

Library provides several different transitions:

- (~>)  By event, forward-only
- (<~>) By event, backable (uses a specified event to allow back transition)
- (>~<) By event, go forward and return unconditionally
- (/>)  Default, forward-only
- (</>) Default, backable
- (-/>) Pass through node unconditionally (overrides other transitions)

Currently, event is `String`, but this can be generalized in the future versions.

Sample
------

```haskell

location :: String -> IO (Event, ())
location name = do
  putStrLn name
  input <- getLine
  pure (input, ())

northOfHouse :: Graph IO () ()
northOfHouse = graph $
  with (location "North of House")
     ~> on "go west" westOfHouse
     /> northOfHouse                -- default case


westOfHouse :: Graph IO () ()
westOfHouse = graph $
  with (location "West of House")
     ~> on "go north" northOfHouse
     /> westOfHouse                 -- default case


main = runGraph' id (== "back") westOfHouse

```

