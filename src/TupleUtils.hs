module TupleUtils where

fstMap :: (a -> c) -> (a, b) -> (c, b)
fstMap f (x, y) = (f x, y)

sndMap :: (b -> c) -> (a, b) -> (a, c)
sndMap f (x, y) = (x, f y)