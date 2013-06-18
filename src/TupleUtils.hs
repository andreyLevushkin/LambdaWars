module TupleUtils where

fstMap :: (a -> c) -> (a, b) -> (c, b)
fstMap f (x, y) = (f x, y)

sndMap :: (b -> c) -> (a, b) -> (a, c)
sndMap f (x, y) = (x, f y)

-- Examples

demoFstMap = fstMap succ (1,1) -- (2,1)

demoSndMap = sndMap succ (1,1) -- (1,2)