import Data.List

type Range = (Int, Int)
data RangePair = RangePair Range Range deriving (Eq, Ord, Show)

overlaps :: Range -> Range -> Bool
overlaps (a,b) (c,d) =
  c < a && a < d ||
  c < b && b < d ||
  a < c && c < b ||
  a < d && d < b

pairsOverlap :: RangePair -> RangePair -> Bool
pairsOverlap (RangePair a b) (RangePair c d) = (overlaps a c) && (overlaps b d)

combineRanges :: Range -> Range -> Range
combineRanges (a,b) (c,d) = (min a c, max b d)

combinePairs :: RangePair -> RangePair -> RangePair
combinePairs (RangePair a b) (RangePair c d) = RangePair (combineRanges a c) (combineRanges b d)

ranges = [
  RangePair (0,6)   (34,40),
  RangePair (1,7)   (35,41),
  RangePair (3,9)   (12,18),
  RangePair (2,8)   (36,42),
  RangePair (13,19) (22,28),
  RangePair (18,29) (23,30)]

combine :: [RangePair] -> [RangePair]
combine = map (foldl1 combinePairs) . groupBy pairsOverlap . sort
