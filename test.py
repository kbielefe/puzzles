def combineRanges(maxValues, result=()):
  if len(maxValues) is 0:
    print("-".join(result))
    return

  for x in range(0, maxValues[0]):
    combineRanges(maxValues[1:], result + (str(x),))

maxValues = (2, 2, 2, 2)
combineRanges(maxValues)
