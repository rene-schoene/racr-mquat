# Meaningful percentages to calculate

1) `total.X.computed / total.X.called` with X = {normal, flushed, noncached}
- the baseline of the method X

2) `total.normal.computed / total.Y.called` with Y = {flushed, noncached}
- the efficiency of the incremental approach in comparison to the method Y,
  i.e. the ratio between actual work done normally compared to possible work done with method Y

3) `(total.Y.computed / total.Y.called) - (total.normal.computed / total.Y.called)` with Y = {flushed, noncached}
- = `baseline(Y) - ratio(normal, Y)`
- the "speed-up" of the incremental approach in comparison to the method Y
