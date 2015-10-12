- X = {normal, flushed, noncached}
- (X1,X2) = {(normal, flushed), (normal, noncached), (flushed, noncached)}

# Meaningful percentages to calculate

1) {X}basline: `total.X.computed / total.X.called`
- the baseline of the method X

2) ratio{X1}To{X2}: `total.{X1}.computed / total.{X2}.called`
- the efficiency of the incremental approach in comparison to the method X2,
  i.e. the ratio between actual work done in X1 compared to possible work done with method X2

3) speedup{X1}To{X2}: `(total.{X2}.computed / total.{X2}.called) - (total.{X1}.computed / total.{X2}.called)`
- = `baseline({X2}) - ratio({X1}, {X2})`
- the "speed-up" of the incremental approach (normal or flushed) in comparison to the method X2
