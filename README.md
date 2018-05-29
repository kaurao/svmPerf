# svmPerf
A simple interface to the SVMperf in R

Usage:

Download the SVMperf from: http://www.cs.cornell.edu/people/tj/svm%5Flight/svm_perf.html

Set the corresponding paths to learn and classify in the svmPerfLinear.R

```
source('svmPerfLinear.R')
data("iris")
x = iris[1:100,1:4]
y = factor(make.names(as.integer(iris[1:100,5])))
model = svmPerfLinear$fit(x,y,lev=levels(y),param=list(C=1))
pred = svmPerfLinear$predict(model, newdata=x)
```
