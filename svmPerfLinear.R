require('Matrix')
require('sparsio')
require('caret')
require('e1071')

svmPerfLinear <- getModelInfo("svmLinear", regex = FALSE)[[1]]

svmPerfLinear$learn <- 'pathto/SVMperf/svm_perf_learn'
svmPerfLinear$classify <- 'pathto/SVMperf/svm_perf_classify'

svmPerfLinear$method <- "svmPerfLinear"
svmPerfLinear$type <- c("Classification")

svmPerfLinearparameters <- data.frame(parameter = c("C"),
                                     class = c("numeric"),
                                     label = c("cost"))

svmPerfLinear$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
        y <- ifelse(y==lev[1], 1, -1)
        out <- list()
        out$param <- param
        out$lev   <- lev
        out$datfile <- tempfile(pattern='svml',fileext='.dat')
        write_svmlight(Matrix(data.matrix(x), sparse=TRUE), y, file=out$datfile, zero_based=FALSE)
        out$modfile <- paste(out$datfile,'.model',sep='')
        out$learn   <- paste(svmPerfLinear$learn,'-w 3 -l 1','-c',param$C,out$datfile,out$modfile)
        out$system  <- system(out$learn, intern=TRUE)

        file.remove(out$datfile)
        # we are keeping the model file
        # in the future version read the file in and then write out to classify
        out
}

svmPerfLinear$predict <- function (modelFit, newdata, submodels = NULL) {
        out <- list()
        out$datfile  <- tempfile(pattern='svml',fileext='.dat')
        write_svmlight(Matrix(data.matrix(newdata), sparse=TRUE), file=out$datfile, zero_based=FALSE)
        out$predfile <- paste(out$datfile,'.pred',sep='')
        out$classify <- paste(svmPerfLinear$classify,out$datfile,modelFit$modfile,out$predfile)
        out$system   <- system(out$classify, intern=TRUE)
        file.remove(out$datfile)
        pred <- read.csv(out$predfile, header=FALSE)[,1]
        #show(out$predfile)
        file.remove(out$predfile)

        y <- ifelse(pred>0, 1, 2)
        y <- factor(modelFit$lev[y], levels=modelFit$lev)
        y
}


svmPerfLinear$prob <- function (modelFit, newdata, submodels = NULL) {
        out <- list()
        out$datfile  <- tempfile(pattern='svml',fileext='.dat')
        write_svmlight(Matrix(data.matrix(newdata), sparse=TRUE), file=out$datfile, zero_based=FALSE)
        out$predfile <- paste(out$datfile,'.pred',sep='')
        out$classify <- paste(svmPerfLinear$classify,out$datfile,modelFit$modfile,out$predfile)
        out$system   <- system(out$classify, intern=TRUE)
        file.remove(out$datfile)

        pred <- read.csv(out$predfile, header=FALSE)[,1]
        file.remove(out$predfile)

        # return pseudo-probabilities by taking sigmoid of the soft assignments
        y <- sigmoid(pred)
        y <- cbind(y, 1-y)
        colnames(y) <- modelFit$lev
        attr(y, "probabilities")
        y
}

svmPerfLinear$grid <- function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(C = 2 ^((1:len) - 3))
                    } else {
                      out <- data.frame(C = 2^runif(len, min = -5, max = 10))
                    }
                    out
                }
