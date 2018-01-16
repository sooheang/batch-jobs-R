library(batchtools)

# Create a job
reg = makeExperimentRegistry(file.dir = NA, seed = 1)

# Define Jobs - Problem
subsample = function(data, job, ratio, ...) {
  n = nrow(data)
  train = sample(n, floor(n * ratio))
  test = setdiff(seq_len(n), train)
  list(test = test, train = train)
}
data("iris", package = "datasets")

addProblem(name = "iris", data = iris, fun = subsample, seed = 42)

# Define Jobs - Algorithm
svm.wrapper = function(data, job, instance, ...) {
  mod = e1071::svm(Species ~ ., data = data[instance$train, ], ...)
  pred = predict(mod, newdata = data[instance$test, ], type = "class")
  table(data$Species[instance$test], pred)
}
addAlgorithm(name = "svm", fun = svm.wrapper)

forest.wrapper = function(data, job, instance, ...) {
  mod = ranger::ranger(Species ~ ., data = data[instance$train, ], write.forest = TRUE)
  pred = predict(mod, data = data[instance$test, ])
  table(data$Species[instance$test], pred$predictions)
}
addAlgorithm(name = "forest", fun = forest.wrapper)

# Define Jobs - Experiment
# problem design: try two values for the ratio parameter
pdes = list(iris = data.table(ratio = c(0.67, 0.9)))

# algorithm design: try combinations of kernel and epsilon exhaustively,
# try different number of trees for the forest
ades = list(
  svm = CJ(kernel = c("linear", "polynomial", "radial"), epsilon = c(0.01, 0.1)),
  forest = data.table(ntree = c(100, 500, 1000))
)

addExperiments(pdes, ades, repls = 5)

summarizeExperiments()

summarizeExperiments(by = c("problem", "algorithm", "ratio"))

# Submit a Job
submitJobs()
waitForJobs()

# Report
reduce = function(res) list(mce = (sum(res) - sum(diag(res))) / sum(res))
results = unwrap(reduceResultsDataTable(fun = reduce))
head(results)

pars = unwrap(getJobPars())
tab = ijoin(pars, results)
head(tab)

pars = unwrap(getJobPars())
tab = ijoin(pars, results)
head(tab)

tab[ratio == 0.67, list(mmce = mean(mce)),
    by = c("algorithm", "kernel", "epsilon", "ntree")]
