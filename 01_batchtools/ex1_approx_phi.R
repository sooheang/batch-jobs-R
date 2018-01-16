
library(batchtools)

reg = makeRegistry(file.dir = NA, seed = 1)

piApprox = function(n) {
  nums = matrix(runif(2 * n), ncol = 2)
  d = sqrt(nums[, 1]^2 + nums[, 2]^2)
  4 * mean(d <= 1)
}
piApprox(1000)

# We now parallelize piApprox() with batchtools: 
# We create 10 jobs, each doing a MC simulation with 105105 jobs. 
# We use batchMap() to define the jobs (note that this does not yet start the calculation):
batchMap(fun = piApprox, n = rep(1e+5, 10))

# The function batchMap(fun, ...) works analogously to Map(f, ...) of the base package

names(getJobTable())

submitJobs(resources = list(walltime = 3600, memory = 1024))

getStatus()
testJob()
showLog()
