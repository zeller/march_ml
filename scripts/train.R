source('model.R')

epsilon <- 0.000000000000000000001
# compute the log-loss metric used to evaluate
log.loss <- function(seeds, results, params) {
    -mean(apply(results, 1, function (game) {
        pval <- p(seeds, game[3], game[5], params)
        if (pval > 1-epsilon) pval <- pval - epsilon
        if (pval < 0+epsilon) pval <- pval + epsilon
        print(sprintf("%s (%s) beats %s (%s): %.3f",
                      game[3], get.seed(seeds, game[3]),
                      game[5], get.seed(seeds, game[5]),
                      pval))
        log(pval)
    }))
}

obj <- function (params) {
    o <- sum(sapply(as.character(seasons), function (season.code) {
        ll <- log.loss(season.seeds(season.code),
                       season.results(season.code),
                       params)
        print(sprintf("Season %s LogLoss = %.3f", season.code, ll))
        ll
    }))
    print(sprintf("-Sum(LogLoss) = %.3f", o))
    print(sprintf("Params = %s", params))
    o
}

cur.params <- c(cur.alpha, cur.beta)
obj(cur.params)

print("Training sigmoid function")
opt <- optim(cur.params, obj, lower=1, method="L-BFGS-B")
cur.params <- opt$par

print("Training region probability weight")
# TODO: update this for optimizing w which is the influence of the region on p
obj.w.region <- function (w) {
    obj(c(cur.params, w))
}

seasons <- subset.seasons[,1] # get only the seasons with the same region as the current
seasons <- seasons[-length(seasons)] # remove current season

cur.w.region <- 0.5 # weight for influence of probability of region vs. region on seed probability
opt <- optim(cur.w.region, obj.w.region, lower=0, upper=1, method="L-BFGS-B")

cur.w.region <- opt$par
cur.params <- c(cur.params, cur.w.region)

obj.w.team <- function (w) {
    obj(c(cur.params, w))
}

cur.w.team <- 0.5 # weight for influence of probability of team vs. team on seed probability
opt <- optim(cur.w.team, obj.w.team, lower=0, upper=1, method="L-BFGS-B")

cur.w.team <- opt$par
cur.params <- c(cur.params, cur.w.team)

# compute the submission matrix using the learned parameters
cur.matrix <- create.matrix(cur.seeds, cur.params)

get.t1.vs.t2 <- function (matrix, t1, t2) {
    matrix[which(rownames(matrix)==t1),which(colnames(matrix)==t2)]
}

options(digits=15)
create.flat.matrix <- function (matrix, filename, team=NA) {
    sink(sprintf('%s/%s', root.dir, filename))
    cat("id,pred\n")
    sapply(sort(colnames(matrix)), function (t1)
           sapply(sort(rownames(matrix)), function (t2) {
               if (as.numeric(t1) < as.numeric(t2)) {
                   t1.vs.t2 <- get.t1.vs.t2(matrix, t2, t1)
                   if (!is.na(team) && (t1 == team || t2 == team))
                       t1.vs.t2 <- ifelse(t1==team, 1, 0)
                   cat(sprintf("S_%s_%s,%.15f\n",t1,t2,t1.vs.t2))
               }
           }))
    sink()
}
create.flat.matrix(cur.matrix, 'submission-1.csv')
create.flat.matrix(cur.matrix, 'submission-2.csv', 651) # 'Louisville'
