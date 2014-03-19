source('settings.R')
source('utils.R')

# the model
f <- function (ds, params) (1+(ds/params[1])^(-params[2]))^(-1)

# compute the team vs. team probabilities
p.team <- function(t1, t2) {
    t1.row <- which(rownames(team.probs)==t1)
    t2.col <- which(colnames(team.probs)==t2)
    if (length(t1.row)==1 && length(t2.col)==1) {
        t1.vs.t2 <- team.probs[t1.row, t2.col]
    } else {
        t1.vs.t2 <- NA
    }
    if (!is.na(t1.vs.t2))
        print(sprintf("%s beats %s = %.3f", t1, t2, t1.vs.t2))
    t1.vs.t2
}

# compute the region vs. region probabilities
p.region <- function(r1.name, r2.name) {
   r1.row <- which(rownames(region.probs)==r1.name)
   r2.col <- which(colnames(region.probs)==r2.name)
   if (length(r1.row)==1 && length(r2.col)==1) {
       r1.vs.r2 <- region.probs[r1.row, r2.col]
   } else {
       r1.vs.r2 <- NA
   }
   if (!is.na(r1.vs.r2))
       print(sprintf("%s beats %s = %.3f", r1.name, r2.name, r1.vs.r2))
   r1.vs.r2
}

# the probability t1 beats t2 based on seed alone
p <- function (seeds, t1, t2, params) {
    s1 <- get.seed(seeds, t1)
    s2 <- get.seed(seeds, t2)
    season.code <- unique(seeds$season)
    r1 <- get.region(seeds, t1)
    r2 <- get.region(seeds, t2)
    r1.name <- get.region.name(season.code, r1)
    r2.name <- get.region.name(season.code, r2)
    ds <- abs(s1-s2)
    p.seed <- (0.5-sign(s1-s2)*f(ds, params)/2)
    w.region <- ifelse(length(params)==3, params[3], 0)
    w.team <- ifelse(length(params)==4, params[4], 0)
    pval.region <- p.region(r1.name, r2.name)
    pval.team <- p.team(t1,t2)
    pval <- p.seed
    if (!is.na(pval.region)) {
        pval <- (1-w.region)*pval+w.region*pval.region
    }
    if (!is.na(pval.team)) {
        pval <- (1-w.team)*pval+w.team*pval.team
    }
    pval
}

cur.alpha <- 1
cur.beta <- 2
p(cur.seeds, cur.seeds$team[1], cur.seeds$team[5], c(cur.alpha, cur.beta))

create.matrix <- function (seeds, params) {
    x <- sapply(seeds$team, function (t1) sapply(seeds$team, function (t2) {
        p(seeds, t1, t2, params)
    }))
    colnames(x) <- rownames(x) <- seeds$team
    x
}
