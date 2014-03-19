data.file <- function (file) sprintf('%s/%s', data.dir, file)
seeds <- read.csv(data.file('tourney_seeds.csv'))
results <- read.csv(data.file('tourney_results.csv'))
full.seasons <- read.csv(data.file('seasons.csv'))
regular.season.results <- read.csv(data.file('regular_season_results.csv'))

region.codes <- c('W', 'X', 'Y', 'Z')
cur.region.names <- c('East', 'South', 'Midwest', 'West')

region.ranks <- read.delim(sprintf('%s/region_rank.csv', root.dir), header=F)[,1]
region.probs <- sapply(cur.region.names, function (r1) sapply(cur.region.names, function (r2) {
    r1.ranks <- match(r1, region.ranks)
    r2.ranks <- match(r2, region.ranks)
    ranks <- rbind(cbind(r1, r1.ranks),
                   cbind(r2, r2.ranks))
    ranks <- ranks[order(ranks[,2]),]
    r1.vs.r2 <- mean(sample(ranks[,1], 1000, replace=T, prob=as.numeric(ranks[,2])/length(region.ranks))==r2)
    print(sprintf("%s beats %s = %.3f", r1, r2, r1.vs.r2))
    r1.vs.r2
}))
diag(region.probs) <- NA
region.probs <- t(region.probs)

# limit seasons to only those with the same regions
subset.seasons <- full.seasons[which(apply(full.seasons, 1, function (season) all(season[4:7] %in% cur.region.names))),]

get.region.name <- function (season.code, region.code) {
    full.seasons[which(full.seasons$season==season.code),paste0("region", region.code)]
}

seasons <- unique(seeds$season)
cur.season <- seasons[length(seasons)]
seasons <- seasons[-length(seasons)]

current.season.results <- subset(regular.season.results, season==cur.season)

all.current.teams <- unique(c(current.season.results$wteam, current.season.results$lteam))
team.probs <- sapply(all.current.teams, function (t1) sapply(all.current.teams, function (t2) {
    games.won <- current.season.results[which(current.season.results$wteam==t1 & current.season.results$lteam==t2),]
    games.lost <- current.season.results[which(current.season.results$wteam==t2 & current.season.results$lteam==t1),]
    diff.points <- mean(c(games.won$wscore-games.won$lscore, games.lost$lscore-games.lost$wscore))
    logistic <- function (x, alpha=7) (1+exp(-x/alpha))^(-1)
    logistic(diff.points)
}))
diag(team.probs) <- NA
team.probs[which(is.nan(team.probs))] <- NA
team.probs <- t(team.probs)
colnames(team.probs) <- rownames(team.probs) <- as.character(all.current.teams)

season.seeds <- function(season.code) subset(seeds, season==season.code)
season.results <- function (season.code) subset(results, season==season.code)

cur.seeds <- season.seeds(cur.season)

seed.regex <- '([A-Z])([0-9]+)[a-z]?'

get.seed <- function (seeds, team) as.numeric(gsub(seed.regex, '\\2', seeds$seed[which(seeds$team==team)]))
get.seed(cur.seeds, cur.seeds$team[1])
get.seed(cur.seeds, cur.seeds$team[5])

get.region <- function(seeds, team) as.character(gsub(seed.regex, '\\1', seeds$seed[which(seeds$team==team)]))
get.region(cur.seeds, cur.seeds$team[1])
get.region(cur.seeds, cur.seeds$team[5])

