# Load in Data
movies <- read.csv("Movies.csv")
head(movies)
names(movies)

# drop unneccesary columns
movies$adult <- NULL
movies$backdrop_path <- NULL
movies$overview <- NULL
movies$poster_path <- NULL
movies$video <- NULL

unique(movies$decade)

movies[movies$decade == "1980s", c("title", "vote_average", "vote_count") ]

nrow(movies)
nrow(movies[movies$decade == "1980s", c("title", "vote_average", "vote_count")])
nrow(movies[movies$decade == "1990s", c("title", "vote_average", "vote_count")])
nrow(movies[movies$decade == "2000s", c("title", "vote_average", "vote_count")])
nrow(movies[movies$decade == "2010s", c("title", "vote_average", "vote_count")])
nrow(movies[movies$decade == "2020s", c("title", "vote_average", "vote_count")])

boxplot(vote_average ~ decade, data = movies,
        xlab = "Decade",
        ylab = "TMDb Rating",
        main = "Ratings by Decade")

# Distribuition verification
sd_by_decade <- tapply(movies$vote_average, movies$decade, sd)
iqr_by_decade <- tapply(movies$vote_average, movies$decade, IQR)

sd_by_decade
iqr_by_decade

# Density plots from each decade
par(mfrow = c(2, 3))
for (d in unique(movies$decade)) {
  x <- movies$vote_average[movies$decade == d]
  plot(density(x), main = d, xlab = "Rating", xlim = range(movies$vote_average))
}
par(mfrow = c(1, 1))



# Kruskall Wallace H statistic
movies$ranked_votes <- rank(movies$vote_average)
N <- nrow(movies)
nj <- 100 # All decades have a sample of 100 movies
Tj <- tapply(movies$ranked_votes, movies$decade, sum)
H <- 12/(N * (N+1)) * sum((Tj ** 2) / nj) - 3 * (N+1)


# Correction factor for ties in the ranks
tab_ties <- table(movies$ranked_votes)           # counts of identical scores
t <- as.numeric(tab_ties[tab_ties > 1])   # only actual tie groups
if (length(t) == 0) {
  C <- 1    # no ties
} else {
  C <- 1 - sum(t^3 - t) / (N^3 - N)
}

H_prime <- H / C               # tie-corrected statistic
df_kw   <- length(Tj) - 1      # degrees of freedom

p_value <- pchisq(H_prime, df = df_kw, lower.tail = FALSE)

list(
  H_raw       = H,
  H_corrected = H_prime,
  df          = df_kw,
  p_value     = p_value
)

# Compare movies from the 2020's to all other decades
movies$era <- ifelse(movies$decade == "2020s", "2020s", "pre_2020s")

w <- wilcox.test(vote_average ~ era,
                 data     = movies,
                 conf.int = TRUE,
                 exact    = FALSE)   # usually safer with larger n

w$estimate      # point estimate of location difference (Hodges–Lehmann)
w$conf.int      # CI for that difference


# summary stats for the 2020's versus other decades groupings
aggregate(vote_average ~ era, data = movies, FUN = mean)

aggregate(vote_average ~ era, data = movies, FUN = median)

aggregate(vote_average ~ era, data = movies, FUN = sd)

aggregate(vote_average ~ era, data = movies, FUN = length)

# Mention Monotonicity- comment 9 P 226 Gabriel 1969
# Pairwise Rankings - comment 11

# ANOVA for comparison
movies$decade <- factor(
  movies$decade,
  levels = c("1980s", "1990s", "2000s", "2010s", "2020s")
)
fit <- aov(vote_average ~ decade, data = movies)

summary(fit)


