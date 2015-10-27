shots_2014_season <- read.csv("2014_regular_season_shots.csv")
shots_2015_season <- read.csv("2015_regular_season_shots.csv")
shots_2014_playoffs <- read.csv("2014_playoffs_shots.csv")
shots_2015_playoffs <- read.csv("2015_playoffs_shots.csv")

shots_2014_season$DATE <- as.Date(shots_2014_season$DATE, format="%b %d, %Y")
shots_2014_season$SHOOTER_ID <- as.factor(shots_2014_season$SHOOTER_ID)
shots_2014_season$PERIOD <- as.factor(shots_2014_season$PERIOD)
shots_2014_season$GAME_CLOCK_PERIOD <- as.factor(shots_2014_season$GAME_CLOCK_PERIOD)
shots_2014_season$PTS_TYPE <- as.factor(shots_2014_season$PTS_TYPE)
shots_2014_season$PTS <- as.factor(shots_2014_season$PTS)
shots_2014_season$FGM[shots_2014_season$SHOT_RESULT == "made"] <- 1
shots_2014_season$FGM[shots_2014_season$SHOT_RESULT == "missed"] <- 0
shots_2014_season$CLOSEST_DEFENDER_PLAYER_ID <- as.factor(shots_2014_season$CLOSEST_DEFENDER_PLAYER_ID)

shots_2014_season$SHOT_CLOCK[is.na(shots_2014_season$SHOT_CLOCK) & shots_2014_season$GAME_CLOCK > 24] <- mean(shots_2014_season$SHOT_CLOCK, na.rm=TRUE)
shots_2014_season$SHOT_CLOCK[is.na(shots_2014_season$SHOT_CLOCK) & shots_2014_season$GAME_CLOCK <= 24] <- 0

all_star_2014_date <- as.Date("2014-02-15")
shots_2014_training <- subset(shots_2014_season, DATE < all_star_2014_date)
shots_2014_validation <- subset(shots_2014_season, DATE > all_star_2014_date)

library(lme4)

variables <- c("SHOT_DIST", "CLOSE_DEF_DIST", "LOCATION", "TOUCH_TIME", "DRIBBLES", "GAME_CLOCK_PERIOD", "PTS_TYPE",
               "SHOOTER_ID", "CLOSEST_DEFENDER_PLAYER_ID", "FGM")

fit <- glmer(FGM ~ .-SHOOTER_ID-CLOSEST_DEFENDER_ID + (1|SHOOTER_ID) + (1|CLOSEST_DEFENDER_PLAYER_ID), 
             shots_2014_training[variables], family=binomial(logit), nAGQ=0)

fit_validation_error <- shots_2014_validation$FGM - predict(fit, shots_2014_validation[variables], allow.new.levels=TRUE, type="response")
fit_validation_rmse <- sqrt(mean(sapply(fit_validation_error, function(x) x^2)))

fit2 <- glmer(FGM ~ .-SHOOTER_ID-CLOSEST_DEFENDER_ID + (1|SHOOTER_ID + SHOT_DIST) + (1|CLOSEST_DEFENDER_PLAYER_ID + SHOT_DIST), 
             shots_2014_training[variables], family=binomial(logit), nAGQ=0)

fit2_validation_error <- shots_2014_validation$FGM - predict(fit2, shots_2014_validation[variables], allow.new.levels=TRUE, type="response")
fit2_validation_rmse <- sqrt(mean(sapply(fit2_validation_error, function(x) x^2)))