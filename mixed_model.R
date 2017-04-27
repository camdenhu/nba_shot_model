shots_2014_season <- read.csv("2014_regular_season_shots.csv", stringsAsFactors = FALSE)
shots_2014_playoffs <- read.csv("2014_playoffs_shots.csv", stringsAsFactors = FALSE)

shots_2014_season$DATE <- as.Date(shots_2014_season$DATE, format="%b %d, %Y")
shots_2014_season$SHOOTER_ID <- as.factor(shots_2014_season$SHOOTER_ID)
shots_2014_season$LOCATION <- as.factor(shots_2014_season$LOCATION)
shots_2014_season$PERIOD <- as.factor(shots_2014_season$PERIOD)
shots_2014_season$GAME_CLOCK_PERIOD <- as.factor(shots_2014_season$GAME_CLOCK_PERIOD)
shots_2014_season$PTS_TYPE <- as.factor(shots_2014_season$PTS_TYPE)
shots_2014_season$FGM[shots_2014_season$SHOT_RESULT == "made"] <- 1
shots_2014_season$FGM[shots_2014_season$SHOT_RESULT == "missed"] <- 0
shots_2014_season$CLOSEST_DEFENDER_PLAYER_ID <- as.factor(shots_2014_season$CLOSEST_DEFENDER_PLAYER_ID)
shots_2014_playoffs$DATE <- as.Date(shots_2014_playoffs$DATE, format="%b %d, %Y")
shots_2014_playoffs$SHOOTER_ID <- as.factor(shots_2014_playoffs$SHOOTER_ID)
shots_2014_playoffs$LOCATION <- as.factor(shots_2014_playoffs$LOCATION)
shots_2014_playoffs$PERIOD <- as.factor(shots_2014_playoffs$PERIOD)
shots_2014_playoffs$GAME_CLOCK_PERIOD <- as.factor(shots_2014_playoffs$GAME_CLOCK_PERIOD)
shots_2014_playoffs$PTS_TYPE <- as.factor(shots_2014_playoffs$PTS_TYPE)
shots_2014_playoffs$FGM[shots_2014_playoffs$SHOT_RESULT == "made"] <- 1
shots_2014_playoffs$FGM[shots_2014_playoffs$SHOT_RESULT == "missed"] <- 0
shots_2014_playoffs$CLOSEST_DEFENDER_PLAYER_ID <- as.factor(shots_2014_playoffs$CLOSEST_DEFENDER_PLAYER_ID)

shots_2014_season$SHOT_CLOCK[is.na(shots_2014_season$SHOT_CLOCK) & shots_2014_season$GAME_CLOCK <= 24] <- 0
shots_2014_playoffs$SHOT_CLOCK[is.na(shots_2014_playoffs$SHOT_CLOCK) & shots_2014_playoffs$GAME_CLOCK <= 24] <- 0

# Split data into training and validation sets
all_star_2014_date <- as.Date("2014-02-15")
shots_2014_training <- subset(shots_2014_season, DATE < all_star_2014_date)
shots_2014_validation <- subset(shots_2014_season, DATE > all_star_2014_date)

# Replace missing values
training_set_mean <- mean(shots_2014_training$SHOT_CLOCK, na.rm = TRUE)
shots_2014_training$SHOT_CLOCK[is.na(shots_2014_training$SHOT_CLOCK) & shots_2014_training$GAME_CLOCK > 24] <- training_set_mean
shots_2014_validation$SHOT_CLOCK[is.na(shots_2014_validation$SHOT_CLOCK) & shots_2014_validation$GAME_CLOCK > 24] <- training_set_mean

shots_2014_playoffs$SHOT_CLOCK[is.na(shots_2014_playoffs$SHOT_CLOCK) & shots_2014_playoffs$GAME_CLOCK > 24] <- mean(shots_2014_season$SHOT_CLOCK, na.rm = TRUE)

log_loss <- function(y, p) {
  stopifnot(length(y) == length(p), all(y == 0 || y == 1), all(p > 0), all(p < 1))
  return(sum((y - 1) * log(1 - p) - y * log(p)) / length(y))
}

library(lme4)

variables <- c("SHOT_DIST", "CLOSE_DEF_DIST", "LOCATION", "TOUCH_TIME", "DRIBBLES", "GAME_CLOCK_PERIOD", "PTS_TYPE",
               "SHOOTER_ID", "CLOSEST_DEFENDER_PLAYER_ID", "FGM")

fit <- glmer(FGM ~ SHOT_DIST + CLOSE_DEF_DIST + LOCATION + 
                   TOUCH_TIME + DRIBBLES + GAME_CLOCK_PERIOD + PTS_TYPE + 
                   (1|SHOOTER_ID) + (1|CLOSEST_DEFENDER_PLAYER_ID), 
             shots_2014_training, family = binomial(logit), nAGQ = 0)

fit_validation_loss <- log_loss(shots_2014_validation$FGM, 
                                predict(fit, shots_2014_validation, 
                                        allow.new.levels = TRUE, type = "response"))

fit2 <- glmer(FGM ~ SHOT_DIST * CLOSE_DEF_DIST * LOCATION *
                    TOUCH_TIME * DRIBBLES * GAME_CLOCK_PERIOD * PTS_TYPE + 
                    (1|SHOOTER_ID) + (1|CLOSEST_DEFENDER_PLAYER_ID), 
              shots_2014_training, family = binomial(logit), nAGQ = 0)

fit2_validation_loss <- log_loss(shots_2014_validation$FGM, 
                                 predict(fit2, shots_2014_validation, 
                                         allow.new.levels = TRUE, type = "response"))

fit3 <- glmer(FGM ~ SHOT_DIST + CLOSE_DEF_DIST + LOCATION + 
                    TOUCH_TIME + DRIBBLES + GAME_CLOCK_PERIOD + PTS_TYPE +
                    (1 + SHOT_DIST|SHOOTER_ID) + (1 + SHOT_DIST|CLOSEST_DEFENDER_PLAYER_ID), 
              shots_2014_training, family = binomial(logit), nAGQ = 0)

fit3_validation_loss <- log_loss(shots_2014_validation$FGM, 
                                 predict(fit3, shots_2014_validation, 
                                         allow.new.levels = TRUE, type = "response"))

fit4 <- glmer(FGM ~ SHOT_DIST * CLOSE_DEF_DIST * LOCATION *
                    TOUCH_TIME * DRIBBLES * GAME_CLOCK_PERIOD * PTS_TYPE + 
                    (1 + SHOT_DIST|SHOOTER_ID) + (1 + SHOT_DIST|CLOSEST_DEFENDER_PLAYER_ID), 
              shots_2014_training, family = binomial(logit), nAGQ = 0)

fit4_validation_loss <- log_loss(shots_2014_validation$FGM, 
                                 predict(fit4, shots_2014_validation, 
                                         allow.new.levels = TRUE, type = "response"))

fit5 <- glmer(FGM ~ SHOT_DIST + CLOSE_DEF_DIST + LOCATION + 
                    TOUCH_TIME + DRIBBLES + GAME_CLOCK_PERIOD + PTS_TYPE +
                    (1 + SHOT_DIST + CLOSE_DEF_DIST|SHOOTER_ID) + 
                    (1 + SHOT_DIST + CLOSE_DEF_DIST|CLOSEST_DEFENDER_PLAYER_ID), 
              shots_2014_training, family = binomial(logit), nAGQ = 0)

fit5_validation_loss <- log_loss(shots_2014_validation$FGM, 
                                 predict(fit5, shots_2014_validation, 
                                         allow.new.levels = TRUE, type = "response"))

fit6 <- glmer(FGM ~ SHOT_DIST * CLOSE_DEF_DIST * LOCATION *
                    TOUCH_TIME * DRIBBLES * GAME_CLOCK_PERIOD * PTS_TYPE + 
                    (1 + SHOT_DIST + CLOSE_DEF_DIST|SHOOTER_ID) + 
                    (1 + SHOT_DIST + CLOSE_DEF_DIST|CLOSEST_DEFENDER_PLAYER_ID), 
              shots_2014_training, family = binomial(logit), nAGQ = 0)

fit6_validation_loss <- log_loss(shots_2014_validation$FGM, 
                                 predict(fit6, shots_2014_validation, 
                                         allow.new.levels = TRUE, type = "response"))

final_fit <- glmer(FGM ~ SHOT_DIST * CLOSE_DEF_DIST * LOCATION *
                         TOUCH_TIME * DRIBBLES * GAME_CLOCK_PERIOD * PTS_TYPE + 
                         (1 + SHOT_DIST + CLOSE_DEF_DIST|SHOOTER_ID) + 
                         (1 + SHOT_DIST + CLOSE_DEF_DIST|CLOSEST_DEFENDER_PLAYER_ID), 
                   shots_2014_season, family = binomial(logit), nAGQ = 0)

test_loss <- log_loss(shots_2014_playoffs$FGM, 
                      predict(final_fit, shots_2014_playoffs, 
                              allow.new.levels = TRUE, type = "response"))
