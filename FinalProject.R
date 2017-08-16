#Final Project - Jonathan Morton - Kevin Lao
#install.packages("car")
install.packages("caret")
install.packages("faraway")
install.packages("dplyr")
require("car")
require("faraway")
require("caret")
require("dplyr")

violationsFull <- read.csv("Traffic_Violations.csv", header = TRUE)
violations <- violationsFull #TODO DELETE ME
#violations <- violationsFull[sample(nrow(violationsFull), 300000), ]
#write.csv(violations, "traffic_violations_sample.csv")

# violations <- read.csv("traffic_violations_sample.csv", header = TRUE)
keptColumns <- c("State", "Year", "Color", "Violation.Type", "Race", "Gender")
violations <- violations[keptColumns]
summary(violations)


#Data exploration and clean up
unique(violations$Gender)
unique(violations$Color)

#TODO comment out for final report
violationsCopy <- violations
violations <- violationsCopy

#Replace SERO and ESERO with NA, not enough information about them and they only make up a small number of the million rows
violations[violations == "ESERO"] <- NA
violations[violations == "SERO"] <- NA

violations[!complete.cases(violations),]
#Year has NA
for(i in 1:ncol(violations)) {
    if(is.factor(violations[,i])) {
        if("No" %in% violations[,i]){
            levels(violations[,i]) <- c(levels(violations[,i]),0)
        }
        if("Yes" %in% violations[,i]){
            levels(violations[,i]) <- c(levels(violations[,i]),1)
        }
    }
}
violations[violations == "No"] <- 0
violations[violations == "Yes"] <- 1
violations[violations == "N/A"] <- NA
violations[violations == "XX"] <- NA
violations$Gender[violations$Gender == "U"] <- NA
violations[violations == ""] <- NA

is_male_dummy <- as.numeric(violations$Gender == "M")
violations["Is.Male"] <- is_male_dummy

is_citation_dummy <- as.numeric(violations$Violation.Type == "Citation")
violations["Received.Citation"] <- is_citation_dummy

in_state_dummy <- as.numeric(violations$State == "MD")
violations["In.State"] <- in_state_dummy

#add bright color category and create bright color column
bright <- c("RED","PINK","YELLOW","MULTICOLOR","GOLD",'ORANGE')

is.bright <- as.numeric(violations$Color %in% bright)

sort(unique(violations$Year))
violations$Year[violations$Year < 1920] <- NA
violations$Year[violations$Year > 2017] <- NA

unique(violations$Race)
race_black <- as.numeric(violations$Race == "BLACK")
race_white <- as.numeric(violations$Race == "WHITE")
race_asian <- as.numeric(violations$Race == "ASIAN")
race_hispanic <- as.numeric(violations$Race == "HISPANIC")
race_native_american <- as.numeric(violations$Race == "NATIVE AMERICAN")
violations["Race.black"] <- race_black
violations["Race.white"] <- race_white
violations["Race.asian"] <- race_asian
violations["Race.hispanic"] <- race_hispanic
violations["Race.native_american"] <- race_native_american
violations["is.bright"] <- is.bright

#Remove columns with dummy variables
unneeded_columns <- c("State", "Color", "Race", "Gender", "Violation.Type")
keptColumns <- setdiff(names(violations), unneeded_columns)
violations <- violations[keptColumns]
#Drop unused factors
violations[] <- lapply(violations, function(x) if(is.factor(x)) factor(x) else x)
summary(violations)
sd(violations$Year, na.rm = TRUE)

#Checks the count of is.bright column specifically
length(which(!is.na(violations$is.bright)))

#Split into a training and test set
# sampleSize <- sample(nrow(violations), size = as.integer(nrow(violations) * (2/3)))
# violationsTesting <- violations[-sampleSize, ]
# violations <- violations[sampleSize, ]

# Perform backward selection to pick best model.
fullModel <- glm(Received.Citation ~ Year +
                   is.bright +
                   Is.Male +
                   Race.black +
                   Race.white +
                   Race.asian +
                   Race.hispanic +
                   Race.native_american +
                   In.State,
                 data=violations, 
                 family=binomial(link="logit"),
                 na.action = na.exclude)
summary(fullModel)
print(vif(fullModel))

noRaceModel <- glm(Received.Citation ~ Year +
                     is.bright +
                     Is.Male +
                     In.State,
                   data=violations, family=binomial(link="logit"))
summary(noRaceModel)
print(vif(noRaceModel))

# plot(x = predict(fullModel), y = residuals(fullModel))
cat("\nBackward Selection:\n")
modelNew <- step(fullModel, direction="backward")
print(summary(modelNew))
modelSummary <- summary(modelNew)
print(vif(modelNew))

blackMaleOldCarOutOfState <- data.frame(Year = 1990, Is.Male = 1, Race.black = 1, Race.white = 0, Race.asian = 0, Race.hispanic = 0, In.State = 0)
asianFemaleNewCarInState <- data.frame(Year = 2017, Is.Male = 0, Race.black = 0, Race.white = 0, Race.asian = 1, Race.hispanic = 0, In.State = 1)
hispanicMaleNewCarOutOfState <- data.frame(Year = 2017, Is.Male = 1, Race.black = 0, Race.white = 0, Race.asian = 0, Race.hispanic = 1, In.State = 0)
hispanicMaleOldCarOutOfState <- data.frame(Year = 1990, Is.Male = 1, Race.black = 0, Race.white = 0, Race.asian = 0, Race.hispanic = 1, In.State = 0)
whiteMaleNewCarOutOfState <- data.frame(Year = 2017, Is.Male = 1, Race.black = 0, Race.white = 1, Race.asian = 0, Race.hispanic = 0, In.State = 0)
whiteFemaleNewCarOutOfState <- data.frame(Year = 2017, Is.Male = 0, Race.black = 0, Race.white = 1, Race.asian = 0, Race.hispanic = 0, In.State = 0)

predict(modelNew, blackMaleOldCarOutOfState, type="response")
predict(modelNew, asianFemaleNewCarInState, type="response")
predict(modelNew, hispanicMaleNewCarOutOfState, type="response")
predict(modelNew, hispanicMaleOldCarOutOfState, type="response")
predict(modelNew, whiteMaleNewCarOutOfState, type="response")
predict(modelNew, whiteFemaleNewCarOutOfState, type="response")

linpred <- predict(modelNew)
violationsResiduals <- residuals(modelNew)

probs <- exp(linpred) / (1+exp(linpred))
probs

violationsCopy <- mutate(violationsCopy, residuals=residuals(modelNew), linpred = predict(modelNew))
violationsBins <- group_by(violationsCopy, cut(linpred, breaks=unique(quantile(linpred, (1:10000 / 10001), na.rm=TRUE))))
diagdf <- summarise(violationsBins, residuals=mean(residuals, na.rm = TRUE), linpred = mean(linpred, na.rm = TRUE))
plot(residuals ~ linpred, diagdf, xlab = "Linear Predictor", ylab="Residuals", main= "Residuals vs Linear Predictors")
abline(h = 0, col = "red", lty = "dashed")

# halfnorm(residuals(modelNew))
#
# confint(modelNew)
#
# exp(coef(modelNew))

cat("Predicted Values:\n")
predictions <- predict(modelNew, newdata=violationsTesting)
residualsTest <- violationsTesting$Received.Citation - predictions
print(residualsTest)


#Influence Points
influence.measures(modelNew)