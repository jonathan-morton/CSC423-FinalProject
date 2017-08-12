#Final Project - Jonathan Morton - Kevin Lao
install.packages("car")
require("car")

violations <- read.csv("Traffic_Violations.csv", header = TRUE)
violations <- violations[sample(nrow(violations), 300000), ]

keptColumns <- c("State", "Year", "Color", "Violation.Type", "Race", "Gender")
violations <- violations[keptColumns]

unique(violations$Gender)
unique(violations$Color)

summary(violations)

violationsCopy <- violations
violations <- violationsCopy

summary(violations)

#Replace SERO and ESERO with NA, not enough information about them and they only make up a small number of the million rows
violations[violations == "ESERO"] <- NA
violations[violations == "SERO"] <- NA
#Replace Gender U with NA

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

# Perform backward selection to pick best model.
fullModel <- glm(violations$Received.Citation ~
violations$Year +
    violations$Is.Male +
    violations$Race.black +
    violations$Race.white +
    violations$Race.asian +
    violations$Race.hispanic +
    violations$Race.native_american +
    violations$In.State,
data=violations, family=binomial(link="logit"))
summary(fullModel)
plot(x = predict(fullModel), y = residuals(fullModel))
cat("\nBackward Selection:\n")
modelNew <- step(fullModel, direction="backward")
print(summary(modelNew))

sum(modelNew$coefficients)
confint(modelNew)

exp(coef(modelNew))

cat("Predicted Values:\n")
print(fitted(modelNew, type="response"))

plot(x = predict(modelNew), y = residuals(modelNew))

#Influence Points
#influence.measures(modelNew)
