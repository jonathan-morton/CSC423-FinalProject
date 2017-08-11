#Final Project - Jonathan Morton - Kevin Lao

violations <- read.csv("Traffic_Violations.csv", header = TRUE)
keptColumns <- c("Time.Of.Stop", "Accident", "Belts", "Personal.Injury", "Property.Damage", "Fatal", "Commercial.License",
                 "HAZMAT", "Commercial.Vehicle", "Alcohol", "Work.Zone", "State", "Year", "Color", "Violation.Type",
                 "Contributed.To.Accident", "Race", "Gender")
violations <- violations[keptColumns]

unique(violations$Gender)
unique(violations$Race)
unique(violations$Color)

summary(violations)

#Remove accident since all are No
keptColumns <- c("Time.Of.Stop", "Belts", "Personal.Injury", "Property.Damage", "Fatal", "Commercial.License",
                 "HAZMAT", "Commercial.Vehicle", "Alcohol", "Work.Zone", "State", "Year", "Color", "Violation.Type",
                 "Contributed.To.Accident", "Race", "Gender")
violations <- violations[keptColumns]
violationsCopy <- violations

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

stateValues <- unique(violations$State)
violations[violations == ""] <- NA

is_male_dummy <- as.numeric(violations$Gender == "M")
is_citation_dummy <- as.numeric(violations$Violation.Type == "Citation")

#Drop unused factors
violations[] <- lapply(violations, function(x) if(is.factor(x)) factor(x) else x)
