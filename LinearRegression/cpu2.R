#import libraries
#install.packages("carData")
#install.packages("car")
library(car)
library(carData)

#read the file
filename <- file.choose()
cpu <- read.csv(filename, header=TRUE, sep=",")

dim(cpu) #dimensions
length(cpu[is.na(cpu)]) #check for zeros

# Assign names to each of the attributes.
colnames(cpu) = c("Vendor",    # CPU Vendor name
                  "Model",     # CPU Model Name
                  "MYCT",      # Machine cycle time in nanoseconds (integer)
                  "MMIN",      # Minimum main memory in kilobytes (integer)
                  "MMAX",      # Maximum main memory in kilobytes (integer)
                  "CACH",      # Cache memory in kilobytes (integer)
                  "CHMIN",     # Minimum channels in units (integer)
                  "CHMAX",     # Maximum channels in units (integer)
                  "PRP",       # Independently published relative performance (Overall CPU Measurement) (integer)
                  "ERP")       # Estimated relative performance using Linear Regression (integer)

str(cpu)
summary(cpu)
summary(cpu$Vendor)

# Now lets check amdahl specific rows for uniqueness
c("Total CPUs (amdahl) : ", nrow(cpu[cpu$Vendor== "amdahl",3:8]), 
  "Uniquely specified CPUs (amdahl) : ", nrow(unique(cpu[cpu$Vendor== "amdahl",3:8])))

# Now lets check apollo specific rows for uniqueness
c("Total CPUs (apollo) : ", nrow(cpu[cpu$Vendor== "apollo",3:8]), 
  "Uniquely specified CPUs (apollo) : ", nrow(unique(cpu[cpu$Vendor== "apollo",3:8])))

# Now lets check basf specific rows for uniqueness
c("Total CPUs (basf) : ", nrow(cpu[cpu$Vendor== "basf",3:8]), 
  "Uniquely specified CPUs (basf) : ", nrow(unique(cpu[cpu$Vendor== "basf",3:8])))

# Now lets check bti specific rows for uniqueness
c("Total CPUs (bti) : ", nrow(cpu[cpu$Vendor== "bti",3:8]), 
  "Uniquely specified CPUs (bti) : ", nrow(unique(cpu[cpu$Vendor== "bti",3:8])))

# Now lets check burroughs specific rows for uniqueness
c("Total CPUs (burroughs) : ", nrow(cpu[cpu$Vendor== "burroughs",3:8]), 
  "Uniquely specified CPUs (burroughs) : ", nrow(unique(cpu[cpu$Vendor== "burroughs",3:8])))

# Now lets check c.r.d specific rows for uniqueness
c("Total CPUs (c.r.d) : ", nrow(cpu[cpu$Vendor== "c.r.d",3:8]), 
  "Uniquely specified CPUs (c.r.d) : ", nrow(unique(cpu[cpu$Vendor== "c.r.d",3:8])))

# Now lets check cambex specific rows for uniqueness
c("Total CPUs (cambex) : ", nrow(cpu[cpu$Vendor== "cambex",3:8]), 
  "Uniquely specified CPUs (cambex) : ", nrow(unique(cpu[cpu$Vendor== "cambex",3:8])))

# Now lets check Cdc specific rows for uniqueness
c("Total CPUs (cdc) : ", nrow(cpu[cpu$Vendor== "cdc",3:8]), 
  "Uniquely specified CPUs (cdc) : ", nrow(unique(cpu[cpu$Vendor== "cdc",3:8])))

# Now lets check dec specific rows for uniqueness
c("Total CPUs (dec) : ", nrow(cpu[cpu$Vendor== "dec",3:8]), 
  "Uniquely specified CPUs (dec) : ", nrow(unique(cpu[cpu$Vendor== "dec",3:8])))

# Now lets check dg specific rows for uniqueness
c("Total CPUs (dg) : ", nrow(cpu[cpu$Vendor== "dg",3:8]), 
  "Uniquely specified CPUs (dg) : ", nrow(unique(cpu[cpu$Vendor== "dg",3:8])))

# Now lets check formation specific rows for uniqueness
c("Total CPUs (formation) : ", nrow(cpu[cpu$Vendor== "formation",3:8]), 
  "Uniquely specified CPUs (formation) : ", nrow(unique(cpu[cpu$Vendor== "formation",3:8])))

# Now lets check gould specific rows for uniqueness
c("Total CPUs (gould) : ", nrow(cpu[cpu$Vendor== "gould",3:8]), 
  "Uniquely specified CPUs (gould) : ", nrow(unique(cpu[cpu$Vendor== "gould",3:8])))

# Now lets check harris specific rows for uniqueness
c("Total CPUs (harris) : ", nrow(cpu[cpu$Vendor== "harris",3:8]), 
  "Uniquely specified CPUs (harris) : ", nrow(unique(cpu[cpu$Vendor== "harris",3:8])))

# Now lets check honeywell specific rows for uniqueness
c("Total CPUs (honeywell) : ", nrow(cpu[cpu$Vendor== "honeywell",3:8]), 
  "Uniquely specified CPUs (honeywell) : ", nrow(unique(cpu[cpu$Vendor== "honeywell",3:8])))

# Now lets check hp specific rows for uniqueness
c("Total CPUs (hp) : ", nrow(cpu[cpu$Vendor== "hp",3:8]), 
  "Uniquely specified CPUs (hp) : ", nrow(unique(cpu[cpu$Vendor== "hp",3:8])))

# Now lets check ibm specific rows for uniqueness
c("Total CPUs (ibm) : ", nrow(cpu[cpu$Vendor== "ibm",3:8]), 
  "Uniquely specified CPUs (ibm) : ", nrow(unique(cpu[cpu$Vendor== "ibm",3:8])))

# Now lets check ipl specific rows for uniqueness
c("Total CPUs (ipl) : ", nrow(cpu[cpu$Vendor== "ipl",3:8]), 
  "Uniquely specified CPUs (ipl) : ", nrow(unique(cpu[cpu$Vendor== "ipl",3:8])))

# Now lets check magnuson specific rows for uniqueness
c("Total CPUs (magnuson) : ", nrow(cpu[cpu$Vendor== "magnuson",3:8]), 
  "Uniquely specified CPUs (magnuson) : ", nrow(unique(cpu[cpu$Vendor== "magnuson",3:8])))

# Now lets check nas specific rows for uniqueness
c("Total CPUs (nas) : ", nrow(cpu[cpu$Vendor== "nas",3:8]), 
  "Uniquely specified CPUs (nas) : ", nrow(unique(cpu[cpu$Vendor== "nas",3:8])))

# Now lets check ncr specific rows for uniqueness
c("Total CPUs (ncr) : ", nrow(cpu[cpu$Vendor== "ncr",3:8]), 
  "Uniquely specified CPUs (ncr) : ", nrow(unique(cpu[cpu$Vendor== "ncr",3:8])))

# Now lets check nixdorf specific rows for uniqueness
c("Total CPUs (nixdorf) : ", nrow(cpu[cpu$Vendor== "nixdorf",3:8]), 
  "Uniquely specified CPUs (nixdorf) : ", nrow(unique(cpu[cpu$Vendor== "nixdorf",3:8])))

# Now lets check perkin-elmer  specific rows for uniqueness
c("Total CPUs (perkin-elmer) : ", nrow(cpu[cpu$Vendor== "perkin-elmer",3:8]), 
  "Uniquely specified CPUs (perkin-elmer) : ", nrow(unique(cpu[cpu$Vendor== "perkin-elmer",3:8])))

# Now lets check prime specific rows for uniqueness
c("Total CPUs (prime) : ", nrow(cpu[cpu$Vendor== "prime",3:8]), 
  "Uniquely specified CPUs (prime) : ", nrow(unique(cpu[cpu$Vendor== "prime",3:8])))

# Now lets check siemens specific rows for uniqueness
c("Total CPUs (siemens) : ", nrow(cpu[cpu$Vendor== "siemens",3:8]), 
  "Uniquely specified CPUs (siemens) : ", nrow(unique(cpu[cpu$Vendor== "siemens",3:8])))

# Now lets check sperry specific rows for uniqueness
c("Total CPUs (sperry) : ", nrow(cpu[cpu$Vendor== "sperry",3:8]), 
  "Uniquely specified CPUs (sperry) : ", nrow(unique(cpu[cpu$Vendor== "sperry",3:8])))

# Now lets check wang specific rows for uniqueness
c("Total CPUs (wang) : ", nrow(cpu[cpu$Vendor== "wang",3:8]), 
  "Uniquely specified CPUs (wang) : ", nrow(unique(cpu[cpu$Vendor== "wang",3:8])))

#Delete cols
cpu$Vendor<- NULL
cpu$Model <- NULL
cpu$ERP <- NULL

#plot cpu
plot (cpu, gap =0)

#scaling
summary(cpu)
cpu$MYCT<-cpu$MYCT/1500
cpu$MMIN<-cpu$MMIN/32000
cpu$MMAX<-cpu$MMAX/64000
cpu$CACH<-cpu$CACH/256
cpu$CHMIN<-cpu$CHMIN/52
cpu$CHMAX<-cpu$CHMAX/176
cpu$PRP<-cpu$PRP/1150

summary(cpu)
plot (cpu, gap =0)

#model1
model_simple <- lm(PRP ~ ., data = cpu)
print(summary(model_simple))


confidence_bands <- predict(model_simple,interval="confidence")
print(confidence_bands)

anova(model_simple) # anova table 
fitted(model_simple) # predicted values
coefficients(model_simple) # model coefficients
residuals(model_simple) # residuals
vcov(model_simple) # covariance matrix for model parameters 
influence(model_simple) # regression diagnostics
confint(model_simple, level=0.95) # CIs for model parameters 

plot (model_simple, gap =0)

residualPlots(model_simple)
avPlots(model_simple)
qqPlot(model_simple)
influenceIndexPlot(model_simple)
influencePlot(model_simple)


#feauture selection 
library(FSelector)
subset <- cfs(PRP ~ ., cpu)
f <- as.simple.formula(subset, "PRP")
print(f)

#model2
model2 <- lm(PRP ~ MMIN + MMAX + CACH + CHMAX, data = cpu)
print(summary(model2))


confidence_bands <- predict(model2,interval="confidence")
print(confidence_bands)

anova(model2) # anova table 
fitted(model2) # predicted values
coefficients(model2) # model coefficients
residuals(model2) # residuals
vcov(model2) # covariance matrix for model parameters 
influence(model2) # regression diagnostics
confint(model2, level=0.95) # CIs for model parameters 

plot (model2, gap =0)

#model3
model3 <- lm(PRP ~ MMIN * MMAX * CACH * CHMAX, data = cpu)
print(summary(model3))

confidence_bands <- predict(model3,interval="confidence")
print(confidence_bands)

anova(model3) # anova table 
fitted(model3) # predicted values
coefficients(model3) # model coefficients
residuals(model3) # residuals
vcov(model3) # covariance matrix for model parameters 
influence(model3) # regression diagnostics
confint(model3, level=0.95) # CIs for model parameters 

plot (model3, gap =0)

#model4
model4 <- lm(PRP ~ (MMIN + MMAX + CHMAX)*CACH , data = cpu)
print(summary(model4))


confidence_bands <- predict(model4,interval="confidence")
print(confidence_bands)

anova(model4) # anova table 
fitted(model4) # predicted values
coefficients(model4) # model coefficients
residuals(model4) # residuals
vcov(model4) # covariance matrix for model parameters 
influence(model4) # regression diagnostics
confint(model4, level=0.95) # CIs for model parameters 

plot (model4, gap =0)

#model5
model5 <- lm(PRP ~ (log2(MMIN) + log2(MMAX))*CACH , data = cpu)
print(summary(model5))


confidence_bands <- predict(model5,interval="confidence")
print(confidence_bands)

anova(model5) # anova table 
fitted(model5) # predicted values
coefficients(model5) # model coefficients
residuals(model5) # residuals
vcov(model5) # covariance matrix for model parameters 
influence(model5) # regression diagnostics
confint(model5, level=0.95) # CIs for model parameters 

plot (model5, gap =0)

#model6
model6 <- lm(PRP ~ log2(MMIN) + log2(MMAX) + CACH , data = cpu)
print(summary(model6))


confidence_bands <- predict(model6,interval="confidence")
print(confidence_bands)

anova(model6) # anova table 
fitted(model6) # predicted values
coefficients(model6) # model coefficients
residuals(model6) # residuals
vcov(model6) # covariance matrix for model parameters 
influence(model6) # regression diagnostics
confint(model6, level=0.95) # CIs for model parameters 

plot (model5, gap =0)


summary(model3)
plot (model3, gap =0)
residualPlots(model3)
avPlots(model3)
qqPlot(model3)
influenceIndexPlot(model3)
influencePlot(model3)
