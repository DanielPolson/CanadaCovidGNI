library(astsa) 
library(forecast) 
library(ggplot2)

##########Read in data and create tables##########

GNI.read <- read.csv("~/Desktop/gni.csv", 
                     header = FALSE, stringsAsFactors = FALSE)

GNI.read.vals <- as.character(as.vector(GNI.read[2,])) 

GNI.read.gni <- as.numeric(gsub(",","",GNI.read.vals)) 

GNI.read.quar <- as.character(as.vector(GNI.read[1,]))

##Full dataset
GNI.df <- data.frame(Quar = GNI.read.quar, 
                     GNI = GNI.read.gni)

##Dataset pre covid 
GNI.df.nocov <- data.frame(Quar = GNI.read.quar[1:236], 
                           GNI = GNI.read.gni[1:236])

##Dataset post covid 
GNI.df.cov <- data.frame(Quar = GNI.read.quar[237:242], 
                         GNI = GNI.read.gni[237:242])

##Convert to time-series
GNI.ts <- ts(data = GNI.df$GNI, frequency = 4, 
             start = c(1961,1), end = c(2021,2))

GNI.ts.nocov <- ts(data = GNI.df.nocov$GNI, frequency = 4, 
                   start = c(1961,1), end = c(2019,4))

GNI.ts.cov <- ts(data = GNI.df.cov$GNI, frequency = 4, 
                 start = c(2020,1), end = c(2021,2))


####################Code for Figure 1####################

GNI.read.quar <- as.factor(as.vector(GNI.read[1,]))
GNI.read.quar.rev <- c() 

for(k in GNI.read.quar){
  GNI.read.quar.rev <- append(GNI.read.quar.rev, 
                              paste(rev(strsplit(k, " ")[[1]]), collapse = " "))
}

GNI.df.gra <- data.frame( Quar = GNI.read.quar.rev, 
                          GNI = GNI.read.gni, 
                          Linetype = append(rep("Pre-COVID", 236), rep("Post-COVID", 6)))

##Plot time series where line type changes when COVID starts 
ggplot(data = GNI.df.gra, aes(x = Quar, y = GNI, linetype = Linetype)) +
  geom_line( aes(group = Linetype) ) +
  scale_x_discrete(breaks = c("1961 Q1", "1971 Q1", "1981 Q1",
                              "1991 Q1", "2001 Q1", "2011 Q1", "2021 Q1")) +
  xlab("Quarter") + ylab("GNI/1000000 (CAD)") +
  theme(axis.title=element_text(size=14),
        axis.text=element_text(size=12),
        legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size=10),
        legend.title = element_text(size=14))

####################Code for figure 2####################

GNI.stat.ts.nocov <- diff(log(GNI.ts.nocov), differences = 2)
GNI.df.nocov.gra <- data.frame(Transformed_GNI = GNI.stat.ts.nocov, 
                               Quar = GNI.read.quar.rev[3:236])

##Plot time series where line type changes when COVID starts 
ggplot(data = GNI.df.nocov.gra, aes(x = Quar, y = Transformed_GNI)) +
  geom_line( aes(group = 1) ) +
  scale_x_discrete(breaks = c("1961 Q3", "1971 Q3", "1981 Q3", 
                              "1991 Q3", "2001 Q3", "2011 Q3", "2021 Q1")) +
  xlab("Quarter") + 
  ylab("Transformed GNI values") +
  theme(axis.title=element_text(size=14),
        axis.text=element_text(size=12))

####################Code for figure 3####################

par(mar=c(3,3,3,3),mfrow = c(1,2)) 
acf(GNI.stat.ts.nocov, lag.max = 30, main = "ACF") 
pacf(GNI.stat.ts.nocov, lag.max = 30, main = "PACF")

####################Code for table 1####################

K1 <- sarima(log(GNI.ts.nocov),0,2,1,no.constant = FALSE) 
K2 <- sarima(log(GNI.ts.nocov),0,2,2,no.constant = FALSE) 
K3 <- sarima(log(GNI.ts.nocov),1,2,1,no.constant = FALSE) 
K4 <- sarima(log(GNI.ts.nocov),1,2,2,no.constant = FALSE) 
K5 <- sarima(log(GNI.ts.nocov),2,2,2,no.constant = FALSE)

ARIMA.df <- data.frame(Model = c("ARIMA(0,2,1)", "ARIMA(0,2,2)", "ARIMA(1,2,1)", "ARIMA(1,2,2)", 
                                 "ARIMA(2,2,2)" ),
                       AIC = c(K1$AIC, K2$AIC, K3$AIC, K4$AIC, K5$AIC),
                       AICc = c(K1$AICc, K2$AICc, K3$AICc, K4$AICc, K5$AICc),
                       BIC = c(K1$BIC, K2$BIC, K3$BIC, K4$BIC, K5$BIC)) 

print(ARIMA.df)

####################Code for Equation 2 & 3####################

K3$ttable

####################Code for Figure 4####################

sarima(log(GNI.ts.nocov),1,2,1,no.constant = FALSE)

####################Code for Figure 5####################

model1 <- arima(log(GNI.ts.nocov), c(1,2,1)) 
fcast <- forecast(model1, h = 10)

##Find the log of the predicted values 
log.lower.80 <- fcast$lower[,1] 
log.lower.95 <- fcast$lower[,2] 
log.upper.80 <- fcast$upper[,1] 
log.upper.95 <- fcast$upper[,2] 
log.predict.line <- fcast$mean

##Function to remove log transformation 
unlog <- function(timser){
  
  vec <- as.vector(timser) 
  newvec <- c()
  
  for(k in vec){
    
    newvec <- append(newvec, exp(k)) }
  
  newts <- ts(newvec, start = c(2020,1), end = c(2022, 2), frequency = 4)
  
  return(newts) }

##Find the actual predicted values 
fcast$lower[,1] <- unlog(log.lower.80) 
fcast$lower[,2] <- unlog(log.lower.95) 
fcast$upper[,1] <- unlog(log.upper.80) 
fcast$upper[,2] <- unlog(log.upper.95) 
fcast$mean <- unlog(log.predict.line)

plot(fcast, xlim = c(2000,2022), ylim = c(500000,3000000), main = "", 
     xlab = "Time", ylab = "GNI")
lines(GNI.ts.cov, col = "red") 
lines(GNI.ts.nocov, col = "black")














