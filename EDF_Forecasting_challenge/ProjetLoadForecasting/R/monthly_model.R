#' Monthly linear model with AIC penalty
#'
#' Fit a linear model for each month based on the equation found with AIC penalty
#'
#' @param dataset the dataset on which the models are fitted
#' @param covid_period the subset of train set during COVID and the test set
#'
#' @return the forecasts computed with the different models on COVID period
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


monthly_model <- function(dataset = Data0[sel_a,], covid_period = rbind(Data0[sel_b,-2],Data1[,-20])){
  jan <- dataset[which(dataset$Month=="1"),]
  feb <- dataset[which(dataset$Month=="2"),]
  mar <- dataset[which(dataset$Month=="3"),]
  apr <- dataset[which(dataset$Month=="4"),]
  may <- dataset[which(dataset$Month=="5"),]
  jun <- dataset[which(dataset$Month=="6"),]
  jul <- dataset[which(dataset$Month=="7"),]
  aug <- dataset[which(dataset$Month=="8"),]
  sep <- dataset[which(dataset$Month=="9"),]
  oct <- dataset[which(dataset$Month=="10"),]
  nov <- dataset[which(dataset$Month=="11"),]
  dec <- dataset[which(dataset$Month=="12"),]
  jan_fit <- lm(formula = Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max +
                  Temp_s99_max + toy + WeekDays + BH + Christmas_break + Time +
                  Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max + Load.1:Temp_s99_max +
                  Load.1:toy + Load.1:WeekDays + Load.1:Christmas_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:Christmas_break + Temp:Temp_s95 +
                  Temp:Temp_s99_max + Temp:WeekDays + Temp:Christmas_break +
                  Temp_s95:Temp_s95_max + Temp_s95:Temp_s99_max + Temp_s95:toy +
                  Temp_s95:WeekDays + Temp_s95:Christmas_break + Temp_s95:Time +
                  Temp_s95_max:Temp_s99_max + Temp_s95_max:toy + Temp_s95_max:Christmas_break +
                  Temp_s95_max:Time + Temp_s99_max:toy + Temp_s99_max:WeekDays +
                  Temp_s99_max:BH + toy:WeekDays + toy:BH + toy:Christmas_break +
                  WeekDays:BH + WeekDays:Christmas_break + WeekDays:Time +
                  BH:Christmas_break + BH:Time,
                data = subset(jan, select = -c(DLS,Summer_break)))
  feb_fit <- lm(formula = Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max +
                  Temp_s99_max + toy + WeekDays + Time +
                  Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max + Load.1:Temp_s99_max +
                  Load.1:toy + Load.1:WeekDays + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Temp:Temp_s95 +
                  Temp:Temp_s99_max + Temp:WeekDays +
                  Temp_s95:Temp_s95_max + Temp_s95:Temp_s99_max + Temp_s95:toy +
                  Temp_s95:WeekDays + Temp_s95:Time +
                  Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Time + Temp_s99_max:toy + Temp_s99_max:WeekDays +
                  toy:WeekDays +
                  WeekDays:Time,
                data = subset(feb, select = -c(DLS, Summer_break,Christmas_break,BH)))
  mar_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + WeekDays + BH + DLS +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:WeekDays + Load.1:DLS +
                  Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:DLS +
                   Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:WeekDays + Temp:DLS + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy + Temp_s95:WeekDays +
                  Temp_s95:DLS +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:WeekDays + Temp_s99_max:BH + Temp_s99_max:DLS +
                  toy:WeekDays + toy:BH + toy:DLS +
                  WeekDays:BH + WeekDays:DLS +
                  WeekDays:Time + BH:DLS +
                  BH:Time,
                data= subset(mar, select = -c(Christmas_break, Summer_break)))
  apr_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + WeekDays + BH +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:WeekDays +
                  Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max +
                  Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:WeekDays + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy + Temp_s95:WeekDays +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:WeekDays + Temp_s99_max:BH +
                  toy:WeekDays + toy:BH + WeekDays:BH +
                  WeekDays:Time +
                  BH:Time,
                data= subset(apr, select = -c(DLS, Summer_break,Christmas_break)))
  may_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + WeekDays + BH +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:WeekDays +
                  Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max +
                  Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:WeekDays + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy + Temp_s95:WeekDays +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:WeekDays + Temp_s99_max:BH +
                  toy:WeekDays + toy:BH+ WeekDays:BH +
                  WeekDays:Time +
                  BH:Time,
                data= subset(may, select = -c(DLS, Summer_break,Christmas_break)))
  jun_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + WeekDays + BH +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:WeekDays +
                  Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max +
                  Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:WeekDays + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy + Temp_s95:WeekDays +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:WeekDays + Temp_s99_max:BH +
                  toy:WeekDays + toy:BH +
                WeekDays:BH +
                  WeekDays:Time +
                  BH:Time,
                data= subset(jun, select = -c(DLS, Summer_break,Christmas_break)))
  jul_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + WeekDays + BH + Summer_break +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:WeekDays +
                  Load.1:Summer_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:Summer_break +
                  Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:WeekDays + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy + Temp_s95:WeekDays +
                   Temp_s95:Summer_break  +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                   Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:WeekDays + Temp_s99_max:BH +
                  toy:WeekDays + toy:BH + toy:Summer_break +
                  WeekDays:BH + WeekDays:Summer_break +
                  WeekDays:Time + BH:Summer_break +
                  BH:Time + Summer_break:Time,
                data= subset(jul, select = -c(DLS, Christmas_break)))
  aug_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + WeekDays + BH + Summer_break +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:WeekDays +
                  Load.1:Summer_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:Summer_break +
                  Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:WeekDays + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy + Temp_s95:WeekDays +
                  Temp_s95:Summer_break  +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:WeekDays + Temp_s99_max:BH +
                  toy:WeekDays + toy:BH + toy:Summer_break +
                  WeekDays:BH + WeekDays:Summer_break +
                  WeekDays:Time + BH:Summer_break +
                  BH:Time + Summer_break:Time,
                data= subset(aug, select = -c(DLS, Christmas_break)))
  sep_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + WeekDays + Summer_break +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:WeekDays +
                  Load.1:Summer_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:Summer_break +
                  Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:WeekDays + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy + Temp_s95:WeekDays +
                  Temp_s95:Summer_break  +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:WeekDays +
                  toy:WeekDays + toy:Summer_break +
                  WeekDays:Summer_break +
                  WeekDays:Time +
                  Summer_break:Time,
                data= subset(sep, select = -c(DLS, BH, Christmas_break)))
  oct_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + WeekDays + DLS+
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:WeekDays + Load.1:DLS +
                  Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:DLS + Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:WeekDays + Temp:DLS + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy + Temp_s95:WeekDays +
                  Temp_s95:DLS +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:WeekDays + Temp_s99_max:DLS +
                  toy:WeekDays + toy:DLS + WeekDays:DLS +
                  WeekDays:Time,
                data= subset(oct, select = -c(Summer_break, BH, Christmas_break)))
  nov_fit <- lm(Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
                  toy + WeekDays + BH +
                  Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
                  Load.1:Temp_s99_max + Load.1:toy + Load.1:WeekDays +
                  Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max +
                  Temp:Temp_s95 + Temp:Temp_s99_max +
                  Temp:WeekDays + Temp_s95:Temp_s95_max +
                  Temp_s95:Temp_s99_max + Temp_s95:toy + Temp_s95:WeekDays +
                  Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
                  Temp_s95_max:Time + Temp_s99_max:toy +
                  Temp_s99_max:WeekDays + Temp_s99_max:BH +
                  toy:WeekDays + toy:BH +
                WeekDays:BH +
                  WeekDays:Time +
                  BH:Time,
                data= subset(nov, select = -c(DLS, Summer_break,Christmas_break)))
  dec_fit <- lm(formula = Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max +
                  Temp_s99_max + toy + WeekDays + BH + Christmas_break + Time +
                  Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max + Load.1:Temp_s99_max +
                  Load.1:toy + Load.1:WeekDays + Load.1:Christmas_break + Load.7:Temp_s95_max +
                  Load.7:Temp_s99_max + Load.7:Christmas_break + Temp:Temp_s95 +
                  Temp:Temp_s99_max + Temp:WeekDays + Temp:Christmas_break +
                  Temp_s95:Temp_s95_max + Temp_s95:Temp_s99_max + Temp_s95:toy +
                  Temp_s95:WeekDays + Temp_s95:Christmas_break + Temp_s95:Time +
                  Temp_s95_max:Temp_s99_max + Temp_s95_max:toy + Temp_s95_max:Christmas_break +
                  Temp_s95_max:Time + Temp_s99_max:toy + Temp_s99_max:WeekDays +
                  Temp_s99_max:BH + toy:WeekDays + toy:BH + toy:Christmas_break +
                  WeekDays:BH + WeekDays:Christmas_break + WeekDays:Time +
                  BH:Christmas_break + BH:Time,
                data = subset(dec, select = -c(DLS,Summer_break)))
  jan_forecast <- cbind(predict(jan_fit, newdata = covid_period[which(covid_period$Month=="1"),]),covid_period[which(covid_period$Month=="1"),]$Time)
  feb_forecast <- cbind(predict(feb_fit, newdata = covid_period[which(covid_period$Month=="2"),]),covid_period[which(covid_period$Month=="2"),]$Time)
  mar_forecast <- cbind(predict(mar_fit, newdata = covid_period[which(covid_period$Month=="3"),]),covid_period[which(covid_period$Month=="3"),]$Time)
  apr_forecast <- cbind(predict(apr_fit, newdata = covid_period[which(covid_period$Month=="4"),]),covid_period[which(covid_period$Month=="4"),]$Time)
  may_forecast <- cbind(predict(may_fit, newdata = covid_period[which(covid_period$Month=="5"),]),covid_period[which(covid_period$Month=="5"),]$Time)
  jun_forecast <- cbind(predict(jun_fit, newdata = covid_period[which(covid_period$Month=="6"),]),covid_period[which(covid_period$Month=="6"),]$Time)
  jul_forecast <- cbind(predict(jul_fit, newdata = covid_period[which(covid_period$Month=="7"),]),covid_period[which(covid_period$Month=="7"),]$Time)
  aug_forecast <- cbind(predict(aug_fit, newdata = covid_period[which(covid_period$Month=="8"),]),covid_period[which(covid_period$Month=="8"),]$Time)
  sep_forecast <- cbind(predict(sep_fit, newdata = covid_period[which(covid_period$Month=="9"),]),covid_period[which(covid_period$Month=="9"),]$Time)
  oct_forecast <- cbind(predict(oct_fit, newdata = covid_period[which(covid_period$Month=="10"),]),covid_period[which(covid_period$Month=="10"),]$Time)
  nov_forecast <- cbind(predict(nov_fit, newdata = covid_period[which(covid_period$Month=="11"),]),covid_period[which(covid_period$Month=="11"),]$Time)
  dec_forecast <- cbind(predict(dec_fit, newdata = covid_period[which(covid_period$Month=="12"),]),covid_period[which(covid_period$Month=="12"),]$Time)
  forecast <- rbind(jan_forecast, feb_forecast, mar_forecast, apr_forecast, may_forecast, jun_forecast,
                    jul_forecast, aug_forecast, sep_forecast, oct_forecast, nov_forecast, dec_forecast)
  forecast <- forecast[order(forecast[,2]),1]
  return(forecast)
}




