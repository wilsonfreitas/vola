
as.volatilitysmile <- function() {

}

smile_fit <- function() {

}

#
# Volatility Structure
#
# Example of a volatility/variance/sigma table
#
# > head(varianceTable)
#      BusinessDays Delta     Variance
# 1081           10  0.99 3.968254e-10
# 1082           10  0.90 3.968254e-10
# 1083           10  0.75 3.968254e-10
# 1084           10  0.63 3.968254e-10
# 1085           10  0.50 3.968254e-10
# 1086           10  0.37 3.968254e-10
# 1087           10  0.25 3.968254e-10
# 1088           10  0.10 1.587302e-09
# 1089           10  0.01 1.587302e-09
#
VolatilityStructure <- function(varianceTable) {
  that <- list()

  .smileBlocks <- split(varianceTable, varianceTable$BusinessDays)
  that$varianceSurface <- sapply(.smileBlocks, function(block) {
    var <- block$Variance
    names(var) <- block$Delta * 100
    var
  })
  that$terms <- as.numeric(colnames(that$varianceSurface))
  that$levels <- as.numeric(rownames(that$varianceSurface))
  that$interpolateAtTerm <- function(term) {
    smileInterpolated <- apply(that$varianceSurface, 1, function(.) {
      approx(that$terms, ., xout = term)$y
    })
    smileInterpolated
  }
  that$generateSmileFunction <- function(smile) {
    xy <- xy.coords(
      x = that$levels,
      y = as.numeric(smile)
    )
    splinefun(xy, method = "monoH.FC")
  }
  that$interpolate <- function(delta, term) {
    smileFUN <- that$generateSmileFunction(that$interpolateAtTerm(term))
    smileFUN(delta)
  }
  that$interpolateToOption <- function(Underlying, Strike, Term, Rate) {
    smile <- that$interpolateAtTerm(Term * 252)
    smileFUN <- that$generateSmileFunction(smile)

    bisect <- function(delta) {
      Sigma <- sqrt(smileFUN(delta) / Term)
      delta2 <- GBSGreeks(
        Selection = "delta", TypeFlag = "c",
        S = Underlying, X = Strike, Time = Term,
        r = Rate, b = 0, sigma = Sigma
      )
      if (abs(delta - delta2) > 1e-6) {
        bisect(delta2)
      } else {
        list(Sigma = Sigma, Delta = delta)
      }
    }

    bisect(0.5)
  }

  class(that) <- "VolatilityStructure"
  that
}

.test <- function() {
  date <- "2013-07-18"
  maturityDate <- "2013-10-10"
  businessDays <- bizdays(date, maturityDate)

  .VID <- bmfh.get.data("VID")
  .vse <- subset(.VID, dt_ref == date)
  .vse <- transform(.vse,
    Delta = f_delta / 100,
    Sigma = f_taxa_m,
    BusinessDays = bizdays(date, d_vencimento)
  )
  .vse <- transform(.vse, Term = BusinessDays / 252)
  .vse <- transform(.vse, Volatility = Sigma * sqrt(Term))
  .vse <- transform(.vse, Variance = Volatility^2)
  varianceTable <- subset(.vse, select = c(BusinessDays, Delta, Variance))

  vol <- VolatilityStructure(varianceTable)
  print(vol$interpolateAtTerm(60))
  print(vol$interpolate(55, 60))
  print(vol$interpolateToOption(267000, 266953.56, 60 / 252, 0.085))
}

# .test()