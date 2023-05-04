package com.komsonandmarch.patternmatching
import java.time.LocalDate


trait Security {
  def ticker: String
  def price: Double
  def quantity: Int
  def value: Double
  override def toString: String = s"$ticker @$price x $quantity (Value: $$${value})"
}

abstract class Bond(ticker: String, price: Double, quantity: Int, val couponRate: Double, val yearsToMaturity: Int) extends Security {
  val value: Double = calculateValue()

  def calculateValue(): Double = {
    price * quantity * (couponRate / 100.0)
  }

  override def toString: String = {
    s"$ticker @$price x $quantity ($couponRate% $yearsToMaturity years to maturity, Value: $$${value})"
  }
}

trait Bond2 extends Security {
  
  def yearsToMaturity: Int
  def isCallable: Boolean
}

case class Cash(amount: Double, currency: String)

trait ExchangeTraded {
  def tradeDuringExchangeHours: Boolean
}

case class Stock(ticker: String, price: Double, quantity: Int) extends Security {
  val value: Double = price * quantity
}






case class ETF(ticker: String, price: Double, quantity: Int,underlyingSecurities: Seq[Security], cash: Double) extends Security with ExchangeTraded {
    override val tradeDuringExchangeHours: Boolean = true
    val value: Double = underlyingSecurities.map(_.price).sum + cash
    override def toString: String = s"$ticker ETF @ $$${price} (Value: $$${value})"
}

trait Derivative extends Security {
  val underlyingSecurity: Security
  val contractMultiplier: Double
  val expirationDate: String
}

sealed trait Future {
  def ticker: String
  def expirationDate: LocalDate
  def contractSize: Int
  def price: Double
  def quantity: Int
  def value: Double = price * quantity * contractSize
}

case class StockFuture(ticker: String, expirationDate: LocalDate, price: Double, quantity: Int) extends Future {
  val contractSize = 100 // 1 future contract = 100 shares of the underlying stock
}

case class IndexFuture(ticker: String, expirationDate: LocalDate, price: Double, quantity: Int) extends Future {
  val contractSize = 50 // 1 future contract = 50 index points
}

sealed trait Option extends Security {
  def ticker: String
  def expirationDate: LocalDate
  def strikePrice: Double
  def optionType: OptionType
  def price: Double
  def quantity: Int
  def value: Double = price * quantity * 100 // 1 option contract = 100 options


} 

trait SwapOption {
  def valueAtExpiration(price: Double): Double
}
case class CallOption(ticker: String, expirationDate: LocalDate, strikePrice: Double,price: Double, quantity: Int) extends Option {
  val optionType = Call
}

case class PutOption(ticker: String, expirationDate: LocalDate, strikePrice: Double, price: Double, quantity: Int) extends Option {
  val optionType = Put
}

sealed trait OptionType
case object Call extends OptionType
case object Put extends OptionType

case class CoveredCall(stock: Stock, callOption: CallOption) {
  val costBasis: Double = stock.price * stock.quantity
  val maxProfit: Double = callOption.price * stock.quantity
  val maxLoss: Double = costBasis - callOption.price * stock.quantity

  def valueAtExpiration(stockPrice: Double): Double = {
    if (stockPrice >= callOption.strikePrice) {
      callOption.strikePrice * stock.quantity + callOption.price * stock.quantity
    } else {
      stockPrice * stock.quantity + callOption.price * stock.quantity
    }
  }

  override def toString: String = {
    s"Covered call strategy on ${stock.ticker}:\n" +
      s"  Stock price: ${stock.price}\n" +
      s"  Call option: ${callOption.strikePrice} @ ${callOption.price}\n" +
      s"  Cost basis: $costBasis\n" +
      s"  Maximum profit: $maxProfit\n" +
      s"  Maximum loss: $maxLoss"
  }
}

case class SwapV(ticker: String, price: Double,quantity: Int, underlyingSecurity: Security, contractMultiplier: Double, expirationDate: String) extends Derivative {
  val value: Double = price * contractMultiplier
  override def toString: String = s"$ticker swap @ $$${price} (Value: $$${value})"
}


case class FixedRateBond(ticker: String, price: Double, quantity: Int, couponRate: Double, yearsToMaturity: Int) extends Security {
  val value: Double = calculateValue()

  private def calculateValue(): Double = {
    price * quantity * (couponRate / 100.0)
  }

  override def toString: String = {
    s"$ticker @$price x $quantity ($couponRate% $yearsToMaturity years to maturity, Value: $$${value})"
  }
}

case class FloatingRateBond(ticker: String, price: Double, quantity: Int, spread: Double, index: String, startDate: LocalDate, 
     override val yearsToMaturity: Int) extends Bond(ticker, price, quantity, 0, yearsToMaturity) {
  private var currentRate: Double = getRate()

  def getRate(): Double = {
    // implementation to get current rate based on the index and start date
    0.0
  }

  override def calculateValue(): Double = {
    price * quantity * ((currentRate + spread) / 100.0)
  }

  override def toString: String = {
    s"$ticker @$price x $quantity (Floating Rate $index + $spread, $yearsToMaturity years to maturity, Value: $$${value})"
  }
}

case class Swap(notional: Double, fixedRate: Double, floatingRateBond: FloatingRateBond) extends Security {
    override val price: Double = calculatePrice()
    override val quantity: Int = 1
    override val ticker: String = s"Swap(${floatingRateBond.ticker})"
    val value: Double = 0.0
    private def calculatePrice(): Double = {
        notional * (fixedRate - floatingRateBond.getRate()) * floatingRateBond.yearsToMaturity
    }
  
    override def toString: String = s"$ticker: Price: $price, Quantity: $quantity"
}







def testModel = 
    println("Testing Model")