package com.komsonandmarch.patternmatching



trait Security {
  val ticker: String
  val price: Double
  val quantity: Int
  def value: Double
  override def toString: String = s"$ticker @$price x $quantity (Value: $$${value})"
}

case class Cash(currency:String,amount: Double) 


trait ExchangeTraded {
  def tradeDuringExchangeHours: Boolean
}

case class Stock(ticker: String, price: Double, quantity: Int) extends Security {
  val value: Double = price * quantity
}

case class Bond(ticker: String, price: Double, quantity: Int,val couponRate: Double,val yearsToMaturity: Int, val isCallable: Boolean) extends Security {
    
    val value: Double = calculateValue()
    
    private def calculateValue(): Double = {
        if (isCallable) {
        price * quantity * (couponRate / 100.0) * 0.9
        } else {
        price * quantity * (couponRate / 100.0)
        }
    }
    override def toString: String = {
        val bondType = if (isCallable) "Callable" else "Non-Callable"
        s"$ticker @$price x $quantity ($couponRate% $yearsToMaturity years to maturity, $bondType, Value: $$${value})"
    }
}
/*
class Bond(ticker: String, price: Double, quantity: Int, val couponRate: Double, val yearsToMaturity: Int, val isCallable: Boolean) extends Security {
    val value: Double = calculateValue()
    
    private def calculateValue(): Double = {
        if (isCallable) {
        price * quantity * (couponRate / 100.0) * 0.9
        } else {
        price * quantity * (couponRate / 100.0)
        }
    }
  
    override def toString: String = {
        val bondType = if (isCallable) "Callable" else "Non-Callable"
        s"$ticker @$price x $quantity ($couponRate% $yearsToMaturity years to maturity, $bondType, Value: $$${value})"
    }
}
*/


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

case class Future(ticker: String, price: Double, quantity: Int,numContracts: Int, underlyingSecurity: Security, contractMultiplier: Double, expirationDate: String) extends Derivative {
  val value: Double = price * numContracts * contractMultiplier
  override def toString: String = s"$numContracts contracts of $ticker @ $$${price} (Value: $$${value})"
}

case class Option(ticker: String, price: Double, quantity: Int,numContracts: Int, underlyingSecurity: Security, contractMultiplier: Double, expirationDate: String, strikePrice: Double) extends Derivative {
  val value: Double = price * numContracts * contractMultiplier
  override def toString: String = s"$numContracts contracts of $ticker @$strikePrice (Value: $$${value})"
}

case class Swap(ticker: String, price: Double,quantity: Int, underlyingSecurity: Security, contractMultiplier: Double, expirationDate: String) extends Derivative {
  val value: Double = price * contractMultiplier
  override def toString: String = s"$ticker swap @ $$${price} (Value: $$${value})"
}



def testModel = 
    println("Testing Model")