package com.komsonandmarch.patternmatching

trait FinSecurity
case class Stock(name:String) extends  FinSecurity
case class Bonds(name:String) extends  FinSecurity
case class Derivatives(name:String) extends  FinSecurity
case class Cash(name:String) extends  FinSecurity
case class ETF(name:String) extends  FinSecurity
case class MutualFund(name:String) extends  FinSecurity