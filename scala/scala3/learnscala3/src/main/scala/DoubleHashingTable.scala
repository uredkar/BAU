package com.komsonandmarch.doubleHash

import scala.collection.mutable.ArrayBuffer


import scala.reflect.ClassTag

class HashTable[K: ClassTag, V: ClassTag](capacity: Int) {
  private val keys = new Array[K](capacity)
  private val values = new Array[V](capacity)
  private val tombstone = new Object
  
  private var size = 0
  
  private def hash1_old(key: K): Int = (key.hashCode() & 0x7fffffff) % capacity
  
  private def hash2_old(key: K): Int = {
    var hash = key.hashCode() & 0x7fffffff
    hash ^= (hash >>> 20) ^ (hash >>> 12)
    hash ^= (hash >>> 7) ^ (hash >>> 4)
    hash & 0x7fffffff % (capacity - 1) + 1
  }
  
  private def hash1(key: K): Int = key.hashCode() % capacity
  
  private def hash2(key: K): Int = {
    val h = capacity - key.hashCode() % capacity
    h
  }

  private def probe_old(key: K, i: Int): Int = (hash1(key) + i * hash2(key)) % capacity
  def probe(key: K, i: Int): Int = {
    
    val h1 = hash1(key)
    val h2 = hash2(key)
    val offset = i * h2 % capacity
    
    //val index = h1 + i * h2 % capacity
    val index = h1 + offset

    if (index >= capacity)
    {
        println(s"$index is greater than $capacity")
    }
    println(s"capacity = $capacity,index = $index h1 = $h1 h2 = $h2,offset = $offset, key = $key,i = $i")
    index 
  }
  def put(key: K, value: V): Unit = {
    if (size == capacity) {
      throw new RuntimeException("Hash table is full")
    }
    var i = 0
    var j = probe(key, i)
    while (keys(j) != null && keys(j) != tombstone && keys(j) != key) {
      i += 1
      j = probe(key, i)
    }
    if (keys(j) == null || keys(j) == tombstone) {
      size += 1
    }
    keys(j) = key
    values(j) = value
  }
  
  def get(key: K): Option[V] = {
    var i = 0
    var j = probe(key, i)
    
    println(s"j=$j")
    
    while (keys(j) != null) {
      if (keys(j) == key) {
        return Some(values(j))
      }
      i += 1
      j = probe(key, i)
      println(s"j=$j")
    }
    None
  }
  
  def delete(key: K): Unit = {
    var i = 0
    var j = probe(key, i)
    while (keys(j) != null) {
      if (keys(j) == key) {
        keys(j) = tombstone.asInstanceOf[K]
        values(j) = null.asInstanceOf[V]
        size -= 1
        return
      }
      i += 1
      j = probe(key, i)
    }
  }
  
 

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append("HashTable(")
    for (i <- keys.indices) {
      if (keys(i) != null && keys(i) != tombstone) {
        sb.append(s"${keys(i)} -> ${values(i)}")
        if (i < keys.length - 1) {
          sb.append(", ")
        }
      }
    }
    sb.append(")")
    sb.toString()
  }
}



def double_hashing = 
    // example usage
    /*
    val table = new DoubleHashingHashTable[String](10)
    table.put(1, "foo")
    table.put(2, "bar")
    table.put(17, "baz")
    table.put(3, "qux")
    table.put(15, "quux")
    println(table)
    println(table.get(1)) // Some(foo)
    table.remove(17)
    println(table)
    */
    val hashTable = new HashTable[String, Int](20)
    (0 to 10).foreach(x => hashTable.probe("k1",x))
    (0 to 10).foreach(x => hashTable.probe("k2",x))
/*
    hashTable.put("k1", 10)
    hashTable.put("k2", 20)
    hashTable.put("k3", 10)
    hashTable.put("k4", 20)
    hashTable.put("k5", 10)
    hashTable.put("k6", 20)
    hashTable.put("k7", 20)
    hashTable.put("k8", 10)
    hashTable.put("k9", 20)
    val value1 = hashTable.get("k1") // returns Some(10)
    val value2 = hashTable.get("k3") // returns None
    println(hashTable)
*/




