package learnscala3;

import java.io.*;
import java.security.cert.TrustAnchor;
public class MyClass { // this is Java

    private int index;
    private String name;

    public MyClass(int index, String name) {
        this.index = index;
        this.name = name;
    }

    public void Print(String msg)
    {
        System.out.printf("\n%s:index %d name %s",msg,index,name);
    }

    public void NameHasUpperCase(String name) 
    {
        boolean nameHasUpperCase = false;  // this is Java
        for (int i = 0; i < name.length(); ++i) {
            if (Character.isUpperCase(name.charAt(i))) {
                nameHasUpperCase = true;
                break;
            }
        }
        if (nameHasUpperCase == true)
        {
            System.out.printf("\nNameHasUpperCase >>%s<<",name);
        }
        else 
        {
            System.out.printf("\nNameHas no UpperCase >>%s<<",name);
        }
    }
}


class HashTable {
    private int capacity;
    private int size;
    private String[] keys;
    private String[] values;
    private boolean[] deleted;

    public HashTable(int capacity) {
        this.capacity = capacity;
        this.size = 0;
        this.keys = new String[capacity];
        this.values = new String[capacity];
        this.deleted = new boolean[capacity];
    }

    private int hash(String key) {
        return Math.abs(key.hashCode() % capacity);
    }

    private int hash2(String key) {
        int hash = Math.abs(key.hashCode() % capacity);
        return 2 * hash + 1;
    }

    public void put(String key, String value) {
        int index = hash(key);
        int offset = 0;
        while (keys[index] != null && !deleted[index] && !keys[index].equals(key)) {
            offset = hash2(key);
            index = (index + offset) % capacity;
        }
        if (keys[index] == null || deleted[index]) {
            size++;
        }
        keys[index] = key;
        values[index] = value;
        deleted[index] = false;
    }

    public String get(String key) {
        int index = hash(key);
        int offset = 0;
        while (keys[index] != null) {
            if (!deleted[index] && keys[index].equals(key)) {
                return values[index];
            }
            offset = hash2(key);
            index = (index + offset) % capacity;
        }
        return null;
    }

    public void delete(String key) {
        int index = hash(key);
        int offset = 0;
        while (keys[index] != null) {
            if (!deleted[index] && keys[index].equals(key)) {
                deleted[index] = true;
                size--;
                return;
            }
            offset = hash2(key);
            index = (index + offset) % capacity;
        }
    }

    public int size() {
        return size;
    }
}
