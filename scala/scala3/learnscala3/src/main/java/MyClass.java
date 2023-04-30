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