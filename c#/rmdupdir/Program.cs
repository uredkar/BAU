using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
namespace rmdupdir
{
    class Program
    {
        static System.Collections.Specialized.StringCollection log = new System.Collections.Specialized.StringCollection();
        static List<System.IO.FileInfo> list = new List<System.IO.FileInfo>();
        static int count = 0;
        static void Main(string[] args)
        {
            
            Console.WriteLine("Root {0}", args[0]);
            var rootDirectory = args[0]; //@"c:\books";
            System.IO.DirectoryInfo rootDir = new DirectoryInfo(rootDirectory);
            WalkDirectoryTree(rootDir);
            // Write out all the files that could not be processed.
            Console.WriteLine("Files with restricted access:");

            foreach (string s in log)
            {
                Console.WriteLine(s);
            }
            var duplicates = list.GroupBy(x => x.Name)
                                .Select(x => new { Count = x.Count(), Name = x.Key}).Where(x => x.Count > 1).ToList();

            var j = from d in duplicates
                    join l in list on d.Name equals l.Name
                    orderby l.Name
                    select new { l.FullName,  l.Name, l.DirectoryName, l.Extension, l.Length, d.Count };
            var ranks = j.GroupBy(p=> (p.Name, p.Extension,p.Length)).SelectMany( g => g.Select((n, i) => new { n, rank=i+1 })).Where(p=>p.rank > 1);
                

            foreach(var x in ranks)
            {
                File.Delete(x.n.FullName);
                Console.WriteLine($"Deleting {x.n.Name}, {x.n.FullName}, {x.n.Length} : {x.n.DirectoryName},Count={x.n.Count},Rank={x.rank}");
            }
            // Keep the console window open in debug mode.
            //Console.WriteLine("Press any key");
            //Console.ReadKey();
        }
        static void WalkDirectoryTree(System.IO.DirectoryInfo root)
        {
            System.IO.FileInfo[] files = null;
            System.IO.DirectoryInfo[] subDirs = null;

            // First, process all the files directly under this folder
            try
            {
                files = root.GetFiles("*.*");
            }
            // This is thrown if even one of the files requires permissions greater
            // than the application provides.
            catch (UnauthorizedAccessException e)
            {
                // This code just writes out the message and continues to recurse.
                // You may decide to do something different here. For example, you
                // can try to elevate your privileges and access the file again.
                log.Add(e.Message);
            }

            catch (System.IO.DirectoryNotFoundException e)
            {
                Console.WriteLine(e.Message);
            }

            if (files != null)
            {
                foreach (System.IO.FileInfo fi in files)
                {
                    // In this example, we only access the existing FileInfo object. If we
                    // want to open, delete or modify the file, then
                    // a try-catch block is required here to handle the case
                    // where the file has been deleted since the call to TraverseTree().
                    //Console.WriteLine("{0},{1},{2},{3}",fi.Extension, fi.FullName,fi.Directory,fi.Name);
                    count++;
                    if (count % 100 == 0)
                    {
                        Console.WriteLine("Files Read Count {0}", count);
                    }
                    list.Add(fi);
                }

                // Now find all the subdirectories under this directory.
                subDirs = root.GetDirectories();

                foreach (System.IO.DirectoryInfo dirInfo in subDirs)
                {
                    // Resursive call for each subdirectory.
                    WalkDirectoryTree(dirInfo);
                }
            }
        }
    }
}
