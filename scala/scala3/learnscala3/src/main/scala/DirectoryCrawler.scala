package com.komsonandmarch.directorycrawler
import java.nio.file.{AccessDeniedException,FileVisitResult,SimpleFileVisitor, Files, Path, Paths}

import java.nio.file.attribute.{BasicFileAttributes, DosFileAttributes, FileAttribute, FileTime}
import org.apache.commons.io.FilenameUtils
import java.util.regex.Pattern;
import java.io.{IOException}
import java.nio.file.FileVisitOption
import java.nio.file.attribute.PosixFilePermission
import java.nio.file.attribute.PosixFilePermissions
import scala.util.{Try, Success, Failure}
import scala.util.CommandLineParser as CLP
import scala.collection.mutable.Map
case class Config(
  dirname: String,
  filter: String
)

case class FileDetails(fileName:String, xsDirName: List[String])

object cli {
    def main(args: Array[String]): Unit = {
        println("Directory Crawler Main")
        println("---------------------------")
        args.foreach(arg => println(arg))
        println("---------------------------")
        if (args.length < 2){
            println("dir name is required as first parameter")
            println("regex is required as second parameter")
            println("example  run c:\fileswithtext .*\\.txt")
        }
        else {
            val dirname = CLP.parseArgument[String](args, 0)
            val pattern = CLP.parseArgument[String](args, 1)
            println(s"dirname $dirname")
            process_dir(dirname,pattern)
        }
    }

    def process_dir(dirname:String,pattern:String) = {        
        val dirPath: Path = Paths.get(dirname)
        val fileExtension: String = ".*"
        val perms = PosixFilePermissions.fromString("rwxr-x---")
        val options = java.util.EnumSet.of(FileVisitOption.FOLLOW_LINKS)
        //val patternCompiled = Pattern.compile(pattern); // Filter all files ending with ".txt"
        val patternCompile = Pattern.compile(pattern)
        val hashMapFiles = Map[String,FileDetails]()

        Files.walkFileTree(dirPath,  new SimpleFileVisitor[Path] {


        override def visitFile(filePath: Path, attrs: java.nio.file.attribute.BasicFileAttributes) = {
            try {
                
                if (patternCompile.matcher(filePath.getFileName.toString).matches){
                    val fileAttrs = Files.readAttributes(filePath, classOf[DosFileAttributes])
                    val fileName = filePath.getFileName().toString()
                    val dirName = filePath.getParent().toString()
                    
                    if (hashMapFiles.contains(fileName)) {
                        

                        //println(s"Duplicate $filePath,${filePath.getFileName().toString()},${filePath.getParent()},${FilenameUtils.getExtension(filePath.getFileName().toString())}")
                        val existingDetails = hashMapFiles(fileName)
                        val updatedDetails = existingDetails.copy(xsDirName = existingDetails.xsDirName ++ List(dirName) )
                        hashMapFiles += (fileName -> updatedDetails)

                    }
                    else {
                            hashMapFiles += (fileName -> FileDetails(fileName,List(dirName)))
                    }
                    
                    //if (!fileAttrs.isReadOnly) {
                    
                    //} else {
                    //    println(s"Access denied to file $filePath: $fileAttrs")
                    // }
                }
    
            } catch {
                case ex: AccessDeniedException => println(s"Access denied to file $filePath: $ex")
                case ex: IOException => println(s"Error processing file $filePath: $ex")
            }
            FileVisitResult.CONTINUE
        }

        override def visitFileFailed(filePath: Path, exc: IOException) = {
            println(s"Failed to visit file $filePath: $exc")
            FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dirPath: Path, exc: IOException) = {
            if (exc != null) {
                println(s"Failed to visit directory $dirPath: $exc")
            }
            FileVisitResult.CONTINUE
        }
        })

        println("After Walking")
        hashMapFiles.map(pair => {
            
            if (pair._2.xsDirName.length > 2) {
                println(s"\n${pair._1},\n\tDir ${pair._2.xsDirName}")
            }
        
        })

    }
}    