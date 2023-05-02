package com.komsonandmarch.directorycrawler
import java.nio.file.{AccessDeniedException,FileVisitResult,SimpleFileVisitor, Files, Path, Paths}
import java.nio.file.attribute.{BasicFileAttributes, DosFileAttributes, FileAttribute, FileTime}

import java.io.{IOException}
import java.nio.file.FileVisitOption
import java.nio.file.attribute.PosixFilePermission
import java.nio.file.attribute.PosixFilePermissions

object cli {
    def main(args: String*): Unit = {
        println("Directory Crawler Main")
        println("---------------------------")
        args.foreach(arg => println(arg))
        println("---------------------------")

        val dirPath: Path = Paths.get("c:/temp")
        val fileExtension: String = ".*"
        val perms = PosixFilePermissions.fromString("rwxr-x---")
        val options = java.util.EnumSet.of(FileVisitOption.FOLLOW_LINKS)
        
        Files.walkFileTree(dirPath, options, Integer.MAX_VALUE, new SimpleFileVisitor[Path] {
        override def visitFile(filePath: Path, attrs: java.nio.file.attribute.BasicFileAttributes) = {
            try {
                val fileAttrs = Files.readAttributes(filePath, classOf[DosFileAttributes])
                if (!fileAttrs.isReadOnly) {
                    println(filePath)
                } else {
                    println(s"Access denied to file $filePath: $fileAttrs")
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
    }
}    