package komsonandmarch

import com.spotify.scio._

/*
sbt "runMain [PACKAGE].WordCount
  --project=[PROJECT] --runner=DataflowRunner --zone=[ZONE]
  --input=gs://dataflow-samples/shakespeare/kinglear.txt
  --output=gs://[BUCKET]/[PATH]/wordcount"
*/

object WordCount {
  def main(cmdlineArgs: Array[String]): Unit = {
    val (sc, args) = ContextAndArgs(cmdlineArgs)

    val exampleData = "file:///F:/learnscala/JustEnoughScalaForSpark/data/shakespeare/comedyoferrors.txt"
    val outputData =  "../../../data/txt/shakespeareout.txt"
    val input = args.getOrElse("input", exampleData)
    val output = args.getOrElse("output", outputData)
    //val output = args("output")

    sc.textFile(input)
      .map(_.trim)
      .flatMap(_.split("[^a-zA-Z']+").filter(_.nonEmpty))
      .countByValue
      .map(t => t._1 + ": " + t._2)
      .saveAsTextFile(output)

    val result = sc.run().waitUntilFinish()
  }
}
