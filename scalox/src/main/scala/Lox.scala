package example

import scala.io.Source

object Lox {

  // Main method. Kicks things off.
  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      println("Usage: scalox <script>")
    } else if (args.length == 1) {
      runFile(args(0))
    } else {
      runPrompt
    }
  }

  // Facilitates interpreting code stored in a file
  def runFile(path: String): Unit = {
    try {
      val f = Source.fromFile(path)
      run(f.toString)
    } catch {
      // probable case
      case e: java.io.FileNotFoundException =>{
       println(s"Source file '$path' was not found")
      }
      // base case or permission error
      case e: java.io.IOException => {
        println("IO Exception")
      }
    }
  }

  // Begins the scalox REPL
  def runPrompt(): Unit = {
    val stdin = Source.stdin.bufferedReader
    println("scalox REPL (^C to exit)")
    // Tail recursive
    def inner(): Unit = {
      print("> ")
      val in = stdin.readLine
      run(in)
      inner
    }
    inner
  }

  // The core function that facilitates code evaluation
  def run(source: String): Unit = {
        println("Code running unimplemented")
  }
}
