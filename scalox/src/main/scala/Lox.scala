package scalox

import scala.io.Source

object Lox {
  // Not really idiomatic Scala, but I'm following a guide
  var hadError = false

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
      run(f.mkString)
      if (hadError) System.exit(65)
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
      hadError = false
      inner
    }
    inner
  }

  // Handler for compilation error messages
  def error(line: Int , message: String, where: String = ""): Unit = {
    println(s"[line $line] Error $where: $message")
    hadError = true
  }

  // The core function that facilitates code evaluation
  def run(source: String): Unit = {
    val tokens: List[Token] = (new Scanner(source)).scanTokens.reverse

    tokens.foreach((x: Token) => println(x.toString))
  }
}

