package com.awesomeware.core.io

object Color {


  var colors: Map[Char, String] = Map(
    'r' -> "\033[0;31m", 'R' -> "\033[1;31m",
    'b' -> "\033[0;34m", 'B' -> "\033[1;34m",
    'g' -> "\033[0;32m", 'G' -> "\033[1;32m",
    'c' -> "\033[0;36m", 'C' -> "\033[1;36m",
    'm' -> "\033[0;35m", 'M' -> "\033[1;35m",
    'w' -> "\033[1;37m", 'Z' -> "\033[0;37m",
    'z' -> "\033[1;30m", 'd' -> "\033[0;30m",
    'y' -> "\033[0;33m", 'Y' -> "\033[1;33m",

    'n' -> Console.RESET,
    'x' -> "\033[2J" // Clear
  )

  def colorize(str: String, keep: Boolean = true): String = {
    val b = new StringBuilder()
    var colorMode: Boolean = false
    for (c <- str) {
      if (colorMode) {
        if (colors.contains(c)) {
          if (true) b ++= colors(c)
        }
        colorMode = false
      } else {
        if (c == '#') {
          colorMode = true
        } else {
          b ++= c.toString
        }
      }
    }

    b.toString()
  }
}
