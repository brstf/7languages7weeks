import scala.io.Source

/**
 * Use fold left to sum the lengths of all strings in a list of strings.
 */
def totalLength(stringList : List[java.lang.String]) : Int = {
    val sum = (0 /: stringList) {(sum, i) => sum + i.length}
    return sum
}

/**
 * Censor trait with a censor function that will replace the words
 * "shoot" and "darn" with "pucky" and "beans" respectively.
 */
trait Censor {
    val curseMap = Map("shoot" -> "pucky", "darn" -> "beans", 
                       "Shoot" -> "Pucky", "Darn" -> "Beans")

    def censor(passage : java.lang.String) : java.lang.String = {
        var censored = passage
        curseMap.foreach(curse => censored = censored.replaceAll(curse._1, curse._2))
        return censored
    }
}

/**
 * Passage class to make use of the Censor trait. Takes a string as
 * an argument and can return the uncensored passage or censored version.
 */
class Passage(passage : java.lang.String) extends Censor {
    def getUncensored() : java.lang.String = passage
    def getCensored()   : java.lang.String = censor(passage)
}

/**
 * As opposed to the Passage class, FilePassage accepts a path to a file,
 * reads it's contentes as plain text, and provides censored or noncensored input.
 */
class FilePassage(pathToFile : java.lang.String) extends Censor {
    // First read the file text
    var passage = "";
    Source.fromFile(pathToFile).foreach{ 
        line => passage += line
    }

    def getUncensored() : java.lang.String = passage
    def getCensored()   : java.lang.String = censor(passage)
}

val list = List("lions", "tigers", "bears", "oh", "my")
println(totalLength(list))

val c = new Passage("Shoot, this passage of text has darn cursewords.")
println("Normal Passage  : " + c.getUncensored())
println("Censored Passage: " + c.getCensored())

val f = new FilePassage("curse_test.txt")
println("Normal File: *****\n" + f.getUncensored())
println("Censored File: ***\n" + f.getCensored())