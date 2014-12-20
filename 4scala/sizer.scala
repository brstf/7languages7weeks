/*
 * Sizer example with Akka actors
 */

import scala.io._
import scala.actors._
import Actor._

object PageLoader {
    def getPageSize( url : String ) = Source.fromURL(url).mkString.length
    def getNumLinks( url : String ) : Int = { 
        var sum = 0
        val source = Source.fromURL(url).mkString
        val reg = "<a href=\"[^\"]*\"".r
        reg.findAllIn(source).foreach(link => sum += 1)
        return sum
    }
}

val urls = List("http://www.amazon.com/",
                "http://www.twitter.com/",
                "http://www.google.com/",
                "http://www.cnn.com/")

def timeMethod(method: () => Unit) = {
    val start = System.nanoTime
    method()
    val end = System.nanoTime
    println("Method took " + (end - start) / 1000000000.0 + " seconds.")
}

def getPageSizeSequentially() = {
    for(url <- urls) {
        println("Size for " + url + ": " + PageLoader.getPageSize(url))
    }
}

def getNumLinksSequentially() = {
    for(url <- urls) {
        println("Number of links on " + url + ": " + PageLoader.getNumLinks(url))
    }
}

def getPageSizeConcurrently() = {
    val caller = self

    for(url <- urls) {
        actor { caller ! (url, PageLoader.getPageSize(url)) }
    }

    for(i <- 1 to urls.size) {
        receive {
            case (url, size) => println("Size for " + url + ": " + size)
        }
    }
}

def getNumLinksConcurrently() = {
    val caller = self

    for(url <- urls) {
        actor { caller ! (url, PageLoader.getNumLinks(url)) }
    }

    for(i <- 1 to urls.size) {
        receive {
            case (url, size) => println("Number of Links on " + url + ": " + size)
        }
    }
}

println("Sequential run:")
timeMethod { getPageSizeSequentially }
timeMethod { getNumLinksSequentially }

println("Concurrent run")
timeMethod { getPageSizeConcurrently }
timeMethod { getNumLinksConcurrently }