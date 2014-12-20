/*
 * Sizer example with Akka actors
 */

import scala.io._
import scala.actors._
import Actor._

object PageLoader {
    def getPage( url : String ) : String = {
        try {
            return Source.fromURL(url, "ISO-8859-1").mkString
        } catch {
            case e:java.lang.Exception => {
                return ""
            }
        }
    }

    def getPageSize( source : String ) = source.length

    def getLinks( source : String ) : List[String] = { 
        val reg = "<a href=\"([^\"]*)\"".r
        var links = List[String]()
        reg.findAllIn(source).matchData foreach(link => {
            // Naively ignore links that aren't relative (begin 
            // with '/') or absolute (begin with 'http')
            if( "^http.*|^/.*".r.pattern.matcher(link.group(1).trim()).matches ) {
                links ::= link.group(1).trim()
            }
        })
        return links
    }

    /**
     * Given the source URL and the URL of a link found on that page (linkUrl)
     * return linkUrl if linkUrl is an absolute link or url + linkUrl if it's
     * a relative link
     */
    def getFullURL( url: String, linkUrl : String ) : String = {
        if(linkUrl.startsWith("/")) {
            return url + linkUrl.substring(1)
        } else {
            return linkUrl
        }
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
        val page = PageLoader.getPage(url)
        println("Size for " + url + ": " + PageLoader.getPageSize(page))

        val links = PageLoader.getLinks(page.toString())
        var externalSize = 0
        for(externalUrl <- links) {
            val linkURL = PageLoader.getFullURL(url, externalUrl)
            val externalPage = PageLoader.getPage(linkURL)
            externalSize += PageLoader.getPageSize(externalPage)
        }

        println("  Size for pages linked from " + url + ": " + externalSize)
    }
}

def getPageSizeConcurrently() = {
    val caller = self
    var sizeMap = Map[String, Int]()

    for(url <- urls) {
        actor { caller ! (url, PageLoader.getPage(url)) }
        sizeMap += (url -> 0)
    }

    // Track number of links we're still waiting to receive back from
    var numLinks = urls.size
    var i = 0
    while(i < numLinks) {
        receive {
            case (url, page) if url.toString().startsWith("e_") => {
                // Received result from a page linked from one of the original pages
                var sourceURL = url.toString().substring(2)
                var size = PageLoader.getPageSize(page.toString())
                sizeMap += (sourceURL -> (sizeMap(sourceURL) + size))
                i += 1
            }
            case (url, page) => {
                println("Size for " + url + ": " + PageLoader.getPageSize(page.toString()))

                // Add all external links to the list
                val links = PageLoader.getLinks(page.toString())
                numLinks += links.length
                for(link <- links) {
                    // prepend "e_" to the source link so that we can differentiate it
                    // from "normal" URLS
                    actor { caller ! ("e_" + url.toString(), PageLoader.getPage(link)) }
                }
                i += 1
            }
        }
    }

    for(url <- urls) {
        println("Size for pages linked from " + url + ": " + sizeMap(url))
    }
}

println("Sequential run:")
timeMethod { getPageSizeSequentially }

println("Concurrent run")
timeMethod { getPageSizeConcurrently }