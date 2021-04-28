package dev.nigredo

import org.specs2.mutable.Specification
import FolderSpec._

class FolderSpec extends Specification {
  "Folder" should {
    "fold" in {

      implicit val inst0: Folder[List[String], String] = Folder.instance(acc => v => acc.:+(v))

      Folder.fold[String, Folder.False](List.empty[String], new Example) must beEqualTo(List(
        "str0"
      ))
    }
  }
}

object FolderSpec {

  class Example {

    val foo: String = "str0"

    def bar(): String = "str1"

    def baz: String = "str2"
  }
}
