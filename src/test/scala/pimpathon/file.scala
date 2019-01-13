package pimpathon

import _root_.java.io.File

import pimpathon.any._
import pimpathon.function.Predicate
import pimpathon.java.io.inputStream._
import pimpathon.util._

import scala.collection.JavaConverters._
import scala.io.Codec
import scala.util.Properties


class FileTest extends PimpathonSuite {
  private val file = FileUtils(currentTime = () ⇒ util.currentTime())

  import file._

  test("rejectsNull") {
    util.assertThrows[Exception]("requirement failed: FileOps cannot be used with null files")(file.FilePimps(null: File))
  }

  test("create") {
    file.withTempDirectory(dir ⇒ {
      val child = dir / "child"
      assert(!child.exists)

      child.create()
      assert(child.exists)
      assert(child.isFile)

      val childDir = dir / "childDir"
      assert(!childDir.exists)

      childDir.create(directory = true)
      assert(childDir.exists)
      assert(childDir.isDirectory)

      val nested = dir / "parent" / "child"
      assert(!nested.exists)

      nested.create()
      assert(nested.exists)
      assert(nested.isFile)
    })
  }

  test("deleteRecursively") {
    file.withTempDirectory(tmp ⇒ {
      assert(!(tmp / "non-existent-file").deleteRecursively().exists)
      assert(!(tmp / "existing-file").create().deleteRecursively().exists)
      assert(!(tmp / "existing-dir").create(directory = true).deleteRecursively().exists)

      val parent = (tmp / "parent").create(directory = true)
      val children = List(parent / "child", parent / "parent" / "child").map(_.create())

      assert(!parent.deleteRecursively().exists)
      children.filter(_.exists) === Nil
    })
  }

  test("deleteRecursivelyOnExit") {
    // Check that 'deleteRecursivelyOnExit' registers a DeleteRecursively shutdown hook
    file.withTempFile(tmp ⇒ {
      assert(!shutdownHooks().contains(DeleteRecursively(tmp)))
      tmp.deleteRecursivelyOnExit()
      assert(shutdownHooks().contains(DeleteRecursively(tmp)))
      assert(!shutdownHooks().contains(DeleteRecursively(file.tempFile())))
    })

    // Check that running DeleteRecursively works
    file.withTempDirectory(tmp ⇒ {
      val parent = (tmp / "parent").create(directory = true)
      val files = parent :: List((parent / "child").create(), (parent / "parent" / "child").create())
      val deleteRecursively = DeleteRecursively(parent)

      assert(files == files.filter(_.exists), "files should exist before Thread has been run")
      //assertEquals("files should exist before Thread has been run", files, files.filter(_.exists))

      deleteRecursively.run()
      assert( files.filter(_.exists).isEmpty,  "files should not exist after Thread has been run")
      //assertEquals("files should not exist after Thread has been run", Nil, files.filter(_.exists))
    })
  }


  test("cwd") {  file.cwd.getPath === Properties.userDir }

  test("named") {  file.withTempFile(tmp ⇒ tmp.named("name").toString === "name") }

  test("canon") {
    file.withTempDirectory(dir ⇒ {
      (dir / "file").canon === (dir / "file").getCanonicalFile
      (dir / "file\u0000").canon === (dir / "file\u0000").getAbsoluteFile
    })
  }

  test("changeToDirectory") {  file.withTempFile(file ⇒ assert(file.changeToDirectory().isDirectory)) }

  test("children") {
    file.withTempDirectory(dir ⇒ {
      dir.children.toSet === Set.empty[File]

      val List(child, toddler) = file.files(dir, "child", "toddler").map(_.create()).toList
      dir.children.map(_.named()).toSet === Set(child, toddler)
    })
  }

  test("childDirs") {
    file.withTempDirectory(dir ⇒ {
      dir.childDirs.toSet === Set.empty[File]

      val List(toddler) = file.files(dir, "parent/toddler").map(_.create()).toList
      dir.childDirs.map(_.named()).toSet === Set(toddler.getParentFile)
    })
  }

  test("ancestors") {
    file.withTempDirectory(dir ⇒ {
      val List(child) = file.files(dir, "parent/child").map(_.create()).toList

      assert(Set(dir, dir / "parent", child).forall(child.ancestors.contains))
    })
  }

  test("isAncestorOf") {
    file.withTempDirectory(dir ⇒ {
      val List(child) = file.files(dir, "parent/child").map(_.create()).toList

      assert(Set(dir, dir / "parent", child).forall(_.isAncestorOf(child)))
    })
  }

  test("relativeTo") {
    file.withTempDirectory(dir ⇒ {
      (dir / "child").create().relativeTo(dir).getPath === "child"
      (dir / "kid").create().relativeTo(dir.getParentFile).getPath === (dir.getName + "/kid")

      val parent = (dir / "parent").create(directory = true)
      parent.relativeTo(dir).getPath === "parent"
      dir.relativeTo(parent).getPath === ".."
      (parent / "child").create().relativeTo(dir).getPath === "parent/child"
    })
  }

  test("tree")  {
    new File("non-existent").tree.toList === Nil

    file.withTempFile(tmp ⇒      tmp.tree.toList === List(tmp))
    file.withTempDirectory(tmp ⇒ tmp.tree.toList === List(tmp))

    file.withTempDirectory(tmp ⇒ {
      val List(child, toddler, brat, unreadableFile, unreadableChild) = file.files(tmp,
        "child", "toddler", "parent/brat", "unreadableFile", "unreadableDir/child").map(_.create()).toList

      val unreadableDir = unreadableChild.getParentFile.tap(_.setReadable(false))

      assertEqualsSet(Set(tmp.named(), child, toddler, brat.getParentFile, brat, unreadableFile, unreadableDir),
        tmp.tree.map(_.named()).toSet)
    })
  }

  test("withTempFile()") {
    assert(!file.withTempFile(tmp ⇒ {
        assertIsTemp(".tmp", "temp", expectedIsFile = true, tmp); tmp
      }).exists, "Temp file should not exist after 'withTempFile'")


    assert(!file.withTempFile("suffix")(tmp ⇒ {
        assertIsTemp("suffix", "temp", expectedIsFile = true, tmp); tmp
      }).exists, "Temp file should not exist after 'withTempFile'")


    assert(
      !file.withTempFile("suffix", "prefix")(tmp ⇒ {
        assertIsTemp("suffix", "prefix", expectedIsFile = true, tmp); tmp
      }).exists, "Temp file should not exist after 'withTempFile'"
    )
  }

  test("withTempDirectory") {
    file.withTempDirectory(tmp ⇒ assertIsTemp("tmp",    "temp",   expectedIsFile = false, tmp))
    file.withTempDirectory("suffix")(tmp ⇒ assertIsTemp("suffix", "temp",   expectedIsFile = false, tmp))
    file.withTempDirectory("suffix", "prefix")(tmp ⇒ assertIsTemp("suffix", "prefix", expectedIsFile = false, tmp))

    assert(
      file.withTempDirectory(tmp ⇒ {
        List(tmp, tmp / "child", tmp / "parent" / "child").map(f ⇒ relativeName(tmp, f.create()))
      }).filter(_.exists).isEmpty, "Temp directory (and contents) should not exist after 'withTempDirectory'"
    )
  }

  test("tempFile") {
    val f = file.tempFile()
    assert(f.isFile)
    assert(f.exists())

    val prefix = "sufferin-"
    val suffix = ".sucotash"

    val f1 = file.tempFile(suffix)
    assert(f1.isFile)
    assert(f1.getName.endsWith(suffix))

    val f2 = file.tempFile(prefix = prefix)
    assert(f2.isFile)
    assert(f2.getName.startsWith(prefix))

    val f3 = file.tempFile(suffix, prefix)
    assert(f3.isFile)
    assert(f3.getName.startsWith(prefix))
    assert(f3.getName.endsWith(suffix))
  }

  test("tempDir") {
    val f = file.tempDir()
    assert(f.isDirectory)
    assert(f.exists())
    assert(shutdownHooks().contains(DeleteRecursively(f)))

    val prefix = "gosh-"
    val suffix = ".darnit"

    val f1 = file.tempDir(suffix)
    assert(f1.isDirectory)
    assert(f1.getName.endsWith(suffix))
    assert(shutdownHooks().contains(DeleteRecursively(f1)))

    val f2 = file.tempDir(prefix = prefix)
    assert(f2.isDirectory)
    assert(f2.getName.startsWith(prefix))
    assert(shutdownHooks().contains(DeleteRecursively(f2)))

    val f3 = file.tempDir(suffix, prefix)
    assert(f3.isDirectory)
    assert(f3.getName.startsWith(prefix))
    assert(f3.getName.endsWith(suffix))
    assert(shutdownHooks().contains(DeleteRecursively(f3)))
  }


  test("newFile") {
    val dir = file.file("this directory does not exist")
    assert(!dir.exists)

    val nested = file.file("parent", "file")
    nested.getParentFile === file.file("parent")

    file.withTempDirectory(dir ⇒ {
      val child = dir / "and this file does not exist"
      assert(!child.exists)
      child.getParentFile === dir
      file.file(dir, "and this file does not exist") === child

      val nested = dir / "parent/child"
      assert(!nested.exists)
      nested.getParentFile.getParentFile === dir
    })
  }

  test("files") {
    file.withTempDirectory(dir ⇒ {
      val List(child, nested) = file.files(dir, "child", "nested/child").toList

      child === dir / "child"
      nested === dir / "nested" / "child"
    })
  }

  test("resource") {
    on("file", "phile").calling(name ⇒ file.resource(s"./pimpathon/$name.class").isDefined).produces(true, false)
  }

  test("readBytes") {
    file.withTempFile(tmp ⇒ {
      createInputStream("contents").drain(tmp.outputStream())
      new String(tmp.readBytes()) === "contents"
    })
  }

  test("readLines") {
    file.withTempFile(tmp ⇒ {
      createInputStream("line1\nline2").drain(tmp.outputStream())
      tmp.readLines() === List("line1", "line2")
    })
  }

  test("readString") {
    file.withTempFile(tmp ⇒ {
      List("ISO-8859-1", "US-ASCII", "UTF-16", "UTF-16BE", "UTF-16LE", "UTF-8").map(Codec(_)).foreach(codec ⇒ {
        tmp.writeBytes("line1\r\nline2".getBytes(codec.charSet)).readString()(codec) === "line1\r\nline2"
      })
    })
  }

  test("write") {
    file.withTempFile(tmp ⇒ {
      tmp.write("content", append = false).readLines() === List("content")
      tmp.write("s", append = true).readLines() === List("contents")
      tmp.write("new content").readLines() === List("new content")
      tmp.write("s", append = true).readLines() === List("new contents")
    })
  }

  test("writeBytes") {
    file.withTempFile(tmp ⇒ {
      tmp.writeBytes("12".getBytes).readLines() === List("12")
      tmp.writeBytes("34".getBytes, append = true).readLines() === List("1234")
      tmp.writeBytes("56".getBytes).readLines() === List("56")
    })
  }

  test("writeLines") {
    file.withTempFile(tmp ⇒ {
      tmp.writeLines(List("1", "2")).readLines() === List("1", "2")
      tmp.writeLines(List("3", "4"), append = true).readLines() === List("1", "2", "3", "4")
      tmp.writeLines(List("5", "6")).readLines() === List("5", "6")
    })
  }

  test("md5") {
    file.withTempFile(tmp ⇒ {
      tmp.writeLines(List("blah")).md5() === "6f1ed002ab5595859014ebf0951522d9"
    })
  }

  test("missing") {
    assert(file.withTempFile(tmp ⇒ {
      assert(!tmp.missing)
      tmp
    }).missing)
  }

  test("hasExtension") { assertFileNameProperty(_.hasExtension("txt"), "a.txt",   "b.tmp") }
  test("isScala")      { assertFileNameProperty(_.isScala,             "a.scala", "b.java") }
  test("isJava")       { assertFileNameProperty(_.isJava,              "a.java",  "b.scala") }
  test("isClass")      { assertFileNameProperty(_.isClass,             "a.class", "b.txt") }
  test("isJar")        { assertFileNameProperty(_.isJar,               "a.jar",   "b.zip") }

  test("isParentOf") {
    file.withTempDirectory(dir ⇒ {
      assert(dir.isParentOf(dir / "child"))
      assert(!(dir / "child").isParentOf(dir))
      assert(!dir.isParentOf(dir / "child" / "kid"))
    })
  }

  test("isChildOf") {
    file.withTempDirectory(dir ⇒ {
      assert((dir / "child").isChildOf(dir))
      assert(!dir.isChildOf(dir / "child"))
      assert(!(dir / "child" / "kid").isChildOf(dir))
    })
  }

  test("contains") {
    file.withTempDirectory(dir ⇒ {
      assert(dir.contains(dir / "child"))
      assert(dir.contains(dir / "parent" / "kid"))
      assert(!(dir / "child").contains(dir))
    })
  }

  test("isContainedIn") {
    file.withTempDirectory(dir ⇒ {
      assert((dir / "child").isContainedIn(dir))
      assert((dir / "parent" / "kid").isContainedIn(dir))
      assert(!dir.isContainedIn(dir / "child"))
    })
  }

  test("className") {
    file.withTempDirectory(dir ⇒ {
      (dir / "Foo.class").className(dir) === "Foo"
      (dir / "com" / "example" / "Foo.class").className(dir) === "com.example.Foo"
    })
  }

  test("touch") {
    file.withTempDirectory(dir ⇒ {
      withTime(123000) {
        assert( 123000 == (dir / "child").touch().lastModified, "Should be able to touch non-existent file")
        (dir / "child").readBytes().length === 0
      }

      (dir / "child").writeLines(List("Don't touch this"))

      withTime(456000) {
        assert(456000 == (dir / "child").touch().lastModified, "Should be able to touch existing file")
        (dir / "child").readBytes().length === "Don't touch this".length + 1
      }

      withTime(789000) {
        dir.touch().lastModified === 789000
      }
    })
  }

  private def assertFileNameProperty(p: Predicate[File], success: String, failure: String): Unit = {
    file.withTempDirectory(dir ⇒ {
      assert(p(dir / success))
      assert(!p(dir / failure))
    })
  }

  private def assertIsTemp(
    expectedSuffix: String, expectedPrefix: String, expectedIsFile: Boolean, tmp: File): Unit = {

    assert(tmp.exists,s"Expected ${tmp.getName} to exist !" )

    assert(tmp.getName.startsWith(expectedPrefix),s"Expected ${tmp.getName}, to begin with $expectedPrefix" )

    assert(tmp.getName.endsWith(expectedSuffix), s"Expected ${tmp.getName}, to end with $expectedSuffix")

    assert(expectedIsFile == tmp.isFile,s"Expected ${tmp.getName} to be a " + (if (expectedIsFile) "file" else "directory") )

    assert(!shutdownHooks().contains(DeleteRecursively(tmp)),
      s"Expected ${tmp.getName} to not be deleted recursively on exit")
  }

  private def shutdownHooks(): Set[Thread] = {
    asScalaSetConverter(Class.forName("java.lang.ApplicationShutdownHooks")
      .getDeclaredField("hooks").tap(_.setAccessible(true))
      .get(null).asInstanceOf[_root_.java.util.Map[Thread, Thread]].keySet).asScala.toSet
  }

  private def relativeName(relativeTo: File, file: File): File =
    file.named(relativeTo.getName + "/" + file.relativeTo(relativeTo).getPath)
}