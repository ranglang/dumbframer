package utl

import java.io._
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipInputStream}

/**
  * Created by tian on 21/12/2016.
  */
//class Unzip {
//  import java.io.{ IOException, FileOutputStream, FileInputStream, File }
//  import java.util.zip.{ ZipEntry, ZipInputStream }

  /**
    * Created by anquegi on 04/06/15.
    */
  object Unzip extends App {

//    val INPUT_ZIP_FILE: String = "src/main/scala/utl/3.framer.zip";
//    val OUTPUT_FOLDER: String = "src/main/scala/utl/my-zip";

    def unzip(zipFile: InputStream, destination: Path): Unit = {
      val zis = new ZipInputStream(zipFile)

      Stream.continually(zis.getNextEntry).takeWhile(_ != null).foreach { file =>
        if (!file.isDirectory) {
          val outPath = destination.resolve(file.getName)
          val outPathParent = outPath.getParent
          if (!outPathParent.toFile.exists()) {
            outPathParent.toFile.mkdirs()
          }

          val outFile = outPath.toFile
          println(outFile.getPath)
          val out = new FileOutputStream(outFile)
          val buffer = new Array[Byte](4096)
          Stream.continually(zis.read(buffer)).takeWhile(_ != -1).foreach(out.write(buffer, 0, _))
        }
      }
    }
//    Unzip.unzip(new FileInputStream(INPUT_ZIP_FILE),new File(OUTPUT_FOLDER).toPath)
}
