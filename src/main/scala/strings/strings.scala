package strings

import scala.util.{Success, Try}
import Data._

object strings extends App {

  /**
    * Obtener los números de una cadena
    * Dada una cadena con este formato:
    * categories:138 OR categories:101
    * se quiere obtener una lista ordenada con los números
    */

  val categoriesValues: List[Int] =
    stringToProcess
      .filterNot(_.isWhitespace)
      .split("[:OR]")
      .toList
      .filter(c => Try(c.toInt).isSuccess)
      .map(_.toInt)
      .sorted

  /**
    * Dada una cadena con el formato:
    * 170, 115
    * obtener una lista con el valor de los números ordenada
    */
  val categoriesValues2: List[Int] =
    """
      |138, 101, 88, 170, 115, 217, 120, 10, 142, 153, 174, 185, 24, 25, 14, 184, 110, 125, 196, 157, 189, 20, 93, 152, 29, 164, 179, 211, 106, 121, 147, 221, 132, 89, 133, 116, 206, 117, 201, 220, 102, 28, 160, 192, 21, 137, 165, 92, 197, 97, 224, 156, 9, 188, 169, 141, 109, 124, 225, 193, 212, 96, 173, 13, 129, 134, 128, 105, 205, 166, 148, 161, 180, 17, 149, 176, 191, 22, 118, 204, 27, 12, 144, 181, 159, 187, 172, 113, 219, 98, 208, 103, 140, 213, 91, 155, 198, 108, 130, 223, 135, 226, 167, 162, 209, 123, 194, 145, 18, 150, 95, 177, 182, 16, 127, 31, 154, 11, 175, 143, 99, 87, 218, 104, 26, 158, 186, 114, 171, 139, 23, 8, 119, 151, 168, 146, 30, 190, 183, 210, 107, 126, 136, 195, 94, 131, 163, 200, 178, 90, 111, 227, 122, 222, 100
      |""".stripMargin
      .filterNot(_.isWhitespace)
      .split(",")
      .toList
      .filter(c => Try(c.toInt).isSuccess)
      .map(_.toInt)
      .sorted

  /**
    * Dada una lista con el formato:
    * sizes:81.15 OR sizes:38.15
    * obtener
    */
  val adsSearchCleaned_ =
    sizesFilter
      .filterNot(_.isWhitespace)
      .split("OR")
      .toList
      //      .filter(c => Try(c.toInt).isSuccess)
      //      .map(_.toInt)
      .sorted
  val productSearchCleaned =
    productSearchFilterString
      .filterNot(_.isWhitespace)
      .split("OR")
      .toList
      //      .filter(c => Try(c.toInt).isSuccess)
      //      .map(_.toInt)
      .sorted
  //  println(adsSearchCleaned)
  //  println(productSearchCleaned)
  //  println(adsSearchCleaned.diff(productSearchCleaned))
  //  println(adsSearchCleaned.diff(productSearchCleaned))

  val fcNumbers = filterCategories
    .filterNot(_.isWhitespace)
    .split("[:OR]")
    .toList
    .filter(c => Try(c.toInt).isSuccess)
//    .map(_.toInt)
    .sorted

  val allCategories =
    categoriesAndDescription
      .split(",")
      .toList
      .map(_.split(":"))
      .map(arr => arr(0) -> arr(1))
      .toMap
//      .map(x => (x._1.toInt, x._2))

//  println(fcNumbers)
//  println(allCategories)
//  println(allCategories.head._1)

//  println(fcNumbers.size)
//  println(
//    s"final filtered ${allCategories.filterNot(x => fcNumbers.contains(x._1))}"
//  )

//  println(
//    fcNumbers.toSet.diff(allCategories.keySet)
//  )
//  println(
//    allCategories.keySet.diff(fcNumbers.toSet)
//  )
//  println(
//    allCategories.keys.toList.map(_.trim).diff(fcNumbers.map(_.trim))
//  )

//  println {
//    val catsIncluded = fcNumbers.map(_.trim).intersect(allCategories.keys.toList.map(_.trim))
//    val ls = allCategories.filterNot(x => catsIncluded.contains(x._1))
//    ls
//  }

  val productSearchSizesCleaned =
    Data.productSearchSizes
      .filterNot(_.isWhitespace)
      .split("[sizes: OR]")
      .toList
      .filterNot(_.isEmpty)
      .sorted
  val adsSearchSizesCleaned =
    Data.adsSearchSizes
      .filterNot(_.isWhitespace)
      .split("[sizes: OR]")
      .toList
      .filterNot(_.isEmpty)
      .sorted
  val productSearchCatesCleaned =
    Data.productSearchCates
      .filterNot(_.isWhitespace)
      .split("[categories: OR]")
      .toList
      .filterNot(_.isEmpty)

  val adsSearchCatesCleaned =
    Data.adsSearchCates
      .filterNot(_.isWhitespace)
      .split("[categories: OR]")
      .toList
      .filterNot(_.isEmpty)

  val adsSearchSizesNewLogicCleaned =
    Data.adsSearchSizesDefinitive
      .filterNot(_.isWhitespace)
      .split("[sizes: OR]")
      .toList
      .filterNot(_.isEmpty)
      .sorted
  val adsSearchCatesNewLogicCleaned =
    Data.adsSearchCatesDefinitive
      .filterNot(_.isWhitespace)
      .split("[categories: OR]")
      .toList
      .filterNot(_.isEmpty)
      .sorted
  val adsSearchSizesNewPRCleaned =
    Data.adsSearchSizesnewDefinPR
      .filterNot(_.isWhitespace)
      .split("[sizes: OR]")
      .toList
      .filterNot(_.isEmpty)
      .sorted
  val adsSearchCatesNewPRCleaned =
    Data.adsSearchCatesnewDefinPR
      .filterNot(_.isWhitespace)
      .split("[categories: OR]")
      .toList
      .filterNot(_.isEmpty)
      .sorted

  println(s"num of sizes ${productSearchSizesCleaned.size} productSearchSizes\n $productSearchSizesCleaned")
  println(s"num of sizes ${adsSearchSizesCleaned.size} adsSearchSizes\n $adsSearchSizesCleaned")

  println(s"diff between ads-search and product-search sizes\n ${adsSearchSizesCleaned.diff(productSearchSizesCleaned)}")
  println(s"diff between product-search and ads-search sizes\n ${productSearchSizesCleaned.diff(adsSearchSizesCleaned)}")

  println(s"num of cates ${productSearchCatesCleaned.size} productSearchCates\n ${productSearchCatesCleaned.toList.sorted}")
  println(s"num of cates ${adsSearchCatesCleaned.size} adsSearchCates\n ${adsSearchCatesCleaned.toList.sorted}")

  println(s"diff between ads-search and product-search categories\n ${adsSearchCatesCleaned.diff(productSearchCatesCleaned)}")
  println(s"diff between product-search and ads-search categories\n ${productSearchCatesCleaned.diff(adsSearchCatesCleaned)}")

  println(s"New logic | num of sizes ${adsSearchSizesNewLogicCleaned.size} sizes\n $adsSearchSizesNewLogicCleaned")
  println(s"New logic | num of cates ${adsSearchCatesNewLogicCleaned.size} cates\n ${adsSearchCatesNewLogicCleaned.toList.sorted}")
  println(s"New logic | diff between ads-search and product-search sizes\n ${adsSearchSizesNewLogicCleaned.diff(productSearchSizesCleaned)}")
  println(s"New logic | diff between product-search and ads-search sizes\n ${productSearchSizesCleaned.diff(adsSearchSizesNewLogicCleaned)}")
  println(s"New logic | diff between ads-search and product-search\n ${adsSearchCatesNewLogicCleaned.diff(productSearchCatesCleaned)}")
  println(s"New logic | diff between product-search and ads-search\n ${productSearchCatesCleaned.diff(adsSearchCatesNewLogicCleaned)}")

  println(s"New logic PR| num of sizes ${adsSearchSizesNewPRCleaned.size} sizes\n $adsSearchSizesNewPRCleaned")
  println(s"New logic PR| num of cates ${adsSearchCatesNewPRCleaned.size} cates\n ${adsSearchCatesNewPRCleaned.toList.sorted}")
  println(s"New logic PR| diff between ads-search and product-search sizes\n ${adsSearchSizesNewPRCleaned.diff(productSearchSizesCleaned)}")
  println(s"New logic PR| diff between product-search and ads-search sizes\n ${productSearchSizesCleaned.diff(adsSearchSizesNewPRCleaned)}")
  println(s"New logic PR| diff between ads-search and product-search\n ${adsSearchCatesNewPRCleaned.diff(productSearchCatesCleaned)}")
  println(s"New logic PR| diff between product-search and ads-search\n ${productSearchCatesCleaned.diff(adsSearchCatesNewPRCleaned)}")

}
