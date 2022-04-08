package Data

import Utils.Dictionary

trait ProductService:
  type BrandName = String
  type ProductName = String

  /**
    * Return the price of the brand name. If product not found then return NaN.
    * @param product product name of the brand
    * @param brand brand name to get the price
    * @return price of the brand
    */
  def getPrice(product: ProductName, brand: BrandName): Double

  /**
    * Return the default brand of the product. If not found then return empty string
    * @param product product name
    * @return get the default brand from the product name
    */
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:

  // Part 2 Step 2
  val brandsPrice: Map[String, Double] = Map(
    "farmer"-> 1,
    "boxer" -> 1,
    "wittekop" -> 2,
    "punkipa" -> 3,
    "jackhammer" -> 3,
    "tenebreuse" -> 4,
    "maison" -> 2,
    "cailler" -> 2,
  )

  def getPrice(product: ProductName, brand: BrandName): Double = {
    brandsPrice.getOrElse(brand,
      brandsPrice.getOrElse(getDefaultBrand(product), Double.NaN)
    )
  }

  def getDefaultBrand(product: ProductName): BrandName = {
    product match {
      case "biere" => "boxer"
      case "croissant" => "maison"
      case _ => ""
    }
  }
end ProductImpl
