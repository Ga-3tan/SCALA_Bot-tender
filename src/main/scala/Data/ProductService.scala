package Data

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

  val brands: Map[String, Double] = Map(
    "farmer"-> 1,
    "boxer" -> 1,
    "wittekop" -> 2,
    "punkipa" -> 3,
    "jackhammer" -> 3,
    "tenebreuse" -> 4,
    "maison" -> 2,
    "cailler" -> 2,
  )

  // TODO - Part 2 Step 2
  def getPrice(product: ProductName, brand: BrandName): Double = {
    brands.getOrElse(brand,
      brands.getOrElse(getDefaultBrand(product),
        Double.NaN)
    )
  }

  def getDefaultBrand(product: ProductName): BrandName = {
    product match {
      case "croissant" => "maison"
      case "biere" => "boxer"
      case _ => ""
    }
  }
end ProductImpl
