package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:

  val brands: Map[String, Double] = Map(
    "farmer"-> 1,
    "boxer" -> 1,
    "wittekop" -> 2,
    "punkipa" -> 3,
    "jackhammer" -> 3,
    "ténébreuse" -> 4,
    "maison" -> 2,
    "cailler" -> 2,
  )

  // TODO - Part 2 Step 2
  // Return the price of the brand name. If product not found then return NaN.
  def getPrice(product: ProductName, brand: BrandName): Double = {
    brands.getOrElse(brand,
      brands.getOrElse(getDefaultBrand(product),
        Double.NaN)
    )
  }

  // Return the default brand name of the product. If not found then return empty string
  def getDefaultBrand(product: ProductName): BrandName = {
    product match {
      case "croissant" => "maison"
      case "biere" => "boxer"
      case _ => ""
    }
  }
end ProductImpl
