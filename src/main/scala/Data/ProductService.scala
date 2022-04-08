package Data

trait ProductService:
  type BrandName = String
  type ProductName = String // TODO add enum ?

  def getPrice(product: ProductName, brand: BrandName): Double

  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService :
  // TODO - Part 2 Step 2

  val productsPrice: Map[BrandName, Double] = Map(
    "boxer" -> 1.0, "farmer" -> 1.0,
    "wittekop" -> 2.0, "punkipa" -> 3.0,
    "jackhammer" -> 3.0, "tenebreuse" -> 4.0,
    "maison" -> 2.0, "cailler" -> 2.0
  )

  def getPrice(product: ProductName, brand: String): Double =
    productsPrice.getOrElse(brand,
      productsPrice.getOrElse(getDefaultBrand(product), Double.NaN)
    )

  def getDefaultBrand(product: ProductName): BrandName =
    product match
      case "biere" => "boxer"
      case "croissant" => "maison"
      case _ => ""

end ProductImpl
