package Chat
import Data.{AccountService, ProductService, Session}

class AnalyzerService(productSvc: ProductService,
                      accountSvc: AccountService):
  import ExprTree._
  /**
    * Compute the price of the current node, then returns it. If the node is not a computational node, the method
    * returns 0.0.
    * For example if we had a "+" node, we would add the values of its two children, then return the result.
    * @return the result of the computation
    */
  // TODO - Part 2 Step 3
  def computePrice(t: ExprTree): Double = {
    t match {
      case Command(t) => computePrice(t)
      case And(tLeft, tRight) => computePrice(tLeft) + computePrice(tRight)
      case Or(tLeft, tRight) => Math.min(computePrice(tLeft), computePrice(tRight))
      case Products(product, brand, number) => productSvc.getPrice(product, brand) * number
      case _ => 0.0
    }
  }

  /**
    * Return the output text of the current node, in order to write it in console.
    * @return the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)

    t match
      // TODO - Part 2 Step 3
      case Thirsty() => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry() => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
      case Identify(pseudo: String) =>
        session.setCurrentUser(pseudo.tail)
        if !accountSvc.isAccountExisting(pseudo.tail)
        then accountSvc.addAccount(pseudo.tail, 30)
        "Bonjour, " + pseudo.tail + "."
      case Command(t: ExprTree) =>
        if session.getCurrentUser.isDefined
        then
          val v = computePrice(t)
          "Voici donc " + inner(t) +
            " ! Cela coûte CHF " + v +
            " et votre nouveau solde est de CHF " +
            accountSvc.purchase(session.getCurrentUser.get, v)
        else "Veuillez d'abord vous identifier."
      case Solde() =>
        if session.getCurrentUser.isDefined
        then
          "Le montant actuel de votre solde est de CHF " +
            accountSvc.getAccountBalance(session.getCurrentUser.get)
        else "Veuillez d'abord vous identifier."
      case Prix(t) => "Cela coûte CHF " + computePrice(t)
      case And(tLeft, tRight) => inner(tLeft) + " et " + inner(tRight)
      case Or(tLeft, tRight) =>
        if computePrice(tLeft) < computePrice(tRight)
        then inner(tLeft)
        else inner(tRight)
      case Products(product, brand, number) =>
        if brand == null then number.toString + " " + productSvc.getDefaultBrand(product)
        else number.toString + " " + brand
end AnalyzerService
