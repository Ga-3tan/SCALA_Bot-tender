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
  def computePrice(t: ExprTree): Double = ???

  /**
    * Return the output text of the current node, in order to write it in console.
    * @return the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    val identificationNeeded: String = "Veuillez d’abord vous identifier."

    t match
      // TODO - Part 2 Step 3
      // Example cases
      case Identification(user) =>
        if !accountSvc.isAccountExisting(user) then accountSvc.addAccount(user, 30.0)
        session.setCurrentUser(user)
      case Balance() =>
        if session.getCurrentUser.isDefined then
          s"Le montant actuel de votre solde est de CHF ${accountSvc.getAccountBalance(session.getCurrentUser)}"
        else
          identificationNeeded
      case Request(request) =>
      case Thirsty() => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry() => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"

end AnalyzerService
