package Chat

import Data.{AccountService, ProductService, Session}

class AnalyzerService(productSvc: ProductService,
                      accountSvc: AccountService):

  import ExprTree._

  /**
    * Compute the price of the current node, then returns it. If the node is not a computational node, the method
    * returns 0.0.
    * For example if we had a "+" node, we would add the values of its two children, then return the result.
    *
    * @return the result of the computation
    */
  // TODO - Part 2 Step 3
  def computePrice(t: ExprTree): Double = t match
    case Cost(order) =>
      computePrice(order)
    case Order(quantity, product, brand) =>
      quantity * productSvc.getPrice(product, brand)
    case Or(e1, e2) =>                                                                // TODO useless
      computePrice(e1) min computePrice(e2)
    case And(e1, e2) =>
      computePrice(e1) + computePrice(e2)
    case _ => 0.0

  /**
    * Return the output text of the current node, in order to write it in console.
    *
    * @return the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    val identificationNeeded: String = "Veuillez d’abord vous identifier."

    t match
      // TODO - Part 2 Step 3
      /** User Interaction */
      case Identification(user) =>
        if !accountSvc.isAccountExisting(user) then accountSvc.addAccount(user, 30.0)
        session.setCurrentUser(user)
        s"Bonjour ${session.getCurrentUser.get} !"
      case Balance() =>
        if session.getCurrentUser.isDefined then
          s"Le montant actuel de votre solde est de CHF ${accountSvc.getAccountBalance(session.getCurrentUser.get)}"
        else
          identificationNeeded

      /** Requests */
      case OrderRequest(req) =>
        if session.getCurrentUser.isDefined then
          val cost = computePrice(req)
          val balance = accountSvc.purchase(session.getCurrentUser.get, cost)
          if balance == -1 then
            s"Cela nous coûte ${cost}. Vous n'avez pas assez d'argent"
          else
            s"Voici donc ${inner(req)} ! Cela nous coûte CHF ${cost} et votre nouveau solde est de CHF ${balance}."
        else
          identificationNeeded
      case CostRequest(req) =>
        s"Cela nous coûte CHF ${computePrice(req)} au total"

      /** Request Types */
      case Order(quantity, product, brand) =>
        s"${quantity} ${if brand == null then productSvc.getDefaultBrand(product) else brand}"
      case Cost(order) =>                                                                 // TODO Useless
        s"${computePrice(order)}"

      /** Logical Operator */
//      case and @ And(e1, e2) => (e1, e2) match                                          // TODO can be factorized ? no nested match case ?
//        case (Order(_, _, _), Order(_, _, _)) =>
//          s"${inner(e1)} et ${inner(e2)}"
//        case (Cost(_), Cost(_)) =>
//          s"${computePrice(and)}"

      case And(order1 @ Order(_, _, _), order2 @ Order(_, _, _)) =>
          s"${inner(order1)} et ${inner(order2)}"
      case and @ And(Cost(_), Cost(_)) =>
          s"${computePrice(and)}"
      case Or(e1, e2) =>
        val cost_1 = computePrice(e1)
        val cost_2 = computePrice(e2)
        if cost_1 < cost_2 then inner(e1) else inner(e2)

      /** User State */
      case Thirsty() => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry() => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"

end AnalyzerService
