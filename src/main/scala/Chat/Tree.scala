package Chat

/**
  * This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/**
  * Declarations of the nodes' types.
  */
object ExprTree:
  // TODO - Part 2 Step 3
  // Example cases
  case class Thirsty() extends ExprTree
  case class Hungry() extends ExprTree
  case class Identification(user: String) extends ExprTree
  case class Balance() extends ExprTree

  case class OrderRequest(request: ExprTree) extends ExprTree
  case class CostRequest(request: ExprTree) extends ExprTree

  case class And(e1: ExprTree, e2: ExprTree) extends ExprTree
  case class Or(e1: ExprTree, e2: ExprTree) extends ExprTree

  case class Order(quantity: Int, product: String, brand: String) extends ExprTree

  case class Cost(request: ExprTree) extends ExprTree

