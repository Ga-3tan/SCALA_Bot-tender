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
  case class Identify(pseudo: String) extends ExprTree
  case class Command(t: ExprTree) extends ExprTree
  case class Solde(t: ExprTree) extends ExprTree
  case class Prix(t: ExprTree) extends ExprTree
  case class And(tLeft: ExprTree, tRight: ExprTree) extends ExprTree
  case class Or(tLeft: ExprTree, tRight: ExprTree) extends ExprTree
  case class Products(product: String, brand: String, number: Int) extends ExprTree
