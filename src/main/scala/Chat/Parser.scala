package Chat

class UnexpectedTokenException(msg: String) extends Exception(msg){}

// step 4
class Parser(tokenized: Tokenized):
  import ExprTree._
  import Chat.Token._

  // Start the process by reading the first token.
  var curTuple: (String, Token) = tokenized.nextToken()

  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = tokenized.nextToken()

  /** "Eats" the expected token and returns it value, or terminates with an error. */
  private def eat(token: Token): String =
    if token == curToken then
      val tmp = curValue
      readToken()
      tmp
    else expected(token)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type Token */
  private def expected(token: Token, more: Token*): Nothing =
    val expectedTokens = more.prepended(token).mkString(" or ")
    throw new UnexpectedTokenException(s"Expected: $expectedTokens, found: $curToken")

  /** the root method of the parser: parses an entry phrase */
  // Part 2 Step 4
  def parsePhrases() : ExprTree = {
    if curToken == BONJOUR then readToken()

    if curToken == JE then
      readToken()
      if curToken == ETRE then
        readToken()
        parseETRE()
      else if curToken == VOULOIR then
        readToken()
        parseVOULOIR()
      else expected(ETRE, VOULOIR)
    else if curToken == QUESTION then
      readToken()
      parseQUESTION()
    else expected(JE, QUESTION)
  }

  def parseETRE(): ExprTree = {
    if curToken == ASSOIFFE then
      readToken()
      Thirsty()
    else if curToken == AFFAME then
      readToken()
      Hungry()
    else if curToken == PSEUDO then
      Identify(curValue)
    else expected(ASSOIFFE, AFFAME, PSEUDO)
  }

  def parseVOULOIR(): ExprTree = {
    if curToken == CONNAITRE then
      readToken()
      if curToken == SOLDE then
        readToken()
        RequestBalance()
      else if curToken == QUESTION then
        readToken()
        parseQUESTION()
      else if curToken == PRIX then
        readToken()
        RequestPrice(parseProducts())
      else expected(SOLDE, QUESTION)
    else if curToken == SOLDE then
      readToken()
      RequestBalance()
    else if curToken == COMMANDER then
      readToken()
      RequestOrder(parseProducts())
    else if curToken == NUM then // Pas de readToken après !!
      RequestOrder(parseProducts())
    else expected(CONNAITRE, COMMANDER)
  }

  def parseQUESTION(): ExprTree = {
    if curToken == ETRE then
      readToken()
      eat(PRIX)
      RequestPrice(parseProducts())
    else if curToken == COUTER then
      readToken()
      RequestPrice(parseProducts())
    else expected(ETRE, COUTER)
  }

  def parseOneProduct(): ExprTree = {
    if curToken != NUM then expected(NUM)
    val num = curValue.toInt
    readToken()

    var brand: String = ""
    var product: String = ""

    if curToken == PRODUCT then
      product = curValue
      readToken()

    if curToken == MARQUE then
      brand = curValue
      readToken()

    if brand.isEmpty && product.isEmpty then expected(PRODUCT, MARQUE)

    Order(product, brand, num)
  }

  def parseProducts(tLeft: ExprTree = null): ExprTree = {
    var tRight: ExprTree = null
    tLeft match {
      case null => tRight = parseOneProduct()
      case _ =>
        if curToken == AND then
          readToken()
          tRight = And(tLeft, parseOneProduct())
        else if curToken == OR then
          readToken()
          tRight = Or(tLeft, parseOneProduct())
    }
    if curToken == AND || curToken == OR then
      parseProducts(tRight)
    else tRight
  }

