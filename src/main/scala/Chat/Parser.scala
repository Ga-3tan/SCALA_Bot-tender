package Chat

class UnexpectedTokenException(msg: String) extends Exception(msg) {}

// TODO - step 4
class Parser(tokenized: Tokenized):

  import ExprTree._
  import Chat.Token._

  // Start the process by reading the first token.
  var curTuple: (String, Token) = tokenized.nextToken()

  def curValue: String = curTuple._1

  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  /** When meeting a STOPWORD Token, ignore it and skip to the next token   */
  def readToken(): Unit =
    val token = tokenized.nextToken()
    if token._2 == Token.STOPWORD then
      readToken()
    else
      curTuple = token

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
  // TODO - Part 2 Step 4
  def parsePhrases(): ExprTree =
    if curToken == BONJOUR then
      readToken()
    if curToken == JE then
      readToken()
      if curToken == VOULOIR then
        // FROM HERE : SHOULD BE LOGGED
        readToken()
        if curToken == CONNAITRE then
          readToken()
          eat(SOLDE)
          Balance() // Check balance
        else if curToken == COMMANDER then
          readToken()
          OrderRequest(handleProductRequest(COMMANDER)) // Handle request order
        else expected(CONNAITRE, COMMANDER)
      else if curToken == ETRE then
        readToken()
        if curToken == ASSOIFFE then
          readToken()
          Thirsty() // Thirsty
        else if curToken == AFFAME then
          readToken()
          Hungry() // Hungry
        else if curToken == PSEUDO then
          Identification(curValue) // Identify user
        else expected(ASSOIFFE, AFFAME, PSEUDO)
      else expected(VOULOIR, ETRE)
    else if curToken == QUEL then
      readToken()
      eat(ETRE)
      eat(PRIX)
      CostRequest(handleProductRequest(COUTER)) // Handle price request
    else if curToken == COMBIEN then
      readToken()
      eat(COUTER)
      CostRequest(handleProductRequest(COUTER)) // Handle price request
    else expected(JE, COUTER)

  def handleProductRequest(requestType: Token): ExprTree =
    val quantity = eat(NUM).toInt
    val productType = eat(PRODUCT)
    val prodctBrand = if curToken == MARQUE then
      eat(MARQUE)
    else
      null

    // Gets the correct request from the requestType
    val request: ExprTree = if requestType == COUTER then
      Cost(Order(quantity, productType, prodctBrand)) // Compute Cost
    else if requestType == COMMANDER then
      Order(quantity, productType, prodctBrand) // Compute Order
    else expected(COUTER, COMMANDER)

    // Checks if there is another request else return the request
    if curToken == ET then
      readToken()
      val request2 = handleProductRequest(requestType)
      And(request, request2) // Handle And request
    else if curToken == OU then
      readToken()
      val request2 = handleProductRequest(requestType)
      Or(request, request2) // Handle Or request
    else
      request
