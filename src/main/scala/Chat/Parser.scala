package Chat

class UnexpectedTokenException(msg: String) extends Exception(msg){}

// TODO - step 4
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
  // TODO - Part 2 Step 4
  def parsePhrases() : ExprTree =
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
          Balance()
        else if curToken == COMMANDER then
          readToken()
          handleProductRequest(COMMANDER)
        else expected(CONNAITRE, COMMANDER)
      else if curToken == ETRE then
        readToken()
        if curToken == ASSOIFFE then
          readToken()
          Thirsty()
        else if curToken == AFFAME then
          readToken()
          Hungry()
        else if curToken == PSEUDO then
          readToken()
          Identification(curValue)
        else expected(ASSOIFFE, AFFAME, PSEUDO)
      else expected(VOULOIR, ETRE)
    else if curToken == QUEL then
      readToken()
      eat(ETRE)
      eat(PRIX)
      handleProductRequest(COUTER)
    else if curToken == COMBIEN then
      readToken()
      eat(COUTER)
      handleProductRequest(COUTER)
    else expected(JE, COUTER)

  def handleProductRequest(requestType : COUTER | COMMANDER): ExprTree =
    val quantity = eat(NUM)
    val productType = eat(PRODUCT)
    val prodctBrand = if curToken == MARQUE then
      eat(MARQUE)
    else
      readToken()
      null

    // Gets the correct request from the requestType
    val request: ExprTree = if requestType == COUTER then
      Cost(quantity, productType, prodctBrand)
    else if requestType == COMMANDER then
      Order(quantity, productType, prodctBrand)
    else expected(COUTER, COMMANDER)

    // Checks if there is another request else return the request
    if curToken == ET || curToken == OU then
      val request2 = handleProductRequest(requestType)
      if curToken == ET then
        And(request1, request2)
      else if curToken == OU then
        Or(request1, request2)
    else
      request
