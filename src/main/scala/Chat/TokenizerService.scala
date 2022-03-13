package Chat

import Chat.Token.*
import Utils.{Dictionary, SpellCheckerImpl, SpellCheckerService}

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  /**
    * Separate the user's input into tokens
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  // TODO - Part 1 Step 3
  def tokenize(input: String): Tokenized = {
    val spellChecker = SpellCheckerImpl(Dictionary.dictionary)
    var tokens = input.split("[ ']").map(s => s.replaceAll("[.,!?*]","")).filter(_.nonEmpty)
    tokens = tokens.map(s => spellChecker.getClosestWordInDictionary(s))
    TokenizedImpl(tokens.map(s => (s, getToken(s))))
  }

  def getToken(s: String): Token = {
    s match {
      case "bonjour" => Token.BONJOUR
      case "je" => Token.JE
      case "svp" => Token.SVP
      case "assoiffe" => Token.ASSOIFFE
      case "affame" => Token.AFFAME
        // Actions
      case "etre" => Token.ETRE
      case "vouloir" => Token.VOULOIR
        // Logic Operators
      case "et" => Token.ET
      case "ou" => Token.OU
        // Products
      case "biere" => Token.PRODUCT
      case "croissant" => Token.PRODUCT
        // Util
      case a if s.head == '_' => Token.PSEUDO
      case a if s.toDoubleOption.isDefined => Token.NUM
      case "EOL" => Token.EOL
      case "" => Token.BAD
      case _ => Token.UNKNOWN
    }
  }
end TokenizerService
