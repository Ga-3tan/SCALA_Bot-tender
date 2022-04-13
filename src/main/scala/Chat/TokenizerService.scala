package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  /**
    * Separate the user's input into tokens
    *
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  // TODO - Part 2 Step 1
  def tokenize(input: String): Tokenized =
    val normalizedTokens = input.replaceAll("[,.!?*]", "")
      .replaceAll("['’]", " ")
      .split("\\s+") // one or more space
      .map(rawToken => spellCheckerSvc.dictionary.getOrElse(rawToken, spellCheckerSvc.getClosestWordInDictionary(rawToken)))

    TokenizedImpl(normalizedTokens.map(s => (s, getToken(s))))
  end tokenize

  /**
    * return a Token based on a string
    *
    * @param word a string word
    * @return a Token
    */
  def getToken(word: String): Token = {
    word match {
      case "bonjour" => Token.BONJOUR
      case "je" => Token.JE
      case "svp" => Token.SVP
      case "assoiffe" => Token.ASSOIFFE
      case "affame" => Token.AFFAME
      // Actions
      case "etre" => Token.ETRE
      case "appeler" => Token.ETRE
      case "vouloir" => Token.VOULOIR
      case "connaitre" => Token.CONNAITRE
      case "combien" => Token.COMBIEN
      case "quel" => Token.QUEL
      case "commander" => Token.COMMANDER
      case "couter" => Token.COUTER
      case "solde" => Token.SOLDE
      case "prix" => Token.PRIX
      // Logic Operators
      case "et" => Token.ET
      case "ou" => Token.OU
      // Products
      case "biere" => Token.PRODUCT
      case "croissant" => Token.PRODUCT
      // Stop Words
      case "le" => Token.STOPWORD
      case "me" => Token.STOPWORD
      case "mon" => Token.STOPWORD
      case "de" => Token.STOPWORD

      // Brands
      case "maison" => Token.MARQUE
      case "cailler" => Token.MARQUE
      case "farmer" => Token.MARQUE
      case "boxer" => Token.MARQUE
      case "wittekop" => Token.MARQUE
      case "punkipa" => Token.MARQUE
      case "jackhammer" => Token.MARQUE
      case "tenebreuse" => Token.MARQUE
      // Util
      case _ if word.head == '_' => Token.PSEUDO
      case _ if word.toDoubleOption.isDefined => Token.NUM
      case _ => Token.UNKNOWN
    }
  }
end TokenizerService
