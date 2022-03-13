package Chat

import Chat.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  /**
    * Separate the user's input into tokens
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  // TODO - Part 1 Step 3
  def tokenize(input: String): Tokenized =
    val normalizedTokens = input.replaceAll("[,.!?*]", "")
      .replaceAll("\'", " ")
      .split("\\s+") // one or more space
      .map(rawToken => spellCheckerSvc.dictionary.getOrElse(rawToken, spellCheckerSvc.getClosestWordInDictionary(rawToken)))

    val tokens = normalizedTokens.map(normalizedToken =>
      normalizedToken match
        case "bonjour" => (normalizedToken, Token.BONJOUR)
        case "je" => (normalizedToken, Token.JE)
        case "svp" => (normalizedToken, Token.SVP)
        case "assoiffe" => (normalizedToken, Token.ASSOIFFE)
        case "affame" => (normalizedToken, Token.AFFAME)
        case "suis" => (normalizedToken, Token.ETRE)
        case "vouloir" => (normalizedToken, Token.VOULOIR)
        case "et" => (normalizedToken, Token.ET)
        case "ou" => (normalizedToken, Token.OU)
        case "biere" => (normalizedToken, Token.PRODUCT)
        case "croissant" => (normalizedToken, Token.PRODUCT)
        case token if token.head == '_' => (normalizedToken, Token.PSEUDO)
        case token if """(\d+)""".r.matches(token) => (normalizedToken, Token.NUM) // TODO trouver une facon d'appliquer le regex plus clean
        case "\n" => ("EOL", Token.EOL) // only support \n
        case _ => (normalizedToken, Token.UNKNOWN)
        // TODO et BAD ?
    )
    return new TokenizedImpl(tokens)
  end tokenize
end TokenizerService
