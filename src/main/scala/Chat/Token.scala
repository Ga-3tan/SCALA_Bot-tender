package Chat

enum Token:
  case // Terms
       // TODO - Part 2 Step 1
       BONJOUR,
       SVP,
       JE,
       // Etat
       ASSOIFFE,
       AFFAME,
       // Actions
       ETRE,
       VOULOIR,
       COMMANDER,
       CONNAITRE,
       COUTER,
       // Logic Operators
       OPERATOR,
       // Products
       PRODUCT,
       MARQUE,
       // Util
       PSEUDO,
       NUM,
       SOLDE,
       PRIX,
       QUESTION,
       STOPWORD,
       EOL,
       UNKNOWN,
       BAD
end Token
