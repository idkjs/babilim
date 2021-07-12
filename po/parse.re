let file = input => {
  let ch = open_in(input);
  let lex = Lexing.from_channel(ch);
  Parser.file(Lexer.main, lex);
};
