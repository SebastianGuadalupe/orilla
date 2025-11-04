#include "tokenizer.h"

namespace {

std::optional<Token> readAlpha(const std::string &fileContents,
                               const size_t index, const size_t fileSize) {
  size_t i = index;
  std::stringstream buffer;
  while (i < fileSize && isalnum(fileContents[i])) {
    buffer << fileContents[i];
    i++;
  }

  if (const auto it = keywordMap.find(buffer.str()); it != keywordMap.end()) {
    return Token(TokenKeyword{it->second});
  }

  if (const auto it = stringToTypeMap.find(buffer.str());
      it != stringToTypeMap.end()) {
    return Token(TokenType{it->second});
  }

  return Token(TokenIdentifier{buffer.str()});
}

std::pair<std::optional<Token>, size_t>
readNumber(const std::string &fileContents, const size_t index,
           const size_t fileSize) {
  size_t i = index;
  std::stringstream buffer;
  bool isAlreadyDecimal = false;
  while (i < fileSize && (isdigit(fileContents[i]) || fileContents[i] == '.')) {
    if (fileContents[i] == '.') {
      if (isAlreadyDecimal) {
        std::cerr << "Invalid float literal at position " << i << std::endl;
        throw std::runtime_error("Failed to tokenize");
      }
      isAlreadyDecimal = true;
    }
    buffer << fileContents[i];
    i++;
  }
  if (isAlreadyDecimal) {
    return std::pair(Token(TokenDecimal{std::stod(buffer.str())}), i);
  }
  return std::pair(Token(TokenInteger{std::stoi(buffer.str())}), i);
}

std::pair<std::optional<Token>, size_t>
readString(const std::string &fileContents, const size_t index,
           const size_t fileSize) {
  size_t i = index + 1;
  std::stringstream buffer;

  while (i < fileSize) {
    if (fileContents[i] == '"') {
      return std::pair(Token(TokenString{buffer.str()}), i);
    } else if (fileContents[i] == '\\' && i + 1 < fileSize) {
      i++;
      switch (fileContents[i]) {
      case 'n':
        buffer << '\n';
        break;
      case 't':
        buffer << '\t';
        break;
      case 'r':
        buffer << '\r';
        break;
      case '\\':
        buffer << '\\';
        break;
      case '"':
        buffer << '"';
        break;
      case '0':
        buffer << '\0';
        break;
      default:
        std::cerr << "Unknown escape sequence \\" << fileContents[i]
                  << " at position " << i << std::endl;
        throw std::runtime_error("Failed to tokenize string at position " +
                                 std::to_string(index));
      }
      i++;
    } else {
      buffer << fileContents[i];
      i++;
    }
  }

  std::cerr << "Unterminated string literal at position " << index << std::endl;
  throw std::runtime_error("Failed to tokenize string at position " +
                           std::to_string(index));
}

struct TokenLengthVisitor {
  size_t operator()(const TokenKeyword &tok) const {
    for (const auto &[str, keyword] : keywordMap) {
      if (keyword == tok.keyword)
        return str.length();
    }
    return 0;
  }

  size_t operator()(const TokenType &tok) const {
    for (const auto &[str, type] : stringToTypeMap) {
      if (type == tok.type)
        return str.length();
    }
    return 0;
  }

  size_t operator()(const TokenIdentifier &tok) const {
    return tok.name.length();
  }

  size_t operator()(const TokenInteger &tok) const {
    return std::to_string(tok.value).length();
  }

  size_t operator()(const TokenDecimal &tok) const {
    return std::to_string(tok.value).length();
  }

  size_t operator()(const TokenString &tok) const {
    return tok.value.length() + 2;
  }

  size_t operator()(const TokenPlus &) const { return 1; }
  size_t operator()(const TokenMinus &) const { return 1; }
  size_t operator()(const TokenStar &) const { return 1; }
  size_t operator()(const TokenSlash &) const { return 1; }
  size_t operator()(const TokenPeriod &) const { return 1; }
  size_t operator()(const TokenColon &) const { return 1; }
  size_t operator()(const TokenOpenParen &) const { return 1; }
  size_t operator()(const TokenCloseParen &) const { return 1; }
  size_t operator()(const TokenOpenCurlyBracket &) const { return 1; }
  size_t operator()(const TokenCloseCurlyBracket &) const { return 1; }
  size_t operator()(const TokenStatementTerminator &) const { return 1; }
  size_t operator()(const TokenEqual &) const { return 1; }
  size_t operator()(const TokenComma &) const { return 1; }
  size_t operator()(const TokenPercent &) const { return 1; }
  size_t operator()(const TokenLessThan &) const { return 1; }
  size_t operator()(const TokenGreaterThan &) const { return 1; }
  size_t operator()(const TokenAmpersand &) const { return 1; }
  size_t operator()(const TokenPipe &) const { return 1; }
  size_t operator()(const TokenBang &) const { return 1; }
};

size_t getTokenLength(const Token &token) {
  return std::visit(TokenLengthVisitor{}, token);
}

} // namespace

std::vector<Token> tokenize(const std::string &input) {
  size_t const fileSize = input.size();
  std::vector<Token> tokens = {};

  for (size_t i = 0; i < fileSize; i++) {
    if (isspace(input[i])) {
    } else if (isalpha(input[i])) {
      if (std::optional<Token> maybeToken = readAlpha(input, i, fileSize);
          maybeToken.has_value()) {
        tokens.push_back(maybeToken.value());
        i = i + getTokenLength(maybeToken.value()) - 1;
      } else {
        std::cerr << "Error: Could not read token at position " << i
                  << std::endl;
        throw std::runtime_error("Failed to tokenize alpha at position " +
                                 std::to_string(i));
      }
    } else if (isdigit(input[i])) {
      if (auto [maybeToken, finalIndex] = readNumber(input, i, fileSize);
          maybeToken.has_value()) {
        tokens.push_back(maybeToken.value());
        i = finalIndex - 1;
      } else {
        throw std::runtime_error("Failed to tokenize number at position " +
                                 std::to_string(i));
      }
    } else if (input[i] == ';') {
      tokens.emplace_back(TokenStatementTerminator{});
    } else if (input[i] == '+') {
      tokens.emplace_back(TokenPlus{});
    } else if (input[i] == '-') {
      tokens.emplace_back(TokenMinus{});
    } else if (input[i] == '*') {
      tokens.emplace_back(TokenStar{});
    } else if (input[i] == '/') {
      if (input[i + 1] == '/') {
        while (i < fileSize && input[i] != '\n') {
          i++;
        }
      } else {
        tokens.emplace_back(TokenSlash{});
      }
    } else if (input[i] == '%') {
      tokens.emplace_back(TokenPercent{});
    } else if (input[i] == '<') {
      tokens.emplace_back(TokenLessThan{});
    } else if (input[i] == '>') {
      tokens.emplace_back(TokenGreaterThan{});
    } else if (input[i] == '&') {
      tokens.emplace_back(TokenAmpersand{});
    } else if (input[i] == '|') {
      tokens.emplace_back(TokenPipe{});
    } else if (input[i] == '!') {
      tokens.emplace_back(TokenBang{});
    } else if (input[i] == '.') {
      tokens.emplace_back(TokenPeriod{});
    } else if (input[i] == ':') {
      tokens.emplace_back(TokenColon{});
    } else if (input[i] == '(') {
      tokens.emplace_back(TokenOpenParen{});
    } else if (input[i] == ')') {
      tokens.emplace_back(TokenCloseParen{});
    } else if (input[i] == '{') {
      tokens.emplace_back(TokenOpenCurlyBracket{});
    } else if (input[i] == '}') {
      tokens.emplace_back(TokenCloseCurlyBracket{});
    } else if (input[i] == '=') {
      tokens.emplace_back(TokenEqual{});
    } else if (input[i] == ',') {
      tokens.emplace_back(TokenComma{});
    } else if (input[i] == '"') {
      if (auto [maybeToken, finalIndex] = readString(input, i, fileSize);
          maybeToken.has_value()) {
        tokens.push_back(maybeToken.value());
        i = finalIndex;
      } else {
        std::cerr << "Error: Could not read string at position " << i
                  << std::endl;
        throw std::runtime_error("Failed to tokenize string at position " +
                                 std::to_string(i));
      }
    } else {
      std::cerr << "Error: Unexpected character '" << input[i]
                << "' at position " << i << std::endl;
      throw std::runtime_error("Failed to tokenize at position " +
                               std::to_string(i));
    }
  }

  return tokens;
}
