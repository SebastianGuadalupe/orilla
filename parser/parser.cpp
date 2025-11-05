#include "parser.h"
#include <variant>

namespace {

const Token &current(const std::vector<Token> &tokens, const size_t pos) {
  if (pos >= tokens.size())
    throw std::runtime_error("Unexpected end of input");
  return tokens[pos];
}

NodePtr parseExpression(const std::vector<Token> &tokens, size_t &pos);
NodePtr parseLogicalOr(const std::vector<Token> &tokens, size_t &pos);
NodePtr parseLogicalAnd(const std::vector<Token> &tokens, size_t &pos);
NodePtr parseEquality(const std::vector<Token> &tokens, size_t &pos);
NodePtr parseComparison(const std::vector<Token> &tokens, size_t &pos);
NodePtr parseAdditive(const std::vector<Token> &tokens, size_t &pos);
NodePtr parseMultiplicative(const std::vector<Token> &tokens, size_t &pos);
NodePtr parseFactor(const std::vector<Token> &tokens, size_t &pos);
NodePtr parseExit(const std::vector<Token> &tokens, size_t &pos);
NodePtr parseLet(const std::vector<Token> &tokens, size_t &pos, std::vector<IdentifierInfo> &identifiers);
NodePtr parseIf(const std::vector<Token> &tokens, size_t &pos, std::vector<IdentifierInfo> &identifiers);
std::vector<NodePtr> parseStatements(const std::vector<Token> &tokens, size_t &pos, std::vector<IdentifierInfo> &identifiers, bool (*untilPredicate)(const Token &));

struct StatementParser {
  const std::vector<Token> &tokens;
  size_t &pos;
  std::vector<NodePtr> &statements;
  std::vector<IdentifierInfo> &identifiers;

  void operator()(const TokenKeyword &keyword) const {
    if (keyword.keyword == KeywordType::Exit) {
      NodePtr stmt = parseExit(tokens, pos);
      statements.push_back(std::move(stmt));
      pos++;
    } else if (keyword.keyword == KeywordType::Let) {
      NodePtr stmt = parseLet(tokens, pos, identifiers);
      statements.push_back(std::move(stmt));
    } else if (keyword.keyword == KeywordType::If) {
      NodePtr stmt = parseIf(tokens, pos, identifiers);
      statements.push_back(std::move(stmt));
    } else if (keyword.keyword == KeywordType::Else) {
      statements.push_back(std::make_unique<Node>(NodeElse{}));
      pos++;
    } else {
      throw std::runtime_error(
          "Unexpected keyword: " +
          std::to_string(static_cast<int>(keyword.keyword)));
    }
  }

  void operator()(const TokenType &) const {
    std::cerr << "Error: Unexpected type keyword" << std::endl;
    throw std::runtime_error("Types cannot appear as statements");
  }

  void operator()(const TokenIdentifier &ident) const {
    if (pos + 1 < tokens.size() &&
        std::holds_alternative<TokenOpenParen>(tokens[pos + 1])) {
      auto expr = parseFactor(tokens, pos);

      statements.push_back(std::move(expr));
    } else {
      statements.push_back(std::make_unique<Node>(
          NodeIdentifier{ident.name, std::nullopt, nullptr}));
      pos++;
    }
  }

  void operator()(const TokenStatementTerminator &) const { pos++; }

  void operator()(const TokenInteger &) const {
    std::cerr << "Error: Unexpected integer literal" << std::endl;
    throw std::runtime_error("Integers cannot appear as statements");
  }

  void operator()(const TokenDecimal &) const {
    std::cerr << "Error: Unexpected decimal literal" << std::endl;
    throw std::runtime_error("Decimals cannot appear as statements");
  }

  void operator()(const TokenString &) const {
    std::cerr << "Error: Unexpected string literal" << std::endl;
    throw std::runtime_error("Strings cannot appear as statements");
  }

  void operator()(const TokenPlus &) const {
    std::cerr << "Error: Unexpected '+' operator" << std::endl;
    throw std::runtime_error("Operators cannot appear as statements");
  }

  void operator()(const TokenMinus &) const {
    std::cerr << "Error: Unexpected '-' operator" << std::endl;
    throw std::runtime_error("Operators cannot appear as statements");
  }

  void operator()(const TokenStar &) const {
    std::cerr << "Error: Unexpected '*' operator" << std::endl;
    throw std::runtime_error("Operators cannot appear as statements");
  }

  void operator()(const TokenSlash &) const {
    std::cerr << "Error: Unexpected '/' operator" << std::endl;
    throw std::runtime_error("Operators cannot appear as statements");
  }

  void operator()(const TokenPercent &) const {
    std::cerr << "Error: Unexpected '%' operator" << std::endl;
    throw std::runtime_error("Operators cannot appear as statements");
  }

  void operator()(const TokenLessThan &) const {
    std::cerr << "Error: Unexpected '<' operator" << std::endl;
    throw std::runtime_error("Operators cannot appear as statements");
  }

  void operator()(const TokenGreaterThan &) const {
    std::cerr << "Error: Unexpected '>' operator" << std::endl;
    throw std::runtime_error("Operators cannot appear as statements");
  }

  void operator()(const TokenAmpersand &) const {
    std::cerr << "Error: Unexpected '&' operator" << std::endl;
    throw std::runtime_error("Operators cannot appear as statements");
  }

  void operator()(const TokenPipe &) const {
    std::cerr << "Error: Unexpected '|' operator" << std::endl;
    throw std::runtime_error("Operators cannot appear as statements");
  }

  void operator()(const TokenBang &) const {
    std::cerr << "Error: Unexpected '!' operator" << std::endl;
    throw std::runtime_error("Operators cannot appear as statements");
  }

  void operator()(const TokenPeriod &) const {
    std::cerr << "Error: Unexpected '.'" << std::endl;
    throw std::runtime_error(
        "Periods cannot appear in the beginning of statements");
  }

  void operator()(const TokenOpenParen &) const {
    std::cerr << "Error: Unexpected '('" << std::endl;
    throw std::runtime_error(
        "Parenthesis cannot appear in the beginning of statements");
  }

  void operator()(const TokenCloseParen &) const {
    std::cerr << "Error: Unexpected ')'" << std::endl;
    throw std::runtime_error(
        "Parenthesis cannot appear in the beginning of statements");
  }

  void operator()(const TokenColon &) const {
    std::cerr << "Error: Unexpected ':'" << std::endl;
    throw std::runtime_error(
        "Colons cannot appear in the beginning of statements");
  }

  void operator()(const TokenEqual &) const {
    std::cerr << "Error: Unexpected '='" << std::endl;
    throw std::runtime_error(
        "Equal sign cannot appear in the beginning of statements");
  }

  void operator()(const TokenOpenCurlyBracket &) const {
    ++pos;
    std::vector<NodePtr> scopeStatements = parseStatements(tokens, pos, identifiers, [](const Token &token) { return std::holds_alternative<TokenCloseCurlyBracket>(token); });
    if (!std::holds_alternative<TokenCloseCurlyBracket>(current(tokens, pos))) {
      throw std::runtime_error("Expected '}' after '{'");
    }
    statements.push_back(
        std::make_unique<Node>(NodeScope{std::move(scopeStatements)}));
    pos++;
  }

  void operator()(const TokenCloseCurlyBracket &) const {
    std::cerr << "Error: Unexpected '}'" << std::endl;
    throw std::runtime_error("Unexpected '}'");
  }

  void operator()(const TokenComma &) const {
    std::cerr << "Error: Unexpected ','" << std::endl;
    throw std::runtime_error(
        "Commas cannot appear in the beginning of statements");
  }
};

std::vector<NodePtr> parseStatements(const std::vector<Token> &tokens, size_t &pos, std::vector<IdentifierInfo> &identifiers, bool (*untilPredicate)(const Token &)) {
  std::vector<NodePtr> statements;
  std::vector<IdentifierInfo> localIdentifiers = identifiers;
  while (pos < tokens.size() && !untilPredicate(current(tokens, pos))) {
    std::visit(StatementParser{tokens, pos, statements, localIdentifiers}, current(tokens, pos));
  }
  return statements;
}

NodePtr parseFactor(const std::vector<Token> &tokens, size_t &pos) {
  if (pos >= tokens.size())
    throw std::runtime_error("Expected factor");

  const Token &token = current(tokens, pos);

  if (const auto *intToken = std::get_if<TokenInteger>(&token)) {
    ++pos;
    return std::make_unique<Node>(NodeInteger{intToken->value});
  }

  if (const auto *decimalToken = std::get_if<TokenDecimal>(&token)) {
    ++pos;
    return std::make_unique<Node>(NodeDecimal{decimalToken->value});
  }

  if (const auto *stringToken = std::get_if<TokenString>(&token)) {
    ++pos;
    return std::make_unique<Node>(NodeString{stringToken->value});
  }

  if (std::holds_alternative<TokenKeyword>(token)) {
    if (std::get<TokenKeyword>(token).keyword == KeywordType::True) {
      ++pos;
      return std::make_unique<Node>(NodeBoolean{true});
    } else if (std::get<TokenKeyword>(token).keyword == KeywordType::False) {
      ++pos;
      return std::make_unique<Node>(NodeBoolean{false});
    }
  }

  if (std::holds_alternative<TokenOpenParen>(token)) {
    ++pos;
    auto expr = parseExpression(tokens, pos);

    if (!std::holds_alternative<TokenCloseParen>(current(tokens, pos))) {
      throw std::runtime_error("Expected ')' after expression");
    }
    ++pos;

    return expr;
  }

  if (const auto *identifierToken = std::get_if<TokenIdentifier>(&token)) {
    const std::string identifier = identifierToken->name;
    ++pos;

    if (pos < tokens.size() &&
        std::holds_alternative<TokenOpenParen>(current(tokens, pos))) {
      ++pos;

      std::vector<NodePtr> arguments;

      while (!std::holds_alternative<TokenCloseParen>(current(tokens, pos))) {
        arguments.push_back(parseExpression(tokens, pos));

        if (std::holds_alternative<TokenCloseParen>(current(tokens, pos))) {
          break;
        }

        if (!std::holds_alternative<TokenComma>(current(tokens, pos))) {
          throw std::runtime_error(
              "Expected ',' or ')' after function argument");
        }
        ++pos;
      }

      if (!std::holds_alternative<TokenCloseParen>(current(tokens, pos))) {
        throw std::runtime_error("Expected ')' after function call");
      }
      ++pos;

      return std::make_unique<Node>(
          NodeFunctionCall{identifier, std::move(arguments)});
    }
    return std::make_unique<Node>(
        NodeIdentifier{identifier, std::nullopt, nullptr});
  }

  throw std::runtime_error("Expected valid factor");
}

NodePtr parseMultiplicative(const std::vector<Token> &tokens, size_t &pos) {
  auto left = parseFactor(tokens, pos);

  while (pos < tokens.size()) {
    const Token &token = current(tokens, pos);

    if (std::holds_alternative<TokenStar>(token)) {
      ++pos;
      auto right = parseFactor(tokens, pos);
      left = std::make_unique<Node>(
          NodeBinaryOp{"*", std::move(left), std::move(right)});
    } else if (std::holds_alternative<TokenSlash>(token)) {
      ++pos;
      auto right = parseFactor(tokens, pos);
      left = std::make_unique<Node>(
          NodeBinaryOp{"/", std::move(left), std::move(right)});
    } else if (std::holds_alternative<TokenPercent>(token)) {
      ++pos;
      auto right = parseFactor(tokens, pos);
      left = std::make_unique<Node>(
          NodeBinaryOp{"%", std::move(left), std::move(right)});
    } else {
      break;
    }
  }

  return left;
}

NodePtr parseAdditive(const std::vector<Token> &tokens, size_t &pos) {
  auto left = parseMultiplicative(tokens, pos);

  while (pos < tokens.size()) {
    const Token &token = current(tokens, pos);

    if (std::holds_alternative<TokenPlus>(token)) {
      ++pos;
      auto right = parseMultiplicative(tokens, pos);
      left = std::make_unique<Node>(
          NodeBinaryOp{"+", std::move(left), std::move(right)});
    } else if (std::holds_alternative<TokenMinus>(token)) {
      ++pos;
      auto right = parseMultiplicative(tokens, pos);
      left = std::make_unique<Node>(
          NodeBinaryOp{"-", std::move(left), std::move(right)});
    } else {
      break;
    }
  }

  return left;
}

NodePtr parseComparison(const std::vector<Token> &tokens, size_t &pos) {
  auto left = parseAdditive(tokens, pos);

  while (pos < tokens.size()) {
    if (std::holds_alternative<TokenLessThan>(current(tokens, pos))) {
      ++pos;
      if (std::holds_alternative<TokenEqual>(current(tokens, pos))) {
        ++pos;
        auto right = parseAdditive(tokens, pos);
        left = std::make_unique<Node>(
            NodeBinaryOp{"<=", std::move(left), std::move(right)});
      } else {
        auto right = parseAdditive(tokens, pos);
        left = std::make_unique<Node>(
            NodeBinaryOp{"<", std::move(left), std::move(right)});
      }
    } else if (std::holds_alternative<TokenGreaterThan>(current(tokens, pos))) {
      ++pos;
      if (std::holds_alternative<TokenEqual>(current(tokens, pos))) {
        ++pos;
        auto right = parseAdditive(tokens, pos);
        left = std::make_unique<Node>(
            NodeBinaryOp{">=", std::move(left), std::move(right)});
      } else {
        auto right = parseAdditive(tokens, pos);
        left = std::make_unique<Node>(
            NodeBinaryOp{">", std::move(left), std::move(right)});
      }
    } else if (std::holds_alternative<TokenEqual>(current(tokens, pos))) {
      ++pos;
      if (std::holds_alternative<TokenLessThan>(current(tokens, pos))) {
        ++pos;
        auto right = parseAdditive(tokens, pos);
        left = std::make_unique<Node>(
            NodeBinaryOp{"<=", std::move(left), std::move(right)});
      } else if (std::holds_alternative<TokenGreaterThan>(
                     current(tokens, pos))) {
        ++pos;
        auto right = parseAdditive(tokens, pos);
        left = std::make_unique<Node>(
            NodeBinaryOp{">=", std::move(left), std::move(right)});
      } else {
        // Put back the = token, it's not part of a comparison
        --pos;
        break;
      }
    } else {
      break;
    }
  }

  return left;
}

NodePtr parseEquality(const std::vector<Token> &tokens, size_t &pos) {
  auto left = parseComparison(tokens, pos);

  while (pos < tokens.size()) {
    if (std::holds_alternative<TokenEqual>(current(tokens, pos))) {
      ++pos;
      if (std::holds_alternative<TokenEqual>(current(tokens, pos))) {
        ++pos;
        auto right = parseComparison(tokens, pos);
        left = std::make_unique<Node>(
            NodeBinaryOp{"==", std::move(left), std::move(right)});
      } else {
        throw std::runtime_error(
            "'=' is an invalid operator, use '==' for equality");
      }
    } else if (std::holds_alternative<TokenBang>(current(tokens, pos))) {
      ++pos;
      if (std::holds_alternative<TokenBang>(current(tokens, pos))) {
        throw std::runtime_error("'!!' is not a valid operator");
      } else if (std::holds_alternative<TokenEqual>(current(tokens, pos))) {
        ++pos;
        auto right = parseComparison(tokens, pos);
        left = std::make_unique<Node>(
            NodeBinaryOp{"!=", std::move(left), std::move(right)});
      } else {
        throw std::runtime_error("'!' is an invalid operator");
      }
    } else {
      break;
    }
  }

  return left;
}

NodePtr parseLogicalAnd(const std::vector<Token> &tokens, size_t &pos) {
  auto left = parseEquality(tokens, pos);

  while (pos < tokens.size()) {
    if (std::holds_alternative<TokenAmpersand>(current(tokens, pos))) {
      ++pos;
      if (std::holds_alternative<TokenAmpersand>(current(tokens, pos))) {
        ++pos;
        auto right = parseEquality(tokens, pos);
        left = std::make_unique<Node>(
            NodeBinaryOp{"&&", std::move(left), std::move(right)});
      } else {
        throw std::runtime_error(
            "'&' is an invalid operator, use '&&' for logical AND");
      }
    } else {
      break;
    }
  }

  return left;
}

NodePtr parseLogicalOr(const std::vector<Token> &tokens, size_t &pos) {
  auto left = parseLogicalAnd(tokens, pos);

  while (pos < tokens.size()) {
    if (std::holds_alternative<TokenPipe>(current(tokens, pos))) {
      ++pos;
      if (std::holds_alternative<TokenPipe>(current(tokens, pos))) {
        ++pos;
        auto right = parseLogicalAnd(tokens, pos);
        left = std::make_unique<Node>(
            NodeBinaryOp{"||", std::move(left), std::move(right)});
      } else {
        throw std::runtime_error(
            "'|' is an invalid operator, use '||' for logical OR");
      }
    } else {
      break;
    }
  }

  return left;
}

NodePtr parseExpression(const std::vector<Token> &tokens, size_t &pos) {
  return parseLogicalOr(tokens, pos);
}

NodePtr parseExit(const std::vector<Token> &tokens, size_t &pos) {
  ++pos;

  NodePtr expr = nullptr;
  if (std::holds_alternative<TokenOpenParen>(current(tokens, pos))) {
    ++pos;
    if (std::holds_alternative<TokenCloseParen>(current(tokens, pos))) {
      expr = std::make_unique<Node>(NodeInteger{0});
    } else {
      expr = parseExpression(tokens, pos);
    }
    if (!std::holds_alternative<TokenCloseParen>(current(tokens, pos))) {
      throw std::runtime_error("Expected ')' after exit expression");
    }
    ++pos;

    if (!std::holds_alternative<TokenStatementTerminator>(current(tokens, pos))) {
      throw std::runtime_error("Missing ';' after exit statement");
    }
  
    return std::make_unique<Node>(NodeExit{std::move(expr)});
  } else {
    throw std::runtime_error("Expected '(' after exit");
  }
}

std::optional<TypeKeyword>
inferType(const NodePtr &node, const std::vector<IdentifierInfo> &identifiers) {
  if (!node)
    return std::nullopt;

  return std::visit(
      [&identifiers]<typename ArgType>(
          ArgType &&arg) -> std::optional<TypeKeyword> {
        using T = std::decay_t<ArgType>;

        if constexpr (std::is_same_v<T, NodeInteger>) {
          return TypeKeyword::Int;
        } else if constexpr (std::is_same_v<T, NodeDecimal>) {
          return TypeKeyword::Double;
        } else if constexpr (std::is_same_v<T, NodeString>) {
          return TypeKeyword::String;
        } else if constexpr (std::is_same_v<T, NodeBoolean>) {
          return TypeKeyword::Bool;
        } else if constexpr (std::is_same_v<T, NodeIdentifier>) {
          if (arg.type.has_value()) {
            return arg.type.value();
          } else {
            const auto identifier =
                std::find_if(identifiers.begin(), identifiers.end(),
                             [&](const IdentifierInfo &info) {
                               return info.name == arg.identifier;
                             });
            if (identifier != identifiers.end()) {
              return identifier->type.value();
            }
            throw std::runtime_error("Undefined variable '" + arg.identifier +
                                     "'");
            return std::nullopt;
          }
        } else if constexpr (std::is_same_v<T, NodeBinaryOp>) {
          return inferType(arg.left, identifiers);
        } else if constexpr (std::is_same_v<T, NodeExit> ||
                             std::is_same_v<T, NodeProg> ||
                             std::is_same_v<T, NodeFunctionCall> ||
                             std::is_same_v<T, NodeScope> ||
                             std::is_same_v<T, NodeIf> ||
                             std::is_same_v<T, NodeElse> ||
                             std::is_same_v<T, NodeBoolean>) {
          return std::nullopt;
        } else {
          static_assert(sizeof(T) == 0, "Unhandled type in inferType");
          return std::nullopt;
        }
      },
      *node);
}

NodePtr parseLet(const std::vector<Token> &tokens, size_t &pos,
                 std::vector<IdentifierInfo> &identifiers) {
  ++pos;

  NodeIdentifier identifier;
  if (!std::holds_alternative<TokenIdentifier>(current(tokens, pos))) {
    throw std::runtime_error("Expected identifier after 'let'");
  }

  const Token &token = current(tokens, pos);
  const auto *identToken = std::get_if<TokenIdentifier>(&token);
  if (identToken == nullptr) {
    throw std::runtime_error("Expected identifier");
  }
  ++pos;
  identifier.identifier = identToken->name;

  if (std::holds_alternative<TokenColon>(current(tokens, pos))) {
    ++pos;

    if (!std::holds_alternative<TokenType>(current(tokens, pos))) {
      throw std::runtime_error("Expected type after ':'");
    }

    const auto *typeToken = std::get_if<TokenType>(&current(tokens, pos));
    if (typeToken == nullptr) {
      throw std::runtime_error("Expected type");
    }
    identifier.type = typeToken->type;
    ++pos;
  }

  if (std::holds_alternative<TokenStatementTerminator>(current(tokens, pos))) {
    if (!identifier.type.has_value()) {
      throw std::runtime_error(
          "Variable '" + identifier.identifier +
          "' must have a type annotation or an assignment");
    }
    identifiers.push_back(
        IdentifierInfo{identifier.identifier, identifier.type});
    return std::make_unique<Node>(NodeIdentifier{std::move(identifier)});
  }

  if (std::holds_alternative<TokenEqual>(current(tokens, pos))) {
    ++pos;
    auto value = parseExpression(tokens, pos);

    const auto inferredType = inferType(value, identifiers);

    if (!inferredType.has_value()) {
      throw std::runtime_error("Cannot infer type for variable '" +
                               identifier.identifier + "'");
    }

    if (identifier.type.has_value()) {
      if (identifier.type.value() != inferredType.value()) {
        // TODO: Better Type casting
        if (identifier.type != TypeKeyword::Float &&
            inferredType.value() != TypeKeyword::Double &&
            inferredType.value() != TypeKeyword::Int) {
          throw std::runtime_error(
              "Type mismatch for variable '" + identifier.identifier +
              "': expected " + typeToStringMap.at(identifier.type.value()) +
              ", but got " + typeToStringMap.at(inferredType.value()));
        }
      }
    } else {
      identifier.type = inferredType;
    }

    if (!std::holds_alternative<TokenStatementTerminator>(
            current(tokens, pos))) {
      throw std::runtime_error("Expected ';' after variable declaration");
    }

    identifier.value = std::move(value);
  } else {
    throw std::runtime_error("Expected '=' or ';' after variable declaration");
  }

  identifiers.push_back(IdentifierInfo{identifier.identifier, identifier.type});

  return std::make_unique<Node>(NodeIdentifier{std::move(identifier)});
}

NodePtr parseIf(const std::vector<Token> &tokens, size_t &pos,
                std::vector<IdentifierInfo> &identifiers) {
  ++pos;

  if (!std::holds_alternative<TokenOpenParen>(current(tokens, pos))) {
    throw std::runtime_error("Expected '(' after 'if'");
  }
  ++pos;
  auto condition = parseExpression(tokens, pos);

  NodePtr thenBranch;
  std::optional<NodePtr> elseBranch;

  if (!std::holds_alternative<TokenCloseParen>(current(tokens, pos))) {
    throw std::runtime_error("Expected ')' after condition");
  }
  ++pos;
  if (std::holds_alternative<TokenOpenCurlyBracket>(current(tokens, pos))) {
    ++pos;
    std::vector<NodePtr> thenBranchStatements = parseStatements(tokens, pos, identifiers, [](const Token &token) { return std::holds_alternative<TokenCloseCurlyBracket>(token); });
    thenBranch = std::make_unique<Node>(NodeScope{std::move(thenBranchStatements)});
    ++pos;
    if (std::holds_alternative<TokenKeyword>(current(tokens, pos))) {
      if (std::get<TokenKeyword>(current(tokens, pos)).keyword == KeywordType::Else) {
        ++pos;
        if (std::holds_alternative<TokenOpenCurlyBracket>(current(tokens, pos))) {
          ++pos;
          std::vector<NodePtr> elseBranchStatements = parseStatements(tokens, pos, identifiers, [](const Token &token) { return std::holds_alternative<TokenCloseCurlyBracket>(token); });
          elseBranch = std::make_unique<Node>(NodeScope{std::move(elseBranchStatements)});
          ++pos;
        } else {
          elseBranch = parseExpression(tokens, pos);
        }
      } else {
        elseBranch = parseExpression(tokens, pos);
        if (!std::holds_alternative<TokenStatementTerminator>(current(tokens, pos))) {
          throw std::runtime_error("Expected ';' after expression in else branch");
        }
        ++pos;
      }
    }
  } else {
    bool hasElse = false;
    size_t tempPos = pos;
    while (tempPos < tokens.size()) {
      if (std::holds_alternative<TokenStatementTerminator>(current(tokens, tempPos))) {
        break;
      } else if (std::holds_alternative<TokenKeyword>(current(tokens, tempPos))) {
        if (std::get<TokenKeyword>(current(tokens, tempPos)).keyword == KeywordType::Else) {
          hasElse = true;
          break;
        }
      }
      tempPos++;
    }
    if (!hasElse) {
      throw std::runtime_error("'if' must have both 'then' and 'else' branches when used as an expression.");
    }
    std::vector<NodePtr> thenBranchStatements = parseStatements(tokens, pos, identifiers, [](const Token &token) {
      if (std::holds_alternative<TokenKeyword>(token)) {
        if (std::get<TokenKeyword>(token).keyword == KeywordType::Else) {
          return true;
        }
      }
      return false;
    });
    ++pos;
    thenBranch = std::make_unique<Node>(NodeScope{std::move(thenBranchStatements)});
    std::vector<NodePtr> elseBranchStatements = parseStatements(tokens, pos, identifiers, [](const Token &token) { return std::holds_alternative<TokenStatementTerminator>(token); });
    elseBranch = std::make_unique<Node>(NodeScope{std::move(elseBranchStatements)});
  }

  return std::make_unique<Node>(NodeIf{std::move(condition), std::move(thenBranch), std::move(elseBranch)});
}

} // namespace

std::optional<NodeProg> parse(const std::vector<Token> &tokens) {
  NodeProg prog;

  try {
    size_t pos = 0;
    std::vector<IdentifierInfo> identifiers;
    prog.statements = std::move(parseStatements(tokens, pos, identifiers, [](const Token &) { return false; }));
  } catch (const std::exception &e) {
    std::cerr << "Parse error: " << e.what() << std::endl;
    return std::nullopt;
  }

  return prog;
}
