#pragma once
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "../types/types.h"

enum class KeywordType { Exit, Let, If, Else };

struct TokenKeyword {
  KeywordType keyword;
};

struct TokenType {
  TypeKeyword type;
};

struct TokenIdentifier {
  std::string name;
};

struct TokenInteger {
  int value;
};

struct TokenDecimal {
  double value;
};

struct TokenString {
  std::string value;
};

struct TokenPlus {};
struct TokenMinus {};
struct TokenStar {};
struct TokenSlash {};
struct TokenPercent {};
struct TokenLessThan {};
struct TokenGreaterThan {};
struct TokenAmpersand {};
struct TokenPipe {};
struct TokenBang {};

struct TokenPeriod {};
struct TokenColon {};
struct TokenOpenParen {};
struct TokenCloseParen {};
struct TokenOpenCurlyBracket {};
struct TokenCloseCurlyBracket {};
struct TokenStatementTerminator {};
struct TokenEqual {};
struct TokenComma {};

struct Token
    : std::variant<TokenKeyword, TokenType, TokenIdentifier, TokenInteger,
                   TokenDecimal, TokenString, TokenPlus, TokenMinus, TokenStar,
                   TokenSlash, TokenPercent, TokenLessThan, TokenGreaterThan,
                   TokenAmpersand, TokenPipe, TokenBang, TokenPeriod,
                   TokenColon, TokenOpenParen, TokenCloseParen,
                   TokenOpenCurlyBracket, TokenCloseCurlyBracket,
                   TokenStatementTerminator, TokenEqual, TokenComma> {
  using std::variant<TokenKeyword, TokenType, TokenIdentifier, TokenInteger,
                     TokenDecimal, TokenString, TokenPlus, TokenMinus,
                     TokenStar, TokenSlash, TokenPercent, TokenLessThan,
                     TokenGreaterThan, TokenAmpersand, TokenPipe, TokenBang,
                     TokenPeriod, TokenColon, TokenOpenParen, TokenCloseParen,
                     TokenOpenCurlyBracket, TokenCloseCurlyBracket,
                     TokenStatementTerminator, TokenEqual, TokenComma>::variant;
};

const std::unordered_map<std::string, KeywordType> keywordMap = {
    {"exit", KeywordType::Exit},
    {"let", KeywordType::Let},
    {"if", KeywordType::If},
    {"else", KeywordType::Else}};

std::vector<Token> tokenize(const std::string &input);
