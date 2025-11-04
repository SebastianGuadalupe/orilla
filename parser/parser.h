#pragma once
#include <iostream>
#include <memory>
#include <optional>
#include <stdexcept>
#include <variant>
#include <vector>

#include "../tokenizer/tokenizer.h"
#include "../types/types.h"

struct NodeProg;
struct NodeExit;
struct NodeIdentifier;
struct NodeBinaryOp;
struct NodeInteger;
struct NodeDecimal;
struct NodeString;
struct NodeFunctionCall;
struct NodeScope;
struct NodeIf;
struct NodeElse;

struct Node;
using NodePtr = std::unique_ptr<Node>;

struct NodeProg {
  std::vector<NodePtr> statements;
};
struct NodeExit {
  NodePtr exitValue;
};
struct NodeIdentifier {
  std::string identifier;
  std::optional<TypeKeyword> type;
  NodePtr value;
};
struct NodeBinaryOp {
  std::string op;
  NodePtr left, right;
};
struct NodeInteger {
  int value;
};
struct NodeDecimal {
  double value;
};
struct NodeString {
  std::string value;
};
struct NodeFunctionCall {
  std::string functionName;
  std::vector<NodePtr> arguments;
};
struct NodeScope {
  std::vector<NodePtr> statements;
};
struct NodeIf {
  NodePtr condition;
  NodePtr thenBranch;
  std::optional<NodePtr> elseBranch;
};
struct NodeElse {};
struct Node : std::variant<NodeProg, NodeExit, NodeIdentifier, NodeBinaryOp,
                           NodeInteger, NodeDecimal, NodeString,
                           NodeFunctionCall, NodeScope, NodeIf, NodeElse> {
  using std::variant<NodeProg, NodeExit, NodeIdentifier, NodeBinaryOp,
                     NodeInteger, NodeDecimal, NodeString, NodeFunctionCall,
                     NodeScope, NodeIf, NodeElse>::variant;
};

struct IdentifierInfo {
  std::string name;
  std::optional<TypeKeyword> type;
};

std::optional<NodeProg> parse(const std::vector<Token> &tokens);