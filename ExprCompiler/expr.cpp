#include <algorithm>
#include <cstddef>
#include <fstream>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace {

enum class TokenKind {
  Integer, Identifier, Input, Print, Plus, Minus, Star, Equal,
  LParen, RParen, End
};

struct Token {
  TokenKind kind;
  std::string text;
  std::size_t line;
  std::size_t column;
};

struct Expr {
  enum class Kind { Constant, Variable, Unary, Binary };
  Kind kind;
  int value = 0;
  std::string name;
  std::string cacheKey;
  char op = '\0';
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;
};

struct Statement {
  enum class Kind { Assign, Input, Print };
  Kind kind;
  std::size_t line;
  std::string target;
  std::unique_ptr<Expr> expr;
};

using Program = std::vector<Statement>;

[[noreturn]] void fail(std::size_t line, std::size_t column,
                       const std::string &message) {
  throw std::runtime_error("line " + std::to_string(line) + ", column " +
                           std::to_string(column) + ": " + message);
}

bool letter(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
bool digit(char c) { return c >= '0' && c <= '9'; }
bool nameStart(char c) { return letter(c) || c == '_'; }
bool namePart(char c) { return nameStart(c) || digit(c); }

std::vector<Token> tokenize(std::string_view line, std::size_t lineNumber) {
  std::vector<Token> tokens;
  for (std::size_t i = 0; i < line.size();) {
    const char c = line[i];
    if (c == ' ' || c == '\t' || c == '\r' || c == '\v' || c == '\f') {
      ++i;
      continue;
    }
    const auto start = i;
    if (nameStart(c)) {
      do { ++i; } while (i < line.size() && namePart(line[i]));
      std::string word(line.substr(start, i - start));
      const auto kind = word == "input" ? TokenKind::Input
                       : word == "print" ? TokenKind::Print
                                         : TokenKind::Identifier;
      tokens.push_back({kind, std::move(word), lineNumber, start + 1});
      continue;
    }
    if (digit(c)) {
      int value = 0;
      do {
        value = value * 10 + (line[i] - '0');
        if (value > 255) fail(lineNumber, start + 1, "integer literal exceeds 255");
        ++i;
      } while (i < line.size() && digit(line[i]));
      tokens.push_back({TokenKind::Integer, std::string(line.substr(start, i - start)),
                        lineNumber, start + 1});
      continue;
    }
    TokenKind kind;
    switch (c) {
    case '+': kind = TokenKind::Plus; break;
    case '-': kind = TokenKind::Minus; break;
    case '*': kind = TokenKind::Star; break;
    case '=': kind = TokenKind::Equal; break;
    case '(': kind = TokenKind::LParen; break;
    case ')': kind = TokenKind::RParen; break;
    default: fail(lineNumber, start + 1, "unsupported character '" +
                                          std::string(1, c) + "'");
    }
    tokens.push_back({kind, std::string(1, c), lineNumber, start + 1});
    ++i;
  }
  tokens.push_back({TokenKind::End, "", lineNumber, line.size() + 1});
  return tokens;
}

class Parser {
public:
  explicit Parser(std::vector<Token> tokens) : tokens_(std::move(tokens)) {}

  Statement parseStatement() {
    if (match(TokenKind::Print)) {
      const auto line = tokens_.front().line;
      expect(TokenKind::LParen, "expected '(' after print");
      auto expr = parseExpression();
      expect(TokenKind::RParen, "expected ')' after print expression");
      expect(TokenKind::End, "unexpected text after print statement");
      return {Statement::Kind::Print, line, "", std::move(expr)};
    }
    if (peek().kind == TokenKind::Input || peek().kind == TokenKind::Print)
      fail(peek().line, peek().column, "reserved word cannot be a variable");
    const Token target = expect(TokenKind::Identifier, "expected assignment or print");
    expect(TokenKind::Equal, "expected '=' after variable name");
    if (match(TokenKind::Input)) {
      expect(TokenKind::LParen, "expected '(' after input");
      expect(TokenKind::RParen, "input() takes no arguments");
      expect(TokenKind::End, "input() must be the entire assignment right side");
      return {Statement::Kind::Input, target.line, target.text, nullptr};
    }
    auto expr = parseExpression();
    expect(TokenKind::End, "unexpected text after assignment expression");
    return {Statement::Kind::Assign, target.line, target.text, std::move(expr)};
  }

private:
  std::vector<Token> tokens_;
  std::size_t pos_ = 0;

  const Token &peek() const { return tokens_[pos_]; }
  bool match(TokenKind kind) {
    if (peek().kind != kind) return false;
    ++pos_;
    return true;
  }
  Token expect(TokenKind kind, const char *message) {
    if (peek().kind != kind) fail(peek().line, peek().column, message);
    return tokens_[pos_++];
  }
  static std::unique_ptr<Expr> node(Expr::Kind kind) {
    auto result = std::make_unique<Expr>();
    result->kind = kind;
    return result;
  }
  std::unique_ptr<Expr> parseExpression() {
    auto lhs = parseTerm();
    while (peek().kind == TokenKind::Plus || peek().kind == TokenKind::Minus) {
      const char op = tokens_[pos_++].text[0];
      auto rhs = parseTerm();
      auto parent = node(Expr::Kind::Binary);
      parent->op = op;
      parent->left = std::move(lhs);
      parent->right = std::move(rhs);
      lhs = std::move(parent);
    }
    return lhs;
  }
  std::unique_ptr<Expr> parseTerm() {
    auto lhs = parseUnary();
    while (match(TokenKind::Star)) {
      auto rhs = parseUnary();
      auto parent = node(Expr::Kind::Binary);
      parent->op = '*';
      parent->left = std::move(lhs);
      parent->right = std::move(rhs);
      lhs = std::move(parent);
    }
    return lhs;
  }
  std::unique_ptr<Expr> parseUnary() {
    if (peek().kind == TokenKind::Plus || peek().kind == TokenKind::Minus) {
      const char op = tokens_[pos_++].text[0];
      auto result = node(Expr::Kind::Unary);
      result->op = op;
      result->left = parseUnary();
      return result;
    }
    return parsePrimary();
  }
  std::unique_ptr<Expr> parsePrimary() {
    if (peek().kind == TokenKind::Integer) {
      const Token token = tokens_[pos_++];
      auto result = node(Expr::Kind::Constant);
      for (char c : token.text) result->value = result->value * 10 + (c - '0');
      return result;
    }
    if (peek().kind == TokenKind::Identifier) {
      auto result = node(Expr::Kind::Variable);
      result->name = tokens_[pos_++].text;
      return result;
    }
    if (match(TokenKind::LParen)) {
      auto result = parseExpression();
      expect(TokenKind::RParen, "unmatched '(' in expression");
      return result;
    }
    if (peek().kind == TokenKind::Input)
      fail(peek().line, peek().column, "input() cannot appear inside an expression");
    fail(peek().line, peek().column, "expected integer, variable, or '(' in expression");
  }
};

Program parseSource(std::istream &source) {
  Program program;
  std::string line;
  std::size_t lineNumber = 0;
  while (std::getline(source, line)) {
    ++lineNumber;
    if (const auto hash = line.find('#'); hash != std::string::npos)
      line.erase(hash);
    auto tokens = tokenize(line, lineNumber);
    if (tokens.size() > 1) program.push_back(Parser(std::move(tokens)).parseStatement());
  }
  if (source.bad()) throw std::runtime_error("failed while reading source file");
  return program;
}

void checkExpr(const Expr &expr, const std::unordered_set<std::string> &defined,
               std::size_t line) {
  switch (expr.kind) {
  case Expr::Kind::Constant: break;
  case Expr::Kind::Variable:
    if (!defined.contains(expr.name))
      fail(line, 1, "undefined variable '" + expr.name + "'");
    break;
  case Expr::Kind::Unary: checkExpr(*expr.left, defined, line); break;
  case Expr::Kind::Binary:
    checkExpr(*expr.left, defined, line);
    checkExpr(*expr.right, defined, line);
    break;
  }
}

void checkNames(const Program &program) {
  std::unordered_set<std::string> defined;
  for (const auto &statement : program) {
    if (statement.expr) checkExpr(*statement.expr, defined, statement.line);
    if (statement.kind != Statement::Kind::Print) defined.insert(statement.target);
  }
}

int wrapByte(int value) { return (value % 256 + 256) % 256; }

void foldExpr(std::unique_ptr<Expr> &expr) {
  if (expr->left) foldExpr(expr->left);
  if (expr->right) foldExpr(expr->right);
  int value = 0;
  if (expr->kind == Expr::Kind::Unary &&
      expr->left->kind == Expr::Kind::Constant) {
    value = expr->op == '-' ? -expr->left->value : expr->left->value;
  } else if (expr->kind == Expr::Kind::Binary &&
             expr->left->kind == Expr::Kind::Constant &&
             expr->right->kind == Expr::Kind::Constant) {
    const int a = expr->left->value, b = expr->right->value;
    value = expr->op == '+' ? a + b : expr->op == '-' ? a - b : a * b;
  } else {
    return;
  }
  expr->kind = Expr::Kind::Constant;
  expr->value = wrapByte(value);
  expr->left.reset();
  expr->right.reset();
}

void substituteKnown(std::unique_ptr<Expr> &expr,
                     const std::unordered_map<std::string, int> &known) {
  if (expr->kind == Expr::Kind::Variable) {
    const auto it = known.find(expr->name);
    if (it != known.end()) {
      expr->kind = Expr::Kind::Constant;
      expr->value = it->second;
      expr->name.clear();
    }
    return;
  }
  if (expr->left) substituteKnown(expr->left, known);
  if (expr->right) substituteKnown(expr->right, known);
}

// Process assignments in source order so an RHS uses the previous value of
// its destination. Input invalidates a known value but is never removed here.
void propagateConstants(Program &program) {
  std::unordered_map<std::string, int> known;
  for (auto &statement : program) {
    if (statement.kind == Statement::Kind::Input) {
      known.erase(statement.target);
      continue;
    }
    substituteKnown(statement.expr, known);
    foldExpr(statement.expr);
    if (statement.kind == Statement::Kind::Assign) {
      if (statement.expr->kind == Expr::Kind::Constant)
        known[statement.target] = statement.expr->value;
      else
        known.erase(statement.target);
    }
  }
}

void collectReads(const Expr &expr, std::unordered_set<std::string> &live) {
  if (expr.kind == Expr::Kind::Variable) live.insert(expr.name);
  if (expr.left) collectReads(*expr.left, live);
  if (expr.right) collectReads(*expr.right, live);
}

void removeDeadAssignments(Program &program) {
  std::unordered_set<std::string> live;
  Program kept;
  for (auto it = program.rbegin(); it != program.rend(); ++it) {
    if (it->kind == Statement::Kind::Print) {
      collectReads(*it->expr, live);
    } else if (it->kind == Statement::Kind::Input) {
      live.erase(it->target);
    } else {
      if (!live.contains(it->target)) continue;
      live.erase(it->target);
      collectReads(*it->expr, live);
    }
    kept.push_back(std::move(*it));
  }
  std::reverse(kept.begin(), kept.end());
  program = std::move(kept);
}

void countProducts(Expr &expr,
                   const std::unordered_map<std::string, std::size_t> &versions,
                   std::unordered_map<std::string, std::size_t> &counts) {
  if (expr.left) countProducts(*expr.left, versions, counts);
  if (expr.right) countProducts(*expr.right, versions, counts);
  if (expr.kind != Expr::Kind::Binary || expr.op != '*' ||
      expr.left->kind != Expr::Kind::Variable ||
      expr.right->kind != Expr::Kind::Variable) return;
  auto left = expr.left->name + "#" + std::to_string(versions.at(expr.left->name));
  auto right = expr.right->name + "#" + std::to_string(versions.at(expr.right->name));
  if (right < left) std::swap(left, right);
  expr.cacheKey = left + "|" + right;
  ++counts[expr.cacheKey];
}

std::unordered_map<std::string, std::size_t> repeatedProducts(Program &program) {
  std::unordered_map<std::string, std::size_t> versions, counts;
  for (auto &statement : program) {
    if (statement.expr) countProducts(*statement.expr, versions, counts);
    if (statement.kind != Statement::Kind::Print) ++versions[statement.target];
  }
  for (auto it = counts.begin(); it != counts.end();) {
    if (it->second < 2) it = counts.erase(it);
    else ++it;
  }
  return counts;
}

class BrainfuckEmitter {
public:
  explicit BrainfuckEmitter(std::unordered_map<std::string, std::size_t> uses)
      : remaining_(std::move(uses)) {}
  std::string generate(const Program &program) {
    for (const auto &statement : program) emitStatement(statement);
    return std::move(output_);
  }

private:
  static constexpr std::size_t tapeSize = 1048576;
  std::string output_;
  std::size_t pointer_ = 0;
  std::size_t nextCell_ = 0;
  std::vector<std::size_t> freeTemps_;
  std::unordered_map<std::string, std::size_t> variableCells_;
  std::unordered_map<std::string, std::size_t> remaining_;
  std::unordered_map<std::string, std::size_t> cacheCells_;

  std::size_t allocate() {
    if (nextCell_ == tapeSize) throw std::runtime_error("Brainfuck tape exhausted");
    return nextCell_++;
  }
  std::size_t variableCell(const std::string &name) {
    auto [it, inserted] = variableCells_.try_emplace(name, nextCell_);
    if (inserted) allocate();
    return it->second;
  }
  std::size_t acquireTemp() {
    if (freeTemps_.empty()) return allocate();
    const auto cell = freeTemps_.back();
    freeTemps_.pop_back();
    return cell;
  }
  void moveTo(std::size_t cell) {
    if (cell > pointer_) output_.append(cell - pointer_, '>');
    else output_.append(pointer_ - cell, '<');
    pointer_ = cell;
  }
  void clear(std::size_t cell) {
    moveTo(cell);
    output_ += "[-]";
  }
  void releaseTemp(std::size_t cell, bool zero = false) {
    if (!zero) clear(cell);
    freeTemps_.push_back(cell);
  }
  void setConstant(std::size_t cell, int value) {
    moveTo(cell); // Every acquired temporary cell starts at zero.
    if (value <= 128) output_.append(static_cast<std::size_t>(value), '+');
    else output_.append(static_cast<std::size_t>(256 - value), '-');
  }
  // Destroy source while adding or subtracting it from destination.
  void addConsume(std::size_t source, std::size_t destination, bool subtract) {
    moveTo(source);
    output_ += '[';
    output_ += '-';
    moveTo(destination);
    output_ += subtract ? '-' : '+';
    moveTo(source);
    output_ += ']';
  }
  // Preserve source; scratch must start at zero and is zero again afterward.
  void copyAdd(std::size_t source, std::size_t destination,
               std::size_t scratch) {
    moveTo(source);
    output_ += "[-";
    moveTo(destination); output_ += '+';
    moveTo(scratch); output_ += '+';
    moveTo(source); output_ += ']';
    addConsume(scratch, source, false);
  }
  // Binary multiplication: halve the multiplier and double the multiplicand.
  std::size_t multiply(std::size_t multiplier, std::size_t multiplicand) {
    const auto result = acquireTemp();
    const auto half = acquireTemp();
    const auto parity = acquireTemp();
    const auto scratch = acquireTemp();
    moveTo(multiplier); output_ += "[[";
    output_ += '-';
    moveTo(scratch); output_ += '+';
    moveTo(parity); output_ += "[-";
    moveTo(half); output_ += '+';
    moveTo(scratch); output_ += '-';
    moveTo(parity); output_ += ']';
    moveTo(scratch); output_ += "[-";
    moveTo(parity); output_ += '+';
    moveTo(scratch); output_ += ']';
    moveTo(multiplier); output_ += ']';
    addConsume(half, multiplier, false);
    moveTo(parity); output_ += "[-";
    copyAdd(multiplicand, result, scratch);
    moveTo(parity); output_ += ']';
    moveTo(multiplicand); output_ += "[-";
    moveTo(scratch); output_ += "++";
    moveTo(multiplicand); output_ += ']';
    addConsume(scratch, multiplicand, false);
    moveTo(multiplier); output_ += ']';
    releaseTemp(multiplier, true);
    releaseTemp(multiplicand);
    releaseTemp(half, true);
    releaseTemp(parity, true);
    releaseTemp(scratch, true);
    return result;
  }
  std::size_t emitExpr(const Expr &expr) {
    if (auto use = remaining_.find(expr.cacheKey); use != remaining_.end()) {
      const auto cached = cacheCells_.find(expr.cacheKey);
      if (cached != cacheCells_.end()) {
        const auto result = acquireTemp();
        const auto scratch = acquireTemp();
        copyAdd(cached->second, result, scratch);
        releaseTemp(scratch, true);
        if (--use->second == 0) {
          releaseTemp(cached->second);
          cacheCells_.erase(cached);
        }
        return result;
      }
    }
    if (expr.kind == Expr::Kind::Constant) {
      const auto result = acquireTemp();
      setConstant(result, expr.value);
      return result;
    }
    if (expr.kind == Expr::Kind::Variable) {
      const auto source = variableCell(expr.name);
      const auto result = acquireTemp();
      const auto scratch = acquireTemp();
      copyAdd(source, result, scratch);
      releaseTemp(scratch, true);
      return result;
    }
    if (expr.kind == Expr::Kind::Unary) {
      const auto child = emitExpr(*expr.left);
      if (expr.op == '+') return child;
      const auto result = acquireTemp();
      addConsume(child, result, true);
      releaseTemp(child, true);
      return result;
    }
    if (expr.op == '*') {
      const Expr *operand = nullptr;
      int factor = 0;
      if (expr.left->kind == Expr::Kind::Constant) {
        operand = expr.right.get();
        factor = expr.left->value;
      } else if (expr.right->kind == Expr::Kind::Constant) {
        operand = expr.left.get();
        factor = expr.right->value;
      }
      if (operand) {
        if (factor == 0) return acquireTemp();
        if (factor == 1) return emitExpr(*operand);
        const auto source = emitExpr(*operand);
        const auto result = acquireTemp();
        moveTo(source);
        output_ += "[-";
        moveTo(result);
        if (factor <= 128) output_.append(static_cast<std::size_t>(factor), '+');
        else output_.append(static_cast<std::size_t>(256 - factor), '-');
        moveTo(source);
        output_ += ']';
        releaseTemp(source, true);
        return result;
      }
    }
    const auto left = emitExpr(*expr.left);
    const auto right = emitExpr(*expr.right);
    if (expr.op != '*') {
      addConsume(right, left, expr.op == '-');
      releaseTemp(right, true);
      return left;
    }
    const auto result = multiply(left, right);
    if (auto use = remaining_.find(expr.cacheKey); use != remaining_.end()) {
      --use->second;
      const auto cell = allocate();
      const auto scratch = acquireTemp();
      copyAdd(result, cell, scratch);
      releaseTemp(scratch, true);
      cacheCells_.emplace(expr.cacheKey, cell);
    }
    return result;
  }
  void emitStatement(const Statement &statement) {
    if (statement.kind == Statement::Kind::Input) {
      moveTo(variableCell(statement.target));
      output_ += ',';
      return;
    }
    if (statement.kind == Statement::Kind::Print &&
        statement.expr->kind == Expr::Kind::Variable) {
      moveTo(variableCell(statement.expr->name));
      output_ += '.';
      return;
    }
    const auto value = emitExpr(*statement.expr);
    if (statement.kind == Statement::Kind::Print) {
      moveTo(value);
      output_ += '.';
    } else {
      const auto destination = variableCell(statement.target);
      clear(destination);
      addConsume(value, destination, false);
    }
    releaseTemp(value, statement.kind == Statement::Kind::Assign);
  }
};

} // namespace

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: exprc input.expr > output.bf\n";
    return 43;
  }
  std::ifstream source(argv[1]);
  if (!source) {
    std::cerr << "Error: cannot open source file\n";
    return 43;
  }
  try {
    Program program = parseSource(source);
    checkNames(program);
    propagateConstants(program);
    removeDeadAssignments(program);
    BrainfuckEmitter emitter(repeatedProducts(program));
    std::string brainfuck = emitter.generate(program);
    std::cout << brainfuck;
    return 0;
  } catch (const std::exception &error) {
    std::cerr << "Error: " << error.what() << '\n';
    return 43;
  }
}
