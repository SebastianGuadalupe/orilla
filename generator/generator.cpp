#include "generator.h"

namespace generator {

struct LibCFunctionInfo {
  std::string libcName;
  llvm::Type *returnType;
  std::vector<llvm::Type *> paramTypes;
  bool isVarArg;
};

llvm::Function *getOrDeclareLibCFunction(llvm::Module *TheModule,
                                         const LibCFunctionInfo &info) {
  llvm::Function *func = TheModule->getFunction(info.libcName);
  if (func) {
    return func;
  }

  llvm::FunctionType *funcType =
      llvm::FunctionType::get(info.returnType, info.paramTypes, info.isVarArg);

  func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage,
                                info.libcName, TheModule);

  return func;
}

std::unordered_map<std::string, LibCFunctionInfo>
createLibCFunctionMap(llvm::LLVMContext &Context) {
  std::unordered_map<std::string, LibCFunctionInfo> map;

  map["print"] = {"printf",
                  llvm::Type::getInt32Ty(Context),
                  {llvm::PointerType::get(Context, 0)},
                  true};

  map["puts"] = {"puts",
                 llvm::Type::getInt32Ty(Context),
                 {llvm::PointerType::get(Context, 0)},
                 false};

  return map;
}

void generateProg(const NodeProg &prog, llvm::LLVMContext &Context,
                  llvm::Module *TheModule, llvm::IRBuilder<> &Builder,
                  llvm::Function *currentFunc) {
  std::unordered_map<std::string, llvm::AllocaInst *> NamedValues;

  std::unordered_map<std::string, LibCFunctionInfo> libcFunctionMap =
      createLibCFunctionMap(Context);

  struct StatementGenerator {
    llvm::LLVMContext &Context;
    llvm::Module *TheModule;
    llvm::IRBuilder<> &Builder;
    std::unordered_map<std::string, llvm::AllocaInst *> &NamedValues;
    std::unordered_map<std::string, LibCFunctionInfo> &LibCFunctionMap;

    StatementGenerator(llvm::LLVMContext &Ctx, llvm::Module *Mod,
                       llvm::IRBuilder<> &Bld,
                       std::unordered_map<std::string, llvm::AllocaInst *> &NV,
                       std::unordered_map<std::string, LibCFunctionInfo> &LFM)
        : Context(Ctx), TheModule(Mod), Builder(Bld), NamedValues(NV),
          LibCFunctionMap(LFM) {}

    llvm::Value *generateExpr(const NodePtr &node) {
      if (!node)
        return nullptr;
      return std::visit(
          [this](const auto &n) -> llvm::Value * {
            return this->generateExprValue(n);
          },
          *node);
    }

    llvm::Value *generateExprValue(const NodeInteger &integer) const {
      return llvm::ConstantInt::get(llvm::Type::getInt32Ty(Context),
                                    integer.value);
    }

    llvm::Value *generateExprValue(const NodeDecimal &decimal) const {
      return llvm::ConstantFP::get(llvm::Type::getDoubleTy(Context),
                                   decimal.value);
    }

    llvm::Value *generateExprValue(const NodeString &string) const {
      return Builder.CreateGlobalString(string.value.c_str(), "str");
    }

    llvm::Value *generateExprValue(const NodeIdentifier &ident) {
      if (NamedValues.find(ident.identifier) == NamedValues.end()) {
        llvm::errs() << "Error: Undefined variable '" << ident.identifier
                     << "'\n";
        return nullptr;
      }
      llvm::AllocaInst *alloca = NamedValues[ident.identifier];
      llvm::Type *varType = alloca->getAllocatedType();
      return Builder.CreateLoad(varType, alloca, ident.identifier.c_str());
    }

    llvm::Value *generateExprValue(const NodeBinaryOp &op) {
      llvm::Value *left = generateExpr(op.left);
      llvm::Value *right = generateExpr(op.right);
      if (!left || !right)
        return nullptr;

      if (left->getType()->isIntegerTy() &&
          right->getType()->isFloatingPointTy()) {
        left = Builder.CreateSIToFP(left, right->getType());
      } else if (left->getType()->isFloatingPointTy() &&
                 right->getType()->isIntegerTy()) {
        right = Builder.CreateSIToFP(right, left->getType());
      }

      if (op.op == "+") {
        if (left->getType()->isFloatingPointTy())
          return Builder.CreateFAdd(left, right, "addtmp");
        return Builder.CreateAdd(left, right, "addtmp");
      } else if (op.op == "-") {
        if (left->getType()->isFloatingPointTy())
          return Builder.CreateFSub(left, right, "subtmp");
        return Builder.CreateSub(left, right, "subtmp");
      } else if (op.op == "*") {
        if (left->getType()->isFloatingPointTy())
          return Builder.CreateFMul(left, right, "multmp");
        return Builder.CreateMul(left, right, "multmp");
      } else if (op.op == "/") {
        if (left->getType()->isFloatingPointTy())
          return Builder.CreateFDiv(left, right, "divtmp");
        return Builder.CreateSDiv(left, right, "divtmp");
      } else if (op.op == "<") {
        if (left->getType()->isFloatingPointTy())
          return Builder.CreateFCmpULT(left, right, "cmptmp");
        return Builder.CreateICmpSLT(left, right, "cmptmp");
      } else if (op.op == ">") {
        if (left->getType()->isFloatingPointTy())
          return Builder.CreateFCmpUGT(left, right, "cmptmp");
        return Builder.CreateICmpSGT(left, right, "cmptmp");
      } else if (op.op == "<=") {
        if (left->getType()->isFloatingPointTy())
          return Builder.CreateFCmpULE(left, right, "cmptmp");
        return Builder.CreateICmpSLE(left, right, "cmptmp");
      } else if (op.op == ">=") {
        if (left->getType()->isFloatingPointTy())
          return Builder.CreateFCmpUGE(left, right, "cmptmp");
        return Builder.CreateICmpSGE(left, right, "cmptmp");
      } else if (op.op == "!=") {
        if (left->getType()->isFloatingPointTy())
          return Builder.CreateFCmpUNE(left, right, "cmptmp");
        return Builder.CreateICmpNE(left, right, "cmptmp");
      } else if (op.op == "==") {
        if (left->getType()->isFloatingPointTy())
          return Builder.CreateFCmpUEQ(left, right, "cmptmp");
        return Builder.CreateICmpEQ(left, right, "cmptmp");
      }
      llvm::errs() << "Error: Unknown binary operator '" << op.op << "'\n";
      return nullptr;
    }

    llvm::Value *generateExprValue(const NodeFunctionCall &func) {
      llvm::Function *calleeFunc = nullptr;

      if (LibCFunctionMap.find(func.functionName) != LibCFunctionMap.end()) {
        const LibCFunctionInfo &info = LibCFunctionMap[func.functionName];
        calleeFunc = getOrDeclareLibCFunction(TheModule, info);
      } else {
        calleeFunc = TheModule->getFunction(func.functionName);
        if (!calleeFunc) {
          llvm::errs() << "Error: Unknown function '" << func.functionName
                       << "'\n";
          return nullptr;
        }
      }

      std::vector<llvm::Value *> args;
      bool isVarArg = calleeFunc->isVarArg();
      for (const auto &arg : func.arguments) {
        llvm::Value *argVal = generateExpr(arg);
        if (!argVal)
          return nullptr;

        if (isVarArg && argVal->getType()->isFloatTy()) {
          argVal = Builder.CreateFPExt(argVal, llvm::Type::getDoubleTy(Context),
                                       "fpext");
        }

        args.push_back(argVal);
      }

      return Builder.CreateCall(calleeFunc, args, "calltmp");
    }

    llvm::Value *generateExprValue(const NodeScope & /*scope*/) {
      return nullptr;
    }

    llvm::Value *generateExprValue(const NodeIf &ifStmt) { return nullptr; }

    llvm::Value *generateExprValue(const NodeElse & /*else*/) {
      return nullptr;
    }

    llvm::Value *generateExprValue(const NodeExit & /*exit*/) {
      return nullptr;
    }

    llvm::Value *generateExprValue(const NodeProg & /*prog*/) {
      return nullptr;
    }

    void operator()(const NodeIdentifier &ident) {
      if (ident.value) {
        llvm::Type *varType = getLLVMType(ident.type);
        llvm::AllocaInst *alloca =
            Builder.CreateAlloca(varType, nullptr, ident.identifier.c_str());

        llvm::Value *initValue = generateExpr(ident.value);
        if (initValue) {
          // Type conversion if needed
          if (initValue->getType() != varType) {
            if (varType->isIntegerTy() &&
                initValue->getType()->isFloatingPointTy()) {
              // Floating point to integer conversion
              initValue = Builder.CreateFPToSI(initValue, varType);
            } else if (varType->isFloatingPointTy() &&
                       initValue->getType()->isIntegerTy()) {
              // Integer to floating point conversion
              initValue = Builder.CreateSIToFP(initValue, varType);
            } else if (varType->isFloatingPointTy() &&
                       initValue->getType()->isFloatingPointTy()) {
              // Floating point to floating point conversion
              if (varType->isFloatTy() && initValue->getType()->isDoubleTy()) {
                // Double to Float: truncate
                initValue =
                    Builder.CreateFPTrunc(initValue, varType, "fptrunc");
              } else if (varType->isDoubleTy() &&
                         initValue->getType()->isFloatTy()) {
                // Float to Double: extend
                initValue = Builder.CreateFPExt(initValue, varType, "fpext");
              }
            }
          }
          Builder.CreateStore(initValue, alloca);
        }
        NamedValues[ident.identifier] = alloca;
      } else if (ident.type.has_value()) {
        llvm::Type *varType = getLLVMType(ident.type);
        llvm::AllocaInst *alloca =
            Builder.CreateAlloca(varType, nullptr, ident.identifier.c_str());
        llvm::Value *zero = llvm::Constant::getNullValue(varType);
        Builder.CreateStore(zero, alloca);
        NamedValues[ident.identifier] = alloca;
      } else {
        generateExprValue(ident);
      }
    }

    void operator()(const NodeBinaryOp &op) { generateExprValue(op); }

    void operator()(const NodeInteger &integer) const {
      generateExprValue(integer);
    }

    void operator()(const NodeDecimal &decimal) const {
      generateExprValue(decimal);
    }

    void operator()(const NodeString &string) const {
      generateExprValue(string);
    }

    void operator()(const NodeFunctionCall &func) { generateExprValue(func); }

    void operator()(const NodeScope &scope) const {
      std::unordered_map<std::string, llvm::AllocaInst *> scopeNamedValues =
          NamedValues;
      for (const auto &statement : scope.statements) {
        std::visit(StatementGenerator{Context, TheModule, Builder,
                                      scopeNamedValues, LibCFunctionMap},
                   *statement);
      }
    }

    void operator()(const NodeExit &exit) {
      if (exit.exitValue) {
        llvm::Value *exitVal = generateExpr(exit.exitValue);
        if (exitVal) {
          // Convert to int32 if needed
          if (exitVal->getType()->isFloatingPointTy()) {
            exitVal =
                Builder.CreateFPToSI(exitVal, llvm::Type::getInt32Ty(Context));
          } else if (exitVal->getType()->isIntegerTy()) {
            exitVal = Builder.CreateIntCast(
                exitVal, llvm::Type::getInt32Ty(Context), true);
          }
          Builder.CreateRet(exitVal);
        } else {
          Builder.CreateRet(
              llvm::ConstantInt::get(llvm::Type::getInt32Ty(Context), 0));
        }
      } else {
        Builder.CreateRet(
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(Context), 0));
      }
    }

    void operator()(const NodeProg &prog) {
      for (const auto &statement : prog.statements) {
        std::visit(*this, *statement);
      }
    }

    void operator()(const NodeIf &ifStmt) {
      llvm::Value *condition = generateExpr(ifStmt.condition);
      if (!condition)
        return;

      llvm::BasicBlock *currentBB = Builder.GetInsertBlock();
      llvm::BasicBlock *thenBB =
          llvm::BasicBlock::Create(Context, "then", currentBB->getParent());
      llvm::BasicBlock *elseBB =
          llvm::BasicBlock::Create(Context, "else", currentBB->getParent());
      llvm::BasicBlock *mergeBB = nullptr;

      Builder.CreateCondBr(condition, thenBB, elseBB);

      Builder.SetInsertPoint(thenBB);
      std::unordered_map<std::string, llvm::AllocaInst *> thenNamedValues =
          NamedValues;
      std::visit(StatementGenerator{Context, TheModule, Builder,
                                    thenNamedValues, LibCFunctionMap},
                 *ifStmt.thenBranch);
      bool thenTerminates = thenBB->getTerminator() != nullptr;

      Builder.SetInsertPoint(elseBB);
      if (ifStmt.elseBranch.has_value()) {
        std::unordered_map<std::string, llvm::AllocaInst *> elseNamedValues =
            NamedValues;
        std::visit(StatementGenerator{Context, TheModule, Builder,
                                      elseNamedValues, LibCFunctionMap},
                   *ifStmt.elseBranch->get());
      }
      bool elseTerminates = elseBB->getTerminator() != nullptr;

      if (!thenTerminates || !elseTerminates) {
        mergeBB =
            llvm::BasicBlock::Create(Context, "merge", currentBB->getParent());

        if (!thenTerminates) {
          Builder.SetInsertPoint(thenBB);
          Builder.CreateBr(mergeBB);
        }
        if (!elseTerminates) {
          Builder.SetInsertPoint(elseBB);
          Builder.CreateBr(mergeBB);
        }

        Builder.SetInsertPoint(mergeBB);
      }
    }

    void operator()(const NodeElse & /*else*/) {
      // Skip else node, it's only a marker for the parser
    }

  private:
    llvm::Type *getLLVMType(const std::optional<TypeKeyword> &type) const {
      llvm::Type *result = nullptr;
      if (!type.has_value()) {
        throw std::runtime_error("Type not found");
      } else {
        switch (type.value()) {
        case TypeKeyword::Int:
          result = llvm::Type::getInt32Ty(Context);
          break;
        case TypeKeyword::Float:
          result = llvm::Type::getFloatTy(Context);
          break;
        case TypeKeyword::Double:
          result = llvm::Type::getDoubleTy(Context);
          break;
        case TypeKeyword::String:
          result = llvm::PointerType::get(Context, 0);
          break;
        case TypeKeyword::Bool:
          result = llvm::Type::getInt1Ty(Context);
          break;
        default:
          throw std::runtime_error("Unknown type");
          break;
        }
      }
      return result;
    }
  };

  StatementGenerator gen(Context, TheModule, Builder, NamedValues,
                         libcFunctionMap);
  gen(prog);
}

} // namespace generator

bool generate(NodeProg prog, const std::string &outputPath) {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  llvm::LLVMContext Context;
  const auto TheModule =
      std::make_unique<llvm::Module>(llvm::StringRef("main_module"), Context);
  llvm::IRBuilder<> Builder(Context);

  llvm::FunctionType *mainType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(Context), false);
  llvm::Function *mainFunc = llvm::Function::Create(
      mainType, llvm::Function::ExternalLinkage, "main", TheModule.get());

  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(Context, "entry", mainFunc);
  Builder.SetInsertPoint(entry);

  generator::generateProg(prog, Context, TheModule.get(), Builder, mainFunc);

  llvm::BasicBlock *currentBlock = Builder.GetInsertBlock();
  if (currentBlock && !currentBlock->getTerminator()) {
    Builder.CreateRet(
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(Context), 0));
  }

  if (llvm::verifyModule(*TheModule, &llvm::errs())) {
    llvm::errs() << "Module verification failed\n";
    return false;
  }

  std::error_code EC;

  llvm::Triple targetTriple(llvm::sys::getDefaultTargetTriple());
  std::string error;
  const llvm::Target *target =
      llvm::TargetRegistry::lookupTarget(targetTriple, error);

  llvm::TargetOptions opt;
  std::optional RM = llvm::Reloc::PIC_;
  std::optional<llvm::CodeModel::Model> CM = std::nullopt;
  llvm::CodeGenOptLevel OL = llvm::CodeGenOptLevel::Default;
  bool JIT = false;

  std::unique_ptr<llvm::TargetMachine> targetMachine(
      target->createTargetMachine(targetTriple, "generic", "", opt, RM, CM, OL,
                                  JIT));

  TheModule->setDataLayout(targetMachine->createDataLayout());
  TheModule->setTargetTriple(targetTriple);

  llvm::raw_fd_ostream dest(outputPath.c_str(), EC, llvm::sys::fs::OF_None);
  llvm::legacy::PassManager pass;
  targetMachine->addPassesToEmitFile(pass, dest, nullptr,
                                     llvm::CodeGenFileType::ObjectFile);
  pass.run(*TheModule);
  dest.flush();

  if (EC) {
    llvm::errs() << "Could not create object file: " << EC.message() << "\n";
    return false;
  }

  return true;
}
