#pragma once

#define LLVM_ENABLE_DUMP

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <memory>
#include <map>

#include "../Util/util.hpp"
#include "../Parser/Parser.hpp"

namespace Compiler {
	using namespace Parser;

	using Value = llvm::Value;
	using Function = llvm::Function;
	using BasicBlock = llvm::BasicBlock;

    llvm::AllocaInst* createEntryBlockStackAllocation (Function* function, llvm::Type* type, const std::string& name);

	struct Compiler {
		llvm::LLVMContext context;
		llvm::IRBuilder<> builder;
		std::unique_ptr<llvm::Module> modul;
		std::map<std::string, llvm::AllocaInst*> named_values;
		// This is ours now, so we do whatever to it.
		ParentASTNode nodes;

		explicit Compiler (ParentASTNode&& t_nodes);

		Value* logErrorValue (const char* str);

		void compile (std::ostream& output);
		void codegenGlobals ();

		Function* codegenFunctionPrototype (const std::string& name, const std::vector<std::unique_ptr<FunctionParameterInfo>>& parameters, const std::unique_ptr<TypeNode>& return_type);

		Function* codegenFunctionBody (std::unique_ptr<FunctionNode>&& func_node);

		void codegenStatement (std::unique_ptr<StatementASTNode>&& statement, Function* function);
		void codegenStatement (std::unique_ptr<VariableStatementNode>&& variable_decl, Function* function);
		void codegenStatement (std::unique_ptr<ReturnStatementNode>&& return_statement, Function* function);
		void codegenStatement (std::unique_ptr<IfStatementNode>&& if_statement, Function*);

		Value* codegenExpression (std::unique_ptr<BaseASTNode>&& expr);

		llvm::Type* convertPrimordialType (PrimordialType type);
	};
}