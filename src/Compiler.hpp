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

#include "Util/util.hpp"
#include "Parser.hpp"

namespace Compiler {
	using namespace llvm;
	using namespace Parser;

    AllocaInst* createEntryBlockStackAllocation (Function* function, Type* type, const std::string& name);

	struct Compiler {
		LLVMContext context;
		IRBuilder<> builder;
		std::unique_ptr<Module> modul;
		std::map<std::string, AllocaInst*> named_values;

		ParentASTNode nodes;

		explicit Compiler (ParentASTNode t_nodes);

		Value* logErrorValue (const char* str);

		void compile (std::ostream& output);

		Function* codegenFunctionPrototype (const std::string& identifier, const std::vector<FunctionParameterNode>& parameters, const TypeNode& return_type);

		Function* codegenFunctionBody (const FunctionNode& node);

		void codegenStatement (const StatementNode& statement, Function* function);
		void codegenStatement (const ExpressionNode& expression, Function* function);
		void codegenStatement (const VariableStatementNode& variable_decl, Function* function);
		void codegenStatement (const ReturnStatementNode& return_statement, Function* function);

		Value* codegenExpression (const ExpressionNode& expression);

		Type* convertPrimordialType (const PrimordialTypeNode& type_node);
	};
}