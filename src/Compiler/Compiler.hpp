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
	using Value = llvm::Value;
	using Function = llvm::Function;
	using BasicBlock = llvm::BasicBlock;

	enum class Stage {
		Global, // Setting up global definitions. Like function prototypes.
		Normal, // Setting up most nodes.
	};

    llvm::AllocaInst* createEntryBlockStackAllocation (Function* function, llvm::Type* type, const std::string& name);

	struct Compiler {
		llvm::LLVMContext context;
		llvm::IRBuilder<> builder;
		std::unique_ptr<llvm::Module> modul;
		std::map<std::string, llvm::AllocaInst*> named_values;
		// This is ours now, so we do whatever to it.
		Parser::ParentASTNode nodes;

		Stage stage;

		explicit Compiler (Parser::ParentASTNode&& t_nodes);

		Value* logErrorValue (const char* str);

		void compile (std::ostream& output);
		void codegenGlobals ();

		void clearScope ();

		Function* getCurrentFunction ();
	};
}