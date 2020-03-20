#include "Compiler.hpp"
#include "../Util/llvm_stdostream.hpp"

#include <iostream>
#include <ostream>

#include <llvm/Support/Casting.h>

namespace Compiler {
    // ==== Utility ====
    llvm::AllocaInst* createEntryBlockStackAllocation (Function* function, llvm::Type* type, const std::string& name) {
        llvm::IRBuilder<> temp(&function->getEntryBlock(), function->getEntryBlock().begin());
        return temp.CreateAlloca(type, nullptr, name);
    }

    // ==== Compiler Class ====

    Compiler::Compiler (Parser::ParentASTNode&& t_nodes) : builder(context), nodes(std::move(t_nodes)) {
        stage = Stage::Global;
        modul = std::make_unique<llvm::Module>("Cackel", context);
    }

    void Compiler::compile (std::ostream& output) {
        codegenGlobals();

        stage = Stage::Normal;
        for (std::unique_ptr<Parser::DeclASTNode>& r_node : nodes.nodes) {
            r_node->codegen(*this);
        }

        auto bridge = Util::LLVM::OStreamBridge(output);

        modul->print(bridge, nullptr, false, false);
    }

    /// Generates 'global' data that should be able to referenced.
    /// This (sadly) does it's own pass.
    /// Generates all function prototypes (ignoring the function bodies)
    void Compiler::codegenGlobals () {
        for (const auto& r_node : nodes.nodes) {
            r_node->codegen(*this);
        }
    }

    void Compiler::clearScope () {
        named_values.clear();
    }
};