#include "FunctionExitVerifier.hpp"

#include <llvm/Support/Casting.h>
#include <iostream>

namespace ParserPass {
    FunctionExitVerifier::FunctionExitVerifier (Parser::ParentASTNode& t_nodes) : nodes(t_nodes) {
        transform();
    }
    static bool handleStatement (std::unique_ptr<Parser::StatementASTNode>& orig_statement, bool is_void_return);

    static bool handleConditionalPart (std::unique_ptr<Parser::ConditionalPart>& conditional_part, bool is_void_return) {
        bool full_return = false;
        for (std::unique_ptr<Parser::StatementASTNode>& statement : conditional_part->body) {
            if (full_return) {
                std::cerr << "Unexpected instruction within if/elif/else-statement: " << statement->toString("") << " after we've already determined that all paths return\n";
            }

            if (handleStatement(statement, is_void_return)) {
                full_return = true;
            }
        }

        std::cout << " Part has full return: " << full_return << "\n";

        conditional_part->always_exits = full_return;
        return full_return;
    }

    static bool handleStatement (std::unique_ptr<Parser::StatementASTNode>& orig_statement, bool is_void_return) {
        Parser::StatementASTNode* ptr = orig_statement.get();

        if (auto If = llvm::dyn_cast<Parser::IfStatementNode>(ptr)) {
            std::cout << " If Statement\n";
            // Check if the root if conditionalpart has full returns.
            bool root_full_return = handleConditionalPart(If->root, is_void_return);
            bool all_has_return = root_full_return;
            for (std::unique_ptr<Parser::ConditionalPart>& conditional_part : If->parts) {
                if (!handleConditionalPart(conditional_part, is_void_return)) {
                    all_has_return = false;
                }
            }

            // If it does not have an else statement then it could skip past the if/elif blocks.
            if (!If->hasElseStatement()) {
                If->always_exits = false;
                return false;
            } else {
                If->always_exits = all_has_return;
                return all_has_return;
            }
        } else if (auto Ret = llvm::dyn_cast<Parser::ReturnStatementNode>(ptr)) {
            // TODO: handle more types here
            if (is_void_return && Ret->value != nullptr) {
                // TODO: support returning a function that returns void.
                std::cerr << "Function returns void but return has a return value!\n";
            }
            return true;
        } else {
            return false;
        }
    }

    void FunctionExitVerifier::transform () {
        for (std::unique_ptr<Parser::DeclASTNode>& decl : nodes.nodes) {
            Parser::DeclASTNode* ptr = decl.get();

            if (auto func = llvm::dyn_cast<Parser::FunctionNode>(ptr)) {
                std::cout << "Function\n";
                bool is_void_return = false;
                if (llvm::cast<Parser::PrimordialTypeNode>(func->return_type.get())->type == Parser::PrimordialType::Void) {
                    // Void does not need to return a value;
                    is_void_return = true;
                }

                bool full_return = false;
                for (std::unique_ptr<Parser::StatementASTNode>& statement : func->body) {
                    if (full_return) {
                        std::cerr << "Unexpected instruction: " << statement->toString("") << " after we've already determined that all paths return\n";
                    }

                    if (handleStatement(statement, is_void_return)) {
                        full_return = true;
                    }
                }

                std::cout << "Function makes full returns: " << full_return << "\n";
            }
        }
    }
}