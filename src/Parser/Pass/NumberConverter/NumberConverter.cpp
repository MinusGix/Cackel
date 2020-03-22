#include "NumberConverter.hpp"

#include <iostream>

// TODO: all of this should support floats
// ex: 5 -> f32 should work.
// 5.42 -> i32 should at the very least warn that it's losing precision
// 533 -> u8 should warn that it's losing information
// TODO: it should also be more careful about non-primordial types. Currently it would just painfully crash.
namespace ParserPass {
    NumberConverter::NumberConverter (Parser::ParentASTNode& t_nodes) : nodes(t_nodes) {
        transform();
    }

    /// Returns nullptr if it couldn't be found.
    static Parser::FunctionSignatureInfo* findSignature (Parser::ParentASTNode& nodes, const std::string& name) {
        for (auto& decl : nodes.nodes)  {
            if (auto function_node = llvm::dyn_cast<Parser::FunctionNode>(decl.get())) {
                if (name == Parser::getIdentityName(function_node->signature.identity)) {
                    return &(function_node->signature);
                }
            } else if (auto extern_function_node = llvm::dyn_cast<Parser::ExternFunctionNode>(decl.get())) {
                if (name == Parser::getIdentityName(extern_function_node->signature.identity)) {
                    return &(extern_function_node->signature);
                }
            }
        }
        return nullptr;
    }

    /// May take ownership of passed in expression.
    /// If it returns a unique_ptr that doesn't hold nullptr, then assume it took ownership of expression
    /// The optional primordialtype is *technically* distinct from void, but it may be changed to just use void.
    std::unique_ptr<Parser::BaseASTNode> NumberConverter::transformExpression (std::unique_ptr<Parser::BaseASTNode>& expression, std::optional<Parser::PrimordialType> expected_type) {
        auto ptr = expression.get();

        if (llvm::isa<Parser::IntegerCastNode>(ptr) || llvm::isa<Parser::AddExpressionNode>(ptr) || llvm::isa<Parser::SubtractExpressionNode>(ptr) ||
                   llvm::isa<Parser::MultiplyExpressionNode>(ptr) || llvm::isa<Parser::UnaryMinusExpressionNode>(ptr) || llvm::isa<Parser::UnaryPlusExpressionNode>(ptr) ||
                   llvm::isa<Parser::VariableAssignment>(ptr) || llvm::isa<Parser::LiteralNumberNode>(ptr)) {
            if (!expected_type.has_value()) {
                // Nothing to do in this case, as there is no expected type.
                return nullptr;
            }
            // TODO: check if they're the same type first.
            return std::make_unique<Parser::IntegerCastNode>(
                std::move(expression),
                expected_type.value()
            );
        } else if (auto func_call = llvm::dyn_cast<Parser::FunctionCallNode>(ptr)) {
            std::string name = Parser::getIdentityName(func_call->identity);
            Parser::FunctionSignatureInfo* signature = findSignature(nodes, name);
            if (signature == nullptr) {
                throw std::runtime_error("Unknown function encountered: " + name);
            }

            size_t size;
            size_t call_size = func_call->arguments.size();
            size_t func_size = signature->parameters.size();
            if (call_size > func_size) {
                std::cerr << "Function call to: '" + name + "' is being given too many parameters\n";
                size = func_size;
            } else if (call_size < func_size) {
                std::cerr << "Function call to '" + name + "' is being given too few parameters\n";
                size = call_size;
            } else {
                size = call_size; // doesn't matter which
            }
            for (size_t i = 0; i < size; i++) {
                // call transform expression on each node with the expected type from the function
                std::unique_ptr<Parser::FunctionParameterInfo>& parameter = signature->parameters.at(i);
                // TODO: handle more complex types here, and also void
                Parser::PrimordialType parameter_expected_type = llvm::cast<Parser::PrimordialTypeNode>(parameter->type.get())->type;

                auto transformed = transformExpression(func_call->arguments.at(i), parameter_expected_type);
                // It may have taken ownership, thus we swap it out if it has
                if (transformed != nullptr) {
                    func_call->arguments.at(i).reset(transformed.release());
                }
            }

            if (!expected_type.has_value()) {
                // Nothing to do in this case as there is no expected type.
                return nullptr;
            }
            // return an integercast'd function call: cast<expected_type>(func(args...))
            return std::make_unique<Parser::IntegerCastNode>(
                std::move(expression),
                expected_type.value()
            );
        } else {
            std::cout << "[Internal:NumberConverter] Unchecked expression: " << expression->toString("") << "\n";
            return nullptr;
        }
    }

    void NumberConverter::transform () {
        for (auto& decl : nodes.nodes) {
            variable_types.clear();

            if (auto function_node = llvm::dyn_cast<Parser::FunctionNode>(decl.get())) {
                Parser::PrimordialType return_type = llvm::cast<Parser::PrimordialTypeNode>(function_node->signature.return_type.get())->type;

                // Store the integer parameters in the state
                for (auto& part : function_node->signature.parameters) {
                    // TODO: bad
                    Parser::PrimordialType prim_type = llvm::cast<Parser::PrimordialTypeNode>(part->type.get())->type;
                    // TODO: support floats
                    if (Parser::isPrimordialTypeAnInteger(prim_type)) {
                        variable_types[part->name] = prim_type;
                    }
                }

                for (std::unique_ptr<Parser::StatementASTNode>& statement : function_node->body) {
                    Parser::StatementASTNode* ptr = statement.get();

                    if (auto variable_decl = llvm::dyn_cast<Parser::VariableStatementNode>(ptr)) {
                        // TODO: bad
                        Parser::PrimordialType prim_type = llvm::cast<Parser::PrimordialTypeNode>(variable_decl->type.get())->type;

                        auto transformed = transformExpression(variable_decl->value, prim_type);
                        if (transformed != nullptr) {
                            variable_decl->value.reset(transformed.release());
                        }

                        // store it for later look up
                        if (Parser::isPrimordialTypeAnInteger(prim_type)) {
                            variable_types[Parser::getIdentityName(variable_decl->identity)] = prim_type;
                        }
                    } else if (auto expression_statement = llvm::dyn_cast<Parser::ExpressionStatementNode>(ptr)) {
                        auto transformed = transformExpression(expression_statement->expression, std::nullopt);
                        if (transformed != nullptr) {
                            expression_statement->expression.reset(transformed.release());
                        }
                    } else if (auto return_statement = llvm::dyn_cast<Parser::ReturnStatementNode>(ptr)) {
                        // If the return type is an integer then we can automatically convert it to the return type.
                        if (Parser::isPrimordialTypeAnInteger(return_type)) {
                            auto transformed = transformExpression(return_statement->value, return_type);
                            if (transformed != nullptr) {
                                return_statement->value.reset(transformed.release());
                            }
                        }
                    } else {
                        std::cout << "[Internal:NumberConverter] Unhandled statement: " << statement->toString("") << "\n";
                    }
                }
            }
        }
    }
}