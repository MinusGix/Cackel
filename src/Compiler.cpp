#include "Compiler.hpp"

#include <iostream>

namespace Compiler {
    AllocaInst* createEntryBlockStackAllocation (Function* function, Type* type, const std::string& name) {
        IRBuilder<> temp(&function->getEntryBlock(), function->getEntryBlock().begin());
        return temp.CreateAlloca(type, nullptr, name);
    }

    // ==== Compiler Class ====

    Compiler::Compiler (ParentASTNode t_nodes) : builder(context), nodes(t_nodes) {
        modul = std::make_unique<Module>("Cackel", context);
    }

    Value* Compiler::logErrorValue (const char* str) {
        std::cout << str << "\n";
        return nullptr;
    }

    void Compiler::compile () {
        for (const auto& r_node : nodes.nodes) {
            std::visit(overloaded {
                [this] (const FunctionNode& node) {
                    this->codegenFunctionBody(node);
                }
            }, r_node);
        }

        modul->print(errs(), nullptr, false, false);
    }

    Function* Compiler::codegenFunctionPrototype (const RawIdentifierNode& identifier, const std::vector<FunctionParameterNode>& parameters, const TypeNode& return_type) {
        std::vector<Type*> gen_parameters(parameters.size());

        for (size_t i = 0; i < parameters.size(); i++) {
            std::visit(overloaded {
                [&gen_parameters] (const UnknownTypeNode&) {
                    throw std::runtime_error("GOT UNKNOWN TYPE NODE AS TYPE IN CODEGEN");
                },
                [&gen_parameters, i, this] (const PrimordialTypeNode& type) {
                    gen_parameters.at(i) = this->convertPrimordialType(type);
                }
            }, parameters[i].type);
        }
        assert(Util::isValidPointerList(gen_parameters.begin(), gen_parameters.end()));

        // TODO: this is bad
        Type* return_type_ptr = convertPrimordialType(std::get<PrimordialTypeNode>(return_type));
        FunctionType* function_type = FunctionType::get(
            return_type_ptr,
            gen_parameters,
            false
        );


        Function* function = Function::Create(function_type, Function::LinkageTypes::ExternalLinkage, identifier.name, modul.get());

        unsigned int index = 0;
        for (auto& arg : function->args()) {
            arg.setName(parameters.at(index).name);
            index++;
        }

        return function;
    }

    Function* Compiler::codegenFunctionBody (const FunctionNode& node) {
        Function* function = modul->getFunction(node.identifier.name);

        if (function == nullptr) {
            function = codegenFunctionPrototype(node.identifier, node.parameters, node.return_type);
        }

        if (function == nullptr) {
            return nullptr;
        }

        // === Create the function body.

        // We don't check if the function exists already since this function creates it rather than using extern

        if (!function->empty()) {
            // Shouldn't be an issue
            throw std::runtime_error("Function cannot be redefined.");
        }



        // Create a basic block
        BasicBlock* basic_block = BasicBlock::Create(context, "entry", function);
        // new instructions should be inserted at the end of this basic block.
        builder.SetInsertPoint(basic_block);

        // Record the function arguments in the values map
        named_values.clear();
        for (auto& arg : function->args()) {
            // Create an alloca for the parameter
            AllocaInst* alloc_value = createEntryBlockStackAllocation(function, arg.getType(), arg.getName());

            builder.CreateStore(&arg, alloc_value);

            named_values[std::string(arg.getName())] = alloc_value;
        }

        for (const auto& body_node : node.body) {
            codegenStatement(body_node, function);
        }


        //builder.CreateRet(ret_value);
        verifyFunction(*function, &errs());

        return function;
    }

    void Compiler::codegenStatement (const StatementNode& statement, Function* function) {
        std::visit([this, function] (auto&& x) {
            this->codegenStatement(x, function);
        }, statement);
    }
    void Compiler::codegenStatement (const ExpressionNode&, Function*) {
        std::cout << "DO NOTHING STATEMENT (EXPRESSION) THIS SHOULD DO SOMETHING!!\n";
    }
    void Compiler::codegenStatement (const VariableStatementNode& variable_decl, Function* function) {
        PrimordialTypeNode type_node = std::get<PrimordialTypeNode>(variable_decl.type);
        AllocaInst* alloc_value = createEntryBlockStackAllocation(function, convertPrimordialType(type_node), variable_decl.name);
        named_values[variable_decl.name] = alloc_value;
        builder.CreateStore(codegenExpression(variable_decl.value), alloc_value);
    }
    void Compiler::codegenStatement (const ReturnStatementNode& return_statement, Function*) {
        if (return_statement.value.has_value()) {
            builder.CreateRet(codegenExpression(return_statement.value.value()));
        } else {
            builder.CreateRetVoid();
        }
    }

    Value* Compiler::codegenExpression (const ExpressionNode& expression) {
        return std::visit(overloaded {
            [this] (const LiteralIdentifierNode& id) -> Value* {
                return this->builder.CreateLoad(this->named_values[id.name]);
            },
            [this] (const LiteralNumberNode& number) -> Value* {
                // TODO: this is bad
                const uint64_t value = std::stoi(number.literal_value);
                size_t size = 64;
                assert (size > 0);
                return ConstantInt::get(this->context, APInt(size, value, false));
            },
            [this] (const AddExpressionNode& add_node) -> Value* {
                return this->builder.CreateAdd(this->codegenExpression(*add_node.left), this->codegenExpression(*add_node.right), "addtmp");
            }
        }, expression);
    }

    Type* Compiler::convertPrimordialType (const PrimordialTypeNode& type_node) {
        // Unsigned/Signed are collapsed into one.
        PrimordialType type = type_node.type;
        switch (type) {
            case PrimordialType::Void:
                // TODO: i have no clue how well this works
                return Type::getVoidTy(context);
            case PrimordialType::Int:
            case PrimordialType::UInt:
                // TODO: make this dependent on system
                return Type::getInt32Ty(context);
            case PrimordialType::Int8:
            case PrimordialType::UInt8:
                return Type::getInt8Ty(context);
            case PrimordialType::Int16:
            case PrimordialType::UInt16:
                return Type::getInt16Ty(context);
            case PrimordialType::Int32:
            case PrimordialType::UInt32:
                return Type::getInt32Ty(context);
            case PrimordialType::Int64:
            case PrimordialType::UInt64:
                return Type::getInt64Ty(context);
            case PrimordialType::Int128:
            case PrimordialType::UInt128:
                return Type::getInt128Ty(context);
            case PrimordialType::Float32:
                return Type::getFloatTy(context);
            case PrimordialType::Float64:
                return Type::getDoubleTy(context);
        }
    }
};