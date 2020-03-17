#include "Compiler.hpp"
#include "Util/llvm_stdostream.hpp"

#include <iostream>
#include <ostream>

namespace Compiler {
    // ==== Utility ====
    AllocaInst* createEntryBlockStackAllocation (Function* function, Type* type, const std::string& name) {
        IRBuilder<> temp(&function->getEntryBlock(), function->getEntryBlock().begin());
        return temp.CreateAlloca(type, nullptr, name);
    }

    /// Note: this is a temp function for use while we still haven't evaluated identifying name nodes (since there isn't even anything to evaluate yet)
    static std::string getIdentityName (const IdentifyingNameNode& ind) {
        return std::visit(overloaded {
            [] (const LiteralIdentifierNode& id) -> std::string {
                return id.name;
            }
        }, ind);
    }

    // ==== Compiler Class ====

    Compiler::Compiler (ParentASTNode t_nodes) : builder(context), nodes(t_nodes) {
        modul = std::make_unique<Module>("Cackel", context);
    }

    void Compiler::compile (std::ostream& output) {
        codegenGlobals();
        for (const auto& r_node : nodes.nodes) {
            std::visit(overloaded {
                [this] (const FunctionNode& node) {
                    this->codegenFunctionBody(node);
                }
            }, r_node);
        }

        auto bridge = Util::LLVM::OStreamBridge(output);

        modul->print(bridge, nullptr, false, false);
    }

    /// Generates 'global' data that should be able to referenced.
    /// This (sadly) does it's own pass.
    /// Generates all function prototypes (ignoring the function bodies)
    void Compiler::codegenGlobals () {
        for (const auto& r_node : nodes.nodes) {
            std::visit(overloaded {
                [this] (const FunctionNode& function) {
                    this->codegenFunctionPrototype(getIdentityName(function.identity), function.parameters, function.return_type);
                }
            }, r_node);
        }
    }

    Function* Compiler::codegenFunctionPrototype (const std::string& name, const std::vector<FunctionParameterNode>& parameters, const TypeNode& return_type) {
        std::vector<Type*> gen_parameters(parameters.size());

        for (size_t i = 0; i < parameters.size(); i++) {
            std::visit(overloaded {
                [] (const UnknownTypeNode&) {
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


        Function* function = Function::Create(function_type, Function::LinkageTypes::ExternalLinkage, name, modul.get());

        unsigned int index = 0;
        for (auto& arg : function->args()) {
            arg.setName(parameters.at(index).name);
            index++;
        }

        return function;
    }

    Function* Compiler::codegenFunctionBody (const FunctionNode& node) {
        const std::string function_name = getIdentityName(node.identity);

        Function* function = modul->getFunction(function_name);

        if (function == nullptr) {
            function = codegenFunctionPrototype(function_name, node.parameters, node.return_type);
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
        // The type of our variable statement
        PrimordialTypeNode type_node = std::get<PrimordialTypeNode>(variable_decl.type);

        const std::string variable_name = std::visit(overloaded {
            [] (const LiteralIdentifierNode& n) { return n.name; }
        }, variable_decl.name);
        // Create an allocation on the stack for the variable.
        AllocaInst* alloc_value = createEntryBlockStackAllocation(function, convertPrimordialType(type_node), variable_name);
        // Store it so that we may refer to it later.
        named_values[variable_name] = alloc_value;
        // Generate the expression it's been set to.
        Value* generated_expr = codegenExpression(variable_decl.value);
        if (generated_expr == nullptr) {
            throw std::runtime_error("[Internal] Failed in generating expression for variable statement: " + Util::toString(variable_decl, ""));
        }
        builder.CreateStore(generated_expr, alloc_value);
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
            [this] (const IdentifyingNameNode& ind) -> Value* {
                std::string name = getIdentityName(ind);
                return this->builder.CreateLoad(this->named_values[name]);
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
            },
            [this] (const SubtractExpressionNode& sub_node) -> Value* {
                return this->builder.CreateSub(this->codegenExpression(*sub_node.left), this->codegenExpression(*sub_node.right), "subtmp");
            },
            [this] (const MultiplyExpressionNode& mul_node) -> Value* {
                return this->builder.CreateMul(this->codegenExpression(*mul_node.left), this->codegenExpression(*mul_node.right), "mul_tmp");
            },
            [this] (const UnaryPlusExpressionNode& u_plus_node) -> Value* {
                return this->codegenExpression(*u_plus_node.right);
            },
            [this] (const UnaryMinusExpressionNode& u_minus_node) -> Value* {
                return this->builder.CreateNeg(this->codegenExpression(*u_minus_node.right), "negated");
            },
            [this] (const FunctionCallNode& func_call_node) -> Value* {
                Function* function = modul->getFunction(getIdentityName(func_call_node.identity));
                std::vector<Value*> arguments;
                for (const ExpressionNode& expr : func_call_node.arguments) {
                    arguments.push_back(this->codegenExpression(expr));
                }
                assert(Util::isValidPointerList(arguments.begin(), arguments.end()));

                // Non-owner array (pointer to memory, which is the vector we created)
                llvm::ArrayRef<Value*> llvm_arguments(arguments);
                return this->builder.CreateCall(function, llvm_arguments);
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