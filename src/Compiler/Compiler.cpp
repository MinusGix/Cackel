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

    /// Note: this is a temp function for use while we still haven't evaluated identifying name nodes (since there isn't even anything to evaluate yet)
    // TODO: should accept generic identity node type
    static std::string getIdentityName (const std::unique_ptr<BaseASTNode>& ind) {
        if (auto lit = llvm::dyn_cast<LiteralIdentifierNode>(ind.get())) {
            return lit->name;
        } else {
            throw std::runtime_error("Failed to get identity name: " + ind->toString(""));
        }
    }

    static void codegenStatementVector (Compiler& compiler, std::vector<std::unique_ptr<StatementASTNode>>&& statements, Function* function) {
        for (std::unique_ptr<StatementASTNode>& statement : statements) {
            compiler.codegenStatement(std::move(statement), function);
        }
    }

    // ==== Compiler Class ====

    Compiler::Compiler (ParentASTNode&& t_nodes) : builder(context), nodes(std::move(t_nodes)) {
        modul = std::make_unique<llvm::Module>("Cackel", context);
    }

    void Compiler::compile (std::ostream& output) {
        codegenGlobals();
        for (std::unique_ptr<DeclASTNode>& r_node : nodes.nodes) {
            if (auto func_node = llvm::unique_dyn_cast<FunctionNode>(r_node)) {
                codegenFunctionBody(std::move(func_node));
            }
        }

        auto bridge = Util::LLVM::OStreamBridge(output);

        modul->print(bridge, nullptr, false, false);
    }

    /// Generates 'global' data that should be able to referenced.
    /// This (sadly) does it's own pass.
    /// Generates all function prototypes (ignoring the function bodies)
    void Compiler::codegenGlobals () {
        for (const auto& r_node : nodes.nodes) {
            if (auto func = llvm::dyn_cast<FunctionNode>(r_node.get())) {
                codegenFunctionPrototype(getIdentityName(func->identity), func->parameters, func->return_type);
            }
        }
    }

    Function* Compiler::codegenFunctionPrototype (const std::string& name, const std::vector<std::unique_ptr<FunctionParameterInfo>>& parameters, const std::unique_ptr<TypeNode>& return_type) {
        /// size() = parameters.size()
        std::vector<llvm::Type*> gen_parameters;

        for (const std::unique_ptr<FunctionParameterInfo>& parameter : parameters) {
            if (const PrimordialTypeNode* prim_type = llvm::dyn_cast<PrimordialTypeNode>(parameter->type.get())) {
                gen_parameters.push_back(convertPrimordialType(prim_type->type));
            } else {
                throw std::runtime_error("Got unknown type node in codegen for function prototype.: " + parameter->toString(""));
            }
        }
        assert(Util::isValidPointerList(gen_parameters.begin(), gen_parameters.end()));

        // TODO: this is bad
        llvm::Type* return_type_ptr = convertPrimordialType(llvm::cast<PrimordialTypeNode>(return_type.get())->type);
        llvm::FunctionType* function_type = llvm::FunctionType::get(
            return_type_ptr,
            gen_parameters,
            false
        );


        Function* function = Function::Create(function_type, Function::LinkageTypes::ExternalLinkage, name, modul.get());

        unsigned int index = 0;
        for (auto& arg : function->args()) {
            arg.setName(parameters.at(index)->name);
            index++;
        }

        return function;
    }

    Function* Compiler::codegenFunctionBody (std::unique_ptr<FunctionNode>&& node) {
        const std::string function_name = getIdentityName(node->identity);

        Function* function = modul->getFunction(function_name);

        if (function == nullptr) {
            function = codegenFunctionPrototype(function_name, node->parameters, node->return_type);
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
            llvm::AllocaInst* alloc_value = createEntryBlockStackAllocation(function, arg.getType(), arg.getName());

            builder.CreateStore(&arg, alloc_value);

            named_values[std::string(arg.getName())] = alloc_value;
        }

        codegenStatementVector(*this, std::move(node->body), function);


        //builder.CreateRet(ret_value);
        verifyFunction(*function, &llvm::errs());

        return function;
    }

    void Compiler::codegenStatement (std::unique_ptr<StatementASTNode>&& statement, Function* function) {
        if (auto var = llvm::unique_dyn_cast<VariableStatementNode>(statement)) {
            return codegenStatement(std::move(var), function);
        } else if (auto ret = llvm::unique_dyn_cast<ReturnStatementNode>(statement)) {
            return codegenStatement(std::move(ret), function);
        } else if (auto if_statement = llvm::unique_dyn_cast<IfStatementNode>(statement)) {
            return codegenStatement(std::move(if_statement), function);
        } else {
            throw std::runtime_error("Unknown statement node: " + statement->toString(""));
        }
    }
    void Compiler::codegenStatement (std::unique_ptr<VariableStatementNode>&& variable_decl, Function* function) {
        // The type of our variable statement
        PrimordialTypeNode* type_node = llvm::cast<PrimordialTypeNode>(variable_decl->type.get());

        const std::string variable_name = getIdentityName(variable_decl->identity);
        // Create an allocation on the stack for the variable.
        llvm::AllocaInst* alloc_value = createEntryBlockStackAllocation(function, convertPrimordialType(type_node->type), variable_name);
        // Store it so that we may refer to it later.
        named_values[variable_name] = alloc_value;
        // Generate the expression it's been set to.
        Value* generated_expr = codegenExpression(std::move(variable_decl->value));
        if (generated_expr == nullptr) {
            throw std::runtime_error("[Internal] Failed in generating expression for variable statement: " + Util::toString(variable_decl, ""));
        }
        builder.CreateStore(generated_expr, alloc_value);
    }
    void Compiler::codegenStatement (std::unique_ptr<ReturnStatementNode>&& return_statement, Function*) {
        if (return_statement->value != nullptr) {
            builder.CreateRet(codegenExpression(std::move(return_statement->value)));
        } else {
            builder.CreateRetVoid();
        }
    }
    void Compiler::codegenStatement (std::unique_ptr<IfStatementNode>&& if_statement, Function*) {
        // Generate code for the condition
        Value* root_condition = this->codegenExpression(std::move(if_statement->root->condition));
        assert(root_condition != nullptr);
        // TODO: this being a 64 bit int to compare against is
        root_condition = builder.CreateICmpNE(root_condition, llvm::ConstantInt::get(this->context, llvm::APInt(64, 0, false)));

        // This might be useless since we pass in function
        Function* function = builder.GetInsertBlock()->getParent();
        // The block for the code in the if-statement's body
        BasicBlock* if_block = BasicBlock::Create(context, "if", function);
        // block for potential code in the else-statement's body
        BasicBlock* else_block = BasicBlock::Create(context, "else");
        // block that the if-block and else-block will merge into once they're done
        BasicBlock* merge_block = BasicBlock::Create(context, "ifcont");

        BasicBlock* other_block = else_block;

        //bool if_has_return = if_statement->root->hasReturnStatement();
        //bool else_has_return = false;


        if (!if_statement->hasElseStatement()) {
            other_block = merge_block;
        }

        builder.CreateCondBr(root_condition, if_block, other_block);
        builder.SetInsertPoint(if_block);

        // Generate if block

        codegenStatementVector(*this, std::move(if_statement->root->body), function);

        // Create a break for next part of the code after else block
        // TODO: only do this if it does not competely return
        builder.CreateBr(merge_block);

        // generate code for else block
        if (if_statement->hasElseStatement()) {
            std::unique_ptr<ConditionalPart>& else_part = if_statement->getElseStatement();
            //else_has_return = else_part->hasReturnStatement();

            function->getBasicBlockList().push_back(else_block);
            builder.SetInsertPoint(else_block);
            codegenStatementVector(*this, std::move(else_part->body), function);

            // TODO: do this only if this does not completely return
            // Generate branch to merging point if we're not returning from this
            builder.CreateBr(merge_block);
        }

        // Generate next part of code
        function->getBasicBlockList().push_back(merge_block);
        builder.SetInsertPoint(merge_block);
        // TODO: generate unreachable if all paths return
        //builder.CreateUnreachable();
    }

    Value* Compiler::codegenExpression (std::unique_ptr<BaseASTNode>&& expr) {
        if (auto ind = llvm::unique_dyn_cast<LiteralIdentifierNode>(expr)) {
            return this->builder.CreateLoad(this->named_values.at(ind->name));
        } else if (auto num = llvm::unique_dyn_cast<LiteralNumberNode>(expr)) {
            const uint64_t value = std::stoi(num->value);
            size_t size = 64;
            assert(size > 0);
            return llvm::ConstantInt::get(context, llvm::APInt(size, value, false));
        } else if (auto boolean = llvm::unique_dyn_cast<LiteralBooleanNode>(expr)) {
            return llvm::ConstantInt::get(context, llvm::APInt(1, boolean->value, false));
        } else if (auto add = llvm::unique_dyn_cast<AddExpressionNode>(expr)) {
            return builder.CreateAdd(codegenExpression(std::move(add->left)), codegenExpression(std::move(add->right)), "addtmp");
        } else if (auto sub = llvm::unique_dyn_cast<SubtractExpressionNode>(expr)) {
            return builder.CreateSub(codegenExpression(std::move(sub->left)), codegenExpression(std::move(sub->right)), "subtmp");
        } else if (auto mult = llvm::unique_dyn_cast<MultiplyExpressionNode>(expr)) {
            return builder.CreateMul(codegenExpression(std::move(mult->left)), codegenExpression(std::move(mult->right)), "multmp");
        } else if (auto plus = llvm::unique_dyn_cast<UnaryPlusExpressionNode>(expr)) {
            // Does nothing since +thing will not change it's sign.
            return codegenExpression(std::move(plus->right));
        } else if (auto minus = llvm::unique_dyn_cast<UnaryMinusExpressionNode>(expr)) {
            return builder.CreateNeg(codegenExpression(std::move(minus->right)), "negated");
        } else if (auto func_call = llvm::unique_dyn_cast<FunctionCallNode>(expr)) {
            Function* function = modul->getFunction(getIdentityName(func_call->identity));
            assert(function != nullptr);
            std::vector<Value*> arguments;
            for (std::unique_ptr<BaseASTNode>& argument : func_call->arguments) {
                arguments.push_back(codegenExpression(std::move(argument)));
            }
            assert(Util::isValidPointerList(arguments.begin(), arguments.end()));

            // Non-owner array (pointer to memory, which is the vector we created)
            llvm::ArrayRef<Value*> llvm_arguments(arguments);
            return builder.CreateCall(function, llvm_arguments);
        } else {
            throw std::runtime_error("Unhandled node type managed to get to compile time: " + expr->toString(""));
        }
    }

    llvm::Type* Compiler::convertPrimordialType (PrimordialType type) {
        using Type = llvm::Type;

        // Unsigned/Signed are collapsed into one.
        switch (type) {
            case PrimordialType::Void:
                // TODO: i have no clue how well this works
                return Type::getVoidTy(context);
            case PrimordialType::Boolean:
                return Type::getInt1Ty(context);
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
        throw std::runtime_error("Failed to convert primordial type: " + primordialTypeToString(type));
    }
};