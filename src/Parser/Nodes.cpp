#include "Nodes.hpp"
#include "../Compiler/Compiler.hpp"

namespace Parser {
    /// ==== Types ====

    std::string primordialTypeToString (PrimordialType type) {
		switch (type) {
			case PrimordialType::Void:
				return "void";
            case PrimordialType::Boolean:
                return "bool";
			case PrimordialType::Int:
				return "int";
			case PrimordialType::UInt:
				return "uint";
			case PrimordialType::Int8:
				return "i8";
			case PrimordialType::UInt8:
				return "u8";
			case PrimordialType::Int16:
				return "i16";
			case PrimordialType::UInt16:
				return "u16";
			case PrimordialType::Int32:
				return "i32";
			case PrimordialType::UInt32:
				return "u32";
			case PrimordialType::Int64:
				return "i64";
			case PrimordialType::UInt64:
				return "u64";
			case PrimordialType::Int128:
				return "i128";
			case PrimordialType::UInt128:
				return "u128";
			case PrimordialType::Float32:
				return "f32";
			case PrimordialType::Float64:
				return "f64";
		}
	}
    size_t getPrimordialTypeSize (PrimordialType type) {
        switch (type) {
            case PrimordialType::Void:
                // TODO: i have no clue how well this works
                return 0;
            case PrimordialType::Boolean:
                // While this is 1 it's likely to be a full byte
                return 1;
            case PrimordialType::Int:
            case PrimordialType::UInt:
            // TODO: make this dependent on system
                return 32;
            case PrimordialType::Int8:
            case PrimordialType::UInt8:
                return 8;
            case PrimordialType::Int16:
            case PrimordialType::UInt16:
                return 16;
            case PrimordialType::Int32:
            case PrimordialType::UInt32:
                return 32;
            case PrimordialType::Int64:
            case PrimordialType::UInt64:
                return 64;
            case PrimordialType::Int128:
            case PrimordialType::UInt128:
                return 128;
            case PrimordialType::Float32:
                return 32;
            case PrimordialType::Float64:
                return 64;
        }
    }

    std::optional<PrimordialType> stringToPrimordialType (const std::string& name) {
        if (name == "void") {
            return PrimordialType::Void;
        } else if (name == "bool") {
            return PrimordialType::Boolean;
        } else if (name == "int") {
            return PrimordialType::Int;
        } else if (name == "uint") {
            return PrimordialType::UInt;
        } else if (name == "i8") {
            return PrimordialType::Int8;
        } else if (name == "u8") {
            return PrimordialType::UInt8;
        } else if (name == "i16") {
            return PrimordialType::Int16;
        } else if (name == "u16") {
            return PrimordialType::UInt16;
        } else if (name == "i32") {
            return PrimordialType::Int32;
        } else if (name == "u32") {
            return PrimordialType::UInt32;
        } else if (name == "i64") {
            return PrimordialType::Int64;
        } else if (name == "u64") {
            return PrimordialType::UInt64;
        } else if (name == "i128") {
            return PrimordialType::Int128;
        } else if (name == "u128") {
            return PrimordialType::UInt128;
        } else if (name == "f32") {
            return PrimordialType::Float32;
        } else if (name == "f64") {
            return PrimordialType::Float64;
        }
        return std::nullopt;
    }

    /// Note: this is a temp function for use while we still haven't evaluated identifying name nodes (since there isn't even anything to evaluate yet)
    // TODO: should accept generic identity node type
    std::string getIdentityName (const std::unique_ptr<BaseASTNode>& ind) {
        if (auto lit = llvm::dyn_cast<LiteralIdentifierNode>(ind.get())) {
            return lit->name;
        } else {
            throw std::runtime_error("Failed to get identity name: " + ind->toString(""));
        }
    }

    llvm::Type* convertPrimordialType (llvm::LLVMContext& context, PrimordialType type) {
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

    /// ==== AST ====

    std::string ParentASTNode::toString (const std::string orig_indent) const {
        const std::string indent = orig_indent + "\t";
        return "Parent: [\n" + indent + Util::stringJoin(nodes, "\n" + indent, indent) + "\n]";
    }

    // ==== Utility Nodes ====
    LiteralIdentifierNode::LiteralIdentifierNode (std::string&& t_name) : BaseASTNode(Kind::IdentifierLiteral), name(t_name) {}
    LiteralIdentifierNode::LiteralIdentifierNode (const Lexer::Token& identifier_token) : BaseASTNode(Kind::IdentifierLiteral) {
        if (!identifier_token.is(Lexer::Token::Type::Identifier)) {
            throw std::logic_error("Got token to LiteralIdentifierNode constructor that wasn't of type identifier.");
        }
        name = identifier_token.getData<Lexer::Token::IdentifierData>().data;
    }
    std::string LiteralIdentifierNode::toString (const std::string&) const {
        return "LID[" + name + "]";
    }

    // Tries creating a load for a variable, though that isn't all that this node is for.
    llvm::Value* LiteralIdentifierNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage == Compiler::Stage::Normal) {
            return compiler.builder.CreateLoad(compiler.named_values.at(name));
        }
        return nullptr;
    }



    LiteralNumberNode::LiteralNumberNode (std::string&& t_value) : BaseASTNode(Kind::NumberLiteral), value(t_value) {}
    LiteralNumberNode::LiteralNumberNode (const Lexer::Token& number_token) : BaseASTNode(Kind::NumberLiteral) {
        if (!number_token.isNumber()) {
            throw std::runtime_error("Got token to literal number node constructor that wasn't of any number type.");
        }

        value = number_token.getData<Lexer::Token::NumberData>().data;
    }
    std::string LiteralNumberNode::toString (const std::string&) const {
        return "LNum[" + value + "]";
    }

    llvm::Value* LiteralNumberNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage != Compiler::Stage::Normal) {
            return nullptr;
        }
        const uint64_t number_value = std::stoi(value);
        size_t size = 64;
        assert(size > 0);
        return llvm::ConstantInt::get(compiler.context, llvm::APInt(size, number_value, false));
    }



    LiteralBooleanNode::LiteralBooleanNode (bool t_value) : BaseASTNode(Kind::BooleanLiteral), value(t_value) {}
    std::string LiteralBooleanNode::toString (const std::string&) const {
        if (value) {
            return "B[true]";
        } else {
            return "B[false]";
        }
    }

    llvm::Value* LiteralBooleanNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage != Compiler::Stage::Normal) {
            return nullptr;
        }
        return llvm::ConstantInt::get(compiler.context, llvm::APInt(1, value, false));
    }



    AddExpressionNode::AddExpressionNode (std::unique_ptr<BaseASTNode>&& t_left, std::unique_ptr<BaseASTNode>&& t_right) : BaseASTNode(Kind::AddExpression), left(std::move(t_left)), right(std::move(t_right)) {}
    std::string AddExpressionNode::toString (const std::string& indent) const {
        return "+(" + left->toString(indent) + ", " + right->toString(indent) + ")";
    }

    llvm::Value* AddExpressionNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage != Compiler::Stage::Normal) {
            return nullptr;
        }
        return compiler.builder.CreateAdd(left->codegen(compiler), right->codegen(compiler), "addtmp");
    }


    SubtractExpressionNode::SubtractExpressionNode (std::unique_ptr<BaseASTNode>&& t_left, std::unique_ptr<BaseASTNode>&& t_right) : BaseASTNode(Kind::SubtractExpression), left(std::move(t_left)), right(std::move(t_right)) {}
    std::string SubtractExpressionNode::toString (const std::string& indent) const {
        return "-(" + left->toString(indent) + ", " + right->toString(indent) + ")";
    }

    llvm::Value* SubtractExpressionNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage != Compiler::Stage::Normal) {
            return nullptr;
        }
        return compiler.builder.CreateSub(left->codegen(compiler), right->codegen(compiler), "subtmp");
    }

    MultiplyExpressionNode::MultiplyExpressionNode (std::unique_ptr<BaseASTNode>&& t_left, std::unique_ptr<BaseASTNode>&& t_right) : BaseASTNode(Kind::MultiplyExpression), left(std::move(t_left)), right(std::move(t_right)) {}
    std::string MultiplyExpressionNode::toString (const std::string& indent) const {
        return "*(" + left->toString(indent) + ", " + right->toString(indent) + ")";
    }

    llvm::Value* MultiplyExpressionNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage != Compiler::Stage::Normal) {
            return nullptr;
        }
        return compiler.builder.CreateMul(left->codegen(compiler), right->codegen(compiler), "multmp");
    }


    UnaryPlusExpressionNode::UnaryPlusExpressionNode (std::unique_ptr<BaseASTNode>&& t_right) : BaseASTNode(Kind::UnaryPlusExpression), right(std::move(t_right)) {}
    std::string UnaryPlusExpressionNode::toString (const std::string& indent) const {
        return "+(" + right->toString(indent) + ")";
    }

    llvm::Value* UnaryPlusExpressionNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage != Compiler::Stage::Normal) {
            return nullptr;
        }
        return right->codegen(compiler);
    }

    UnaryMinusExpressionNode::UnaryMinusExpressionNode (std::unique_ptr<BaseASTNode>&& t_right) : BaseASTNode(Kind::UnaryMinusExpression), right(std::move(t_right)) {}
    std::string UnaryMinusExpressionNode::toString (const std::string& indent) const {
        return "-(" + right->toString(indent) + ")";
    }

    llvm::Value* UnaryMinusExpressionNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage != Compiler::Stage::Normal) {
            return nullptr;
        }
        return compiler.builder.CreateNeg(right->codegen(compiler), "negated");
    }


    FunctionCallNode::FunctionCallNode (std::unique_ptr<BaseASTNode>&& t_identity) : BaseASTNode(Kind::FunctionCallExpression), identity(std::move(t_identity)) {}
    FunctionCallNode::FunctionCallNode (std::unique_ptr<BaseASTNode>&& t_identity, std::vector<std::unique_ptr<BaseASTNode>>&& t_arguments) : BaseASTNode(Kind::FunctionCallExpression), identity(std::move(t_identity)), arguments(std::move(t_arguments)) {}
    std::string FunctionCallNode::toString (const std::string& indent) const {
        return "Func[" + identity->toString(indent) + "(" + "... arguments TODO" + ")]";
    }

    llvm::Value* FunctionCallNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage != Compiler::Stage::Normal) {
            return nullptr;
        }
        llvm::Function* function = compiler.modul->getFunction(getIdentityName(identity));
        assert(function != nullptr);

        std::vector<llvm::Value*> parameters;
        for (std::unique_ptr<BaseASTNode>& argument : arguments) {
            parameters.push_back(argument->codegen(compiler));
        }
        assert(Util::isValidPointerList(parameters.begin(), parameters.end()));

        // Non-owner array (pointer to memory, which is the vector of arguments we created)
        llvm::ArrayRef<llvm::Value*> llvm_parameters(parameters);
        return compiler.builder.CreateCall(function, llvm_parameters);
    }

    // ==== Statement Nodes ====


    VariableStatementNode::VariableStatementNode (std::unique_ptr<BaseASTNode>&& t_identity, std::unique_ptr<TypeNode>&& t_type, std::unique_ptr<BaseASTNode>&& t_value) : StatementASTNode(Kind::Variable), identity(std::move(t_identity)), type(std::move(t_type)), value(std::move(t_value)) {}
    std::string VariableStatementNode::toString (const std::string& indent) const {
        return "VS[" + identity->toString(indent) + " = " + value->toString(indent) + "]";
    }

    llvm::Value* VariableStatementNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage != Compiler::Stage::Normal) {
            return nullptr;
        }

        llvm::Function* function = compiler.builder.GetInsertBlock()->getParent();

        /// TODO: replace this with better type getting method
        PrimordialTypeNode* type_node = llvm::cast<PrimordialTypeNode>(type.get());

        const std::string name = getIdentityName(identity);
        // Create an allocation on the stack for the variable. This makes it mutable.
        llvm::AllocaInst* alloc_value = Compiler::createEntryBlockStackAllocation(function, convertPrimordialType(compiler.context, type_node->type), name);
        // Store it in the scope.
        compiler.named_values.at(name) = alloc_value;
        // generate the expression it has been set to
        llvm::Value* generated_expression = value->codegen(compiler);
        if (generated_expression == nullptr) {
            throw std::runtime_error("[Internal] Failed in generating expression for variable statement: " + toString(""));
        }
        compiler.builder.CreateStore(generated_expression, alloc_value);

        return alloc_value;
    }


    ReturnStatementNode::ReturnStatementNode (std::unique_ptr<BaseASTNode>&& t_value) : StatementASTNode(Kind::Return), value(std::move(t_value)) {}

    std::string ReturnStatementNode::toString (const std::string& indent) const {
        if (value == nullptr) {
            return "Return[]";
        } else {
            return "Return[" + value->toString(indent) + "]";
        }
    }

    llvm::Value* ReturnStatementNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage != Compiler::Stage::Normal) {
            return nullptr;
        }

        if (value != nullptr) {
            return compiler.builder.CreateRet(value->codegen(compiler));
        } else {
            return compiler.builder.CreateRetVoid();
        }
    }

    // Note: this should perhaps be made into it's own virtual node type so that it can be replaced in a pass as well.
    ConditionalPart::ConditionalPart (std::unique_ptr<BaseASTNode>&& t_condition, std::vector<std::unique_ptr<StatementASTNode>>&& t_body) : condition(std::move(t_condition)), body(std::move(t_body)) {}

    std::string ConditionalPart::toString (const std::string& indent) const {
        std::string result;
        if (condition != nullptr) {
            result = "?If[" + Util::toString(condition, indent) + "]";
        } else {
            result = "Else";
        }
        return result + " {\n" + indent + "\t" + Util::stringJoin(body, ";\n" + indent + "\t", indent + "\t") + ";\n" + indent + "}";
    }

    IfStatementNode::IfStatementNode (std::unique_ptr<ConditionalPart>&& t_root) : StatementASTNode(Kind::If), root(std::move(t_root)) {}

    std::string IfStatementNode::toString (const std::string& indent) const {
        return root->toString(indent) + Util::stringJoin(parts, " ", indent);
    }

    bool IfStatementNode::hasElseStatement () const {
        if (parts.size() > 0) {
            return parts.at(parts.size() - 1)->condition == nullptr;
        }
        return false;
    }

    /// Gets the else statement. Does not check if one should exist, use with care.
    std::unique_ptr<ConditionalPart>& IfStatementNode::getElseStatement () {
        return parts.at(parts.size() - 1);
    }

    llvm::Value* IfStatementNode::codegenIf (Compiler::Compiler& compiler) {
        using BasicBlock = llvm::BasicBlock;

        llvm::Value* root_condition = root->condition->codegen(compiler);
        assert(root_condition != nullptr);
        // TODO: this is iffy to compare directly against a 64 bit int.
        root_condition = compiler.builder.CreateICmpNE(root_condition, llvm::ConstantInt::get(compiler.context, llvm::APInt(64, 0, false)));

        llvm::Function* function = compiler.builder.GetInsertBlock()->getParent();

        // Code in the if-statements body
        BasicBlock* if_block = BasicBlock::Create(compiler.context, "if", function);
        // Code in potential else-statement's body
        BasicBlock* else_block = BasicBlock::Create(compiler.context, "else");
        // Merge point for the two blocks
        BasicBlock* merge_block = BasicBlock::Create(compiler.context, "ifcont");

        BasicBlock* other_block = else_block;

        if (!hasElseStatement()) {
            other_block = merge_block;
        }

        // Create branch to if-block or else-block
        compiler.builder.CreateCondBr(root_condition, if_block, other_block);

        // Move to editing if-statement code block
        compiler.builder.SetInsertPoint(if_block);
        // Add statements to if-statement block
        for (std::unique_ptr<StatementASTNode>& statement : root->body) {
            statement->codegen(compiler);
        }

        // Create branch to merge block
        // TODO: only do this if it doesn't completely return
        compiler.builder.CreateBr(merge_block);

        // Generate code for else block
        if (hasElseStatement()) {
            std::unique_ptr<ConditionalPart>& else_part = getElseStatement();

            function->getBasicBlockList().push_back(else_block);
            compiler.builder.SetInsertPoint(else_block);
            for (std::unique_ptr<StatementASTNode>& statement : else_part->body) {
                statement->codegen(compiler);
            }

            compiler.builder.CreateBr(merge_block);
        }

        function->getBasicBlockList().push_back(merge_block);
        compiler.builder.SetInsertPoint(merge_block);

        // Not entirely sure what to return, so we return nullptr.
        return nullptr;
    }

    llvm::Value* IfStatementNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage != Compiler::Stage::Normal) {
            return nullptr;
        }
    }

    // then function node
    // then start working on parser.cpp. You will remove compiler for now while you get this back to working.


    // ==== Declaration nodes ====

    // This could potentially be turned into a node of it's own status for now it's purely for functions
    FunctionParameterInfo::FunctionParameterInfo (std::unique_ptr<TypeNode>&& t_type, std::string t_name) : type(std::move(t_type)), name(t_name) {}
    std::string FunctionParameterInfo::toString (const std::string& indent) const {
        return "FParam[" + type->toString(indent) + ", " + name + "]";
    }

    FunctionNode::FunctionNode (std::unique_ptr<BaseASTNode>&& t_identity, std::vector<std::unique_ptr<FunctionParameterInfo>>&& t_parameters, std::unique_ptr<TypeNode>&& t_return_type, std::vector<std::unique_ptr<StatementASTNode>>&& t_body) :
        DeclASTNode(Kind::Function), identity(std::move(t_identity)), parameters(std::move(t_parameters)), return_type(std::move(t_return_type)), body(std::move(t_body)) {}

    std::string FunctionNode::toString (const std::string& indent) const {
        return "Function[" + Util::toString(identity, indent) + ",\n" +
            indent + " \t(" + Util::stringJoin(parameters, ", ", indent + "\t") + "),\n" +
            indent + "\t->" + Util::toString(return_type, indent) + ", ""{\n" +
            indent + "\t\t" + Util::stringJoin(body, ";\n" + indent + "\t\t", indent + "\t\t", true) + "\n" + indent + "\t}";
    }

    llvm::Value* FunctionNode::codegenPrototype (Compiler::Compiler& compiler) {
        std::vector<llvm::Type*> generated_parameters;
        for (const std::unique_ptr<FunctionParameterInfo>& parameter : parameters) {
            // TODO: give primordialtypenode and typenode a function to generate the llvm::Type*
            if (const PrimordialTypeNode* prim_type = llvm::dyn_cast<PrimordialTypeNode>(parameter->type.get())) {
                generated_parameters.push_back(convertPrimordialType(compiler.context, prim_type->type));
            } else {
                throw std::runtime_error("Unknown function parameter type: " + parameter->toString(""));
            }
        }
        assert(Util::isValidPointerList(generated_parameters.begin(), generated_parameters.end()));

        // TODO: this is bad
        llvm::Type* return_type_ptr = convertPrimordialType(compiler.context, llvm::cast<PrimordialTypeNode>(return_type.get())->type);
        llvm::FunctionType* function_type = llvm::FunctionType::get(
            return_type_ptr,
            generated_parameters,
            false
        );

        llvm::Function* function = llvm::Function::Create(
            function_type,
            llvm::Function::LinkageTypes::ExternalLinkage,
            getIdentityName(identity),
            compiler.modul.get()
        );

        size_t index = 0;
        for (auto& arg : function->args()) {
            arg.setName(parameters.at(index)->name);
            index++;
        }

        return function;
    }
    llvm::Value* FunctionNode::codegenBody (Compiler::Compiler& compiler) {
        /// Clear the current scope
        compiler.clearScope();

        const std::string name = getIdentityName(identity);

        llvm::Function* function = compiler.modul->getFunction(name);

        if (function == nullptr) {
            throw std::runtime_error("Function prototype was not defined: " + toString(""));
        }

        if (!function->empty()) {
            throw std::runtime_error("Function can not be redefined: " + toString(""));
        }

        // Create entrypoint for function
        llvm::BasicBlock* entry_point = llvm::BasicBlock::Create(compiler.context, "entry", function);
        compiler.builder.SetInsertPoint(entry_point);

        for (auto& arg : function->args()) {
            // Store the function arguments explicitly on the stack. This lets them be modified.
            llvm::AllocaInst* alloc_value = Compiler::createEntryBlockStackAllocation(function, arg.getType(), arg.getName());
            compiler.builder.CreateStore(&arg, alloc_value);

            compiler.named_values.at(std::string(arg.getName())) = alloc_value;
        }

        for (std::unique_ptr<StatementASTNode>& statement : body) {
            statement->codegen(compiler);
        }

        llvm::verifyFunction(*function, &llvm::errs());

        return function;
    }
    llvm::Value* FunctionNode::codegen (Compiler::Compiler& compiler) {
        if (compiler.stage == Compiler::Stage::Global) {
            return codegenPrototype(compiler);
        } else if (compiler.stage == Compiler::Stage::Normal) {
            return codegenBody(compiler);
        } else {
            return nullptr;
        }
    }
}