#pragma once

#include <string>
#include <optional>
#include <memory>
#include <llvm/IR/Module.h>
#include "../Lexer/Lexer.hpp"
#include "../Util/util.hpp"

namespace Compiler {
    struct Compiler;
}

namespace Parser {
        /// ==== Types ====
    enum class PrimordialType {
        Void,
        Boolean,
        Int, UInt,
        Int8, UInt8,
        Int16, UInt16,
        Int32, UInt32,
        Int64, UInt64,
        Int128, UInt128,
        Float32, Float64,
    };
    std::string primordialTypeToString (PrimordialType type);
    size_t getPrimordialTypeSize (PrimordialType type);
    std::optional<PrimordialType> stringToPrimordialType (const std::string& name);

    struct BaseASTNode;
    std::string getIdentityName (const std::unique_ptr<BaseASTNode>& ind);

    llvm::Type* convertPrimordialType (llvm::LLVMContext& context, PrimordialType type);


    /// ==== AST ====

    // Anything extending this should eventually 
    struct BaseASTNode {
        enum class Kind {
            #define BASE_KIND(X) X,
            #include "BaseKinds.inc"
        };
        private:
        const Kind kind;
        public:
        Kind getKind () const {
            return kind;
        }

        BaseASTNode (Kind t_kind) : kind(t_kind) {}
        virtual ~BaseASTNode () {}
        virtual std::string toString (const std::string&) const = 0;

        virtual llvm::Value* codegen (Compiler::Compiler&) {
            throw std::runtime_error("Unimplemented codegen: " + toString(""));
        }
    };

    struct DeclASTNode {
        enum class Kind {
            #define DECL_KIND(X) X,
            #include "DeclKinds.inc"
        };
        private:
        const Kind kind;
        public:
        Kind getKind () const {
            return kind;
        }

        DeclASTNode (Kind t_kind) : kind(t_kind) {}
        virtual ~DeclASTNode () {}
        virtual std::string toString (const std::string&) const = 0;
        virtual llvm::Value* codegen (Compiler::Compiler&) {
            throw std::runtime_error("Unimplemented codegen: " + toString(""));
        }
    };

    struct ParentASTNode {
        std::vector<std::unique_ptr<DeclASTNode>> nodes;
        explicit ParentASTNode () {}
        ParentASTNode(ParentASTNode&& other) : nodes(std::move(other.nodes)) {}
        ParentASTNode (const ParentASTNode&) = delete;
        ParentASTNode& operator= (const ParentASTNode&) = delete;
        std::string toString (const std::string orig_indent) const;
    };


    // ==== TypeNode ====
    struct TypeNode {
        enum class Kind {
            #define TYPE_KIND(X) X,
            #include "TypeKinds.inc"
        };
        private:
        const Kind kind;
        public:
        Kind getKind () const {
            return kind;
        }

        explicit TypeNode (Kind t_kind) : kind(t_kind) {}
        virtual ~TypeNode () {}

        virtual std::string toString (const std::string&) const = 0;
    };

    struct PrimordialTypeNode : public TypeNode {
        PrimordialType type;
        explicit PrimordialTypeNode (PrimordialType t_type) : TypeNode(Kind::Primordial), type(t_type) {}
        std::string toString (const std::string&) const override {
            return "PT[" + primordialTypeToString(type) + "]";
        }

        static bool classof (const TypeNode* t) {
            return t->getKind() == Kind::Primordial;
        }
    };

    struct UnknownTypeNode : public TypeNode {
        explicit UnknownTypeNode () : TypeNode(Kind::Unknown) {}
        std::string toString (const std::string&) const override {
            return "UT";
        }

        static bool classof (const TypeNode* t) {
            return t->getKind() == Kind::Unknown;
        }
    };

    // ==== Utility Nodes ====
    struct LiteralIdentifierNode : public BaseASTNode {
        std::string name;
        explicit LiteralIdentifierNode (std::string&& t_name);
        explicit LiteralIdentifierNode (const Lexer::Token& identifier_token);

        std::string toString (const std::string&) const override;

        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::IdentifierLiteral;
        }

        // Tries creating a load for a variable, though that isn't all that this node is for.
        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };

    struct LiteralNumberNode : public BaseASTNode {
        std::string value;
        explicit LiteralNumberNode (std::string&& t_value);
        explicit LiteralNumberNode (const Lexer::Token& number_token);
        std::string toString (const std::string&) const override;
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::NumberLiteral;
        }

        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };

    struct LiteralBooleanNode : public BaseASTNode {
        bool value;
        explicit LiteralBooleanNode (bool t_value);
        std::string toString (const std::string&) const override;
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::BooleanLiteral;
        }

        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };

    // These should probably somehow go under an operator node-type
    struct AddExpressionNode : public BaseASTNode {
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> left;
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> right;

        explicit AddExpressionNode (std::unique_ptr<BaseASTNode>&& t_left, std::unique_ptr<BaseASTNode>&& t_right);
        std::string toString (const std::string& indent) const override;
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::AddExpression;
        }

        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };
    struct SubtractExpressionNode : public BaseASTNode {
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> left;
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> right;

        explicit SubtractExpressionNode (std::unique_ptr<BaseASTNode>&& t_left, std::unique_ptr<BaseASTNode>&& t_right);
        std::string toString (const std::string& indent) const override;
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::SubtractExpression;
        }

        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };
    struct MultiplyExpressionNode : public BaseASTNode {
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> left;
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> right;

        explicit MultiplyExpressionNode (std::unique_ptr<BaseASTNode>&& t_left, std::unique_ptr<BaseASTNode>&& t_right);
        std::string toString (const std::string& indent) const override;
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::MultiplyExpression;
        }

        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };

    struct UnaryPlusExpressionNode : public BaseASTNode {
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> right;

        explicit UnaryPlusExpressionNode (std::unique_ptr<BaseASTNode>&& t_right);
        std::string toString (const std::string& indent) const override;
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::UnaryPlusExpression;
        }

        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };
    struct UnaryMinusExpressionNode : public BaseASTNode {
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> right;

        explicit UnaryMinusExpressionNode (std::unique_ptr<BaseASTNode>&& t_right);
        std::string toString (const std::string& indent) const override;
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::UnaryMinusExpression;
        }

        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };

    struct FunctionCallNode : public BaseASTNode {
        // TODO: replace this with a unique_ptr
        /// IdentifyingNameNode* ew
        std::unique_ptr<BaseASTNode> identity;
        ///                        ExpressionNode*
        std::vector<std::unique_ptr<BaseASTNode>> arguments;
        explicit FunctionCallNode (std::unique_ptr<BaseASTNode>&& t_identity);
        explicit FunctionCallNode (std::unique_ptr<BaseASTNode>&& t_identity, std::vector<std::unique_ptr<BaseASTNode>>&& t_arguments);
        std::string toString (const std::string& indent) const override;
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::FunctionCallExpression;
        }

        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };

    // ==== Statement Nodes ====
    struct StatementASTNode {
        enum class Kind {
            #define STATEMENT_KIND(X) X,
            #include "StatementKinds.inc"
        };
        private:
        const Kind kind;
        public:
        Kind getKind () const {
            return kind;
        }

        StatementASTNode (Kind t_kind) : kind(t_kind) {}
        virtual ~StatementASTNode () {}
        virtual std::string toString (const std::string&) const = 0;

        virtual llvm::Value* codegen (Compiler::Compiler&) {
            throw std::runtime_error("Unimplemented statement codegen.");
        }
    };

    struct ExpressionStatementNode : public StatementASTNode {
        std::unique_ptr<BaseASTNode> expression;
        explicit ExpressionStatementNode (std::unique_ptr<BaseASTNode>&& t_expression);
        std::string toString(const std::string& indent) const override;
        static bool classof (const StatementASTNode* s) {
            return s->getKind() == Kind::Expression;
        }

        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };

    struct VariableStatementNode : public StatementASTNode {
        // TODO: make this use unique_ptr
        // IdentifyingNameNode* ew
        std::unique_ptr<BaseASTNode> identity;
        std::unique_ptr<TypeNode> type;
        // ExpressionNode*
        std::unique_ptr<BaseASTNode> value;
        explicit VariableStatementNode (std::unique_ptr<BaseASTNode>&& t_identity, std::unique_ptr<TypeNode>&& t_type, std::unique_ptr<BaseASTNode>&& t_value);
        std::string toString (const std::string& indent) const override;
        static bool classof (const StatementASTNode* s) {
            return s->getKind() == Kind::Variable;
        }

        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };

    struct ReturnStatementNode : public StatementASTNode {
        // TODO: make this a possible_null structure since we mostly assume pointers aren't null
        /// ExpressionNode* possibly null
        std::unique_ptr<BaseASTNode> value = nullptr;

        explicit ReturnStatementNode (std::unique_ptr<BaseASTNode>&& t_value=nullptr);

        std::string toString (const std::string& indent) const override;

        static bool classof (const StatementASTNode* s) {
            return s->getKind() == Kind::Return;
        }

        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };

    // Note: this should perhaps be made into it's own virtual node type so that it can be replaced in a pass as well.
    struct ConditionalPart {
        // TODO: wrap in possible_null
        /// ExpressionNode* may be null
        std::unique_ptr<BaseASTNode> condition = nullptr;
        std::vector<std::unique_ptr<StatementASTNode>> body;
        bool always_exits = false;

        explicit ConditionalPart (std::unique_ptr<BaseASTNode>&& t_condition, std::vector<std::unique_ptr<StatementASTNode>>&& t_body);

        std::string toString (const std::string& indent) const;
    };
    struct IfStatementNode : public StatementASTNode {
        std::unique_ptr<ConditionalPart> root;
        std::vector<std::unique_ptr<ConditionalPart>> parts;
        // This is more than just a (root->always_exits && parts[i]->always_exits...) 
        // If there is no else statement then it's possible that it exits.
        bool always_exits = false;
        explicit IfStatementNode (std::unique_ptr<ConditionalPart>&& t_root);

        std::string toString (const std::string& indent) const override;

        static bool classof (const StatementASTNode* s) {
            return s->getKind() == Kind::If;
        }

        virtual bool hasElseStatement () const;

        /// Gets the else statement. Does not check if one should exist, use with care.
        virtual std::unique_ptr<ConditionalPart>& getElseStatement ();

        virtual llvm::Value* codegenIf (Compiler::Compiler& compiler);

        llvm::Value* codegen (Compiler::Compiler& compiler) override;

        protected:
        llvm::Value* createConditional (Compiler::Compiler& compiler, llvm::Value* condition);
    };

    // then function node
    // then start working on parser.cpp. You will remove compiler for now while you get this back to working.


    // ==== Declaration nodes ====

    // This could potentially be turned into a node of it's own status for now it's purely for functions
    struct FunctionParameterInfo {
        // TODO: unique_ptriffiy
        std::unique_ptr<TypeNode> type;
        // TODO: identitynode-ify
        std::string name;
        explicit FunctionParameterInfo (std::unique_ptr<TypeNode>&& t_type, std::string t_name);
        std::string toString (const std::string& indent) const;
    };
    struct FunctionNode : public DeclASTNode {
        // TODO: should this be allowed to be null for compiler generated functions
        /// IdentifyingNameNode*
        std::unique_ptr<BaseASTNode> identity;
        // This is made a list of pointers despite functionparameterinfo not needing it, because it may soon in the future.
        std::vector<std::unique_ptr<FunctionParameterInfo>> parameters;
        std::unique_ptr<TypeNode> return_type;
        std::vector<std::unique_ptr<StatementASTNode>> body;
        explicit FunctionNode (std::unique_ptr<BaseASTNode>&& t_identity, std::vector<std::unique_ptr<FunctionParameterInfo>>&& t_parameters, std::unique_ptr<TypeNode>&& t_return_type, std::vector<std::unique_ptr<StatementASTNode>>&& t_body);

        std::string toString (const std::string& indent) const override;

        static bool classof (const DeclASTNode* d) {
            return d->getKind() == Kind::Function;
        }

        virtual llvm::Value* codegenPrototype (Compiler::Compiler& compiler);
        virtual llvm::Value* codegenBody (Compiler::Compiler& compiler);
        llvm::Value* codegen (Compiler::Compiler& compiler) override;
    };
}