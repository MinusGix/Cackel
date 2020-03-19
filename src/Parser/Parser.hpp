#pragma once

#include <string>
#include <vector>
#include <optional>
#include <memory>

#include "../Util/util.hpp"
#include "../Lexer/Lexer.hpp"

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
    };

    struct ParentASTNode {
        std::vector<std::unique_ptr<DeclASTNode>> nodes;
        explicit ParentASTNode () {}
        ParentASTNode(ParentASTNode&& other) : nodes(std::move(other.nodes)) {}
        ParentASTNode (const ParentASTNode&) = delete;
        ParentASTNode& operator= (const ParentASTNode&) = delete;
        std::string toString (const std::string orig_indent) const {
            const std::string indent = orig_indent + "\t";
            return "Parent: [\n" + indent + Util::stringJoin(nodes, "\n" + indent, indent) + "\n]";
        }
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
        explicit LiteralIdentifierNode (std::string&& t_name) : BaseASTNode(Kind::IdentifierLiteral), name(t_name) {}
        explicit LiteralIdentifierNode (const Lexer::Token& identifier_token) : BaseASTNode(Kind::IdentifierLiteral) {
            if (!identifier_token.is(Lexer::Token::Type::Identifier)) {
                throw std::logic_error("Got token to LiteralIdentifierNode constructor that wasn't of type identifier.");
            }
            name = identifier_token.getData<Lexer::Token::IdentifierData>().data;
        }

        std::string toString (const std::string&) const override {
            return "LID[" + name + "]";
        }

        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::IdentifierLiteral;
        }
    };

    struct LiteralNumberNode : public BaseASTNode {
        std::string value;
        explicit LiteralNumberNode (std::string&& t_value) : BaseASTNode(Kind::NumberLiteral), value(t_value) {}
        explicit LiteralNumberNode (const Lexer::Token& number_token) : BaseASTNode(Kind::NumberLiteral) {
            if (!number_token.isNumber()) {
                throw std::runtime_error("Got token to literal number node constructor that wasn't of any number type.");
            }

            value = number_token.getData<Lexer::Token::NumberData>().data;
        }
        std::string toString (const std::string&) const override {
            return "LNum[" + value + "]";
        }
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::NumberLiteral;
        }
    };

    struct LiteralBooleanNode : public BaseASTNode {
        bool value;
        explicit LiteralBooleanNode (bool t_value) : BaseASTNode(Kind::BooleanLiteral), value(t_value) {}
        std::string toString (const std::string&) const override {
            if (value) {
                return "B[true]";
            } else {
                return "B[false]";
            }
        }
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::BooleanLiteral;
        }
    };

    // These should probably somehow go under an operator node-type
    struct AddExpressionNode : public BaseASTNode {
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> left;
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> right;

        explicit AddExpressionNode (std::unique_ptr<BaseASTNode>&& t_left, std::unique_ptr<BaseASTNode>&& t_right) : BaseASTNode(Kind::AddExpression), left(std::move(t_left)), right(std::move(t_right)) {}
        std::string toString (const std::string& indent) const override{
            return "+(" + left->toString(indent) + ", " + right->toString(indent) + ")";
        }
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::AddExpression;
        }
    };
    struct SubtractExpressionNode : public BaseASTNode {
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> left;
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> right;

        explicit SubtractExpressionNode (std::unique_ptr<BaseASTNode>&& t_left, std::unique_ptr<BaseASTNode>&& t_right) : BaseASTNode(Kind::SubtractExpression), left(std::move(t_left)), right(std::move(t_right)) {}
        std::string toString (const std::string& indent) const override {
            return "-(" + left->toString(indent) + ", " + right->toString(indent) + ")";
        }
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::SubtractExpression;
        }
    };
    struct MultiplyExpressionNode : public BaseASTNode {
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> left;
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> right;

        explicit MultiplyExpressionNode (std::unique_ptr<BaseASTNode>&& t_left, std::unique_ptr<BaseASTNode>&& t_right) : BaseASTNode(Kind::MultiplyExpression), left(std::move(t_left)), right(std::move(t_right)) {}
        std::string toString (const std::string& indent) const override {
            return "*(" + left->toString(indent) + ", " + right->toString(indent) + ")";
        }
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::MultiplyExpression;
        }
    };
    
    struct UnaryPlusExpressionNode : public BaseASTNode {
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> right;

        explicit UnaryPlusExpressionNode (std::unique_ptr<BaseASTNode>&& t_right) : BaseASTNode(Kind::UnaryPlusExpression), right(std::move(t_right)) {}
        std::string toString (const std::string& indent) const override {
            return "+(" + right->toString(indent) + ")";
        }
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::UnaryPlusExpression;
        }
    };
    struct UnaryMinusExpressionNode : public BaseASTNode {
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> right;

        explicit UnaryMinusExpressionNode (std::unique_ptr<BaseASTNode>&& t_right) : BaseASTNode(Kind::UnaryMinusExpression), right(std::move(t_right)) {}
        std::string toString (const std::string& indent) const override {
            return "-(" + right->toString(indent) + ")";
        }
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::UnaryMinusExpression;
        }
    };

    struct FunctionCallNode : public BaseASTNode {
        // TODO: replace this with a unique_ptr
        /// IdentifyingNameNode* ew
        std::unique_ptr<BaseASTNode> identity;
        ///                        ExpressionNode*
        std::vector<std::unique_ptr<BaseASTNode>> arguments;
        explicit FunctionCallNode (std::unique_ptr<BaseASTNode>&& t_identity) : BaseASTNode(Kind::FunctionCallExpression), identity(std::move(t_identity)) {}
        explicit FunctionCallNode (std::unique_ptr<BaseASTNode>&& t_identity, std::vector<std::unique_ptr<BaseASTNode>>&& t_arguments) : BaseASTNode(Kind::FunctionCallExpression), identity(std::move(t_identity)), arguments(std::move(t_arguments)) {}
        std::string toString (const std::string& indent) const override {
            return "Func[" + identity->toString(indent) + "(" + "... arguments TODO" + ")]";
        }
        static bool classof (const BaseASTNode* i) {
            return i->getKind() == Kind::FunctionCallExpression;
        }
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
    };

    struct VariableStatementNode : public StatementASTNode {
        // TODO: make this use unique_ptr
        // IdentifyingNameNode* ew
        std::unique_ptr<BaseASTNode> identity;
        std::unique_ptr<TypeNode> type;
        // ExpressionNode*
        std::unique_ptr<BaseASTNode> value;
        explicit VariableStatementNode (std::unique_ptr<BaseASTNode>&& t_identity, std::unique_ptr<TypeNode>&& t_type, std::unique_ptr<BaseASTNode>&& t_value) : StatementASTNode(Kind::Variable), identity(std::move(t_identity)), type(std::move(t_type)), value(std::move(t_value)) {}
        std::string toString (const std::string& indent) const override {
            return "VS[" + identity->toString(indent) + " = " + value->toString(indent) + "]";
        }
        static bool classof (const StatementASTNode* s) {
            return s->getKind() == Kind::Variable;
        }
    };

    struct ReturnStatementNode : public StatementASTNode {
        // TODO: make this a possible_null structure since we mostly assume pointers aren't null
        /// ExpressionNode* possibly null
        std::unique_ptr<BaseASTNode> value = nullptr;

        explicit ReturnStatementNode (std::unique_ptr<BaseASTNode>&& t_value=nullptr) : StatementASTNode(Kind::Return), value(std::move(t_value)) {}

        std::string toString (const std::string& indent) const override {
            if (value == nullptr) {
                return "Return[]";
            } else {
                return "Return[" + value->toString(indent) + "]";
            }
        }

        static bool classof (const StatementASTNode* s) {
            return s->getKind() == Kind::Return;
        }
    };

    // Note: this should perhaps be made into it's own virtual node type so that it can be replaced in a pass as well.
    struct ConditionalPart {
        // TODO: wrap in possible_null
        /// ExpressionNode* may be null
        std::unique_ptr<BaseASTNode> condition = nullptr;
        std::vector<std::unique_ptr<StatementASTNode>> body;
        explicit ConditionalPart (std::unique_ptr<BaseASTNode>&& t_condition, std::vector<std::unique_ptr<StatementASTNode>>&& t_body) : condition(std::move(t_condition)), body(std::move(t_body)) {}

        std::string toString (const std::string& indent) const {
            std::string result;
            if (condition != nullptr) {
                result = "?If[" + Util::toString(condition, indent) + "]";
            } else {
                result = "Else";
            }
            return result + " {\n" + indent + "\t" + Util::stringJoin(body, ";\n" + indent + "\t", indent + "\t") + ";\n" + indent + "}";
        }
    };
    struct IfStatementNode : public StatementASTNode {
        std::unique_ptr<ConditionalPart> root;
        std::vector<std::unique_ptr<ConditionalPart>> parts;
        explicit IfStatementNode (std::unique_ptr<ConditionalPart>&& t_root) : StatementASTNode(Kind::If), root(std::move(t_root)) {}

        std::string toString (const std::string& indent) const override {
            return root->toString(indent) + Util::stringJoin(parts, " ", indent);
        }

        static bool classof (const StatementASTNode* s) {
            return s->getKind() == Kind::If;
        }

        virtual bool hasElseStatement () const {
            if (parts.size() > 0) {
                return parts.at(parts.size() - 1)->condition == nullptr;
            }
            return false;
        }

        /// Gets the else statement. Does not check if one should exist, use with care.
        virtual std::unique_ptr<ConditionalPart>& getElseStatement () {
            return parts.at(parts.size() - 1);
        }
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
        explicit FunctionParameterInfo (std::unique_ptr<TypeNode>&& t_type, std::string t_name) : type(std::move(t_type)), name(t_name) {}
        std::string toString (const std::string& indent) const {
            return "FParam[" + type->toString(indent) + ", " + name + "]";
        }
    };
    struct FunctionNode : public DeclASTNode {
        // TODO: should this be allowed to be null for compiler generated functions
        /// IdentifyingNameNode*
        std::unique_ptr<BaseASTNode> identity;
        // This is made a list of pointers despite functionparameterinfo not needing it, because it may soon in the future.
        std::vector<std::unique_ptr<FunctionParameterInfo>> parameters;
        std::unique_ptr<TypeNode> return_type;
        std::vector<std::unique_ptr<StatementASTNode>> body;
        explicit FunctionNode (std::unique_ptr<BaseASTNode>&& t_identity, std::vector<std::unique_ptr<FunctionParameterInfo>>&& t_parameters, std::unique_ptr<TypeNode>&& t_return_type, std::vector<std::unique_ptr<StatementASTNode>>&& t_body) :
            DeclASTNode(Kind::Function), identity(std::move(t_identity)), parameters(std::move(t_parameters)), return_type(std::move(t_return_type)), body(std::move(t_body)) {}

        std::string toString (const std::string& indent) const override {
            return "Function[" + Util::toString(identity, indent) + ",\n" +
				indent + " \t(" + Util::stringJoin(parameters, ", ", indent + "\t") + "),\n" +
				indent + "\t->" + Util::toString(return_type, indent) + ", ""{\n" +
				indent + "\t\t" + Util::stringJoin(body, ";\n" + indent + "\t\t", indent + "\t\t", true) + "\n" + indent + "\t}";
        }

        static bool classof (const DeclASTNode* d) {
            return d->getKind() == Kind::Function;
        }
    };





    struct Parser : public Util::IndexStack {
        std::vector<Lexer::Token> tokens;
        ParentASTNode nodes;

        explicit Parser (std::vector<Lexer::Token>&& t_tokens);

        const Lexer::Token& at (std::optional<size_t> value=std::nullopt) const;
        Lexer::Token& at (std::optional<size_t> value=std::nullopt);
        bool indexValid () const;
        bool indexValid (size_t value) const;
        bool is (Lexer::Token::Type type, std::optional<size_t> value=std::nullopt) const;
        bool isIdentifier (const std::string& expected_value, std::optional<size_t> value=std::nullopt) const;
        bool isr (Lexer::Token::Type type, size_t offset) const;
        const Lexer::Token& expect (Lexer::Token::Type type, std::optional<size_t> value=std::nullopt) const;
        const Lexer::Token& expectIdentifier (const std::string& expected_value, std::optional<size_t> value=std::nullopt) const;

        void parse ();
        std::unique_ptr<FunctionNode> parseFunction ();
        std::unique_ptr<TypeNode> parseType ();
        std::unique_ptr<StatementASTNode> parseStatement ();
        std::unique_ptr<StatementASTNode> parseStatement_variableDeclaration ();
        std::unique_ptr<StatementASTNode> parseStatement_return ();
        std::unique_ptr<StatementASTNode> parseStatement_if ();
        Util::Result<std::unique_ptr<FunctionCallNode>> tryParseFunctionCall ();

        std::unique_ptr<BaseASTNode> parseExpression ();
        Util::Result<std::unique_ptr<BaseASTNode>> tryParseIdentifyingName ();
    };
}