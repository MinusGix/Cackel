#pragma once

#include <string>
#include <vector>
#include <optional>
#include <memory>

#include "util.hpp"
#include "Lexer.hpp"

namespace Parser {
    using Token = Lexer::Token;

	struct FunctionNode;
	using ASTNode = std::variant<
		FunctionNode
	>;

	struct BaseASTNode {
		std::string toString (const std::string&) const {
			return "BaseASTNode";
		}
	};
	// mixin
	struct ChildrenASTNode {
		std::vector<ASTNode> nodes;
		std::string toString (const std::string& orig_indent) const {
			const std::string indent = orig_indent + "\t";
			return "[\n" + indent + Util::stringJoin(nodes, ",\n", indent) + "\n]";
		}
	};
	struct ParentASTNode : public BaseASTNode, ChildrenASTNode {
		std::string toString (const std::string& indent) const {
			return "Parent " + ChildrenASTNode::toString(indent) + "\n";
		}
	};

	// Types
	enum class PrimordialType {
		Void,
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
	struct PrimordialTypeNode {
		PrimordialType type;
		explicit PrimordialTypeNode (PrimordialType t) : type(t) {}
		std::string toString (const std::string&) const {
			return "PT[" + primordialTypeToString(type) + "]";
		}
	};
	struct UnknownTypeNode {
		//Token value;
		//explicit UnknownTypeNode (Token t) : value(t) {}
		std::string toString (const std::string&) const {
			return "UT";//[" + value.toString(indent) + "]";
		}
	};
	using TypeNode = std::variant<
		PrimordialTypeNode,
		UnknownTypeNode
	>;



	struct RawIdentifierNode : public BaseASTNode {
		std::string name;
		explicit RawIdentifierNode (std::string n) : name(n) {}
		std::string toString (const std::string&) const {
			return "RID[" + name + "]";
		}
	};
	// This is basically the same as Raw, but we use this for cases where it likely refers to something
	// (like another variable) rather than being the name of the variable where it's declared
	struct LiteralIdentifierNode : public BaseASTNode {
		std::string name;
		explicit LiteralIdentifierNode (std::string&& n) : name(n) {}
		explicit LiteralIdentifierNode (const Token& identifier_token) {
			if (!identifier_token.is(Token::Type::Identifier)) {
				throw std::logic_error("Got token to LiteralIdentifierNode constructor that wasn't of type Identifier.");
			}

			name = identifier_token.getData<Token::IdentifierData>().data;
		}
		std::string toString (const std::string&) const {
			return "LID[" + name + "]";
		}
	};
	// Absurdly simple, always positive since parsing of +/- is done later.
	struct LiteralNumberNode : public BaseASTNode {
		// Not yet parsed because we don't want to lose information.
		std::string literal_value;

		explicit LiteralNumberNode (std::string&& value) : literal_value(value) {}
		explicit LiteralNumberNode (const Token& number_token) {
			if (!number_token.isNumber()) {
				throw std::runtime_error("Got token to literalnumbernode constructor that wasn't of type enum.");
			}

			literal_value = number_token.getData<Token::NumberData>().data;
		}
		std::string toString (const std::string&) const {
			return "LNum[" + literal_value + "]";
		}
	};
    struct AddExpressionNode;
	using ExpressionNode = std::variant<
		LiteralIdentifierNode,
		LiteralNumberNode,
		AddExpressionNode
	>;
    struct AddExpressionNode : public BaseASTNode {
		// non-null
		Util::DeepUniquePtr<ExpressionNode> left;
		// non-null
		Util::DeepUniquePtr<ExpressionNode> right;

		explicit AddExpressionNode (ExpressionNode t_left, ExpressionNode t_right);
		std::string toString (const std::string& indent) const;
	};

	struct VariableStatementNode {
		std::string name;
		TypeNode type;
		ExpressionNode value;
		explicit VariableStatementNode (std::string t_name, TypeNode t_type, ExpressionNode t_value) : name(t_name), type(t_type), value(t_value) {}
		explicit VariableStatementNode (std::string&& t_name, TypeNode t_type, ExpressionNode t_value) : name(t_name), type(t_type), value(t_value) {}

		std::string toString (const std::string& indent) const {
			return "VS[" + name + " = " + Util::toString(value, indent) + "]";
		}
	};

	struct ReturnStatementNode {
		std::optional<ExpressionNode> value;
		// Void
		explicit ReturnStatementNode () {}
		explicit ReturnStatementNode (ExpressionNode t_value) : value(t_value) {}

		// We can kindof check if the return value is void by just checking if we have a value,
		// but that's incorrect since we might be returning the result of a function call that returns void.
		// which is why we don't provide a utility function, to force thinking about it since we can't make a general-use one.

		std::string toString (const std::string& indent) const {
			if (value.has_value()) {
				return "Return[" + Util::toString(value.value(), indent) + "]";
			} else {
				return "Return[]";
			}
		}
	};

	using StatementNode = std::variant<
		ExpressionNode, // expressions can be by themselves
		VariableStatementNode,
        ReturnStatementNode
	>;




	// Functions
	struct FunctionParameterNode : public BaseASTNode {
		TypeNode type;
		std::string name;
		explicit FunctionParameterNode (TypeNode t, std::string n) : type(t), name(n) {}
		std::string toString (const std::string& indent) const {
			return "FParam[" + Util::toString(type, indent) + ", " + name + "]";
		}
	};
	struct FunctionNode : public BaseASTNode {
		RawIdentifierNode identifier;
		std::vector<FunctionParameterNode> parameters;
		TypeNode return_type;
		std::vector<StatementNode> body;
		explicit FunctionNode (RawIdentifierNode id, std::vector<FunctionParameterNode>&& p, TypeNode ret, std::vector<StatementNode>&& b) :
			identifier(id), parameters(p), return_type(ret), body(b) {}

		std::string toString (const std::string& indent) const {
			return "Function[" + identifier.toString(indent) + ",\n" +
				indent + " \t(" + Util::stringJoin(parameters, ", ", indent + "\t") + "),\n" +
				indent + "\t->" + Util::toString(return_type, indent) + ", ""{\n" +
				indent + "\t\t" + Util::stringJoin(body, ";\n" + indent + "\t\t", indent + "\t\t", true) + "\n" + indent + "\t}";
		}
	};

    struct Parser : public Util::IndexStack {
        std::vector<Token> tokens;
        ParentASTNode nodes;

        const Token& at (std::optional<size_t> value=std::nullopt) const;
        Token& at (std::optional<size_t> value=std::nullopt);
        bool indexValid () const;
        bool indexValid (size_t value) const;
        bool is (Token::Type type, std::optional<size_t> t_value=std::nullopt) const;
        bool isIdentifier (const std::string& expected_value, std::optional<size_t> value=std::nullopt) const;
        bool isr (Token::Type type, size_t offset) const;
        const Token& expect (Token::Type type, std::optional<size_t> value=std::nullopt) const;
        const Token& expectIdentifier (const std::string& expected_value, std::optional<size_t> value=std::nullopt) const;

        explicit Parser (std::vector<Token>&& t_tokens);

        void parse ();

        std::optional<FunctionNode> parseFunction ();

        std::optional<TypeNode> parseType ();
        // TODO: move this out of class
        std::optional<PrimordialType> parsePrimordialType_helper (const std::string& name);

        std::optional<StatementNode> parseStatement ();
        std::optional<StatementNode> parseStatement_variableDeclaration ();
        std::optional<StatementNode> parseStatement_return ();


        std::optional<ExpressionNode> parseExpression ();
        std::optional<ExpressionNode> tryParseSingularExpression ();
    };
}