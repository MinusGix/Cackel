#include "Parser.hpp"

#include <iostream>
#include <memory>

namespace Parser {
    // ==== Node Utilities ===
	std::string primordialTypeToString (PrimordialType type) {
		switch (type) {
			case PrimordialType::Void:
				return "void";
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
        if (name == "int") {
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

    // ==== Nodes ====

    AddExpressionNode::AddExpressionNode (ExpressionNode t_left, ExpressionNode t_right) : left(new ExpressionNode(std::move(t_left))), right(new ExpressionNode(std::move(t_right))) {}
    std::string AddExpressionNode::toString (const std::string& indent) const {
        return "[" + Util::toString(left.get(), indent) + " + " + Util::toString(right.get(), indent) + "]";
	}

    // ==== Parser Utilities ====

    // ==== Parser ====

    Parser::Parser (std::vector<Token>&& t_tokens) : tokens(t_tokens) {}

    const Token& Parser::at (std::optional<size_t> value) const {
        return tokens.at(value.value_or(getIndex()));
    }
    Token& Parser::at (std::optional<size_t> value) {
        return tokens.at(value.value_or(getIndex()));
    }
    bool Parser::indexValid () const {
        return indexValid(getIndex());
    }
    bool Parser::indexValid (size_t value) const {
        return value < tokens.size();
    }
    bool Parser::is (Token::Type type, std::optional<size_t> t_value) const {
        const size_t value = t_value.value_or(getIndex());
        if (indexValid(value)) {
            return at(value).is(type);
        } else {
            return false; // it doesn't exist. can't be a type
        }
    }
    bool Parser::isIdentifier (const std::string& expected_value, std::optional<size_t> value) const {
        if (is(Token::Type::Identifier, value)) {
            const auto& data = at(value).getData<Token::IdentifierData>();
            if (data.data == expected_value) {
                return true;
            }
        }
        return false;
    }
    bool Parser::isr (Token::Type type, size_t offset) const {
        return is(type, getIndex() + offset);
    }
    const Token& Parser::expect (Token::Type type, std::optional<size_t> value) const {
        if (!is(type, value)) {
            throw std::runtime_error("Expected token of (" + Token::typeToString(type) + ") got " + Token::typeToString(at().type));
        }
        return at(value);
    }
    const Token& Parser::expectIdentifier (const std::string& expected_value, std::optional<size_t> value) const {
        expect(Token::Type::Identifier, value);
        const auto& data = at().getData<Token::IdentifierData>();
        if (data.data != expected_value) {
            throw std::runtime_error("Unexpected identifier, expected: '" + expected_value + "' got: '" + data.data + "'");
        }
        return at(value);
    }

    void Parser::parse () {
        while (indexValid()) {
            if (is(Token::Type::Identifier)) {
                auto& data = at().getData<Token::IdentifierData>();
                if (data.data == "func") {
                    // Function parsing
                    nodes.nodes.push_back(parseFunction().value());
                } else {
                    throw std::runtime_error("Unexpected identifier: " + at().toString(""));
                }
            } else {
                throw std::runtime_error("Unexpected token: " + at().toString(""));
            }
        }
    }
    std::optional<FunctionNode> Parser::parseFunction () {
        expectIdentifier("func");         // func
        advance();

        const Token& identifier = expect(Token::Type::Identifier);   // name
        advance();

        TypeNode return_type = PrimordialTypeNode(PrimordialType::Void);

        // Function parameter list. (name1 : type1, name2: type2)
        expect(Token::Type::LParen); // (
        advance();

        std::vector<FunctionParameterNode> parameters;
        while (!is(Token::Type::RParen)) {
            const Token& name_token = expect(Token::Type::Identifier); // name
            const std::string& name = name_token.getData<Token::IdentifierData>().data;
            advance();
            expect(Token::Type::Colon); // :
            advance();
            auto type = parseType(); // type
            if (!type.has_value()) {
                throw std::runtime_error("Got parameter name (" + name + ") and colon, but type information was invalid.");
            }

            parameters.push_back(FunctionParameterNode(type.value(), name));

            if (is(Token::Type::Comma)) {
                advance();
                expect(Token::Type::Identifier);
            }
        }

        expect(Token::Type::RParen); // )
        advance();

        // TODO: parse possible modifiers here: (mut, reass)

        // Return type, -> type
        if (is(Token::Type::Minus)) {
            advance();
            if (is(Token::Type::RAngle)) { // ->
                advance();
                std::optional<TypeNode> written_type = parseType();

                if (!written_type.has_value()) {
                    throw std::runtime_error("Failed parsing return type of function.");
                }
                return_type = written_type.value();
            } else {
                throw std::runtime_error("Unexpected lone - after function parameters.");
            }
        }

        // opening brace
        expect(Token::Type::LBracket); // {
        advance();

        // function body
        std::vector<StatementNode> body;

        while (true) {
            std::optional<StatementNode> statement = parseStatement();
            if (!statement.has_value()) {
                break;
            }
            body.push_back(statement.value());
        }

        // closing brace
        expect(Token::Type::RBracket); // }
        advance();

        //
        return FunctionNode(
            RawIdentifierNode(identifier.getData<Token::IdentifierData>().data),
            std::move(parameters),
            return_type,
            std::move(body)
        );
    }

    std::optional<TypeNode> Parser::parseType () {
        pushIndice();

        const Token& name_token = expect(Token::Type::Identifier);
        const auto& name = name_token.getData<Token::IdentifierData>();
        std::optional<TypeNode> node;
        std::optional<PrimordialType> primordial_type = stringToPrimordialType(name.data);
        if (primordial_type.has_value()) {
            node.emplace(PrimordialTypeNode(primordial_type.value()));
        } else {
            node.emplace(UnknownTypeNode());
        }
        advance();

        slideIndice(node.has_value());
        return node;
    }

    std::optional<StatementNode> Parser::parseStatement () {
        pushIndice();

        std::optional<StatementNode> statement;

        if (isIdentifier("let")) {
            statement = parseStatement_variableDeclaration();
        } else if (isIdentifier("return")) {
            statement = parseStatement_return();
        }

        slideIndice(statement.has_value());
        return statement;
    }
    std::optional<StatementNode> Parser::parseStatement_variableDeclaration () {
        expectIdentifier("let");
        advance();

        // variable name
        const Token& identifier = expect(Token::Type::Identifier);
        advance();

        // : before type information. We don't do any automatic types yet
        expect(Token::Type::Colon);
        advance();

        // Type
        std::optional<TypeNode> type = parseType();
        if (!type.has_value()) {
            throw std::runtime_error("Expected type in declaration of variable.");
        }

        // Currently we require there to be a value it is set to. This might be changed in the future.
        // =
        expect(Token::Type::Equals);
        advance();

        std::optional<ExpressionNode> expression = parseExpression();
        if (!expression.has_value()) {
            throw std::runtime_error("Expected expression to be what variable is set to.");
        }

        expect(Token::Type::Semicolon);
        advance();

        return VariableStatementNode(
            identifier.getData<Token::IdentifierData>().data,
            type.value(),
            expression.value()
        );
    }
    std::optional<StatementNode> Parser::parseStatement_return () {
        expectIdentifier("return");
        advance();

        std::cout << "parse return statement.\n";

        if (is(Token::Type::Semicolon)) { // return;
            advance();
            return ReturnStatementNode();
        }

        std::optional<ExpressionNode> expression = parseExpression();
        if (!expression.has_value()) {
            throw std::runtime_error("Expected expression or no expression after `return`, got something that was not a valid expression.");
        }

        expect(Token::Type::Semicolon);
        advance();

        return ReturnStatementNode(expression.value());
    }


    std::optional<ExpressionNode> Parser::parseExpression () {
        const Token& current = at();

        // TODO: We can totaly move parsing binary operators into it's own function :]
        // then just have it call this.... gotta be careful not to just loop forever.
        std::optional<ExpressionNode> first = tryParseSingularExpression();
        if (!first.has_value()) {
            throw std::runtime_error("Unknown token in what should be an expression: " + current.toString(""));
        }

        // TODO: multiply *
        // TODO: divide /
        // TODO: modulo %
        // TODO: bitwise-and &
        // TODO: bitwise-or |
        // TODO: logical-or ||
        // TODO: logical-and &&
        // TODO: bitwise-xor ^
        // TODO: bitwise-not ~
        // TODO: logical-not !
        // TODO: integer division dunno

        // [x] + [y], [x] - [y]
        Token::Type type;
        if (is(Token::Type::Plus)) {
            type = Token::Type::Plus;
        } else if (is(Token::Type::Minus)) {
            type = Token::Type::Minus;
        } else {
            return first; // simple singular expression;
        }
        advance();

        std::optional<ExpressionNode> second = tryParseSingularExpression();
        if (!second.has_value()) {
            throw std::runtime_error("Expected second expression after binary operator [" + Token::typeToString(type) + "]");
        }

        if (type == Token::Type::Plus) {
            //return AddExpressionNode(first.value(), second.value());
            return AddExpressionNode(first.value(), second.value());
        } else if (type == Token::Type::Minus) {
            //return SubtractExpressionNode(first.value(), second.value());
            throw std::runtime_error("Unsupported subtract operation");
        } else {
            throw std::runtime_error("Expected expression.");
        }
        // TODO: support remotely complex expressions.
    }
    // parse a singular expression item. [x]
    std::optional<ExpressionNode> Parser::tryParseSingularExpression () {
        // TODO: support -[literal] and +[literal]
        const Token& current = at();

        // literal identifier, so potentially a variable name
        if (is(Token::Type::Identifier)) {
            advance();
            return LiteralIdentifierNode(current);
        } else if (current.isNumber()) { // literal number
            advance();
            return LiteralNumberNode(current);
        }
        return std::nullopt;
    }
}