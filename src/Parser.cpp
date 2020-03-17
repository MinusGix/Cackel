#include "Parser.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <stack>

namespace Parser {
    // ==== Node Utilities ===
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

    // ==== Nodes ====

    AddExpressionNode::AddExpressionNode (ExpressionNode t_left, ExpressionNode t_right) : left(new ExpressionNode(std::move(t_left))), right(new ExpressionNode(std::move(t_right))) {}
    std::string AddExpressionNode::toString (const std::string& indent) const {
        return "(" + Util::toString(left.get(), indent) + " + " + Util::toString(right.get(), indent) + ")";
	}
    SubtractExpressionNode::SubtractExpressionNode (ExpressionNode t_left, ExpressionNode t_right) : left(new ExpressionNode(std::move(t_left))), right(new ExpressionNode(std::move(t_right))) {}
    std::string SubtractExpressionNode::toString (const std::string& indent) const {
        return "(" + Util::toString(left.get(), indent) + " - " + Util::toString(right.get(), indent) + ")";
	}
    MultiplyExpressionNode::MultiplyExpressionNode (ExpressionNode t_left, ExpressionNode t_right) : left(new ExpressionNode(std::move(t_left))), right(new ExpressionNode(std::move(t_right))) {}
    std::string MultiplyExpressionNode::toString (const std::string& indent) const {
        return "(" + Util::toString(left.get(), indent) + " * " + Util::toString(right.get(), indent) + ")";
	}
    UnaryMinusExpressionNode::UnaryMinusExpressionNode (ExpressionNode t_right) : right(new ExpressionNode(t_right)) {}
	std::string UnaryMinusExpressionNode::toString (const std::string& indent) const {
        return "-(" + Util::toString(right.get(), indent) + ")";
    }
    UnaryPlusExpressionNode::UnaryPlusExpressionNode (ExpressionNode t_right) : right(new ExpressionNode(t_right)) {}
	std::string UnaryPlusExpressionNode::toString (const std::string& indent) const {
        return "+(" + Util::toString(right.get(), indent) + ")";
    }


    FunctionCallNode::FunctionCallNode (IdentifyingNameNode id, std::vector<ExpressionNode> args) : identity(id), arguments(args) {}
    FunctionCallNode::~FunctionCallNode () = default;
    FunctionCallNode::FunctionCallNode (FunctionCallNode&& other) = default;
	FunctionCallNode::FunctionCallNode (const FunctionCallNode& other) = default;
    FunctionCallNode& FunctionCallNode::operator= (FunctionCallNode&& other) noexcept = default;
	FunctionCallNode& FunctionCallNode::operator= (const FunctionCallNode& other) noexcept = default;
    std::string FunctionCallNode::toString (const std::string& indent) const {
        return "Func[" + Util::toString(identity, indent) + "(" + Util::mapJoin(arguments, [&indent] (auto&& v) { return Util::toString(v, indent); }, ", ") + ")]";
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

    // TODO: it'd be good to simply return the root node here rather than forcing us to access the 'internal' state of this class
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

        Util::Result<IdentifyingNameNode> identity = tryParseIdentifyingName();
        if (identity.holdsError()) {
            throw std::runtime_error(identity.getError());
        }

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
            identity.get(),
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
        Util::Result<IdentifyingNameNode> identity = tryParseIdentifyingName();
        if (identity.holdsError()) {
            throw std::runtime_error(identity.getError());
        }

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
            identity.get(),
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

    Util::Result<FunctionCallNode> Parser::tryParseFunctionCall () {
        pushIndice();

        Util::Result<IdentifyingNameNode> identity = tryParseIdentifyingName();
        if (identity.holdsError()) {
            return identity.getError();
        }

        if (!is(Token::Type::LParen)) {
            popIndice();
            return "Expected ( after function name.";
        }
        // it is LParen
        advance();

        // func()
        if (is(Token::Type::RParen)) {
            advance();
            transformIndice();
            return FunctionCallNode(identity.get(), {});
        }


        // When the function call has parameters

        std::vector<ExpressionNode> expressions;

        while (true) {
            std::optional<ExpressionNode> expr = parseExpression();
            if (!expr.has_value()) {
                popIndice();
                return "Expected closing parentheses, or expression, received neither.";
            }

            expressions.push_back(expr.value());

            if (is(Token::Type::RParen)) {
                advance();
                transformIndice();
                return FunctionCallNode(identity.get(), expressions);
            } else if (is(Token::Type::Comma)) {
                advance();
            } else {
                popIndice();
                return "Expected comma or closing parentheses after expression in function argument list.";
            }
        }
    }

    // ==== Expression Parsing Internals ====

    /// Note: Due to the way the climbing algo works, the higher number the is the more it's precedence
    /// rather than the usual "lower means earlier"
    /// Note: Since the values are precedence levels, this means operators are equivalent...
    /// So you should not use this enum to identify the operator
    enum class OperatorPrecedence : int {
        Addition = 1, // x + y
        Subtraction = Addition, // x - y

        Multiplication = 2, // x * y
        Division = Multiplication, // x / y
        Modulo = Multiplication, // x % y

        Exponentiation = 3, // **

        UnaryPlus = 4, // +x
        UnaryMinus = UnaryPlus, // -x
        PlusPlus = UnaryPlus, // x++ ++x
        MinusMinus = UnaryPlus, // x-- --x
    };

    enum class Associativity {
        Left,
        Right
    };

    static std::optional<ExpressionNode> parseExpressionExpr (Parser& parser, int min_prec);

    static bool isBinaryOperator (const Token& token) {
        return token.isOne(Token::Type::Plus, Token::Type::Minus, Token::Type::Star, Token::Type::Forwardslash, Token::Type::Percent);
    }
    static OperatorPrecedence getBinaryOperatorPrecedence (Token::Type type) {
        switch (type) {
            case Token::Type::Plus: return OperatorPrecedence::Addition;
            case Token::Type::Minus: return OperatorPrecedence::Subtraction;
            case Token::Type::Star: return OperatorPrecedence::Multiplication;
            case Token::Type::Forwardslash: return OperatorPrecedence::Division;
            case Token::Type::Percent: return OperatorPrecedence::Modulo;
            case Token::Type::StarStar: return OperatorPrecedence::Exponentiation;
            default:
                throw std::runtime_error("Unknown binary operator from token type: " + Token::typeToString(type));
        }
    }

    static Associativity getBinaryAssociativity (Token::Type type) {
        switch (type) {
            // Something like exponent might be right associative
            case Token::Type::StarStar:
                return Associativity::Right;
            default:
                return Associativity::Left;
        }
    }

    static bool isUnaryOperator (const Token& token) {
        return token.isOne(Token::Type::Plus, Token::Type::Minus);
    }
    static OperatorPrecedence getUnaryOperatorPrecedence (Token::Type type) {
        if (type == Token::Type::Plus) {
            return OperatorPrecedence::UnaryPlus;
        } else if (type == Token::Type::Minus) {
            return OperatorPrecedence::UnaryMinus;
        } else {
            throw std::runtime_error("Unknown unary operator from token: " + Token::typeToString(type));
        }
    }

    // This is ew, but it should work
    static bool isIdentifyingName (Parser& parser) {
        parser.pushIndice();
        Util::Result<IdentifyingNameNode> identity = parser.tryParseIdentifyingName();
        parser.popIndice();

        if (identity.holds()) {
            return true;
        } else {
            return false;
        }
    }

    static bool isLiteral (Parser& parser) {
        // identifying name checks if the token(s) is an identifier
        return parser.at().isNumber() || isIdentifyingName(parser);
    }

    /// Note: this simple creates the literal, it doesn not advacne the parser!
    static ExpressionNode createLiteral (Parser& parser) {
        const Token& token = parser.at();
        if (isIdentifyingName(parser)) {
            // Try parsing a function call, but this will return an error message if it failed.
            // This is fine, since we then try to check if it's just a normal identifier
            Util::Result<FunctionCallNode> function_call = parser.tryParseFunctionCall();
            if (function_call.holds()) {
                return function_call.get();
            }

            // If it is a raw identifier then we check it for some special literals
            if (parser.is(Token::Type::Identifier)) {
                if (parser.isIdentifier("true")) {
                    parser.advance();
                    return LiteralBooleanNode(true);
                } else if (parser.isIdentifier("false")) {
                    parser.advance();
                    return LiteralBooleanNode(false);
                }
            }

            // It wasn't a function call so we try parsing a normal identifier.
            Util::Result<IdentifyingNameNode> identity = parser.tryParseIdentifyingName();
            if (identity.holdsError()) {
                throw std::runtime_error("[Internal] Original error: '" + identity.getError() + "', but this should not have happened as directly before it is checked for if it is an identifying name.");
            }

            return identity.get();
        } else if (token.isNumber()) {
            parser.advance();
            return LiteralNumberNode(token);
        } else {
            throw std::runtime_error("Cannot make literal. Unknown token type: " + token.toString(""));
        }
    }

    static ExpressionNode createUnaryNode (Parser&, Token::Type type, ExpressionNode right) {
        if (type == Token::Type::Plus) {
            return UnaryPlusExpressionNode(right);
        } else if (type == Token::Type::Minus) {
            return UnaryMinusExpressionNode(right);
        } else {
            throw std::runtime_error("[Internal] Failed in creating node for unary operator: " + Token::typeToString(type) + " for expression: " + Util::toString(right, ""));
        }
    }
    static ExpressionNode createBinaryNode (Parser&, Token::Type type, ExpressionNode left, ExpressionNode right) {
        if (type == Token::Type::Plus) {
            return AddExpressionNode(left, right);
        } else if (type == Token::Type::Minus) {
            return SubtractExpressionNode(left, right);
        } else if (type == Token::Type::Star) {
            return MultiplyExpressionNode(left, right);
        } else if (type == Token::Type::StarStar) {
            throw std::runtime_error("Impl exponents");
        } else {
            throw std::runtime_error("[Internal] Failed in creating node for binary operator: " + Token::typeToString(type) + " for expressions: (" +
                Util::toString(left, "") + ", " + Util::toString(right, "") + ")"
            );
        }
    }

    static std::optional<ExpressionNode> parseExpressionPart (Parser& parser) {
        if (!parser.indexValid()) {
            std::cout << "We're past our stay. There's no more parts since index is past the end\n";
            return std::nullopt;
        } else if (parser.is(Token::Type::LParen)) { // (expression)
            parser.advance();
            std::optional<ExpressionNode> expr = parseExpressionExpr(parser, 1);
            if (!expr.has_value()) {
                throw std::runtime_error("Expected expression inside parentheses");
            }
            parser.expect(Token::Type::RParen);
            parser.advance();
            return expr;
        } else if (isUnaryOperator(parser.at())) {
            const Token& op_token = parser.at();
            OperatorPrecedence op = getUnaryOperatorPrecedence(op_token.type);
            parser.advance();
            std::optional<ExpressionNode> right = parseExpressionExpr(parser, static_cast<int>(op));
            if (!right.has_value()) {
                throw std::runtime_error("Expected expression after unary operator.");
            }
            return createUnaryNode(parser, op_token.type, right.value());
        } else if (isBinaryOperator(parser.at())) {
            throw std::runtime_error("Expected part while parsing expression but got binary operator: " + parser.at().toString(""));
        } else if (isLiteral(parser)) {
            return createLiteral(parser);
        } else {
            throw std::runtime_error("Unexpected token: " + parser.at().toString(""));
        }
    }

    static std::optional<ExpressionNode> parseExpressionExpr (Parser& parser, int min_prec) {
        std::optional<ExpressionNode> left = parseExpressionPart(parser);
        if (!left.has_value()) {
            std::cout << "Expected part but didn't get one.\n";
            return std::nullopt;
        }

        while (true) {
            const Token& current_token = parser.at();
            const Token::Type current_type = current_token.type;

            if (!isBinaryOperator(current_token)) {
                std::cout << "Breaking from loop (" + current_token.toString("") + ") was not a binary operator\n";
                break;
            }

            if (static_cast<int>(getBinaryOperatorPrecedence(current_type)) < min_prec) {
                break;
            }

            OperatorPrecedence op = getBinaryOperatorPrecedence(current_type);
            Associativity assoc = getBinaryAssociativity(current_type);

            int next_min_prec = static_cast<int>(op);
            if (assoc == Associativity::Left) {
                next_min_prec += 1;
            }

            parser.advance();
            std::optional<ExpressionNode> right = parseExpressionExpr(parser, next_min_prec);
            if (!right.has_value()) {
                throw std::runtime_error("Expected expression on right side of binary operator.");
            }
            left = createBinaryNode(parser, current_type, left.value(), right.value());
        }
        return left;
    }

    // ==== Continuation of Parser Code ====

    std::optional<ExpressionNode> Parser::parseExpression () {
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

        //return parseExpression_Root(*this);
        return parseExpressionExpr(*this, 1);
    }

    Util::Result<IdentifyingNameNode> Parser::tryParseIdentifyingName () {
        pushIndice();

        if (is(Token::Type::Identifier)) {
            // We know we're trying to parse an identifying name, so rather than Raw we go for Literal identifier
            auto temp = LiteralIdentifierNode(at());
            advance();
            transformIndice();
            return IdentifyingNameNode(temp);
        }

        popIndice();
        return "Expected identifying name (ex: an identifier), but got: " + at().toString("");
    }
}