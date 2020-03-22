#include "Parser.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <stack>

namespace Parser {
    // ==== Node Utilities ===

    // ==== Nodes ====

    // ==== Parser Utilities ====

    // ==== Parser ====

    Parser::Parser (std::vector<Lexer::Token>&& t_tokens) : tokens(t_tokens) {}

    const Lexer::Token& Parser::at (std::optional<size_t> value) const {
        return tokens.at(value.value_or(getIndex()));
    }
    Lexer::Token& Parser::at (std::optional<size_t> value) {
        return tokens.at(value.value_or(getIndex()));
    }
    bool Parser::indexValid () const {
        return indexValid(getIndex());
    }
    bool Parser::indexValid (size_t value) const {
        return value < tokens.size();
    }
    bool Parser::is (Lexer::Token::Type type, std::optional<size_t> t_value) const {
        const size_t value = t_value.value_or(getIndex());
        if (indexValid(value)) {
            return at(value).is(type);
        } else {
            return false; // it doesn't exist. can't be a type
        }
    }
    bool Parser::isIdentifier (const std::string& expected_value, std::optional<size_t> value) const {
        if (is(Lexer::Token::Type::Identifier, value)) {
            const auto& data = at(value).getData<Lexer::Token::IdentifierData>();
            if (data.data == expected_value) {
                return true;
            }
        }
        return false;
    }
    bool Parser::isr (Lexer::Token::Type type, size_t offset) const {
        return is(type, getIndex() + offset);
    }
    const Lexer::Token& Parser::expect (Lexer::Token::Type type, std::optional<size_t> value) const {
        if (!is(type, value)) {
            throw std::runtime_error("Expected token of (" + Lexer::Token::typeToString(type) + ") got " + Lexer::Token::typeToString(at().type));
        }
        return at(value);
    }
    const Lexer::Token& Parser::expectIdentifier (const std::string& expected_value, std::optional<size_t> value) const {
        expect(Lexer::Token::Type::Identifier, value);
        const auto& data = at().getData<Lexer::Token::IdentifierData>();
        if (data.data != expected_value) {
            throw std::runtime_error("Unexpected identifier, expected: '" + expected_value + "' got: '" + data.data + "'");
        }
        return at(value);
    }

    // TODO: it'd be good to simply return the root node here rather than forcing us to access the 'internal' state of this class
    void Parser::parse () {
        while (indexValid()) {
            if (is(Lexer::Token::Type::Identifier)) {
                auto& data = at().getData<Lexer::Token::IdentifierData>();
                if (data.data == "func") {
                    // Function parsing
                    nodes.nodes.push_back(parseFunction());
                } else {
                    throw std::runtime_error("Unexpected identifier: " + at().toString(""));
                }
            } else {
                throw std::runtime_error("Unexpected token: " + at().toString(""));
            }
        }
    }
    std::unique_ptr<FunctionNode> Parser::parseFunction () {
        using Type = Lexer::Token::Type;

        expectIdentifier("func");         // func
        advance();

        /// unique_ptr<IdentifyingNameNode>
        Util::Result<std::unique_ptr<BaseASTNode>> identity = tryParseIdentifyingName();
        if (identity.holdsError()) {
            throw std::runtime_error(identity.getError());
        }

        std::unique_ptr<TypeNode> return_type = std::make_unique<PrimordialTypeNode>(PrimordialType::Void);

        // Function parameter list. (name1 : type1, name2: type2)
        expect(Type::LParen); // (
        advance();

        std::vector<std::unique_ptr<FunctionParameterInfo>> parameters;
        while (!is(Type::RParen)) {
            bool is_mutable = false;
            if (isIdentifier("mut")) {
                advance();
                is_mutable = true;
            }

            const Lexer::Token& name_token = expect(Type::Identifier); // name
            const std::string& name = name_token.getData<Lexer::Token::IdentifierData>().data;
            advance();
            expect(Type::Colon); // :
            advance();
            std::unique_ptr<TypeNode> type = parseType(); // type
            if (type == nullptr) {
                throw std::runtime_error("Got parameter name (" + name + ") and colon, but type information was invalid.");
            }

            parameters.push_back(std::make_unique<FunctionParameterInfo>(std::move(type), name, is_mutable));

            if (is(Type::Comma)) {
                advance();
                expect(Type::Identifier);
            }
        }

        expect(Type::RParen); // )
        advance();

        // TODO: parse possible modifiers here: (mut, reass)

        // Return type, -> type
        if (is(Type::Minus)) {
            advance();
            if (is(Type::RAngle)) { // ->
                advance();
                std::unique_ptr<TypeNode> written_type = parseType();

                if (written_type == nullptr) {
                    throw std::runtime_error("Failed parsing return type of function.");
                }
                return_type.reset(written_type.release());
            } else {
                throw std::runtime_error("Unexpected lone - after function parameters.");
            }
        }

        // opening brace
        expect(Type::LBracket); // {
        advance();

        // function body
        std::vector<std::unique_ptr<StatementASTNode>> body;

        while (true) {
            std::unique_ptr<StatementASTNode> statement = parseStatement();
            if (statement == nullptr) {
                break;
            }
            body.push_back(std::move(statement));
        }

        // closing brace
        expect(Type::RBracket); // }
        advance();

        //
        return std::make_unique<FunctionNode>(
            std::move(identity.get()),
            std::move(parameters),
            std::move(return_type),
            std::move(body)
        );
    }

    /// Returns nullptr if it could not parse the type.
    std::unique_ptr<TypeNode> Parser::parseType () {
        using Type = Lexer::Token::Type;
        pushIndice();

        const Lexer::Token& name_token = expect(Type::Identifier);
        const auto& name = name_token.getData<Lexer::Token::IdentifierData>();
        std::unique_ptr<TypeNode> node = nullptr;
        std::optional<PrimordialType> primordial_type = stringToPrimordialType(name.data);
        if (primordial_type.has_value()) {
            node = std::make_unique<PrimordialTypeNode>(primordial_type.value());
        } else {
            node = std::make_unique<UnknownTypeNode>();
        }
        advance();

        slideIndice(node != nullptr);
        return node;
    }

    std::unique_ptr<StatementASTNode> Parser::parseStatement () {
        pushIndice();

        std::unique_ptr<StatementASTNode> statement = nullptr;

        if (isIdentifier("let")) {
            statement = parseStatement_variableDeclaration();
        } else if (isIdentifier("return")) {
            statement = parseStatement_return();
        } else if (isIdentifier("if")) {
            statement = parseStatement_if();
        } else if (is(Lexer::Token::Type::Identifier)) {
            // TODO: this is a really iffy way of allowed expressions all by themselves..
            // make parseExpression become tryParseExpression so that you can recover.
            // TODO: also, making this only allow certain types of expressions would be nice.
            statement = parseStatement_expression();
        }

        slideIndice(statement != nullptr);
        return statement;
    }
    std::unique_ptr<StatementASTNode> Parser::parseStatement_expression () {
        std::unique_ptr<BaseASTNode> expression = parseExpression();
        expect(Lexer::Token::Type::Semicolon);
        advance();
        return std::make_unique<ExpressionStatementNode>(std::move(expression));
    }
    /*
        'let ' mut? identifier '=' expression ';'
    */
    std::unique_ptr<StatementASTNode> Parser::parseStatement_variableDeclaration () {
        using Type = Lexer::Token::Type;
        expectIdentifier("let");
        advance();

        bool is_mutable = false;
        if (isIdentifier("mut")) {
            is_mutable = true;
            advance();
        }

        // variable name
        /// IdentifyingNameNode*
        Util::Result<std::unique_ptr<BaseASTNode>> identity = tryParseIdentifyingName();
        if (identity.holdsError()) {
            throw std::runtime_error(identity.getError());
        }

        // : before type information. We don't do any automatic types yet
        expect(Type::Colon);
        advance();

        // Type
        std::unique_ptr<TypeNode> type = parseType();
        if (type == nullptr) {
            throw std::runtime_error("Expected type in declaration of variable.");
        }

        // Currently we require there to be a value it is set to. This might be changed in the future.
        // =
        expect(Type::Equals);
        advance();

        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> expression = parseExpression();
        if (expression == nullptr) {
            throw std::runtime_error("Expected expression to be what variable is set to.");
        }

        expect(Type::Semicolon);
        advance();

        return std::make_unique<VariableStatementNode>(
            std::move(identity.get()),
            std::move(type),
            std::move(expression),
            is_mutable
        );
    }
    // return returnstatementnode
    std::unique_ptr<StatementASTNode> Parser::parseStatement_return () {
        using Type = Lexer::Token::Type;
        expectIdentifier("return");
        advance();

        if (is(Type::Semicolon)) { // return;
            advance();
            return std::make_unique<ReturnStatementNode>();
        }

        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> expression = parseExpression();
        if (expression == nullptr) {
            throw std::runtime_error("Expected expression or no expression after `return`, got something that was not a valid expression.");
        }

        expect(Type::Semicolon);
        advance();

        return std::make_unique<ReturnStatementNode>(std::move(expression));
    }

    /// Note: does not consume the identifier, assumes it's already been consumed.
    static std::unique_ptr<ConditionalPart> parseIfPart (Parser& parser, bool should_have_conditional) {
        using Type = Lexer::Token::Type;
        /// ExpressionNode*
        std::unique_ptr<BaseASTNode> condition = nullptr;
        if (should_have_conditional) {
            parser.expect(Type::LParen);
            parser.advance();

            if (parser.is(Type::RParen)) {
                throw std::runtime_error("Expected condition in conditial statement, but got empty condition.");
            }

            condition = parser.parseExpression();
            if (condition == nullptr) {
                throw std::runtime_error("Expected condition with conditional statement.");
            }

            parser.expect(Type::RParen);
            parser.advance();
        }

        parser.expect(Type::LBracket);
        parser.advance();

        std::vector<std::unique_ptr<StatementASTNode>> body;
        while (true) {
            std::unique_ptr<StatementASTNode> statement = parser.parseStatement();
            if (statement == nullptr) {
                break;
            }
            body.push_back(std::move(statement));
        }

        parser.expect(Type::RBracket);
        parser.advance();

        return std::make_unique<ConditionalPart>(std::move(condition), std::move(body));
    }
    // TODO: return ifstatementnode instead..
    std::unique_ptr<StatementASTNode> Parser::parseStatement_if () {
        expectIdentifier("if");
        advance();

        std::unique_ptr<IfStatementNode> if_statement = std::make_unique<IfStatementNode>(parseIfPart(*this, true));

        // parse else if and else
        while (true) {
            if (isIdentifier("elif")) {
                advance();
                if_statement->parts.push_back(parseIfPart(*this, true));
            } else if (isIdentifier("else")) {
                advance();
                if_statement->parts.push_back(parseIfPart(*this, false));
                break; // else statement is the last.
            } else {
                break;
            }
        }
        return std::move(if_statement);
    }

    Util::Result<std::unique_ptr<FunctionCallNode>> Parser::tryParseFunctionCall () {
        using Type = Lexer::Token::Type;
        pushIndice();

        /// IdentifyingNameNode*
        Util::Result<std::unique_ptr<BaseASTNode>> identity = tryParseIdentifyingName();
        if (identity.holdsError()) {
            return identity.getError();
        }

        if (!is(Type::LParen)) {
            popIndice();
            return "Expected ( after function name.";
        }
        // it is LParen
        advance();

        // func()
        if (is(Type::RParen)) {
            advance();
            transformIndice();
            return std::make_unique<FunctionCallNode>(std::move(identity.get()));
        }


        // When the function call has parameters
        
        /// ExpressionNode*
        std::vector<std::unique_ptr<BaseASTNode>> expressions;

        while (true) {
            std::unique_ptr<BaseASTNode> expr = parseExpression();
            if (expr == nullptr) {
                popIndice();
                return "Expected closing parentheses, or expression, received neither.";
            }

            expressions.push_back(std::move(expr));

            if (is(Type::RParen)) {
                advance();
                transformIndice();
                return std::make_unique<FunctionCallNode>(std::move(identity.get()), std::move(expressions));
            } else if (is(Type::Comma)) {
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

    static std::unique_ptr<BaseASTNode> parseExpressionExpr (Parser& parser, int min_prec);

    static bool isBinaryOperator (const Lexer::Token& token) {
        using Type = Lexer::Token::Type;
        return token.isOne(Type::Plus, Type::Minus, Type::Star, Type::Forwardslash, Type::Percent);
    }
    static OperatorPrecedence getBinaryOperatorPrecedence (Lexer::Token::Type type) {
        using Type = Lexer::Token::Type;
        switch (type) {
            case Type::Plus: return OperatorPrecedence::Addition;
            case Type::Minus: return OperatorPrecedence::Subtraction;
            case Type::Star: return OperatorPrecedence::Multiplication;
            case Type::Forwardslash: return OperatorPrecedence::Division;
            case Type::Percent: return OperatorPrecedence::Modulo;
            case Type::StarStar: return OperatorPrecedence::Exponentiation;
            default:
                throw std::runtime_error("Unknown binary operator from token type: " + Lexer::Token::typeToString(type));
        }
    }

    static Associativity getBinaryAssociativity (Lexer::Token::Type type) {
        using Type = Lexer::Token::Type;
        switch (type) {
            // Something like exponent might be right associative
            case Type::StarStar:
                return Associativity::Right;
            default:
                return Associativity::Left;
        }
    }

    static bool isUnaryOperator (const Lexer::Token& token) {
        using Type = Lexer::Token::Type;
        return token.isOne(Type::Plus, Type::Minus);
    }
    static OperatorPrecedence getUnaryOperatorPrecedence (Lexer::Token::Type type) {
        using Type = Lexer::Token::Type;
        if (type == Type::Plus) {
            return OperatorPrecedence::UnaryPlus;
        } else if (type == Type::Minus) {
            return OperatorPrecedence::UnaryMinus;
        } else {
            throw std::runtime_error("Unknown unary operator from token: " + Lexer::Token::typeToString(type));
        }
    }

    // This is ew, but it should work
    static bool isIdentifyingName (Parser& parser) {
        parser.pushIndice();
        /// IdentifyingNameNode*
        Util::Result<std::unique_ptr<BaseASTNode>> identity = parser.tryParseIdentifyingName();
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
    /// returns ExpressionNode*
    static std::unique_ptr<BaseASTNode> createLiteral (Parser& parser) {
        const Lexer::Token& token = parser.at();
        if (isIdentifyingName(parser)) {
            // Try parsing a function call, but this will return an error message if it failed.
            // This is fine, since we then try to check if it's just a normal identifier
            Util::Result<std::unique_ptr<FunctionCallNode>> function_call = parser.tryParseFunctionCall();
            if (function_call.holds()) {
                return std::move(function_call.get());
            }

            // If it is a raw identifier then we check it for some special literals
            if (parser.is(Lexer::Token::Type::Identifier)) {
                if (parser.isIdentifier("true")) {
                    parser.advance();
                    return std::make_unique<LiteralBooleanNode>(true);
                } else if (parser.isIdentifier("false")) {
                    parser.advance();
                    return std::make_unique<LiteralBooleanNode>(false);
                }
            }

            // It wasn't a function call so we try parsing a normal identifier.
            /// IdentifyingNameNode*
            Util::Result<std::unique_ptr<BaseASTNode>> identity = parser.tryParseIdentifyingName();
            if (identity.holdsError()) {
                throw std::runtime_error("[Internal] Original error: '" + identity.getError() + "', but this should not have happened as directly before it is checked for if it is an identifying name.");
            }

            return std::move(identity.get());
        } else if (token.isNumber()) {
            parser.advance();
            return std::make_unique<LiteralNumberNode>(token);
        } else {
            throw std::runtime_error("Cannot make literal. Unknown token type: " + token.toString(""));
        }
    }

    /// Returns ExpressionNode*
    /// Owns [right]
    static std::unique_ptr<BaseASTNode> createUnaryNode (Parser&, Lexer::Token::Type type, std::unique_ptr<BaseASTNode>&& right) {
        using Type = Lexer::Token::Type;
        if (type == Type::Plus) {
            return std::make_unique<UnaryPlusExpressionNode>(std::move(right));
        } else if (type == Type::Minus) {
            return std::make_unique<UnaryMinusExpressionNode>(std::move(right));
        } else {
            throw std::runtime_error("[Internal] Failed in creating node for unary operator: " + Lexer::Token::typeToString(type) + " for expression: " + right->toString(""));
        }
    }
    /// Returns ExpressionNode*
    /// Owns [left] and [right]
    static std::unique_ptr<BaseASTNode> createBinaryNode (Parser&, Lexer::Token::Type type, std::unique_ptr<BaseASTNode>&& left, std::unique_ptr<BaseASTNode>&& right) {
        using Type = Lexer::Token::Type;
        if (type == Type::Plus) {
            return std::make_unique<AddExpressionNode>(std::move(left), std::move(right));
        } else if (type == Type::Minus) {
            return std::make_unique<SubtractExpressionNode>(std::move(left), std::move(right));
        } else if (type == Type::Star) {
            return std::make_unique<MultiplyExpressionNode>(std::move(left), std::move(right));
        } else if (type == Type::StarStar) {
            throw std::runtime_error("Impl exponents");
        } else {
            throw std::runtime_error("[Internal] Failed in creating node for binary operator: " + Lexer::Token::typeToString(type) + " for expressions: (" +
                left->toString("") + ", " + right->toString("") + ")"
            );
        }
    }

    /// Returns ExpressionNode*
    /// may return nullptr
    static std::unique_ptr<BaseASTNode> parseExpressionPart (Parser& parser) {
        using Type = Lexer::Token::Type;
        std::cout << "ParseExpressionPart " << Lexer::Token::typeToString(parser.at().type) << "\n";
        if (!parser.indexValid()) {
            std::cout << "We're past our stay. There's no more parts since index is past the end\n";
            return nullptr;
        } else if (parser.is(Type::LParen)) { // (expression)
            parser.advance();
            std::unique_ptr<BaseASTNode> expr = parseExpressionExpr(parser, 1);
            if (expr == nullptr) {
                throw std::runtime_error("Expected expression inside parentheses");
            }
            parser.expect(Type::RParen);
            parser.advance();
            return expr;
        } else if (isUnaryOperator(parser.at())) {
            const Lexer::Token& op_token = parser.at();
            OperatorPrecedence op = getUnaryOperatorPrecedence(op_token.type);
            parser.advance();
            std::unique_ptr<BaseASTNode> right = parseExpressionExpr(parser, static_cast<int>(op));
            if (right == nullptr) {
                throw std::runtime_error("Expected expression after unary operator.");
            }
            return createUnaryNode(parser, op_token.type, std::move(right));
        } else if (isBinaryOperator(parser.at())) {
            throw std::runtime_error("Expected part while parsing expression but got binary operator: " + parser.at().toString(""));
        } else if (isLiteral(parser)) {
            return createLiteral(parser);
        } else {
            throw std::runtime_error("Unexpected token: " + parser.at().toString(""));
        }
    }

    /// Returns ExpressionNode*
    static std::unique_ptr<BaseASTNode> parseExpressionExpr (Parser& parser, int min_prec) {
        std::cout << "parseExpressionExpr\n";
        /// ExpressNode*
        std::unique_ptr<BaseASTNode> left = parseExpressionPart(parser);
        if (left == nullptr) {
            std::cout << "Expected part but didn't get one.\n";
            return nullptr;
        }

        while (true) {
            const Lexer::Token& current_token = parser.at();
            const Lexer::Token::Type current_type = current_token.type;

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
            std::unique_ptr<BaseASTNode> right = parseExpressionExpr(parser, next_min_prec);
            if (right == nullptr) {
                throw std::runtime_error("Expected expression on right side of binary operator.");
            }
            left = createBinaryNode(parser, current_type, std::move(left), std::move(right));
        }
        return left;
    }

    // ==== Continuation of Parser Code ====

    /// Returns ExpressionNode*
    std::unique_ptr<BaseASTNode> Parser::parseExpression () {
        std::cout << "Parse expression *function*\n";
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

    /// Returns IdentifyingNameNode*
    Util::Result<std::unique_ptr<BaseASTNode>> Parser::tryParseIdentifyingName () {
        pushIndice();

        if (is(Lexer::Token::Type::Identifier)) {
            // We know we're trying to parse an identifying name, so rather than Raw we go for Literal identifier
            std::unique_ptr<LiteralIdentifierNode> temp = std::make_unique<LiteralIdentifierNode>(at());
            advance();
            transformIndice();
            return std::move(temp);
        }

        popIndice();
        return "Expected identifying name (ex: an identifier), but got: " + at().toString("");
    }
}