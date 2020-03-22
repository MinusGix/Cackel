#pragma once

#include <string>
#include <vector>
#include <optional>
#include <memory>

#include "../Util/util.hpp"
#include "../Lexer/Lexer.hpp"
#include "Nodes.hpp"

namespace Parser {
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
        FunctionSignatureInfo parseFunctionSignature ();
        std::unique_ptr<FunctionNode> parseFunction ();
        std::unique_ptr<ExternNode> parseExtern ();
        std::unique_ptr<TypeNode> parseType ();
        std::unique_ptr<StatementASTNode> parseStatement ();
        std::unique_ptr<StatementASTNode> parseStatement_expression ();
        std::unique_ptr<VariableStatementNode> parseStatement_variableDeclaration ();
        std::unique_ptr<ReturnStatementNode> parseStatement_return ();
        std::unique_ptr<IfStatementNode> parseStatement_if ();
        Util::Result<std::unique_ptr<FunctionCallNode>> tryParseFunctionCall ();

        std::unique_ptr<BaseASTNode> parseExpression ();
        Util::Result<std::unique_ptr<BaseASTNode>> tryParseIdentifyingName ();
    };
}