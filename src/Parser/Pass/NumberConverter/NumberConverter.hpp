#pragma once

#include "../../Parser.hpp"
#include <optional>

namespace ParserPass {
    struct NumberConverter {
        Parser::ParentASTNode& nodes;

        std::map<std::string, Parser::PrimordialType> variable_types;
        explicit NumberConverter (Parser::ParentASTNode& nodes);
        std::unique_ptr<Parser::BaseASTNode> transformExpression (std::unique_ptr<Parser::BaseASTNode>& expression, std::optional<Parser::PrimordialType> expected_type);
        void transform ();
    };
}