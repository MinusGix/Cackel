#pragma once

#include "../../Nodes.hpp"
#include "../../../Compiler/Compiler.hpp"

namespace ParserPass {
    struct FunctionExitVerifier {
        Parser::ParentASTNode& nodes;

        // Applies it's transformation immediately.
        explicit FunctionExitVerifier (Parser::ParentASTNode& t_nodes);

        void transform ();
        
        bool doesStatementsAlwaysReturn (std::unique_ptr<Parser::StatementASTNode>& statements);
    };
}