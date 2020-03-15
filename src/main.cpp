#include <string>
#include <optional>
#include <vector>
#include <fstream>
#include <streambuf>

#include <iostream>

#include "util.hpp"

#include "Lexer.hpp"

#include "Parser.hpp"

#include "Compiler.hpp"

int main () {
	std::ifstream file("../test/test.l");
	std::string content((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
	file.close();

	Lexer::Lexer lex(content);
	auto result = lex.lex();
	//for (auto&& token : result) {
	//	std::cout << token.toString("") << "\n";
	//}

	Parser::Parser parse(std::move(result));
	parse.parse();
	std::cout << parse.nodes.toString("") << "\n";

	Compiler::Compiler compiler(parse.nodes);
	compiler.compile();

	return 0;
}
