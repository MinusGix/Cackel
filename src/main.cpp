#include <string>
#include <vector>
#include <ostream>

#include <iostream>

#include "Util/util.hpp"

#include "Lexer.hpp"

#include "Parser.hpp"

#include "Compiler.hpp"

struct CompilerState {
	/// The filename to read the data from.
	std::string input_filename;
	/// The filename to output data to.
	/// An ostream is created from this if one is not given when needed.
	std::ostream& output_stream;

	bool log_tokens = false;
	bool log_nodes = false;

	explicit CompilerState (std::ostream& output) : output_stream(output) {}

	/// Sets the input filename. Likely has no effect if we've already read the data from it.
	void setInputFilename (std::string input) {
		input_filename = input;
	}

	/// Lexs the data. Reads in the file from the input.
	std::vector<Lexer::Token> lex () {
		std::string content = Util::File::readTextWhole(input_filename);

		Lexer::Lexer lexer(content);
		std::vector<Lexer::Token> tokens = lexer.lex();

		return tokens;
	}

	/// Parses the lexer output.
	Parser::ParentASTNode parse (std::vector<Lexer::Token>&& tokens) {
		Parser::Parser parser(std::move(tokens));
		parser.parse();
		return parser.nodes;
	}

	void compile (const Parser::ParentASTNode& nodes, std::ostream& output) {
		Compiler::Compiler compiler(nodes);
		compiler.compile(output);
	}

	void performFullTrip () {
		std::vector<Lexer::Token> tokens = lex();
		if (log_tokens) {
			std::cout << "Lexed Token:\n";
			for (const Lexer::Token& token : tokens) {
				std::cout << "\t" << token.toString("") << "\n";
			}
		}

		Parser::ParentASTNode nodes = parse(std::move(tokens));
		if (log_nodes) {
			std::cout << nodes.toString("  ") << "\n";
		}

		compile(nodes, output_stream);
	}
};

int main (int argc, char* argv[]) {
	if (argc < 2) {
		std::cerr << "No supplied input file. (ex: test.l)\n";
		return 1;
	}

	std::string input_filename = argv[1];
	std::cout << "Input Filename: " << input_filename;

	std::unique_ptr<std::ostream> output_stream = nullptr;

	if (argc >= 3) {
		output_stream.reset(new std::ofstream(std::string(argv[2])));
	} else {
		output_stream.reset(&std::cout);
		std::cout << "No output filename specified (ex: test.ll), defaulting to stdio.\n";
	}

	CompilerState state = CompilerState(*output_stream);
	state.setInputFilename(std::string(argv[1]));
	state.log_tokens = true;
	state.log_nodes = true;

	state.performFullTrip();

	return 0;
}
