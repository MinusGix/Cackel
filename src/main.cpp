#include <string>
#include <vector>
#include <ostream>

#include <iostream>

#include "Util/util.hpp"

#include "Lexer/Lexer.hpp"

#include "Parser/Parser.hpp"
#include "Parser/Pass/FunctionExitVerifier/FunctionExitVerifier.hpp"
#include "Parser/Pass/NumberConverter/NumberConverter.hpp"

#include "Compiler/Compiler.hpp"

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
		return std::move(parser.nodes);
	}

	void compile (Parser::ParentASTNode&& nodes, std::ostream& output) {
		Compiler::Compiler compiler(std::move(nodes));
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

		Parser::ParentASTNode parsed_nodes = parse(std::move(tokens));
		if (log_nodes) {
			std::cout << parsed_nodes.toString("  ") << "\n";
		}

		ParserPass::FunctionExitVerifier exit_verifier(parsed_nodes);
		ParserPass::NumberConverter number_converter(parsed_nodes);

		compile(std::move(parsed_nodes), output_stream);
	}
};

int main (int argc, char* argv[]) {
	if (argc < 2) {
		std::cerr << "No supplied input file. (ex: test.l)\n";
		return 1;
	}

	std::string input_filename = argv[1];
	std::cout << "Input Filename: " << input_filename;

	// We do not use std::unique_ptr here as we may assign an ostream we do not own to it
	// TODO: it might be better to make a util class for PotentiallyOwningUniquePtr (name needs work)
	//       that requires knowledge of whether it owns the pointer (default yes to make it work with 
	//       generic smart-pointer funcs)
	std::ostream* output_stream = nullptr;
	bool output_stream_owned = false;

	if (argc >= 3) {
		output_stream = new std::ofstream(std::string(argv[2]));
		output_stream_owned = true;
	} else {
		output_stream = &std::cout;
		output_stream_owned = false;
		std::cout << "No output filename specified (ex: test.ll), defaulting to stdio.\n";
	}

	CompilerState state = CompilerState(*output_stream);
	state.setInputFilename(std::string(argv[1]));
	state.log_tokens = true;
	state.log_nodes = true;

	state.performFullTrip();



	// Deconstruction logic
	if (output_stream_owned) {
		delete output_stream;
	}

	return 0;
}
