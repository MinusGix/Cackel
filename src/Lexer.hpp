#pragma once

#include <variant>
#include <vector>
#include <string>
#include <optional>

#include "util.hpp"

namespace Lexer {
	struct Token {
		enum class Type {
			Identifier,
			DecInteger,
			HexInteger,
			OctInteger,
			BinInteger,
			Float, // Called float but it could be a double

			LBracket,     // {
			RBracket,     // }
			LSquare,      // [
			RSquare,      // ]
			LParen,       // (
			RParen,       // )
			LAngle,       // <
			RAngle,       // >

			Equals,       // =
			Period,       // .
			Comma,        // ,
			Exclamation,  // !
			Question,     // ?
			Forwardslash, // /
			Backslash,    // you know
			Colon,        // :
			Semicolon,    // ;
			Quote,        // '
			DoubleQuote,  // "

			Plus,         // +
			PlusPlus,     // ++
			Minus,        // -
			MinusMinus,   // --
			Star,         // *
			StarStar,     // **
			Ampersand,    // &
			Caret,        // ^
			Percent,      // %
			Hash,         // #
			At,           // @
			Tilde,        // ~
			Grave,        // `
		};

		/// Turns the type into it's enum name (or close enough)
		static std::string typeToString (Type type);

		struct EmptyData {
			std::string toString (const std::string&) const {
				return "";
			}
		};
		struct IdentifierData : public EmptyData {
			std::string data;
			explicit IdentifierData (std::string&& t_data) : data(t_data) {}
			std::string toString (const std::string&) const {
				return "Identifier[" + data + "]";
			}
		};
		struct LiteralData : public EmptyData {
			std::string data;
			explicit LiteralData (std::string&& t_data) : data(t_data) {}
			std::string toString (const std::string&) const {
				return "Literal[" + data + "]";
			}
		};
		struct NumberData : public LiteralData {
			explicit NumberData (std::string&& t_data) : LiteralData(std::move(t_data)) {}
		};

        Util::SourceLocation location;

		// I can't make this constant because it seems to kill the copy constructor...
		Type type;
		// I dislike this, but it seems the 'best' choice without resorting complex template shenanigans or virtuals
		using DataType = std::variant<
			EmptyData,
			IdentifierData,
			LiteralData,
			NumberData
		>;

		DataType data = EmptyData{};

		explicit Token (size_t t_index, size_t t_line_index, Type t_type);
		explicit Token (size_t t_index, size_t t_line_index, Type t_type, DataType t_data);
		bool is (Type is_type) const;
		bool isOne (Type type1, Type type2) const;
		template<typename... Types>
		bool isOne (Type type1, Type type2, Types... types) const {
			return is(type1) || isOne(type2, types...);
		}

		bool isInteger () const;

		bool isFloat () const;

		bool isNumber () const;

        template<typename T>
        T& getData () {
			return std::get<T>(data);
        }
        template<typename T>
        const T& getData () const {
			return std::get<T>(data);
        }

		std::string toString (const std::string& indent) const;
	};

	class Lexer : public Util::IndexStack {
		public:

		std::string file_contents;

		size_t line_index = 0;

		// Ignores whitespace
		bool isStartOfLine = true;

		explicit Lexer (std::string t_file_contents);

		char at (std::optional<size_t> ind=std::nullopt) const;
		char atr (size_t offset) const;
		void advance ();
		char atAndAdvance ();
		bool indexValid () const;

		Token makeToken (Token::Type type) const;
		Token makeToken (Token::Type type, Token::DataType data) const;

		std::vector<Token> lex ();

		std::optional<Token> lexToken ();

		bool lexComments ();

		std::string lexIdentifier (size_t slice_start);

		Token lexNumber (size_t start_index);

		template<Token::Type TokenValue, bool (*Validator)(char)>
		Token lexBaseInteger (const std::string& base_name, bool has_opening) {
			const size_t start = getIndex();
			size_t amount = 0;

			// Skip over anything like 0x, 0b, 0d, 0o
			if (has_opening) {
				amount += 2;
				advance();
				advance();
			}

			while (Validator(at())) {
				advance();
				amount++;
			}
			if (Util::isAlphabeticCharacter(at())) {
				throw std::runtime_error("Unexpected alphabetic character after " + base_name + " number: " + std::to_string(at()));
			}

			return makeToken(TokenValue, Token::NumberData(file_contents.substr(start, amount)));
		}

		Token lexHexadecimalInteger (bool has_opening);
		Token lexBinaryInteger (bool has_opening);
		Token lexOctalInteger (bool has_opening);
		Token lexDecimalInteger (bool has_opening);
		Token lexDecimalNumber ();
	};
}