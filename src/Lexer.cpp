#include "Lexer.hpp"

namespace Lexer {
    // ==== Token Utilities ====
    std::string Token::typeToString (Type type) {
        switch (type) {
            case Type::Identifier: return "Identifier";
            case Type::DecInteger: return "DecInteger";
            case Type::HexInteger: return "HexInteger";
            case Type::OctInteger: return "OctInteger";
            case Type::BinInteger: return "BinInteger";
            case Type::Float: return "Float";
            case Type::LBracket: return "LBracket";
            case Type::RBracket: return "RBracket";
            case Type::LSquare: return "LSquare";
            case Type::RSquare: return "RSquare";
            case Type::LParen: return "LParen";
            case Type::RParen: return "RParen";
            case Type::LAngle: return "LAngle";
            case Type::RAngle: return "RAngle";
            case Type::Equals: return "Equals";
            case Type::Period: return "Period";
            case Type::Comma: return "Comma";
            case Type::Exclamation: return "Exclamation";
            case Type::Question: return "Question";
            case Type::Forwardslash: return "Forwardslash";
            case Type::Backslash: return "Backslash";
            case Type::Colon: return "Colon";
            case Type::Semicolon: return "Semicolon";
            case Type::Quote: return "Quote";
            case Type::DoubleQuote: return "DoubleQuote";
            case Type::Plus: return "Plus";
            case Type::PlusPlus: return "PlusPlus";
            case Type::Minus: return "Minus";
            case Type::MinusMinus: return "MinusMinus";
            case Type::Star: return "Star";
            case Type::StarStar: return "StarStar";
            case Type::Ampersand: return "Ampersand";
            case Type::Caret: return "Caret";
            case Type::Percent: return "Percent";
            case Type::Hash: return "Hash";
            case Type::At: return "At";
            case Type::Tilde: return "Tilde";
            case Type::Grave: return "Grave";
        }
    }


    // ==== Token ====
    Token::Token (size_t t_index, size_t t_line_index, Type t_type) : location(t_index, t_line_index), type(t_type) {}
    Token::Token (size_t t_index, size_t t_line_index, Type t_type, DataType t_data) : location(t_index, t_line_index), type(t_type), data(t_data) {}

    bool Token::is (Type is_type) const {
        return type == is_type;
    }
    bool Token::isOne (Type type1, Type type2) const {
        return is(type1) || is(type2);
    }
    bool Token::isInteger () const {
        return isOne(Type::BinInteger, Type::OctInteger, Type::DecInteger, Type::HexInteger);
    }

    bool Token::isFloat () const {
        return is(Type::Float);
    }

    bool Token::isNumber () const {
        return isInteger() || isFloat();
    }

    std::string Token::toString (const std::string& indent) const {
        return Token::typeToString(type) + " " + Util::toString(data, indent);
    }

    // ==== Lexer Utilities ====

    // === Lexer ====

    Lexer::Lexer (std::string t_file_contents) : file_contents(t_file_contents) {}

    char Lexer::at (std::optional<size_t> ind) const {
        if (indexValid()) {
            return file_contents.at(ind.value_or(getIndex()));
        } else {
            return '\0';
        }
    }
    char Lexer::atr (size_t offset) const {
        return at(getIndex() + offset);
    }
    void Lexer::advance () {
        getIndex()++;
    }
    char Lexer::atAndAdvance () {
        char value = at();
        advance();
        return value;
    }
    bool Lexer::indexValid () const {
        return getIndex() < file_contents.size();
    }

    Token Lexer::makeToken (Token::Type type) const {
        return Token(getIndex(), line_index, type);
    }
    Token Lexer::makeToken (Token::Type type, Token::DataType data) const {
        return Token(getIndex(), line_index, type, data);
    }

    std::vector<Token> Lexer::lex () {
        std::vector<Token> tokens;
        while (indexValid()) {
            std::optional<Token> token = lexToken();
            if (token.has_value()) {
                tokens.push_back(token.value());
            }
        }

        return tokens;
    }

    std::optional<Token> Lexer::lexToken () {
        // get rid of simple whitespace

        // strip ' '|'\t'
        while (at() == ' ' || at() == '\t') {
            getIndex()++;
        }

        char chr = atAndAdvance();
        Token::Type type;

        // switch on characters and create token

        switch (chr) {
            case '\0':
                throw std::runtime_error("Unexpected null byte in file. (This could also be caused by a weird bug in compiler!)");
            case '{':
                type = Token::Type::LBracket;
                break;
            case '}':
                type = Token::Type::RBracket;
                break;
            case '[':
                type = Token::Type::LSquare;
                break;
            case ']':
                type = Token::Type::RSquare;
                break;
            case '(':
                type = Token::Type::LParen;
                break;
            case ')':
                type = Token::Type::RParen;
                break;
            case '<':
                type = Token::Type::LAngle;
                break;
            case '>':
                type = Token::Type::RAngle;
                break;

            case '=':
                type = Token::Type::Equals;
                break;
            case '.':
                type = Token::Type::Period;
                break;
            case ',':
                type = Token::Type::Comma;
                break;
            case '!':
                type = Token::Type::Exclamation;
                break;
            case '?':
                type = Token::Type::Question;
                break;
            case '/':
            {
                bool result = lexComments();
                if (result) {
                    return std::nullopt;
                }
                type = Token::Type::Forwardslash;
                break;
            }
            case '\\':
                type = Token::Type::Backslash;
                break;
            case ':':
                type = Token::Type::Colon;
                break;
            case ';':
                type = Token::Type::Semicolon;
                break;
            case '\'':
                type = Token::Type::Quote;
                break;
            case '"':
                type = Token::Type::DoubleQuote;
                break;
            case '+':
                if (at() == '+') {
                    advance();
                    type = Token::Type::PlusPlus;
                } else {
                    type = Token::Type::Plus;
                }
                break;
            case '-':
                if (at() == '-') {
                    advance();
                    type = Token::Type::MinusMinus;
                } else {
                    type = Token::Type::Minus;
                }
                break;
            case '*':
                if (at() == '*') {
                    advance();
                    type = Token::Type::StarStar;
                } else {
                    type = Token::Type::Star;
                }
                break;
            case '&':
                type = Token::Type::Ampersand;
                break;
            case '^':
                type = Token::Type::Caret;
                break;
            case '%':
                type = Token::Type::Percent;
                break;
            case '#':
                type = Token::Type::Hash;
                break;
            case '@':
                type = Token::Type::At;
                break;
            case '~':
                type = Token::Type::Tilde;
                break;
            case '`':
                type = Token::Type::Grave;
                break;
            case ' ':
            case '\t':
            case '\v':
                // ignore, this is a bit wasteful since there's likely multiple whitespace and it has to go through again
                return std::nullopt;
            case '\n':
                line_index++;
                return std::nullopt;

            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                return lexNumber(getIndex() - 1);

            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
            case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
            case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
            case 'V': case 'W': case 'X': case 'Y': case 'Z':
            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
            case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
            case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
            case 'v': case 'w': case 'x': case 'y': case 'z':
            case '$':
            case '_':
                return makeToken(Token::Type::Identifier, Token::IdentifierData(lexIdentifier(getIndex() - 1)));

            default:
                throw std::runtime_error("Unexpected character: " + std::to_string(chr));
        }

        return makeToken(type);
    }

    bool Lexer::lexComments () {
        if (at() != '/') {
            return false;
        }

        advance();
        while (indexValid() && at() != '\n') {
            advance();
        }
        if (indexValid() && at() == '\n') {
            advance();
        }

        return true;
    }

    std::string Lexer::lexIdentifier (size_t slice_start) {
        pushIndice(slice_start);

        size_t amount = 0;

        if (!Util::isIdentifierBeginCharacter(at(slice_start))) {
            throw std::runtime_error("[internal] lexIdentifier called when first character was not a valid identifier beginning character.");
        }

        amount++;
        advance();

        while (Util::isIdentifierCharacter(at())) {
            amount++;
            advance();
        }

        transformIndice();

        return file_contents.substr(slice_start, amount);
    }

    Token Lexer::lexNumber (size_t start_index) {
        pushIndice(start_index);

        char chr = at();

        std::optional<Token> token;
        if (chr == '0') {
            char next = atr(1);
            if (next == 'b') {
                token = lexBinaryInteger(true);
            } else if (next == 'o') {
                token = lexOctalInteger(true);
            } else if (next == 'x') {
                token = lexHexadecimalInteger(true);
            } else if (next == 'd') {
                token = lexDecimalInteger(true);
            }
        }
        if (!token.has_value()) {
            token = lexDecimalNumber();
        }
        transformIndice();
        return token.value();
    }

    Token Lexer::lexHexadecimalInteger (bool has_opening) {
        return lexBaseInteger<Token::Type::HexInteger, Util::isHexadecimalDigit>("hexadecimal", has_opening);
    }
    Token Lexer::lexBinaryInteger (bool has_opening) {
        return lexBaseInteger<Token::Type::BinInteger, Util::isBinaryDigit>("binary", has_opening);
    }
    Token Lexer::lexOctalInteger (bool has_opening) {
        return lexBaseInteger<Token::Type::OctInteger, Util::isOctalDigit>("octal", has_opening);
    }
    Token Lexer::lexDecimalInteger (bool has_opening) {
        return lexBaseInteger<Token::Type::DecInteger, Util::isDecimalDigit>("decimal", has_opening);
    }
    Token Lexer::lexDecimalNumber () {
        const size_t start = getIndex();
        size_t amount = 0;
        bool seen_decimal = false;

        // TODO: support 10e+42
        while (Util::isDecimalDigit(at()) || at() == '.') {
            if (at() == '.') {
                if (seen_decimal) {
                    throw std::runtime_error("Found multiple decimals in number.");
                }
                seen_decimal = true;
            }
            advance();
            amount++;
        }
        if (Util::isAlphabeticCharacter(at())) {
            throw std::runtime_error("Unexpected alphabetic character after decimal number: " + std::to_string(at()));
        }

        Token::Type type;
        if (seen_decimal) {
            type = Token::Type::Float;
        } else {
            type = Token::Type::DecInteger;
        }
        return makeToken( Token::Type::DecInteger, Token::NumberData(file_contents.substr(start, amount)));
    }
}