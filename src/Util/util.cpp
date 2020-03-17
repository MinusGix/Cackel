#include "util.hpp"

namespace Util {
    std::string File::readTextWhole (const std::string& filename) {
        std::ifstream file(filename);
	    // Read entire file into memory.
	    return std::string((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    }

    // ==== IndexStack ====
    const size_t& IndexStack::getIndex () const {
        return indices.at(indices.size() - 1);
    }
    size_t& IndexStack::getIndex () {
        return indices.at(indices.size() - 1);
    }

    size_t IndexStack::popIndice () {
        const size_t value = getIndex();
        if (indices.size() == 1) {
            return value;
        } else {
            indices.pop_back();
            return value;
        }
    }

    void IndexStack::pushIndice () {
        pushIndice(getIndex());
    }
    void IndexStack::pushIndice (size_t index) {
        indices.push_back(index);
    }
    void IndexStack::transformIndice () {
        if (indices.size() == 1) {
            return;
        }
        indices.at(indices.size() - 2) = indices.at(indices.size() - 1);
        popIndice();
    }
    /// Makes it slightly easier to to do a if(x){transformIndice();}else{popIndice();}
    /// so you can do slideIndice(x);
    void IndexStack::slideIndice (bool overwrite) {
        if (overwrite) {
            transformIndice();
        } else {
            popIndice();
        }
    }

    void IndexStack::advance () {
        getIndex()++;
    }

    /// ==== Other Utilities ====

    Base getBaseSimple (const std::string& number) {
        const size_t size = number.size();
        size_t start = 0;
        // TODO: check for + or -

        if (number.find('.') != std::string::npos) {
            return Base::Decimal; // We only parse floats as decimal
        }

        if (size >= (start + 2) && number.at(start) == '0') {
            char next = number.at(start + 1);
            if (next == 'b') {
                return Base::Binary;
            } else if (next == 'o') {
                return Base::Octal;
            } else if (next == 'd') {
                return Base::Decimal;
            } else if (next == 'x') {
                return Base::Hexadecimal;
            }

            if (!isDecimalDigit(next)) {
                throw std::runtime_error("getBase seemingly doesn't consider all properties properly");
            }
        }
        return Base::Decimal;
    }
}