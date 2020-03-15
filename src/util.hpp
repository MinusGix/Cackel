#pragma once

#include <variant>
#include <string>
#include <memory>

// TODO: move this into util and `using ..` it or something.
template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

namespace Util {
    enum class Base {
        Binary,
        Octal,
        Decimal,
        Hexadecimal,
    };
    constexpr bool isLowercaseCharacter (char c) {
        return c >= 'a' && c <= 'z';
    }
    constexpr bool isUppercaseCharacter (char c) {
        return c >= 'A' && c <= 'Z';
    }
    constexpr bool isAlphabeticCharacter (char c) {
        return isLowercaseCharacter(c) || isUppercaseCharacter(c);
    }
    constexpr bool isBinaryDigit (char c) {
        return c == '0' || c == '1';
    }
    constexpr bool isOctalDigit (char c) {
        return c >= '0' && c <= '7';
    }
    constexpr bool isDecimalDigit (char c) {
        return c >= '0' && c <= '9';
    }
    constexpr bool isHexadecimalDigit (char c) {
        return isDecimalDigit(c) || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
    }


    constexpr bool isIdentifierBeginCharacter (char c) {
        return isAlphabeticCharacter(c) || c == '$' || c == '_';
    }
    constexpr bool isIdentifierCharacter (char c) {
        return isIdentifierBeginCharacter(c) || isDecimalDigit(c);
    }

    template<typename Func, typename Type>
    std::string mapJoin (const std::vector<Type>& data, Func func, const std::string& join_with, bool include_last=false) {
        std::string result = "";
        for (size_t i = 0; i < data.size(); i++) {
            result += func(data.at(i));
            if (include_last || i != (data.size() - 1)) {
                result += join_with;
            }
        }
        return result;
    }

    template<class T>
    struct is_variant : std::false_type {};
    template<class... Args>
    struct is_variant<std::variant<Args...>> : std::true_type {};
    template<class T>
    constexpr bool is_variant_v = is_variant<T>::value;

    // TODO: it'd be nice to automatically support std::unique_ptr<Type>
    template<class T>
    std::string toString(T t, const std::string& indent) {
        if constexpr(std::is_pointer_v<T>) {
            return toString(*t, indent);
        } else if constexpr(is_variant_v<T>) {
            return std::visit([&indent](auto&& v){ return toString(v, indent); }, t);
        } else {
            return t.toString(indent);
        }
    }

    template<typename Type>
    std::string stringJoin (const std::vector<Type>& data, const std::string& join_with, const std::string& indent, bool include_last=false) {
        return mapJoin(data, [indent] (auto&& x) { return Util::toString(x, indent); }, join_with, include_last);
    }

    /// Copyable unique ptr, holds a unique ptr inside it
    /// Note: Does NOT support virtual-esque polymorphic types.
    /// This is purely intended for normal classes that you know the type of as it uses that to construct it.
    template<typename T, typename D = std::default_delete<T>>
    struct DeepUniquePtr {
        using unique_ptr = std::unique_ptr<T, D>;

        unique_ptr value;

        using pointer = typename unique_ptr::pointer;
        using element_type = typename unique_ptr::element_type;
        using deleter_type = typename unique_ptr::deleter_type;

        // Default constructor
        constexpr DeepUniquePtr () noexcept : value() {}

        // Takes ownership of a pointer
        explicit DeepUniquePtr (pointer ptr) noexcept : value(ptr) {}

        // We don't provide construction with the deleter

        constexpr DeepUniquePtr (std::nullptr_t) noexcept : value(nullptr) {}

        // Move constructor

        DeepUniquePtr (DeepUniquePtr&& v) noexcept : value(v.release()) {}

        // Copy constructor
        DeepUniquePtr (const DeepUniquePtr& v) noexcept {
            if (v) {
                reset(new T(*v));
            } else {
                reset();
            }
        }

        // Assignment
        DeepUniquePtr& operator= (DeepUniquePtr&& v) noexcept {
            value = std::move(v.value);
            return *this;
        }

        DeepUniquePtr& operator= (std::nullptr_t) noexcept {
            reset();
            return *this;
        }

        // Copy assignment
        DeepUniquePtr& operator= (const DeepUniquePtr& v) noexcept {
            if (v) {
                reset(new T(*v));
            } else {
                reset();
            }
        }

        typename std::add_lvalue_reference<element_type>::type operator* () const {
            return *get();
        }

        pointer operator-> () const noexcept {
            return get();
        }

        pointer get () const noexcept {
            return value.get();
        }

        deleter_type& get_deleter () noexcept {
            return value.get_deleter();
        }

        const deleter_type& get_deleter () const noexcept {
            return value.get_deleter();
        }

        explicit operator bool () const noexcept {
            if (value) {
                return true;
            } else {
                return false;
            }
        }

        pointer release () noexcept {
            return value.release();
        }

        void reset (pointer p=nullptr) noexcept {
            return value.reset(p);
        }
    };

    struct SourceLocation {
        size_t index;
        size_t line_index;
        SourceLocation (size_t t_index, size_t t_line_index) : index(t_index), line_index(t_line_index) {}
    };

    struct IndexStack {
        protected:
        std::vector<size_t> indices = {0};

        public:
        const size_t& getIndex () const {
            return indices.at(indices.size() - 1);
        }
        size_t& getIndex () {
            return indices.at(indices.size() - 1);
        }

        size_t popIndice () {
            const size_t value = getIndex();
            if (indices.size() == 1) {
                return value;
            } else {
                indices.pop_back();
                return value;
            }
        }

        void pushIndice () {
            pushIndice(getIndex());
        }
        void pushIndice (size_t index) {
            indices.push_back(index);
        }
        void transformIndice () {
            if (indices.size() == 1) {
                return;
            }
            indices.at(indices.size() - 2) = indices.at(indices.size() - 1);
            popIndice();
        }
        /// Makes it slightly easier to to do a if(x){transformIndice();}else{popIndice();}
        /// so you can do slideIndice(x);
        void slideIndice (bool overwrite) {
            if (overwrite) {
                transformIndice();
            } else {
                popIndice();
            }
        }

        void advance () {
            getIndex()++;
        }
    };

    /// Does simple checks for the base of the number
    /// Does not check for negative/positive number, so don't pass in a number starting with +/-
    static Base getBaseSimple (const std::string& number) {
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

    /// Only wants pointer type
    /// Checks list for nullptr
    template<typename It>
    bool isValidPointerList (It first, It last) {
        for (auto it = first; it != last; it++) {
            auto* value = *it;
            if (value == nullptr) {
                return false;
            }
        }
        return true;
    }
}