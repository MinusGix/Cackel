#pragma once

#include <llvm/Support/raw_ostream.h>
#include <ostream>

namespace Util::LLVM {
    /// A class to bridge between an std::ostream instance and llvm::raw_ostream
    class OStreamBridge : public llvm::raw_ostream {
        protected:
        std::ostream& output_stream;

        public:

        explicit OStreamBridge (std::ostream& output) : llvm::raw_ostream(), output_stream(output) {}

        private:
        void write_impl (const char* value, size_t size) override {
            output_stream.write(value, static_cast<std::streamsize>(size));
        }

        uint64_t current_pos () const override {
            return output_stream.tellp();
        }
    };
}