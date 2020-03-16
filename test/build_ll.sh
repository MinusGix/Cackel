# ensure it doesn't use old files
rm start.o
rm $1.o
rm ./a.out


echo Assembling start.s
nasm -felf64 ./start.s

echo Generating object file from llvm ir
llc -filetype=obj $1.ll

echo Linking files.
ld.lld -pie --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 \
    -o a.out \
    -z notext \
    ./start.o \
    ./$1.o

# make sure it's executable
chmod +x ./a.out